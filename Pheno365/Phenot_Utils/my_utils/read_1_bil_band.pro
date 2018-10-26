FUNCTION read_1_BIL_band, fname, dt, ns, nl, nb, boi
;This function read a BIL and return a single band (boi, Band Of Interst)
;INPUT:
;-fname: full path file name
;-dt: datatype can be 2 or 4 (integer or float)
;-ns,nl,nb: number of sample, lines, bands
;-boi: band of interst to return (band 1 is boi=0)
;RETURNED:
;-the band of interest
;-10 if file was not found
;-20 if datatype is not implemnted
;EXAMPLE
; ngspy=read_1_BIL_band(path+'\'+fname_globstat, 2, ns, nl, 2, 1)
IF (dt NE 2) AND (dt NE 4) THEN RETURN, 20
IF FILE_TEST(fname) EQ 0 THEN RETURN, 10
OPENR, R1, fname, /GET_LUN
CASE dt OF
  2: BEGIN
      assline = ASSOC(R1, INTARR(ns,nb))
      data = INTARR(ns,nl,nb)
     END
  4: BEGIN
      assline = ASSOC(R1, FLTARR(ns,nb))
      data = FLTARR(ns,nl,nb)
     END
  ELSE: return, 20 
ENDCASE 

FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
    data[*,line,*]=float(assline[line])
ENDFOR
FREE_LUN, R1
RETURN, data[*,*,boi]
END