FUNCTION avg3Dimage, fname, ns, nl, nb
IF FILE_TEST(fname) EQ 0 THEN BEGIN 
  PRINT, fname + ' was not found (avg3Dimage.pro)'
  RETURN, 10
ENDIF
in=FLTARR(ns,nl,nb)
OPENR, R1, fname, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
FOR line=0, nl-1, 1L DO in[*, line, *]=float(line_ass_data[line])
CLOSE, R1
;consider season failure (-999) as NaM
ind999 = WHERE(in EQ -999, count999)
IF (count999 GT 0) THEN in[ind999]=!VALUES.F_NAN
out=fltarr(ns,nl)
FOR s=0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    out(s,l)=mean(in[s,l,*], /NAN, /DOUBLE)
  ENDFOR
ENDFOR
RETURN, out
END