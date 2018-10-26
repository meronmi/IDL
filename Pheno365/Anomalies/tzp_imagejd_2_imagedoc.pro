Function TZP_imageJD_2_imageDOC, frstDOYofCycle, fname_in, fname_out, dtype, ns, nl, nb

; Purpose: Transform pheno indicator from JD (Julian day) to relative DOC (Day of Cycle) starting
; at frstDOYofCycle. Doc can be both <0 and >365 if the event is expecte in year Y and happens in
; Y-1 or y+1, respectively.


; Outcome: Write a file with the pheno indicator as DOC

; Results are stored in a file named fname_out +'_DOC'

;RESTRICTIONS
; - input file must be bil
; -999 in pheno product (expected but failed season) is kept

;INPUT

; frstDOYofCycle: first day (DOY) of the cycle (1 if calendar year, 2-365 otherwise)    
; fname_in:       file name with full path in
; fname_out:      base file name with full path out
; dtype:       datatype (2 integer, 4 float)
; ns, nl, nb:  number of samples, lines, bands

;OUTPUT
; none
;
;return code:
;0:     normal completion    
;10:    input file does not exists
;20:    invalid datatype


;EXAMPLE
;print, TZP_imageJD_2_imageDOC(1,'Q:\fromDek2DayDebug\pheno_products_day\REALIGN_ON_eos\A280eos-1997_sos1' , 'Q:\fromDek2DayDebug\pheno_products_day\REALIGN_ON_eos\A280eos-1997_sos1', 4,3586,3810,16)
 
IF FILE_TEST(fname_in) EQ 0 THEN return, 10
OPENR, R1, fname_in, /GET_LUN

fileout = fname_out+'_DOC'
IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

CASE dtype OF
  2: assline = ASSOC(R1, INTARR(ns,nb))
  4: assline = ASSOC(R1, FLTARR(ns,nb))
  ELSE: return, 20 
ENDCASE

data=FLTARR(ns, nb)
out=FLTARR(ns, nb)

;Load band names from HDR

IF (FILE_TEST(fname_in+'.hdr') NE 1) THEN BEGIN
  PRINT, 'TZP_imageJD_2_imageDOC Warning: hdr of input file was not found'
ENDIF ELSE BEGIN
  strarray=read_band_names(fname_in)
ENDELSE

;New method not assuming that the first band refer to the first year
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines  
  dataLine=float(assline[line]) ;+ abs_offset_dek_in
  dataLineOut = dataLine *!VALUES.F_NAN
  FOR column = 0, ns-1, 1L DO BEGIN
    ;IF (column EQ 3233) AND (line EQ 1115) THEN STOP
    ;IF (line EQ 2477) AND (column EQ 973) THEN STOP
    dataLineOut[column, *] = JD2DOC(REFORM(dataLine[column, *]), frstDOYofCycle)
  ENDFOR
  WRITEU, W1, dataLineOut
ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, lun, HEADER_OUT, /GET_LUN
PRINTF,lun,'ENVI'
PRINTF,lun,'description = TZP_imageJD_2_imageDOC of ' + string(fileout)
PRINTF,lun,'samples ='+STRCOMPRESS(ns)
PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
PRINTF,lun,'bands   ='+STRCOMPRESS(nb)
PRINTF,lun,'header offset = 0'
PRINTF,lun,'file type = ENVI Standard'
PRINTF,lun,'data type = 4'
PRINTF,lun,'interleave = bil'
PRINTF,lun,'byte order = 0'
PRINTF,lun,'values = {DOC, Number of days counting from DOY' + STRTRIM(frstDOYofCycle,2)+'}'
FOR i = 0, N_ELEMENTS(strarray)-1 DO PRINTF, lun, strarray[i]
CLOSE, /ALL

return, 0
End