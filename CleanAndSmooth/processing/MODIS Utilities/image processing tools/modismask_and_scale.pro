;Function build_MODISmask, data_file_in, blu_file_in, file_data_out, file_mask_out, ns, nl, nb, blu_pc, search_lim, fract_threshold
Function MODISmask_and_scale, data_file_in, qc_file_in, ui_file_in, file_data_out, file_mask_out, ns, nl, nb, qcmax, uimax


; Input files are opened by performing ASSOC to one line
; Input is uscaled NDVI (it's scaled here) 


OPENR, R1, data_file_in, /GET_LUN
line_ass_data = ASSOC(R1, INTARR(ns,nb))
OPENR, R2, qc_file_in, /GET_LUN
line_ass_qc = ASSOC(R2, BYTARR(ns,nb))
OPENR, R3, ui_file_in, /GET_LUN
line_ass_ui = ASSOC(R3, BYTARR(ns,nb))

; Create files for output
IF FILE_TEST(file_data_out) eq 1 THEN FILE_DELETE, file_data_out
OPENW, W2, file_data_out, /GET_LUN, /APPEND

data = INTARR(ns, nb)
qc = BYTARR(ns, nb)
ui = qc


dataout=INTARR(ns, nb) * 0 - 999
countGood = 0
countLand = 0

FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  IF ((line mod 200) EQ 0) THEN print, STRTRIM(line/float(nl)*100, 2)+ ' % processed, ' +  SYSTIME()
  dataout = dataout * 0 - 999
  ; Now create per line the stack of all dates
  data=line_ass_data[line]
  qc=line_ass_qc[line]
  ui=line_ass_ui[line]
  
  indGoodQc = WHERE(qc LE 1, countGoodQc)
  indGoodUi = WHERE(ui LE 5, countGoodQi)
  indGood = SetIntersection(indGoodQc, indGoodUi)
  indLand = WHERE(data GT -999, countLineLand)
  indGood = SetIntersection(indGood, indLand)
  countGood = countGood + N_ELEMENTS(indGood)
  countLand = countLand + countLineLand
  IF (indGood[0] NE -1) THEN BEGIN
    dataout[indGood] = data[indGood];/10000.0  ;select a scale
  ENDIF ; ELSE they remain all nan
  
  WRITEU, W2, dataout
ENDFOR

PRINT, 'Retained % = ' + STRTRIM(countGood/FLOAT(countLand)*100.0,2)

; WRITE HEADER OF THE OUTPUT

HEADER_OUT2=file_data_out+'.hdr'
OPENW, W4, HEADER_OUT2, /GET_LUN
printf,W4,'ENVI'
printf,W4,'description = {Scaled product, screene with QC LE 1 and UI LE 5}'
printf,W4,'samples ='+STRCOMPRESS(ns)
printf,W4,'lines   ='+STRCOMPRESS(nl)
printf,W4,'bands   ='+STRCOMPRESS(nb)
printf,W4,'header offset = 0'
printf,W4,'file type = ENVI Standard'
printf,W4,'data type = 2'
printf,W4,'interleave = bil'
printf,W4,'sensor type = MODIS'
printf,W4,'byte order = 0'

CLOSE, /ALL
return, 1
End