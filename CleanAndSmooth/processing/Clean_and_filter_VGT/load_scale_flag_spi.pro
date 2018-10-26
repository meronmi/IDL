Function load_scale_flag_spi, filein, fileout, ns, nl, nb, dt, gain, offset, maxval, minval, useBPinverseScaling
; Input files are opened by performing ASSOC to one line
OPENR, R1, filein, /GET_LUN
line_ass_data = ASSOC(R1, INTARR(ns,nb))
; Create files for output
IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

data=FLTARR(ns, nb)
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  ;if line eq 843 then stop
  data=float(line_ass_data[line])
  ind=where((data gt maxval) OR (data lt minval), count)
  
  ;snow treatment, MODIS fapar, remove after use!!!!!
  ;ind_snow=where(data eq 253, count_snow)
  if (count ne 0) then data[ind]=!VALUES.F_NAN
;  if (count_snow ne 0) then data[ind_snow]=0.0
  

  ;data = data * gain + offset
  IF (useBPinverseScaling EQ 1) THEN $
    data = (FLOAT(TEMPORARY(data)) - FLOAT(offset))/FLOAT(gain)  $
  ELSE $ 
    data = FLOAT(TEMPORARY(data)) * FLOAT(gain) + FLOAT(offset)
  WRITEU, W1, data
  ;Debug tentativo
  
;  FOR band=0, nb-1, 1L DO BEGIN
;    data[*,band]=float(line_ass_data[line+(band*LONG(nl))])
;    ind=where(data gt 250, count)
;    if (count ne 0) then data[ind]=!VALUES.F_NAN
;    data=data*0.005
;    WRITEU, W1, data
;  ENDFOR
ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {var scaled}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 4'
printf,3,'interleave = bil'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'


CLOSE, /ALL
return, 1
End
