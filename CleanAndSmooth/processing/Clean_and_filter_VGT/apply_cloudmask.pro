Function apply_cloudmask, fn_fapar_sc, fn_cloudmask, fn_fapar_sc_ms, ns, nl, nb

OPENR, R1, fn_fapar_sc, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
fileout=fn_fapar_sc_ms
OPENR, R2, fn_cloudmask, /GET_LUN
line_ass_mask = ASSOC(R2, BYTARR(ns,nb))

; Create files for output
IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

data=FLTARR(ns, nb)
mask=BYTARR(ns, nb)
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  data=float(line_ass_data[line])
  mask=line_ass_mask[line]
  ind=where(mask eq 1, count)
  if count ne 0 then data[ind]=!VALUES.F_NAN  
  WRITEU, W1, data
ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {fAPAR scaled and cloud screened}'
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
