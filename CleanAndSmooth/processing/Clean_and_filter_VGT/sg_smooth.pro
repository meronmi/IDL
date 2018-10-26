Function sg_smooth, filein, fileout, ns, nl, nb
; Input files are opened by performing ASSOC to one line
OPENR, R1, filein, /GET_LUN
line_ass_data = ASSOC(R1, INTARR(ns,nb))
; Create files for output
IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

IF FILE_TEST(fileout+'_prct_bad') eq 1 THEN FILE_DELETE, fileout
OPENW, W2, fileout+'_prct_bad', /GET_LUN, /APPEND

data=INTARR(ns, nb)
data_sm=data * 0 -999
data_bad=INTARR(ns)
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  data=line_ass_data[line]
  data_sm = data_sm * 0 -999
  FOR sample=0, ns-1, 1L DO BEGIN      ; loop over samples
;     savgolFilter = SAVGOL(5,5,0,4)
;     data_sm[sample,*] = CONVOL(reform(data[sample,*]),savgolFilter,/EDGE_TRUNCATE, /NAN)
;    if (sample eq 52) and (line eq 94) then begin
;      print, 'break'
;    endif
    Data1=reform(data[sample,*]);*10000.0    ;rescaled back to integer (for savgol)
    Cloud=indgen(nb)*0
    ;ind=where(finite(Data1) ne 1, count)
    ind=where(Data1 EQ -999, count)
    ;store how many data were discarded
    data_bad[sample] = FIX(count/FLOAT(N_ELEMENTS(Data1))*100)
    if count ne 0 then Cloud[ind]=1
    IF (count EQ N_ELEMENTS(Data1)) THEN BEGIN
      data_sm[sample,*]=-999 
    ENDIF ELSE BEGIN 
      SAGO_INTERPOL2, Data1, Cloud, smNDVI=sm_pix, iMAX=10
      data_sm[sample,*]=sm_pix ;/10000.0    ;scale back to standard NDVI floating
    ENDELSE 
        
    
    ;endif 
  ENDFOR 
  WRITEU, W1, data_sm
  WRITEU, W2, data_bad

ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {fAPAR scaled savgol5 filtered}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'

HEADER_OUT=fileout+'%bad''.hdr'
OPENW, 4, HEADER_OUT
printf,4,'ENVI'
printf,4,'description = {% of bad data}'
printf,4,'samples ='+STRCOMPRESS(ns)
printf,4,'lines   ='+STRCOMPRESS(nl)
printf,4,'bands   ='+STRCOMPRESS(1)
printf,4,'header offset = 0'
printf,4,'file type = ENVI Standard'
printf,4,'data type = 2'
printf,4,'interleave = bil'
printf,4,'sensor type = VGT'
printf,4,'byte order = 0'

CLOSE, /ALL
return, 1
End
