; NAME:
;   build_gobron_cldmsk
;
; PURPOSE:
;   Build a cloud mask based on N. Gobron specification (used in fAPAR
;   computation from VGT data. The pixel is masked as cloud (or snow/oce) if
;   Rblu ge 0.3
;   OR Rred ge 0.5
;   OR Rnir ge 0.7
; CALLING SEQUENCE:
;   build_gobron_cldmsk, B_file_name, R_file_name, NIR_file_name, ns, nl, nb, output_file_name
;   
; INPUTS:
;   B/R/NIR_file_name:  names of bil files containing reflectances, can be multibands , they just have to match (e.g. decade 32 is band 32 for all) 
;   output_file_name:   filename of output cloud mask, multiband
;   ns, nl, nb:         number os sample, lines, bands
;   
;   all names are absolute path
;   file are in Vito format (to be scaled)
;
; OUTPUTS:
;   0/1: for failure/success of the function
;   cloud mask saved as  output_file_name

; MODIFICATION HISTORY:
;   Written by:  Michele, 15 feb 2011


Function build_gobron_cldmsk, B_file_name, R_file_name, NIR_file_name, ns, nl, nb, output_file_name

;gains
gb=0.0025   ;blue band
gr=0.0025   ;red
gnir=0.0033 ;nir

;open files performing ASSOC to one line
OPENR, R1, B_file_name, /GET_LUN
line_ass_b = ASSOC(R1, BYTARR(ns,nb))
OPENR, R2, R_file_name, /GET_LUN
line_ass_r = ASSOC(R2, BYTARR(ns,nb))
OPENR, R3, NIR_file_name, /GET_LUN
line_ass_nir = ASSOC(R3, BYTARR(ns,nb))

; Create files for output
IF FILE_TEST(output_file_name) eq 1 THEN FILE_DELETE, output_file_name
OPENW, W1, output_file_name, /GET_LUN, /APPEND

;tmp, svae scaled images
;OPENW, W2, B_file_name+'_sc', /GET_LUN, /APPEND
;OPENW, W3, R_file_name+'_sc', /GET_LUN, /APPEND
;OPENW, W4, NIR_file_name+'_sc', /GET_LUN, /APPEND

line_b=FLTARR(ns, nb)
line_r=FLTARR(ns, nb)
line_nir=FLTARR(ns, nb)
cmask=BYTARR(ns, nb)
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  line_b=float(line_ass_b[line])
  line_r=float(line_ass_r[line])
  line_nir=float(line_ass_nir[line])
   
  ;check for bad data (gt 250)
  ind_b=where(line_b gt 250, count_b)
  line_b=gb*line_b
  if (count_b ne 0) then line_b[ind_b]=!VALUES.F_NAN
  
  ind_r=where(line_r gt 250, count_r)
  line_r=gr*line_r
  if (count_r ne 0) then line_r[ind_r]=!VALUES.F_NAN
  
  ind_nir=where(line_nir gt 250, count_nir)
  line_nir=gnir*line_nir
  if (count_nir ne 0) then line_nir[ind_nir]=!VALUES.F_NAN
  
  ;check where they are statisfying the cloud condition
  cmask[*,*]=0
  ind_b=where(line_b gt 0.3, count_b)
  if (count_b ne 0) then cmask[ind_b]=1
  ind_r=where(line_r gt 0.5, count_r)
  if (count_r ne 0) then cmask[ind_r]=1
  ind_nir=where(line_nir gt 0.7, count_nir)
  if (count_nir ne 0) then cmask[ind_nir]=1
  WRITEU, W1, cmask
  ;tmp
;  WRITEU, W2, line_b
;  WRITEU, W3, line_r
;  WRITEU, W4, line_nir

ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=output_file_name+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {Cloud mask}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 1'
printf,3,'interleave = bil'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'

;HEADER_OUT=B_file_name+'_sc'+'.hdr'
;OPENW, 4, HEADER_OUT
;printf,4,'ENVI'
;printf,4,'description = {bands}'
;printf,4,'samples ='+STRCOMPRESS(ns)
;printf,4,'lines   ='+STRCOMPRESS(nl)
;printf,4,'bands   ='+STRCOMPRESS(nb)
;printf,4,'header offset = 0'
;printf,4,'file type = ENVI Standard'
;printf,4,'data type = 4'
;printf,4,'interleave = bil'
;printf,4,'sensor type = VGT'
;printf,4,'byte order = 0'

CLOSE, /ALL
return, 1
End


Pro mask_builder
path='X:\Niger\input\AFR s10 bands and other'
file_b=path+'\'+'bil_niger_1_blue'
file_r=path+'\'+'bil_niger_1_red'
file_nir=path+'\'+'bil_niger_1_nir_fake35'
file_out=path+'\'+'cldmsk'
ns=2018
nl=1038
nb=396
res=build_gobron_cldmsk(file_b, file_r, file_nir, ns, nl, nb, file_out)
End