; NAME:
;   SAGO_WRAPPER
;
; PURPOSE:
;   IDL wrapper for Savitzky Golay filtering of NDVI time series
;
; INPUTS:
;   NDVI data stack, cloud mask stack, Mask file for processing  (all as BSQ)
;   number of samples, lines, & bands
;
; OUTPUTS:
;   Filtered & interpolated NDVI using Savitzky-Golay
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, January 2008 (based on Allard de Wit, November 2004)

PRO SAGO_WRAPPER

STARTTIME = SYSTIME(1)

data_path='X:\GIMMS-AVHRR\processing\Africa\'           ; location where AVHRR GIMMS data are
work_path='X:\GIMMS-AVHRR\processing\Africa\Savitzky\'  ; location where output will be written to

datafile=data_path+'Africa_GIMMS.stk'
cloudfile=work_path+'Africa_cloudmask.stk'
maskfile=work_path+'Africa_mask.img'

ns=1152                   ; number of samples
nl=1152                   ; number of lines
nb= 612                   ; number of bands

; Input files are opened by performing ASSOC to one line.
OPENR, R1, datafile, /GET_LUN
line_ass_data = ASSOC(R1, INTARR(ns))
OPENR, R2, cloudfile, /GET_LUN
line_ass_cloud = ASSOC(R2, BYTARR(ns))
OPENR, R3, maskfile, /GET_LUN
line_ass_mask = ASSOC(R3, BYTARR(ns))

; Create files for output
OUT_smNDVI=work_path+'Africa_GIMMS_filteredSG'+'.stk'       ; file to hold savitzky-golay filtered timeseries
IF FILE_TEST(OUT_smNDVI) eq 1 THEN FILE_DELETE, OUT_smNDVI
OPENW, W1, OUT_smNDVI, /GET_LUN, /APPEND
OUT_iNDVI=work_path+'Africa_GIMMS_interpolated'+'.stk'      ; file to hold interpolated cloud timeseries
IF FILE_TEST(OUT_iNDVI) eq 1 THEN FILE_DELETE, OUT_iNDVI
OPENW, W2, OUT_iNDVI, /GET_LUN, /APPEND

data=INTARR(ns,nb)
cloud=BYTARR(ns,nb)
mask=BYTARR(ns)

FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  mask=line_ass_mask[line]
  ; Now create per line the stack of all dates
  FOR band=0, nb-1, 1L DO BEGIN
    data[*,band]=line_ass_data[line+(band*LONG(nl))]
    cloud[*,band]=line_ass_cloud[line+(band*LONG(nl))]
  ENDFOR

  smNDVI=INTARR(ns,nb)                  ; smoothed NDVI by Savitzky-Golay
  iNDVI=INTARR(ns,nb)                   ; interpolated NDVI (the cloud flags)
  tmp1=INTARR(nb)
  tmp2=INTARR(nb)

  FOR j=0, ns-1 DO BEGIN                ; loop over all pixels in line
    IF mask[j] EQ 1 THEN BEGIN          ; process only pixels in mask
      tmp_cloud=cloud[j,*]
      tmp_data=data[j,*]
      ; in the following function call the SG-filtering is performed
      sago_interpol,tmp_data, tmp_cloud, iNDVI=tmp1, smNDVI=tmp2
    ENDIF ELSE BEGIN
      tmp1=data[j,*]
      tmp2=data[j,*]
    ENDELSE
    iNDVI[j,*]=tmp1
    smNDVI[j,*]=tmp2
  ENDFOR

  WRITEU, W1, smNDVI
  WRITEU, W2, iNDVI

ENDFOR

CLOSE, /ALL


; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=STRMID(OUT_smNDVI,0,STRLEN(OUT_smNDVI)-3)+'hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {'
printf,3,'  Filtered GIMMS AVHRR by Savitzky-Golay }'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'sensor type = AVHRR'
printf,3,'byte order = 0'
printf,3,'map info = '+'{GIMMS_AF, 1.0000, 1.0000, -4607918.3400, 4608147.4688, 8.0000000000e+03, 8.0000000000e+03, WGS-84, units=Meters}'
printf,3,'projection info ='+'{9, 6378137.0, 6356752.3, 1.000000, 20.000000, 0.0, 0.0, -19.000000, 21.000000, WGS-84, GIMMS_AF, units=Meters}'
CLOSE, 3

; WRITE HEADER OF THE OUTPUT2
HEADER_OUT=STRMID(OUT_iNDVI,0,STRLEN(OUT_iNDVI)-3)+'hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {'
printf,3,'  Interpolated cloud flags from GIMMS AVHRR }'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'sensor type = AVHRR'
printf,3,'byte order = 0'
printf,3,'map info = '+'{GIMMS_AF, 1.0000, 1.0000, -4607918.3400, 4608147.4688, 8.0000000000e+03, 8.0000000000e+03, WGS-84, units=Meters}'
printf,3,'projection info ='+'{9, 6378137.0, 6356752.3, 1.000000, 20.000000, 0.0, 0.0, -19.000000, 21.000000, WGS-84, GIMMS_AF, units=Meters}'
CLOSE, 3


; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED FILTERING GIMMS AVHRR DATA'

END ;Procedure SAGO_WRAPPER

