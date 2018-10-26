PRO find_index_of_progress
;this function save the position (subscript within the 9901-1336 stack) of 0% (SOS), 25%, 50%, 75%
;and 100% (EOS) prgres of the season

;**************************************************************************
rfe_dataset = 'chirps';'tamsat' ;'chirps'
CASE rfe_dataset OF
  'tamsat': BEGIN
      ;TAMSAT RESOLUTION
      ns = 1867
      nl = 348
      dir_out = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution'
      pheno_dir = 'E:\WA\EWP\Pheno_at_tamsat_res'
      fn_start = pheno_dir + '\1gspy_1s_A1sos-1997_sos1_DOC_TZPavg_res_to_Tamsat.img'
      fn_stop = pheno_dir + '\1gspy_1s_A1sos-1997_eos1_DOC_TZPavg_res_to_Tamsat.img'
    END
   'chirps': BEGIN
     ;CHIRPS RESOLUTION
     ns = 1402
     nl = 262
     dir_out = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution'
     pheno_dir = 'E:\WA\EWP\Pheno_at_chirps_res'
     fn_start = pheno_dir + '\1gspy_1s_A1sos-1997_sos1_DOC_TZPavg_res_to_Chirps.img'
     fn_stop = pheno_dir + '\1gspy_1s_A1sos-1997_eos1_DOC_TZPavg_res_to_Chirps.img'
    END
   ELSE: STOP
ENDCASE
;**************************************************************************
indexmat = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%
indexmat1 = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%
indexmat2 = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%




;sos
OPENR, R3, fn_start, /GET_LUN
start_doy_image = FLTARR(ns, nl)
READU, R3, start_doy_image
CLOSE, R3
ind = WHERE(start_doy_image EQ -9999)
start_doy_image[ind] = !VALUES.F_NAN

;eos
OPENR, R3, fn_stop, /GET_LUN
stop_doy_image = FLTARR(ns, nl)
READU, R3, stop_doy_image
CLOSE, R3
ind = WHERE(stop_doy_image EQ -9999)
stop_doy_image[ind] = !VALUES.F_NAN

;now transform it into dekadal
start_dek_image = doy2dek(start_doy_image)
stop_dek_image = doy2dek(stop_doy_image)

OPENW, lun, dir_out + '\start_dek' , /GET_LUN
WRITEU, lun, start_dek_image
FREE_LUN, lun
OPENW, lun, dir_out + '\stop_dek' , /GET_LUN
WRITEU, lun, stop_dek_image
FREE_LUN, lun
;processing divided in 2, 1: where SOS comes first in the caldendar year, 2 when it comes later
;line 1
indFin = WHERE(FINITE(stop_dek_image) AND FINITE(stop_dek_image))
ind1 = WHERE(stop_dek_image[indFin] GE start_dek_image[indFin])
ind2 = WHERE(stop_dek_image[indFin] LT start_dek_image[indFin])
;compute GSL
GSL_dek = start_dek_image * !VALUES.F_NAN
GSL_dek[indFin[ind1]] = stop_dek_image[indFin[ind1]] - start_dek_image[indFin[ind1]] + 1
GSL_dek[indFin[ind2]] = stop_dek_image[indFin[ind2]] + 36.0 - start_dek_image[indFin[ind2]] +1
OPENW, lun, dir_out + '\gsl_dek' , /GET_LUN
WRITEU, lun, GSL_dek
FREE_LUN, lun


pdf = HISTOGRAM(GSL_dek[indFin], BINSIZE=1, LOCATIONS=xbin);, MIN = -36, MAX = 36)
h0 = PLOT(xbin, pdf) ;, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i] + ' Sig r', XTITLE='Correlation wiith zFAPAR', $

;consider only pixels with finite phenology AND gsl GE 5 and gslt LE 30
ind1 = WHERE((stop_dek_image[indFin] GE start_dek_image[indFin]) AND $
              (GSL_dek[indFin] GE 5) AND (GSL_dek[indFin] LE 30))
ind2 = WHERE((stop_dek_image[indFin] LT start_dek_image[indFin]) AND $
              (GSL_dek[indFin] GE 5) AND (GSL_dek[indFin] LE 30))
check = start_dek_image * !VALUES.F_NAN
check[indFin[ind1]] = 1.0
check[indFin[ind2]] = 2.0
OPENW, lun, dir_out + '\check_dek' , /GET_LUN
WRITEU, lun, check
FREE_LUN, lun
;processing divided in 2, 1: where SOS comes first in the caldendar year, 2 when it comes later
;line 1
FOR i = 0, 4 DO BEGIN
  tmp = GSL_dek * !VALUES.F_NAN
  n = GSL_dek[indFin[ind1]] 
  ;compute the subscript value, starting from SOS of the various progresses
  tmp[indFin[ind1]] = start_dek_image[indFin[ind1]] + FLOAT(ROUND(i * (GSL_dek[indFin[ind1]]-1) / 4.0))
  indexmat1[*,*,i] = tmp
  ;check
  IF (i EQ 4) THEN BEGIN
    IF TOTAL(tmp[indFin[ind1]]  - stop_dek_image[indFin[ind1]]) NE 0 THEN STOP
  ENDIF
ENDFOR
;line 2
FOR i = 0, 4 DO BEGIN
  tmp = GSL_dek * !VALUES.F_NAN
  n = GSL_dek[indFin[ind2]]
  ;compute the subscript value, starting from SOS of the various progresses
  tmp[indFin[ind2]] = start_dek_image[indFin[ind2]] + FLOAT(ROUND(i * (GSL_dek[indFin[ind2]]-1) / 4.0))
  ;be care here not to overwrite
  res =  ARRAY_INDICES(tmp, indFin[ind2])
  indexmat2[*,*,i] = tmp
  ;check
  IF (i EQ 4) THEN BEGIN
    ind = WHERE(tmp[indFin[ind2]] GT 36, count)
    tmp2 = tmp 
    IF (count GT 0) THEN tmp2[indFin[ind2[ind]]] = tmp2[indFin[ind2[ind]]] - 36 
    IF TOTAL(tmp2[indFin[ind2]]  - stop_dek_image[indFin[ind2]]) NE 0 THEN STOP
  ENDIF
ENDFOR
;put toghether the two index mat
FOR i = 0, 4 DO BEGIN
  tmp1 = indexmat1[*,*,i]
  tmp2 = indexmat2[*,*,i]
  ind1 = WHERE(FINITE(tmp1)) 
  ind2 = WHERE(FINITE(tmp2))
  tmp = tmp1 *!VALUES.F_NAN
  tmp[ind1] = tmp1[ind1]
  tmp[ind2] = tmp2[ind2]
  indexmat[*,*,i] = tmp
ENDFOR
;up to here the index can be greater than 36 (if the season stop in the following year) 
OPENW, lun, dir_out + '\index_of_prog' , /GET_LUN
WRITEU, lun, indexmat
FREE_LUN, lun

;now make the big bil files that contain the position of progress for each year of the 9901 1336 file, 
;540 bands
pos_prog = INTARR(ns, nl, 5, 15) * 0 - 999
FOR yy = 1999, 2013 DO BEGIN
  y = yy-1999
  FOR i = 0, 4 DO BEGIN
    tmp = indexmat[*,*,i]
    indFin = WHERE(FINITE(tmp))
    indNaN = WHERE(~FINITE(tmp))
    tmp[indFin] = y * 36 + tmp[indFin] - 1
    tmp[indNaN] = -999
    pos_prog[*,*,i,y] =   FIX(tmp)
  ENDFOR  
ENDFOR
;here I could have values greater than 539 if the season ends in 2014
;set all the values to -9999
ind = WHERE(pos_prog GT 539, count)
IF (count GT 0) THEN pos_prog[ind] = -999
prog = ['0','25','50','75','100']
FOR i = 0, 4 DO BEGIN
  OPENW, lun, dir_out + '\in99-13sub_of_prog' + prog[i] , /GET_LUN
  WRITEU, lun,  pos_prog[*,*,i,*]
  FREE_LUN, lun
ENDFOR

END