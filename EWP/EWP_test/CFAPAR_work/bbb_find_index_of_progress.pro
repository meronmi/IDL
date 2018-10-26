PRO run_BBB_find_index_of_progress
  dir_pheno = '\\ies\d5\asap\TEST_PREDICTORS\Pheno_SPIRITS_asap20\ROI_Africa'
  base_pheno_s = 'phenos'
  base_pheno_e = 'phenoe'
  yy1 = 2007
  tt1 = 1
  yy2 = 2017
  tt2 = 14
  fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\SWI_1km_ROI_Africa\metaSWI_015_200701-2017-14.mta'
  dir_out = '\\ies\d5\asap\TEST_PREDICTORS\PROGRESS_SUBSCRIPTS'
  BBB_find_index_of_progress, dir_pheno, base_pheno_s, base_pheno_e, yy1, tt1, yy2, tt2, fn_meta, dir_out
END

PRO BBB_find_index_of_progress, dir_pheno, base_pheno_s, base_pheno_e, yy1, tt1, yy2, tt2, fn_meta, dir_out
;given a period YY1TT1 - YY2TT2 of dekadal images
;this function save the position (subscript within the YY1TT1 - YY2TT2 images) 
;of 0% (SOS), 25%, 50%, 75% and 100% (EOS) prgres of the season
;this is done for season 1 and season 2, thus includes bi-modal
;INPUT
; - spirits phenology
; - period, the period must start at the first dekad of a year
; - metafile that will be indexed (to get file dims)

IF (tt1 NE 1) THEN BEGIN
  PRINT, 'The period of interest MUST start at dek 1'
  STOP
ENDIF
;get data dims
info = READ_META(fn_meta)
ns = info.ns
nl = info.nl
;check that the number of bands correspond to the input period
n = 36 - tt1 + 1                  ;number of dekads of the first year
n = n + ((yy2 - 1) - yy1 ) * 36   ;number of dekads in the complete years
nDeks = n + tt2                       ;number of dekads in the last year
IF (nDeks NE info.nb) THEN BEGIN
  PRINT, 'The metafile has ' + STRTRIM(info.nb,2) + ' files while the period if of ' + STRTRIM(nDeks,2) + ' dkads'
  STOP
ENDIF

FOR spy = 0, 1 DO BEGIN     ;loop on the two seasons
  PRINT, 'Processing season ' + STRTRIM(spy+1,2)  
  indexmat = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%
  indexmat1 = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%
  indexmat2 = FLTARR(ns, nl, 5) * !VALUES.F_NAN ;0,25,50,75,100%   
  ;read SPIRITS pheno
  ;sos for season spy
  OPENR, R, dir_pheno + '\' + base_pheno_s + STRTRIM(spy+1,2) + '.img', /GET_LUN
  start_dek_image = BYTARR(ns, nl)
  READU, R, start_dek_image
  CLOSE, R
  start_dek_image = SPIRITS_from_108_to_36_ARRAY(start_dek_image) ;data now floating with NaN where no seasonality    
  ;eos for seaon spy 
  OPENR, R, dir_pheno + '\' + base_pheno_e + STRTRIM(spy+1,2) + '.img', /GET_LUN
  stop_dek_image = BYTARR(ns, nl)
  READU, R, stop_dek_image
  CLOSE, R
  stop_dek_image = SPIRITS_from_108_to_36_ARRAY(stop_dek_image) ;data now floating with NaN where no seasonality
  
  ;processing divided in 2, 1: where SOS comes first in the caldendar year, 2 when it comes later

  indFin = WHERE(FINITE(stop_dek_image) AND FINITE(stop_dek_image))
  ind1 = WHERE(stop_dek_image[indFin] GE start_dek_image[indFin])
  ind2 = WHERE(stop_dek_image[indFin] LT start_dek_image[indFin])
  ;compute GSL
  GSL_dek = start_dek_image * !VALUES.F_NAN
  GSL_dek[indFin[ind1]] = stop_dek_image[indFin[ind1]] - start_dek_image[indFin[ind1]] + 1
  GSL_dek[indFin[ind2]] = stop_dek_image[indFin[ind2]] + 36.0 - start_dek_image[indFin[ind2]] +1
;  OPENW, lun, dir_out + '\gsl_dek' , /GET_LUN
;  WRITEU, lun, GSL_dek
;  FREE_LUN, lun
  
  
  pdf = HISTOGRAM(GSL_dek[indFin], BINSIZE=1, LOCATIONS=xbin);, MIN = -36, MAX = 36)
  h0 = PLOT(xbin, pdf) ;, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i] + ' Sig r', XTITLE='Correlation wiith zFAPAR', $
  
  ;consider only pixels with finite phenology AND gsl GE 5 and gsl LE 30, those outside are considered artifacts here
  ;here I update index of line 1 and 2, 
  ;1: where SOS comes first in the caldendar year
  ind1 = WHERE((stop_dek_image[indFin] GE start_dek_image[indFin]) AND $
                (GSL_dek[indFin] GE 5) AND (GSL_dek[indFin] LE 30))
  ;2: when it comes later
  ind2 = WHERE((stop_dek_image[indFin] LT start_dek_image[indFin]) AND $
                (GSL_dek[indFin] GE 5) AND (GSL_dek[indFin] LE 30))
;  check = start_dek_image * !VALUES.F_NAN
;  check[indFin[ind1]] = 1.0
;  check[indFin[ind2]] = 2.0
;  OPENW, lun, dir_out + '\check_dek' , /GET_LUN
;  WRITEU, lun, check
;  FREE_LUN, lun
  ;line 1 1: where SOS comes first in the caldendar year, 2 when it comes later
  FOR i = 0, 4 DO BEGIN
    tmp = GSL_dek * !VALUES.F_NAN
    ;n = GSL_dek[indFin[ind1]] 
    ;compute the subscript value, starting from SOS of the various progresses
    tmp[indFin[ind1]] = start_dek_image[indFin[ind1]] + FLOAT(ROUND(i * (GSL_dek[indFin[ind1]]-1) / 4.0))
    indexmat1[*,*,i] = tmp ;here I am storing the value (in dekads) of the 6 progresses; as stop>start it can't be that one value is > 36
    ;check
    IF (i EQ 4) THEN BEGIN
      IF TOTAL(tmp[indFin[ind1]]  - stop_dek_image[indFin[ind1]]) NE 0 THEN STOP
    ENDIF
  ENDFOR
  ;line 2 (when sos is after eos in the calendar year)
  FOR i = 0, 4 DO BEGIN
    tmp = GSL_dek * !VALUES.F_NAN
    ;n = GSL_dek[indFin[ind2]]
    ;compute the subscript value, starting from SOS of the various progresses
    tmp[indFin[ind2]] = start_dek_image[indFin[ind2]] + FLOAT(ROUND(i * (GSL_dek[indFin[ind2]]-1) / 4.0)) ;this can go beyond 36, it is ok, see later
    ;be care here not to overwrite
    ;res =  ARRAY_INDICES(tmp, indFin[ind2])
    indexmat2[*,*,i] = tmp
    ;check
    IF (i EQ 4) THEN BEGIN
      ;this adjustment here is made just to make the test
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
  OPENW, lun, dir_out + '\dek_of_prog' , /GET_LUN
  WRITEU, lun, indexmat
  FREE_LUN, lun
  res = write_envi_hdr(dir_out+'\dek_of_prog'+'.hdr' , info.ns, info.nl, 4, NBANDS=3, INTERLEAVE='bsq')
  
  
  ;now make the big bil files that contain the position of progress for each year of the 9901 1336 file, 
  ;540 bands
  
;  pos_prog = INTARR(ns, nl, 5, 15) * 0 - 999
;  FOR yy = 1999, 2013 DO BEGIN
;    y = yy-1999
;    FOR i = 0, 4 DO BEGIN
;      tmp = indexmat[*,*,i]
;      indFin = WHERE(FINITE(tmp))
;      indNaN = WHERE(~FINITE(tmp))
;      tmp[indFin] = y * 36 + tmp[indFin] - 1
;      tmp[indNaN] = -999
;      pos_prog[*,*,i,y] =   FIX(tmp)
;    ENDFOR  
;  ENDFOR

  ;now I have seasons that start and stop in the CY (Calendar Year) and seasons that start in CY and stop in CY+1
  ;I have to find those that are complete in my period and save their subscripts
  ;No matter if we are looking at a monomodal or at one of the two bi-modal, we can have a max number of season equal to the period in year.
  ;That is, I can only have less (e.g. the first start is not in my period). These I have to set to -999
  nYears = yy2 - yy1 + 1
  ;nDeks is the number of dekads
  pos_prog = INTARR(ns, nl, 5, nYears) * 0 - 999
  FOR yy = yy1, yy2 DO BEGIN
    y = yy-yy1
    FOR i = 0, 4 DO BEGIN
      tmp = indexmat[*,*,i]
      indFin = WHERE(FINITE(tmp))
      indNaN = WHERE(~FINITE(tmp))
      tmp[indFin] = y * 36 + tmp[indFin] - 1
      tmp[indNaN] = -999
      pos_prog[*,*,i,y] = FIX(tmp)
    ENDFOR
  ENDFOR
  ;here I could have values greater than nDeks if the season ends after the last dekad of my period
  ;set all the values to -999
  ind = WHERE(pos_prog GT nDeks-1, count)
  IF (count GT 0) THEN pos_prog[ind] = -999
  prog = ['0','25','50','75','100']
  FOR i = 0, 4 DO BEGIN
    IF (tt1 LT 10) THEN ttmp1 = '0'+STRTRIM(tt1,2) ELSE ttmp1 = STRTRIM(tt1,2)
    IF (tt2 LT 10) THEN ttmp2 = '0'+STRTRIM(tt2,2) ELSE ttmp2 = STRTRIM(tt2,2)
    tmp = 'season'+STRTRIM(spy+1,2)+'_sub_in_period_' + STRTRIM(yy1,2) + STRTRIM(ttmp1,2) + '-' + STRTRIM(yy2,2) + STRTRIM(ttmp2,2) + '_of_prog_
    OPENW, lun, dir_out + '\' + tmp + prog[i] , /GET_LUN
    WRITEU, lun,  pos_prog[*,*,i,*]
    FREE_LUN, lun
    res = write_envi_hdr(dir_out +'\'+tmp+prog[i]+'.hdr' , info.ns, info.nl, 2, NBANDS=nYears, INTERLEAVE='bsq')
  ENDFOR
ENDFOR  ; spy loop on 1 and 2 growing seasons per year
END