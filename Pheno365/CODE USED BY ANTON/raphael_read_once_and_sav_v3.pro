PRO Raphael_read_once_and_sav_v3
  ;csv_file = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190325.csv'
  ;csv_file = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190328.csv'
  
  csv_file = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190520.csv'
  res = READ_CSV(csv_file, HEADER=hdr)
  x = rename_tags(res, TAG_NAMES(res), ['pid','pointID','date','x','y','nuts0','crop_ID','orbit','cropname','surv_date','index_name','index_value'])

  ;make a big table
  indS1 = WHERE(x.index_name EQ 'VH_mean', ns1)
  indS2 = WHERE(x.index_name EQ 'NDVI_mean', ns2)
  ntot = ns1 + ns2
  y = CREATE_STRUCT('pointID', LONARR(ntot), 'dateJD', LONARR(ntot), 'x', FLTARR(ntot), 'y', FLTARR(ntot), 'nuts0', STRARR(ntot), $
                    'crop', STRARR(ntot), 'surveyDateJD', LONARR(ntot), 's12', BYTARR(ntot), $ 
                    'orbit', STRARR(ntot), 'CR', FLTARR(ntot), 'RVI', FLTARR(ntot), $
                    'NDVIm', FLTARR(ntot), 'NDVIsd', FLTARR(ntot)) 
  ;fill it
  
  ;start with S1
  ind = WHERE(x.index_name EQ 'VV_mean', n)
  IF (n NE ns1) THEN STOP
  ;check that point id matches
  IF (TOTAL(ABS(x.pointID[indS1]-x.pointID[ind])) NE 0) THEN STOP
  y.pointID[0:ns1-1] = x.pointID[indS1]
  ;get VH annd VV data, check them for equal size and dates
  vh_value = x.index_value[indS1] 
  vh_dateJD = YYYYbMMbDD2jd(x.date[indS1])
  vv_value = x.index_value[ind]
  vv_dateJD = YYYYbMMbDD2jd(x.date[ind])
  IF (TOTAL(ABS(vh_dateJD-vv_dateJD)) NE 0) THEN STOP
  y.dateJD[0:ns1-1] = vh_dateJD
  y.x[0:ns1-1] = x.x[indS1]
  y.y[0:ns1-1] = x.y[indS1]
  y.nuts0[0:ns1-1] = x.nuts0[indS1] 
  y.crop[0:ns1-1] = x.cropname[indS1]
  ;treat survey daye format 
  months = ['   ','JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  surv_date = x.surv_date[indS1]
  FOR i = 0, N_ELEMENTS(surv_date)-1 DO BEGIN
    ;example '25-JUL-18 00.00.00.000000000'
    tmp = STRSPLIT(surv_date[i], ' ',/EXTRACT)
    tmp = STRSPLIT(tmp[0], '-',/EXTRACT)
    ;[25, JUL, 18]
    MM = WHERE(months EQ tmp[1], count)
    IF (count EQ 0) THEN STOP
    surv_date[i] = '20' + STRTRIM(tmp[2],2) + '-' + STRING(MM, FORMAT='(I02)') + '-' + STRTRIM(tmp[0],2)
  ENDFOR
  y.surveyDateJD[0:ns1-1] = YYYYbMMbDD2jd(surv_date)
  y.orbit[0:ns1-1] = x.orbit[indS1]
  IF (TOTAL(x.orbit[ind] NE y.orbit[0:ns1-1]) NE 0) THEN STOP
  y.s12[0:ns1-1] = 1
  ;compute Cross Ratio
  y.CR[0:ns1-1] = vh_value/vv_value
  ;compute RVI
  y.RVI[0:ns1-1] = 4.0 * vh_value / (vv_value + vh_value)
  ;ADJUST low backscatter values (after 20 5 2019 it should be adjusted by Raphel but leave the check for some other datasets that may come
  ;it may happen that values comes from the border of the scene where values are noise and are equal, resulting in a ratio of 1 (in [-)], set NaN
  indNoisy = WHERE(y.CR[0:ns1-1] EQ 1.0, countNoisy)
  IF (countNoisy GT 0) THEN BEGIN
    y.CR[indNoisy]=!VALUES.F_NAN
    y.RVI[indNoisy]=!VALUES.F_NAN
  ENDIF
  ;it may also happen that they are not equal but both very small and similar, resulting in VH_VV close to 1
  ;according to guido anu value < 10^(-4) is noise
  indNoisyVV = WHERE(vv_value LT 0.0001, countNoisyVV)
  indNoisyVH = WHERE(vh_value LT 0.0001, countNoisyVH)
  IF (countNoisyVV GT 0) THEN BEGIN
    y.CR[indNoisyVV]=!VALUES.F_NAN
    y.RVI[indNoisyVV]=!VALUES.F_NAN
  ENDIF
  IF (countNoisyVH GT 0) THEN BEGIN
    y.CR[indNoisyVH]=!VALUES.F_NAN
    y.RVI[indNoisyVH]=!VALUES.F_NAN
  ENDIF
  ind = 0
  
  ;now S2
  ind = WHERE(x.index_name EQ 'NDVI_stdDev', n)
  IF (n NE ns2) THEN STOP
  ;check that point id matches
  IF (TOTAL(ABS(x.pointID[indS2]-x.pointID[ind])) NE 0) THEN STOP
  y.pointID[ns1:-1] = x.pointID[indS2]
  ;get nean and stDev, check them for equal size and dates
  y.NDVIm[ns1:-1] = x.index_value[indS2]
  y.NDVIsd[ns1:-1] = x.index_value[ind]
  y.dateJD[ns1:-1] = YYYYbMMbDD2jd(x.date[indS2])
  ;check same date
  IF (TOTAL(ABS(y.dateJD[ns1:-1]-YYYYbMMbDD2jd(x.date[ind]))) NE 0) THEN STOP
  y.x[ns1:-1] = x.x[indS2]
  y.y[ns1:-1] = x.y[indS2]
  y.nuts0[ns1:-1] = x.nuts0[indS2]
  y.crop[ns1:-1] = x.cropname[indS2]
  ;treat survey daye format
  surv_date = x.surv_date[indS2]
  FOR i = 0, N_ELEMENTS(surv_date)-1 DO BEGIN
    ;example '25-JUL-18 00.00.00.000000000'
    tmp = STRSPLIT(surv_date[i], ' ',/EXTRACT)
    tmp = STRSPLIT(tmp[0], '-',/EXTRACT)
    ;[25, JUL, 18]
    MM = WHERE(months EQ tmp[1], count)
    IF (count EQ 0) THEN STOP
    surv_date[i] = '20' + STRTRIM(tmp[2],2) + '-' + STRING(MM, FORMAT='(I02)') + '-' + STRTRIM(tmp[0],2)
  ENDFOR
  y.surveyDateJD[ns1:-1] = YYYYbMMbDD2jd(surv_date)
  y.s12[ns1:-1] = 2
  fn = FILE_DIRNAME(csv_file) + '\' +FILE_BASENAME(csv_file, '.csv') + '_v3.sav';STRSPLIT(csv_file, '.', /EXTRACT)
  SAVE, y,  FILENAME=fn
END