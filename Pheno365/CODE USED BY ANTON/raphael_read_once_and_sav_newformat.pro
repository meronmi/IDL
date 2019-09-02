PRO Raphael_read_once_and_sav_newFormat
  
  s1_csv_file = 'D:\RAPHAEL_pheno_test\LUCAS_S1_extraction_postprocess_20190704.csv';'D:\RAPHAEL_pheno_test\S1_20190625_EU__LUCAS_V3.csv' ;'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190520.csv'
  s2_csv_file = 'D:\RAPHAEL_pheno_test\LUCAS_S2_extraction_postprocess_20190703.csv'
  
  ;SENTINEL 1
  x = READ_CSV(s1_csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN)
  hdr[0] = 'ProgID'
  hdr = hdr.Replace('.','_')
  x= rename_tags(x, TAG_NAMES(x), hdr)
  ;make a new table with relevant info
  ntot = N_ELEMENTS(x.POINT_I)
  ; x.angle_mean has 'NA' values and it is thus a string array, replace it with NaN
  tmp =  x.angle_mean
  indNaN = WHERE(tmp EQ 'NA', COMPLEMENT=indNum, count)
  tmpOK = FLTARR(tmp.LENGTH) * !VALUES.F_NAN
  tmpOK[indNum] = FLOAT(tmp[indNum])
  
  s1 = CREATE_STRUCT('pointID', x.POINT_I, 'dateJD', LONARR(ntot),  $
    'crop', x.LC1_3__, 'crop_id', x.LC1,'x', x.X_WGS84, 'y', x.Y_WGS84, 'nuts0', x.NUTS0_1, 'surveyDateJD',  LONARR(ntot), $
    'VH_count', x.VH_count, 'VH_fract_available', FLTARR(ntot), 'VH_mean', x.VH_mean, 'VH_sd', x.VH_stdDev, $
    'VV_mean', x.VV_mean, 'VV_sd', x.VV_stdDev, $
    'RVI', FLTARR(ntot), 'CR', FLTARR(ntot), $ 
    'View_angle', tmpOK, 'orbit', x.orbitProperties_pass, 'rel_orbit', x.relativeOrbitNumber_start)  
  ;make date as jd
;  tmp =  x.angle_mean
;  ON_IOERROR, BAD
;  FOR i = 0, tmp.LENGTH-1 DO BEGIN
;    print, i, '--', FLOAT(tmp[i])
;  ENDFOR
;  BAD: STOP
  PRINT,'S1 date'
  s1.dateJD = YYYYbMMbDD2jd(x.date)
  s1.surveyDateJD = DDbMMbYYYY2jd(x.SURVEYD)
  x = 0
  ;compute RVI an CR
  ;compute Cross Ratio
  s1.CR = s1.VH_mean/s1.VV_mean
  ;compute RVI
  s1.RVI = 4.0 * s1.VH_mean / (s1.VH_mean + s1.VV_mean)
  ;get the max number of pixel count by ID and compute VH_fract_available, this is done by rel_orbit
  uniqIds = s1.pointID
  uniqIds = uniqIds[UNIQ(uniqIds, SORT(uniqIds))]
  FOR i = 0, N_ELEMENTS(uniqIds)-1 DO BEGIN
    indId = WHERE(s1.pointID EQ uniqIds[i])
    uniqRelOrb = s1.rel_orbit[indId]
    uniqRelOrb = uniqRelOrb[UNIQ(uniqRelOrb, SORT(uniqRelOrb))]
    FOR j = 0, N_ELEMENTS(uniqRelOrb)-1 DO BEGIN
      inIdOrb = WHERE(s1.rel_orbit[indId] EQ uniqRelOrb[j])
      s1.VH_fract_available[indId[inIdOrb]] = s1.VH_count[indId[inIdOrb]] / FLOAT(MAX(s1.VH_count[indId[inIdOrb]]))
    ENDFOR
  ENDFOR
  
  fn = FILE_DIRNAME(s1_csv_file) + '\' +FILE_BASENAME(s1_csv_file, '.csv') + '_v0.sav'
  SAVE, s1,  FILENAME=fn
  PRINT,'S1 end'
  ;tests
;  ind = WHERE(s1.pointID EQ 26561990)
;  dims_fit_plot = [900,300] ;[900,300]
;  h = PLOT(s1.dateJD[ind], s1.RVI[ind], LINESTYLE = '', SYMBOL = 'o', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', DIMENSIONS=dims_fit_plot, YTITLE='ndvi')
  
  ;SENTINEL 2
  x = READ_CSV(s2_csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN)
  hdr[0] = 'ProgID'
  hdr = hdr.Replace('.','_')
  x= rename_tags(x, TAG_NAMES(x), hdr)
  ;make a new table with relevant info
  ntot = N_ELEMENTS(x.POINT_I)
  s2 = CREATE_STRUCT('pointID', x.POINT_I, 'dateJD', LONARR(ntot),  $
    'crop', x.LC1_3__, 'crop_id', x.LC1,'x', x.X_WGS84, 'y', x.Y_WGS84, 'nuts0', x.NUTS0_1, 'surveyDateJD',  LONARR(ntot), $
    'NDVI_count', x.NDVI_count, 'Fract_cloudfree', FLTARR(ntot), 'NDVI_mean', x.NDVI_mean, 'NDVI_sd', x.NDVI_stdDev, $
    'Snow_count', x.snowMask_count)
  ;make date as jd  
  PRINT,'S2 date'
  s2.dateJD = YYYYbMMbDD2jd(x.date)
  s2.surveyDateJD = DDbMMbYYYY2jd(x.SURVEYD)
  x = 0
  ;get the max number of pixel count by ID and compute Fract_cloudfree
  uniqIds = s2.pointID
  uniqIds = uniqIds[UNIQ(uniqIds, SORT(uniqIds))]
  FOR i = 0, N_ELEMENTS(uniqIds)-1 DO BEGIN
    indId = WHERE(s2.pointID EQ uniqIds[i])
    s2.Fract_cloudfree[indId] = s2.NDVI_count[indId] / FLOAT(MAX(s2.NDVI_count[indId]))  
  ENDFOR
  fn = FILE_DIRNAME(s2_csv_file) + '\' +FILE_BASENAME(s2_csv_file, '.csv') + '_v0.sav'
  SAVE, s2,  FILENAME=fn
  
  ;tests
;  ind = WHERE(s2.pointID EQ 46183012)
;  dims_fit_plot = [900,300] ;[900,300]
;  h = PLOT(s2.dateJD[ind], s2.NDVI_mean[ind], LINESTYLE = '', SYMBOL = 'o', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', DIMENSIONS=dims_fit_plot, YTITLE='ndvi')
END