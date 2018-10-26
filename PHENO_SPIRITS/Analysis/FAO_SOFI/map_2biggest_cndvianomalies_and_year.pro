PRO map_2biggest_cNDVIanomalies_and_year
  ;get one anomaly type (can be gsl or sos) and per pixel and per season:
  ; - find the two largest absolute value, save the value and the year it happended
  exclude2003 = 1
  fn_cumNDVI = 'F:\CumulatedNDVIfromX\AAAA_cNDVI_by_year_2003-2016.img' ;occhio, questo e'un bsq, no data e' -32000
  outPath = 'F:\CumulatedNDVIfromX\2LargestDev'
  FILE_MKDIR, outPath
  dirsep = '\'
  IF (exclude2003 EQ 1) THEN frstyear = 2004 ELSE frstyear = 2003
  

  cNDVI = ReadEnviWithHdr(fn_cumNDVI) ;3d
  IF (exclude2003 EQ 1) THEN cNDVI = cNDVI[*,*,1:-1]
  sz = SIZE(cNDVI)
  valMin = MIN(cNDVI, subMin, DIMENSION=3) ;valmin 2d, subMin refers to 3d
  yearMin = FIX(valMin * 0 - 9999)  ;2d
;  FOR s = 0, sz[1]-1 DO BEGIN
;    FOR l = 0, sz[2]-1 DO BEGIN
;      posMin = ARRAY_INDICES(cNDVI, subMin[s,l])
;      yearMin[s,l] = posMin[2] + frstyear
;    ENDFOR
;  ENDFOR
  
  
  FOR l = 0, sz[2]-1 DO BEGIN
    posMin = ARRAY_INDICES(cNDVI, subMin[*,l])
    yearMin[*,l] = posMin[2,*] + frstyear
  ENDFOR
  
  
;  ;posMin = ARRAY_INDICES(cNDVI, subMin) ;posMin is 2D, each line is a record indicating the position of the min (column, line, z)
; 
;  FOR r = 0, N_ELEMENTS(posMin[0,*]) - 1 DO BEGIN
;    yearMin[posMin[0,r],posMin[1,r]] = posMin[2,r] + frstyear
;  ENDFOR
  posMin = 0
  indNoData = WHERE(valMin EQ -32000) ;rerer to 2D
  ;make the value as % devaition from avg
  avgcNDVI = MEAN(cNDVI, DIMENSION=3) ;2d
  valMin = ((valMin-avgcNDVI)/FLOAT(avgcNDVI))*100.0
  ;there si some divison by 0
  indNaN = WHERE(~FINITE(valMin), countNan)
  IF (countNan GT 0) THEN valMin[indNaN] = -9999
  ;valMin= FIX(valMin)
  ;check than none had two minima
  cNDVI[subMin] = 32000 ;here aI put a big values so that ie will not be founfd as min
  valMin2 = MIN(cNDVI, subMin2, DIMENSION=3)
  indDuplic = WHERE(valMin-valMin2 EQ 0, countDuplic)
  IF (countDuplic GT 0) THEN BEGIN
    yearMin[indDuplic] = -9999
    valMin[indDuplic] = -9999    
  ENDIF

  valMin[indNoData] = -9999
  valMin[indDuplic] = -9999    
  fn_out = outPath + dirsep + '1st_largest_cNDVIdeviation'
  IF (exclude2003 EQ 1) THEN fn_out = fn_out + '2003_excluded'
  res = write_envi_img(valMin, fn_out + '.img')
  mapinfo = read_info('map info',  remove_ext_from_fn(fn_cumNDVI) + '.hdr')
  res = write_envi_hdr(fn_out, sz[1], sz[2], 4, MAPINFO=mapinfo)
  
  fn_out = outPath + dirsep + 'year_of_1st_largest_cNDVIdeviation'
  IF (exclude2003 EQ 1) THEN fn_out = fn_out + '2003_excluded'
  res = write_envi_img(yearMin, fn_out + '.img')
  
  res = write_envi_hdr(fn_out, sz[1], sz[2], 2, MAPINFO=mapinfo)
  
END  
