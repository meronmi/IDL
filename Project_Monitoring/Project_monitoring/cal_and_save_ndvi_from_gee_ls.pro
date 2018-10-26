FUNCTION cal_and_save_NDVI_from_GEE_LS, in_dir, out_dir, fn
;fn is the tif LS file from GEE
;base_name(fn) + 'csv' is assumed to be the file properties (from GEE)
;print, cal_and_save_NDVI_from_GEE_LS('S:\Actions\FOODSEC\projects\GGW_monitoring\LUISA\Michele_tests', 'S:\Actions\FOODSEC\projects\GGW_monitoring\LUISA\Michele_tests', 'LC82040492015257LGN00_E.tif')
;print, cal_and_save_NDVI_from_GEE_LS('S:\Actions\FOODSEC\projects\GGW_monitoring\LUISA\Michele_tests', 'S:\Actions\FOODSEC\projects\GGW_monitoring\LUISA\Michele_tests', 'LE72040492007259EDC00_E.tif')

fn_base = FILE_BASENAME(fn, '.tif') 
test = 'TOArefNDVI_' + fn_base + '.tif'
IF (FILE_SEARCH(out_dir + '\' + test) NE '') THEN BEGIN
  PRINT, 'WARNING: File ' + test + ' already exists, it will be used as it is'
  ndvi = READ_TIFF(out_dir + '\' + test)
ENDIF ELSE BEGIN
  ;1 Determine if it's LS7 or LS8
  ;open the metadata file (csv on one column)
  mta_fn = fn_base + '.csv'
  mtdata = READ_CSV(in_dir + '\' + mta_fn, HEADER=hdr) 
  sensor = mtdata.(WHERE(hdr EQ 'SENSOR_ID')) ;can be 'ETM+' or 'OLI_TIRS'
  scene_id = mtdata.(WHERE(hdr EQ 'LANDSAT_SCENE_ID'))
  
  ;2 Read the tif
  data = READ_TIFF(in_dir + '\' +fn)
  res = QUERY_TIFF(in_dir + '\' +fn, GEOTIFF=geoinfo)
  sz = SIZE(data)
  PRINT, 'Bands: ' + STRTRIM(sz[1],2)
  
  ;3 get the red and nir bands and all relevant info, position depending on being LS7 or 8
  CASE sensor OF
    'ETM+': BEGIN
      IF (sz[1] NE 9) THEN BEGIN
        PRINT, 'LS7 file does not have 9 bands, the program will stop'
        STOP
      ENDIF
      red_band = '3'
      nir_band = '4'
      ESUN =  {red:1533.0, nir:1039.0}  ;from Chander et al, 2009 (http://www.sciencedirect.com/science/article/pii/S0034425709000169)
      ;set the coefficients to transform into OLI NDVI (from Roy et al, 2015, RSE)
      gain = 0.9352
      offset = 0.0490 
      ;remove SLC contaminated pixels
      ns = sz[2] & nl = sz[3]
      ;remove non refletive bands to spot SLC
      data_ref = BYTARR(6, ns, nl)
      FOR i = 0,4 DO data_ref[i,*,*] = data[i,*,*]
      data_ref[5,*,*] = data[7,*,*]
;      mult = PRODUCT(FLOAT(data_ref), 1)
;      ind = WHERE(mult EQ 0, count)
      data = FLOAT(data)
;      FOR i = 0, sz[1]-1 DO BEGIN
;        tmp = REFORM(data[i,*,*])
;        tmp[ind] = !VALUES.F_NAN
;        data[i,*,*] = tmp
;      ENDFOR
      END
    'OLI_TIRS': BEGIN
      IF (sz[1] NE 11) THEN BEGIN
        PRINT, 'LS8 file does not have 11 bands, the program will stop'
        STOP
      ENDIF
      red_band = '4'
      nir_band = '5'
      ESUN =  {red:1536.0, nir:1145.0}  ;from Chander et al, 2009 (http://www.sciencedirect.com/science/article/pii/S0034425709000169)
      gain = 1.0
      offset = 0.0
      END  
    ELSE: BEGIN
      PRINT, 'Sensor ' + STRTRIM(sensor,2) + ' is not coded, the program will stop'
      STOP
      END   
  ENDCASE
  red = FLOAT(REFORM(data[FIX(red_band)-1,*,*]))
  nir =  FLOAT(REFORM(data[FIX(nir_band)-1,*,*]))
  DELVAR, data
  QCALMAX =  {red:0.0, nir:0.0}
  QCALMIN =  {red:0.0, nir:0.0}
  LMAX =  {red:0.0, nir:0.0}
  LMIN =  {red:0.0, nir:0.0}
  QCALMAX.red = mtdata.(WHERE(hdr EQ 'QUANTIZE_CAL_MAX_BAND_'+red_band))
  QCALMAX.nir = mtdata.(WHERE(hdr EQ 'QUANTIZE_CAL_MAX_BAND_'+nir_band))
  QCALMIN.red = mtdata.(WHERE(hdr EQ 'QUANTIZE_CAL_MIN_BAND_'+red_band))
  QCALMIN.nir = mtdata.(WHERE(hdr EQ 'QUANTIZE_CAL_MIN_BAND_'+nir_band))
  LMAX.red = mtdata.(WHERE(hdr EQ 'RADIANCE_MAXIMUM_BAND_'+red_band))
  LMAX.nir = mtdata.(WHERE(hdr EQ 'RADIANCE_MAXIMUM_BAND_'+nir_band))
  LMIN.red = mtdata.(WHERE(hdr EQ 'RADIANCE_MINIMUM_BAND_'+red_band))
  LMIN.nir = mtdata.(WHERE(hdr EQ 'RADIANCE_MINIMUM_BAND_'+nir_band))
  ;get the date to compute correction for sun-earth distance
  tmp = STRSPLIT(mtdata.(WHERE(hdr EQ 'DATE_ACQUIRED')),'-', /EXTRACT)
  ddmmyyyy_of_acq = [tmp[2],tmp[1],tmp[0]]  ; date of acquisition
  doy_dist = SE_dist()
  ind = WHERE(doy_dist[*,0] EQ ddmmyyyy2doy(ddmmyyyy_of_acq[0], ddmmyyyy_of_acq[1], ddmmyyyy_of_acq[2]))
  d = (REFORM(doy_dist[ind, 1]))[0]
  ;now get the SZA
  SZA = 90.0 - mtdata.(WHERE(hdr EQ 'SUN_ELEVATION'))[0]
  
  PRINT, 'Processing sensor: ' + sensor + ', scene ID: ' + scene_id
  
  ;catch slc problem
  ind = WHERE((nir EQ 0.0) OR (red EQ 0), count)
  IF (count GT 0) THEN BEGIN
    nir[ind] = !VALUES.F_NAN
    red[ind] = !VALUES.F_NAN
  ENDIF
  
  ;4. calibrate and save Ls7 NDVI
   CASE sensor OF
    'ETM+': BEGIN
      nir_rad = ((LMAX.nir - LMIN.nir)/(QCALMAX.nir-QCALMIN.nir)) * (nir-QCALMIN.nir) + LMIN.nir
      red_rad = ((LMAX.red - LMIN.red)/(QCALMAX.red-QCALMIN.red)) * (red-QCALMIN.red) + LMIN.red
      nir_r = (!PI * nir_rad * d^2) / (FLOAT(ESUN.nir) * COS(SZA*!DtoR))
      red_r = (!PI * red_rad * d^2) / (FLOAT(ESUN.red) * COS(SZA*!DtoR))
    END
    'OLI_TIRS': BEGIN
      ;GEE LS L1T_TOA is already reflectance
      nir_r = nir
      red_r = red
    END
    ELSE: BEGIN
      PRINT, 'Sensor ' + STRTRIM(sensor,2) + ' is not coded, the program will stop'
    END
  ENDCASE
  
  
  ndvi = (nir_r - red_r) / (nir_r + red_r)
  
  ndvi = gain * ndvi + offset
  fn_ndvi = 'TOArefNDVI_' + fn_base
  res = FILE_SEARCH(out_dir, /TEST_DIRECTORY)
  IF (res EQ '') THEN FILE_MKDIR,out_dir
  ;5 save it as a geotif and then as envi file
  WRITE_TIFF, out_dir + '\' + fn_ndvi + '.tif', ndvi, /FLOAT, GEOTIFF = geoinfo 
;  e = ENVI(/HEADLESS)
;  raster1 = e.OpenRaster(out_dir + '\' + fn_ndvi + '.tif')
;  res = FILE_SEARCH(out_dir + '\' + fn_ndvi + '.img')
;  IF (res NE '') THEN FILE_DELETE, res
;  raster1.Export, out_dir + '\' + fn_ndvi + '.img', 'envi'
;  e.Close
  ;CLOSE, /ALL
ENDELSE
RETURN, ndvi

END