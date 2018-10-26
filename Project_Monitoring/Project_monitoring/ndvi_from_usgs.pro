FUNCTION NDVI_from_USGS, in_dir, out_dir, fn
;fn is the tif LS file fromUSGS
;calibration is taken from USGS reference docs

fn_base = FILE_BASENAME(fn, '.tif') 
fn_out = 'srNDVI_' + fn_base + '.tif'
fn_mask_out = 'mask_' + fn_base + '.tif'
IF (FILE_SEARCH(out_dir + '\' + fn_out) NE '') THEN BEGIN
  PRINT, 'WARNING: File ' + fn_out + ' already exists, it will be used as it is'
  ndvi = READ_TIFF(out_dir + '\' + fn_out)
ENDIF ELSE BEGIN
  ;1 Determine if it's LS7 or LS8
  sceneID = STRSPLIT(FILE_BASENAME(fn, '.tif'), '_', /EXTRACT)
  sceneID = sceneID[0]
  sensor = STRMID(sceneID,0, 3)
  PRINT, 'Sensor: ' + sensor
 
 
  ;2 Read the tif
  data = READ_TIFF(in_dir + '\' +fn)
  res = QUERY_TIFF(in_dir + '\' +fn, GEOTIFF=geoinfo)
  sz = SIZE(data)
  PRINT, 'Bands: ' + STRTRIM(sz[1],2)
  ;3 get the red and nir bands and all relevant info, position depending on being LS7 or 8
  CASE sensor OF
    'LE7': BEGIN
        IF (sz[1] NE 16) THEN BEGIN
          PRINT, 'LS7 file does not have 16 bands, the program will stop'
          STOP
        ENDIF
        red = REFORM(data(2,*,*))
        nir = REFORM(data(3,*,*))
        tmp = REFORM(data(14,*,*))  ;cloud and shadow mask (values 1 to 4 and 255 that is fill value)       
      END
    'LC8': BEGIN
        IF (sz[1] NE 10) THEN BEGIN
          PRINT, 'LS8 file does not have 10 bands, the program will stop'
          STOP
        ENDIF
        red = REFORM(data(3,*,*))
        nir = REFORM(data(4,*,*))
        tmp = REFORM(data(8,*,*))  ;cloud and shadow mask (values 1 to 4)
        
      END  
    ELSE: BEGIN
      PRINT, 'Sensor ' + STRTRIM(sensor,2) + ' is not coded, the program will stop'
      STOP
      END   
  ENDCASE
  
  ind = WHERE(tmp GT 0, count)
  mask = FIX(tmp * 0)
  IF (count GT 0) then mask[ind] = 1
  ;add outside range values (SLC has -9999, so it's removed here)
  indO = WHERE((red LT 0) OR (red GT 10000) OR $
               (nir LT 0) OR (nir GT 10000), countO)
  IF (counto GT 0) then mask[indo] = 1
  ;scale red and nir
  red = FLOAT(red) * 0.0001
  nir = FLOAT(nir) * 0.0001 
  
  ndvi = (nir - red) / (nir + red)
  ind_mask = WHERE(mask EQ 1, count_mask) 
  IF (count_mask GT 0) THEN ndvi[ind_mask] = !VALUES.F_NAN
  

  res = FILE_SEARCH(out_dir, /TEST_DIRECTORY)
  IF (res EQ '') THEN FILE_MKDIR,out_dir
  ;5 save it as a geotif and then as envi file
  WRITE_TIFF, out_dir + '\' + fn_out, ndvi, /FLOAT, GEOTIFF = geoinfo 
  WRITE_TIFF, out_dir + '\' + fn_mask_out, mask, /SHORT, /SIGNED, GEOTIFF = geoinfo
ENDELSE
RETURN, ndvi

END