FUNCTION build_slc_mask_funct, in_dir, out_dir, fn
  ;test it's a tif
  tmp = STRSPLIT(fn, '.', /EXTRACT)
  IF (tmp[1] NE 'tif') THEN BEGIN
    PRINT, 'Execution stopped at build_slc_mask_funct because ' + fn + ' is not a tif file'
    STOP
  ENDIF
  isENVI = 0 ;1 if it is ENVI format, 0 if is tif, ENVI was omitted 05 03 2016
  ;** END OF USER PART

  IF isENVI THEN BEGIN
    ;not allowed anymore
    STOP
  ENDIF ELSE BEGIN
    ;implement reading TIF
    data = READ_TIFF(in_dir + '\' +fn)
    res = QUERY_TIFF(in_dir + '\' +fn, GEOTIFF = geoinfo)
    sz = SIZE(data) & ns = sz[2] & nl = sz[3]
    ;remove non refletive bands
    data_ref = BYTARR(6, ns, nl)
    FOR i = 0,4 DO data_ref[i,*,*] = data[i,*,*]
    ;deprecated: data_ref[5,*,*] = data[6,*,*]
    data_ref[5,*,*] = data[7,*,*]
    fn_o = FILE_BASENAME(fn, '.tif')
    mult = PRODUCT(FLOAT(data_ref), 1)
  ENDELSE
  mask = BYTARR(ns, nl) * 0B

  ind = WHERE(mult GT 0, count)
  mask[ind] = 1B

  fn_o = out_dir + '\' + fn_o + '_mask_SLC_bands_1-5and8'
  WRITE_TIFF, fn_o + '.tif', mask, /FLOAT, GEOTIFF = geoinfo
  e = ENVI(/HEADLESS)
  raster1 = e.OpenRaster(fn_o + '.tif')
  res = FILE_SEARCH(fn_o + '.img')
  IF (res NE '') THEN FILE_DELETE, res
  raster1.Export, fn_o + '.img', 'envi'
  raster1.Close
  CLOSE, /ALL
  RETURN, fn_o + '.img'
END
