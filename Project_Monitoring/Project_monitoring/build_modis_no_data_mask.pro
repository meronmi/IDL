FUNCTION build_MODIS_no_data_mask, in_dir, out_dir, fn, MODIS_ts_bands2use, fn_o, valueNaN 
;fn_classification_mask = build_MODIS_no_data_mask(in_dir, out_dir, fn_eMODIS_ts4class, 255)

tmp = STRSPLIT(fn, '.', /EXTRACT)
IF (tmp[1] NE 'tif') THEN BEGIN
  PRINT, 'Execution stopped at build_slc_mask_funct because ' + fn[i] + ' is not a tif file'
  STOP
ENDIF
res = QUERY_TIFF(in_dir + '\' +fn, infos)
dims_ref = infos.dimensions
nb_ref = infos.channels
dt_ref = infos.pixel_type
data = READ_TIFF(in_dir + '\' +fn)
;here check all pixels that have 255
mask = BYTARR(dims_ref[0],dims_ref[1])*0B
FOR s = 0, dims_ref[0]-1 DO BEGIN
  FOR l = 0, dims_ref[1]-1 DO BEGIN
    ind = WHERE(data[MODIS_ts_bands2use,s,l] GT valueNaN, count)
    IF (count EQ 0) THEN mask[s,l]=1
  ENDFOR
ENDFOR
res = QUERY_TIFF(in_dir + '\' + fn, GEOTIFF = geoinfo)
WRITE_TIFF, out_dir + '\' + fn_o + '.tif', mask, /FLOAT, GEOTIFF = geoinfo
 e = ENVI(/HEADLESS)
raster1 = e.OpenRaster(out_dir + '\' + fn_o + '.tif')
res = FILE_SEARCH(out_dir + '\' + fn_o + '.img')
IF (res NE '') THEN FILE_DELETE, res
raster1.Export, out_dir + '\' + fn_o + '.img', 'envi'
raster1.Close
;CLOSE, /ALL
RETURN, fn_o + '.img'
END
