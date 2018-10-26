FUNCTION build_USGS_mask, out_dir, fn_before, fn_after, pID
;input mask is 1 when there is cloud or SLC
fns = [fn_before, fn_after]
FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
  IF (FILE_SEARCH(out_dir + '\' + 'mask_' + fns[i]) EQ '') THEN STOP
  IF (i EQ 0) THEN BEGIN
    data = READ_TIFF(out_dir + '\' + 'mask_' + fns[i])
    res = QUERY_TIFF(out_dir + '\' + 'mask_' + fns[i], GEOTIFF=geoinfo)
  ENDIF ELSE BEGIN
    tmp = READ_TIFF(out_dir + '\' + 'mask_' + fns[i])
    data = data + tmp
  ENDELSE
ENDFOR
;output mask is 1 when data are ok
mask = data * 0B
ind = WHERE(data EQ 0, count)
IF (count GT 0) THEN mask[ind] = 1B

fn_o = STRTRIM(pID,2) + '_GoodDataMask'
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