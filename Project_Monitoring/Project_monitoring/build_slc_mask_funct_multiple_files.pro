FUNCTION build_slc_mask_funct_multiple_files, in_dir, out_dir, fns, LS7bands2use 
;use SLC in 4321, discard 5 and 7 that would increase its dimension
  FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
    ;test it's a tif
    tmp = STRSPLIT(fns[i], '.', /EXTRACT)
    IF (tmp[1] NE 'tif') THEN BEGIN
      PRINT, 'Execution stopped at build_slc_mask_funct because ' + fn[i] + ' is not a tif file'
      STOP
    ENDIF
    ;check for equal dims
    IF (i eq 0) THEN BEGIN
      res = QUERY_TIFF(in_dir + '\' +fns[i], infos)
      dims_ref = infos.dimensions
      nb_ref = infos.channels
      dt_ref = infos.pixel_type
    ENDIF ELSE BEGIN
      res = QUERY_TIFF(in_dir + '\' +fns[i], infos)
      IF (infos.dimensions[0] NE dims_ref[0]) OR (infos.dimensions[1] NE dims_ref[1]) THEN STOP
      IF (infos.channels NE nb_ref) THEN STOP
      IF (infos.pixel_type NE dt_ref) THEN STOP
    ENDELSE
  ENDFOR
  ;accumulate data
  data = FLTARR(N_ELEMENTS(fns),dims_ref[0], dims_ref[1]) ;six is the number of bands analazyed for slc
  FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
    ;implement reading TIF
    data_tmp = READ_TIFF(in_dir + '\' +fns[i])
    ;remove non refletive bands
    ;data_ref = BYTARR(6, dims_ref[0], dims_ref[1])
    ;swir omitted
    data_ref = BYTARR(N_ELEMENTS(LS7bands2use), dims_ref[0], dims_ref[1])
    ;FOR j = 0,4 DO data_ref[j,*,*] = data_tmp[j,*,*]
    FOR j = 0, N_ELEMENTS(LS7bands2use)-1 DO data_ref[j,*,*] = data_tmp[LS7bands2use[j],*,*]
    ;deprecated: data_ref[5,*,*] = data[6,*,*]
    ;data_ref[5,*,*] = data_tmp[7,*,*]
    mult = PRODUCT(FLOAT(data_ref), 1)
    data[i,*,*] = mult
  ENDFOR
  mult = PRODUCT(FLOAT(data), 1)
  res = QUERY_TIFF(in_dir + '\' + fns[0], GEOTIFF = geoinfo)
  ;use the first base name as output file
  fn_o = FILE_BASENAME(fns[0], '.tif')
  mask = BYTARR(dims_ref[0], dims_ref[1]) * 0B
  ind = WHERE(mult GT 0, count)
  mask[ind] = 1B

  fn_o = fn_o + '_mask_SLC_bands_1-4'
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
