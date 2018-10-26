FUNCTION make_multi_file_tif, dir, out_dir, fns, bands, id_proj
;out of different tiff files, it makes a single file joining the bands "bands" of ech file
;it returns the name of the joined file (if it does not exists)

;make the file name
;file = 'Joined_Bands'
;FOR j = 0, N_ELEMENTS(bands)-1 DO file = file + STRTRIM(bands[j],2) + '_'
FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
  ;test it's a tif
  tmp = STRSPLIT(fns[i], '.', /EXTRACT)
  IF (tmp[1] NE 'tif') THEN STOP
;  tmp = STRSPLIT(tmp[0], '\', /EXTRACT)
;  file = file + STRTRIM(tmp[-1],2)
;  IF (i NE  (N_ELEMENTS(fns)-1)) THEN file = file +'_'
ENDFOR
file = 'Joined_Bands_for_proj_' + STRTRIM(id_proj,2)
;checke if exists (if exists do nothing and return the file name)
IF ((FILE_SEARCH(dir + '\' + file + '.tif')) EQ '')THEN BEGIN
  res = QUERY_TIFF(dir + '\' +fns[0], infos)
  dims_ref = infos.dimensions
  nb_ref = infos.channels
  dt_ref = infos.pixel_type
  IF (dt_ref NE 2) THEN STOP
  ;accumulate data
  data = INTARR(N_ELEMENTS(fns)*N_ELEMENTS(bands),dims_ref[0], dims_ref[1]) ;six is the number of bands analazyed for slc
  b = 0
  FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
    ;implement reading TIF
    data_tmp = READ_TIFF(dir + '\' +fns[i])
    FOR j = 0, N_ELEMENTS(bands)-1 DO BEGIN
      data[b,*,*] = data_tmp[bands[j],*,*] & b = b + 1
    ENDFOR
  ENDFOR
  res = QUERY_TIFF(dir + '\' + fns[0], GEOTIFF = geoinfo)
  ;IF (STRLEN(file) GT 20) THEN file = STRMID(file,0,15) + '_
  WRITE_TIFF, out_dir + '\' + file + '.tif', data, /FLOAT, GEOTIFF = geoinfo
ENDIF 
RETURN, file + '.tif'
END