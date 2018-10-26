FUNCTION READ_META, fn_meta
  ;read file list and info of a meta file.  
  OPENR, lun, fn_meta, /GET_LUN
  first_record = 1 ;the first file is used as template to get file infos
  file_list = !NULL
  WHILE ~ EOF(lun) DO BEGIN
    str = ''
    READF, lun, str
    str = STRTRIM(STRSPLIT(str, ':', /EXTRACT),2)
    IF (str[0] EQ 'File') THEN BEGIN
      IF (first_record EQ 1) THEN BEGIN
        first_record = 0
        fn  = FILE_BASENAME(str[1], '.img') + '.hdr'
        path = FILE_DIRNAME(str[1])
        IF (LONG(read_info('bands', path + '\' + fn)) NE 1) THEN STOP
        template_ns = LONG(read_info('samples', path + '\' + fn))
        template_nl = LONG(read_info('lines', path + '\' + fn))
        template_dt = LONG(read_info('data type', path + '\' + fn))
        template_geo1_name = 'map info' &
        template_geo1_val = read_info(template_geo1_name, path + '\' + fn)
        template_geo2_name = 'coordinate system string'
        template_geo2_val = read_info(template_geo2_name, path + '\' + fn)
      ENDIF
      file_list = [file_list, str[1]]
    ENDIF
    IF (str[0] EQ 'Bands') THEN IF (str[1] NE '1') THEN STOP
    IF (str[0] EQ 'Dims') THEN BEGIN
      strDim = STRTRIM(STRSPLIT(str[1], '-,', /EXTRACT),2)
      IF (strDim[1] NE template_ns) THEN STOP
      IF (strDim[3] NE template_nl) THEN STOP
    ENDIF
  ENDWHILE
  FREE_LUN, lun
  info = CREATE_STRUCT('filenames', file_list, 'ns', template_ns, 'nl', template_nl, 'nb', N_ELEMENTS(file_list), 'dt', template_dt, $
    'geo1_name', template_geo1_name, 'geo1_val', template_geo1_val, 'geo2_name', template_geo2_name, 'geo2_val', template_geo2_val)
  RETURN, info
END
