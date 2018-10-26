PRO handler_for_tileX
  X = 'H22V08'


  root = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles'
  rootX = root + '\' + X + 'ENVI'
  subdirs = FILE_SEARCH(rootX+'\*',/TEST_DIRECTORY)
  PRINT, TRANSPOSE(subdirs)
  FOR i=0,N_ELEMENTS(subdirs)-1 DO BEGIN
    tmp = STRSPLIT(subdirs[i], '\', /EXTRACT)
    IF (STRMATCH(tmp[-1], 'O*') EQ 1) THEN BEGIN 
      ;qui devo chimare make meta
      dir_in = subdirs[i]
      ;se la dir e' *UNfilte devo fare due run, una per ndvi e una per similJD of acq
      date_format = 'YYYYTT'
      first_date = '200301'
      last_date = '201636'
      dir_out = rootX
      IF (STRMATCH(dir_in, '*Unfilte')) THEN BEGIN
        ;ndvi
        fileList = FILE_SEARCH(dir_in, '*U.img')
        ;get filneame
        fn = FILE_BASENAME(fileList[0], '.img')
        ;get suffix and prefix from the first file
        lastPre = STRPOS(fn,'2.t') + 3
        prefix = STRMID(fn, 0, lastPre)
        firstSuf = STRPOS(fn, '.006')
        suffix = STRMID(fn, firstSuf, STRLEN(fn)-firstSuf)
        PRINT, prefix, '  ',suffix
        tmp = STRSPLIT(dir_in,'\', /EXTRACT)
        meta_out = tmp[-1] + '2003-2016.mta'  ;il file prende il nome dalla directory
        res = make_meta(dir_in, prefix, date_format, suffix, first_date, last_date, dir_out, meta_out)
        tmp = FILE_BASENAME(meta_out, '.mta')
        res = make_bil_from_meta(dir_out + '\' + meta_out, dir_out + '\' + tmp + '_bil')
        ;pseudo jd
        fileList = FILE_SEARCH(dir_in, '*UDM.img')
        ;get filneame
        fn = FILE_BASENAME(fileList[0], '.img')
        ;get suffix and prefix from the first file
        lastPre = STRPOS(fn,'2.t') + 3
        prefix = STRMID(fn, 0, lastPre)
        firstSuf = STRPOS(fn, '.006')
        suffix = STRMID(fn, firstSuf, STRLEN(fn)-firstSuf)
        PRINT, prefix, '  ',suffix
        tmp = STRSPLIT(dir_in,'\', /EXTRACT)
        meta_out = tmp[-1] + 'UDM_2003-2016.mta'  ;il file prende il nome dalla directory
        res = make_meta(dir_in, prefix, date_format, suffix, first_date, last_date, dir_out, meta_out)
        tmp = FILE_BASENAME(meta_out, '.mta')
        res = make_bil_from_meta(dir_out + '\' + meta_out, dir_out + '\' + tmp + '_bil')
      ENDIF ELSE BEGIN
        ;normal case
        fileList = FILE_SEARCH(dir_in, '*.img')
        ;get filneame
        fn = FILE_BASENAME(fileList[0], '.img')
        ;get suffix and prefix from the first file
        lastPre = STRPOS(fn,'2.t') + 3
        prefix = STRMID(fn, 0, lastPre)
        firstSuf = STRPOS(fn, '.006')
        suffix = STRMID(fn, firstSuf, STRLEN(fn)-firstSuf)
        PRINT, prefix, '  ',suffix
        tmp = STRSPLIT(dir_in,'\', /EXTRACT)
        meta_out = tmp[-1] + '2003-2016.mta'  ;il file prende il nome dalla directory
        res = make_meta(dir_in, prefix, date_format, suffix, first_date, last_date, dir_out, meta_out)
        tmp = FILE_BASENAME(meta_out, '.mta')
        res = make_bil_from_meta(dir_out + '\' + meta_out, dir_out + '\' + tmp + '_bil')
      ENDELSE
    ENDIF
  ENDFOR
END
