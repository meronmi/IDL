

FUNCTION make_meta, dir_in, prefix, date_format, suffix, first_date, last_date, dir_out, meta_out
;works only with date format YYYYTT
;EXAMPLE
; res = make_meta('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI_old_with_missing\O0Unfilte', 'MCD13A2.t', 'YYYTT', '.006.H22V08.1_km_10_days_NDVI.O0UDM', '200301', '201636', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI_old_with_missing\','AAAAAAtest.mta')
list = FILE_SEARCH(dir_in, prefix + '??????' + suffix + '.img')
listYYYYTT = FILE_BASENAME(list, '*.img')
listYYYYTT = STRMID(listYYYYTT, STRLEN(prefix), 6)
indSorted = SORT(listYYYYTT)
listYYYYTT = listYYYYTT[indSorted]
list = list[indSorted]
indFirst = WHERE(listYYYYTT EQ first_date)
indLast = WHERE(listYYYYTT EQ last_date)
list = list[indFirst:indLast]
OPENW, lun, dir_out + '\' + meta_out, /GET_LUN
PRINTF, lun, 'ENVI META FILE'

FOR i = 0, N_ELEMENTS(list)-1 DO BEGIN
  nl = read_info('lines', dir_in + '\' + FILE_BASENAME(list[i], '.img') + '.hdr')
  ns = read_info('samples', dir_in + '\' + FILE_BASENAME(list[i], '.img') + '.hdr')
  IF (i GT 1) THEN BEGIN
    IF (ns NE ns0) THEN STOP
    IF (nl NE nl0) THEN STOP
  ENDIF
  ns0 = ns
  nl0= nl
  PRINTF, lun, 'File : ' + list[i]
  PRINTF, lun, 'Bands: 1' 
  PRINTF, lun, 'Dims : ' + '1-' + STRTRIM(ns,2) + ',' + '1-' + STRTRIM(nl,2) 
  PRINTF, lun, ''
ENDFOR
FREE_LUN, lun

RETURN, 0
END