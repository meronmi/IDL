PRO raphael_read_csv_and_query
  
  use_sav = 1
  
  csv_file = 'X:\LUCAS_S1-S2_extraction_20190520.csv'
  IF use_sav EQ 1 THEN BEGIN
    RESTORE, FILENAME=csv_file+'.original.sav'
  ENDIF ELSE BEGIN
    res = READ_CSV(csv_file, HEADER=hdr)
    x = rename_tags(res, TAG_NAMES(res), ['pid','pointID','date','x','y','nuts0','crop_ID','orbit','cropname','surv_date','index_name','index_value'])
    SAVE, x,  FILENAME=csv_file+'.original.sav'
  ENDELSE
  
  res = 0
  ind = WHERE(x.index_name EQ 'VH_mean' AND $
              x.pointID EQ 31442152, count)
  dateJD = YYYYbMMbDD2jd(x.date[ind])
  subAscDate= SORT(dateJD)
  ind = ind[subAscDate]
  PRINT, count
  PRINT, TAG_NAMES(x)
  FOR i = 0, count-1 DO BEGIN
    str = ''
    FOR j = 0, N_TAGS(x)-1 DO str = [str, STRTRIM(x.(j)[ind[i]],2)]
    PRINT, str
    IF i EQ 10 THEN STOP
  ENDFOR
  PRINT, 'here'
END