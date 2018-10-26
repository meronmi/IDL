
PRO test
 storage_dir = 'E:\WA\all sahel\data\as images\'
 base_pattern = 'vt*as.img'
 posYY = 2  ;position of year in filename
 posTT = 4  ;position of dekad in filenam
 ns = 7841
 nl = 1458
 ll = 600 ;line to read
 datatype = 1
 first_YY = 1999
 first_TT = 1
 last_YY = 2013
 last_TT = 36
 files = retrive_files(storage_dir, base_pattern, posYY, posTT, first_YY, first_TT, last_YY, last_TT)
 res = READ_TIME_SERIES_BY_LINE (storage_dir, files, datatype, ns, ll)
 print, REFORM(res[507,0:50])

 
END

FUNCTION retrive_files, storage_dir, base_pattern, posYY, posTT, first_YY, first_TT, last_YY, last_TT
files = FILE_BASENAME(FILE_SEARCH(storage_dir + base_pattern))
;order by timestamp
yy = FLOAT(STRMID(files, posYY, 2))
tt = FLOAT(STRMID(files, 4, 2))
yy[WHERE(yy GE 50)] = yy[WHERE(yy GE 50)] + 1900
yy[WHERE(yy LT 50)] = yy[WHERE(yy LT 50)] + 2000
yy = yy + tt/36.0
indKeep = WHERE((yy GE first_YY + first_TT/36.0) AND (yy LE last_YY + last_TT/36.0), countKeep)
IF (countKeep GT 0) THEN BEGIN
  files = files[indKeep]
  yy = yy[indKeep]
ENDIF
files = files[SORT(yy)]

RETURN, files
END

FUNCTION READ_TIME_SERIES_BY_LINE, storage_dir, files, datatype, ns, ll
IF (datatype NE 1) THEN STOP ;proper computation of datastart must be done to use with something else then byet
;PRINT, 'START'
;STARTTIME = SYSTIME(1)
lineprofile = BYTARR(ns, N_ELEMENTS(files))
;be care DATASTART is in byte 
FOR f = 0, N_ELEMENTS(files)-1 DO BEGIN
  lineprofile[*,f] =  READ_BINARY (storage_dir + files[f], DATA_START = (LONG(ns) * LONG(ll)), DATA_TYPE = datatype, DATA_DIMS=ns)
ENDFOR
;ELAPSED_TIME = SYSTIME(1) - STARTTIME
;PRINT, 'Reading line-profile TOOK seconds: ' + STRTRIM(ELAPSED_TIME,2)
RETURN, lineprofile
END