PRO TEST_READ_SINGLE_FILES
  storage_dir = 'U:\data\cid-bulk15\TS\42002\AFR\ACT\S10\'
  ns = 7841
  nl = 8289
  files = FILE_BASENAME(FILE_SEARCH(storage_dir+'vt*a.img'))
  ;order by timestamp
  yy = FLOAT(STRMID(files, 2, 2))
  tt = FLOAT(STRMID(files, 4, 2))
  yy[WHERE(yy GE 50)] = yy[WHERE(yy GE 50)] + 1900
  yy[WHERE(yy LT 50)] = yy[WHERE(yy LT 50)] + 2000
  yy = yy + tt/100.0
  files = files[SORT(yy)]
  
  ss = 3000
  ll = 3000
  ds = 1
  dl = 1
  PRINT, 'START'
  STARTTIME = SYSTIME(1)
  zprofile = BYTARR(ds, dl, N_ELEMENTS(files))
  FOR f = 0, N_ELEMENTS(files)-1 DO BEGIN
    zprofile[*,*,f] =  READ_BINARY (storage_dir + files[f], DATASTART = (ss * ll), DATA_TYPE = 1, DATA_DIMS=[ds,dl])
  ENDFOR
  ELAPSED_TIME = SYSTIME(1) - STARTTIME
  PRINT, 'Reading z-profile TOOK seconds: ' + STRTRIM(ELAPSED_TIME,2)
END