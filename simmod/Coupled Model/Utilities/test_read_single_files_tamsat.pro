PRO analysis
zProfileFile = 'd:\Users\meronmi\Documents\IDL\simmod\Coupled Model\z_tamsat.sav'
RESTORE, FILENAME = zProfileFile
y = REFORM(zprofile)
yy = FLTARR(jd[-1]-jd[0]+1+9)
yy2 = yy
xx = LONG(yy)
xx2 = xx
ind = jd-jd[0] 
xx[ind] = jd
yy[ind] = y
FOR i = 0, N_ELEMENTS(ind)-1 DO BEGIN
  ind_beginning_of_dek = ind[i]
  IF (i EQ N_ELEMENTS(ind)-1) THEN ind_end_of_dek = N_ELEMENTS(yy)-1 ELSE ind_end_of_dek = ind[i+1]-1 
  yy2[ind_beginning_of_dek:ind_end_of_dek] = yy[ind_beginning_of_dek]/FLOAT(ind_end_of_dek-ind_beginning_of_dek+1)
ENDFOR
xx2 = jd[0] + LINDGEN(N_ELEMENTS(xx))
ddmmyy = JD2DDMMYYYY(xx2)
WRITE_CSV, 'd:\Users\meronmi\Documents\DB connection\tamsat.csv', ddmmyy[*,2], ddmmyy[*,1], ddmmyy[*,0], yy2, $
   HEADER = ['Year','Month','Day','Tamsat_rain_from_dek']
;xx2 is now day in JD and yy2 is daily rain
PRINT, 'xxx'

END


PRO TEST_READ_SINGLE_FILES_TAMSAT

  zProfileFile = 'd:\Users\meronmi\Documents\IDL\simmod\Coupled Model\z_tamsat.sav'
  storage_dir = 'S:\Actions\FOODSEC\base_data\meteo\tamsat\'
  ns = 1894
  nl = 1974
  files = FILE_BASENAME(FILE_SEARCH(storage_dir+'tamsat_*.img'))
  ;order by timestamp
  yyyy = FIX(STRMID(files, 7, 4))
  mm = FIX(STRMID(files, 11, 2))
  dd = FLOAT(STRMID(files, 13, 2))
  jd = JULDAY(mm, dd, yyyy) 
  
  files = files[SORT(jd)]
  
  ;IDL coord 
  ss = 1320
  ll = 660
  
  ds = 1
  dl = 1
  PRINT, 'START'
  STARTTIME = SYSTIME(1)
  zprofile = INTARR(ds, dl, N_ELEMENTS(files))
  FOR f = 0, N_ELEMENTS(files)-1 DO BEGIN
    zprofile[*,*,f] =  READ_BINARY (storage_dir + files[f], DATA_START = 2*(LONG(ns)*ll+ss), DATA_TYPE = 2, DATA_DIMS=[ds,dl])
  ENDFOR
  ELAPSED_TIME = SYSTIME(1) - STARTTIME
  
  SAVE, /VARIABLES, FILENAME = zProfileFile
  PRINT, 'Reading z-profile TOOK seconds: ' + STRTRIM(ELAPSED_TIME,2)
  END