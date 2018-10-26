PRo move2root
root = 'Y:\meteo\GWSI\v6_noRoot_kcTimingUpdated'
;root = 'Y:\meteo\GWSI\v5_25prctRoot';'Y:\meteo\GWSI\v4_2017_05_04'
dirs = FILE_SEARCH(root, '*', /TEST_DIRECTORY)
PRINT, dirs
FOR i = 0, N_ELEMENTS(dirs)-1 DO BEGIN
  PRINT, dirs[i]
  FILE_COPY, dirs[i] + '\*.*', root
ENDFOR
END