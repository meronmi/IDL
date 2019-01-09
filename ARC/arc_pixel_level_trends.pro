PRO make_all_wsi2002_in_a_dir_Anton
  dir_in = 'D:\ARC\2018 11 15 rasters';'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI
  files_in = FILE_SEARCH(dir_in,'*.img')
  FOR i = 0, N_ELEMENTS(files_in)-1 DO  BEGIN
    PRINT, make_bil_from_bsq2(files_in[i])
  ENDFOR
END


PRO make_all_in_a_dir_Anton
dir_in = 'D:\ARC\2018 11 15 rasters';'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI
files_in = FILE_SEARCH(dir_in,'*.img')
FOR i = 0, N_ELEMENTS(files_in)-1 DO  BEGIN
  PRINT, make_bil_from_bsq(files_in[i])
ENDFOR
END


FUNCTION arc_pixel_level_trends_startup
disk_letter = 'D';'X';'D'
dir = 'ARC\2018 11 15 rasters';'ARC\2018 11 13 lastWSI';'ARC\2018 10 24 Anton';'ARC'
version = 'v1'
fns = FILE_SEARCH(disk_letter + ':\' + dir, '*_bil')
startup = CREATE_STRUCT($
  'dir', disk_letter + ':\' + dir, $
  'out_dir', 'TRENDS', $
  'version', version, $
  'no_data_val', -99, $
  'fns', fns) 
RETURN, startup
END

PRO arc_pixel_level_trends
startup = arc_pixel_level_trends_startup()

FOR i = 0, N_ELEMENTS(startup.fns)-1 DO BEGIN
 fnout = FILE_BASENAME(startup.fns[i]) + '_time_trend'
 ;if it is NDVI the min max interval is [0,99000], if it is WSI is [0,100]
 res = STRMATCH(startup.fns[i], '*WSI*')
 IF (res EQ 1) THEN BEGIN
  ret = time_trend_stats(FILE_BASENAME(startup.fns[i]), startup.dir, startup.dir+'\'+startup.out_dir, fnout, [0,100])
 ENDIF ELSE BEGIN
  ret = time_trend_stats(FILE_BASENAME(startup.fns[i]), startup.dir, startup.dir+'\'+startup.out_dir, fnout, [0,99000])
 ENDELSE
 
ENDFOR
END