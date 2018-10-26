PRO bbb_resize_spi_roi
  
  dir = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_ROI_Africa\CHIRPS_SPI1_correct_fn'
  dir_out = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_1km_ROI_Africa\CHIRPS_SPI1_1km_ROI_Africa'
  ;get the list of files to be resampled
  fn_list = FILE_BASENAME(FILE_SEARCH(dir, '*.img'), '.img')
      str1spawn = 'set path=C:\Program Files\EMC NetWorker\nsr\bin;C:\Python278\Scripts;C:\Python278\Lib\site-packages;C:\Python278;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\OSGeo4W64\bin;C:\Program Files\TortoiseGit\bin;C:\Program Files\Exelis\IDL82\bin\bin.x86_64;C:\Program Files (x86)\Skype\Phone\;C:\Program Files\MATLAB\R2016a\bin;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3\Scripts;X:\dev\gdal\2.2.1\bin;X:\dev\gdal\2.2.1\bin\gdal\apps;c:\MRT\bin;d:\Users\meronmi\AppData\Local\Microsoft\WindowsApps'
  FOR i = 0, N_ELEMENTS(fn_list)-1 DO BEGIN
    ;str2spawn = 'gdalwarp -t_srs EPSG:4326 -te -180 -56.00892859 180 75 -s_srs EPSG:6933 -tr 0.00892857143 0.00892857143 -of ENVI ' + base_fn_out + '.img ' + base_fn_out + 'BOKUgrid.img'
    ;xls 
;           ul          n      res         lr
;    east  -18.0089285  8178  0.008928571 55.00892865
;    N      38.0089286  8234  0.008928571 -35.50892855

    str2spawn = 'gdalwarp -of ENVI -tr 0.00892857143 0.00892857143 -r near -te -18.0089285 -35.50892855 55.00892865 38.0089286' + $
                ' ' + dir + '\' + fn_list[i] + '.img' + ' ' + dir_out  + '\' + fn_list[i] + '_1km.img'
    PRINT, fn_list[i]              
    SPAWN, str1spawn + ' & ' + str2spawn,/HIDE ;/NOWAIT removed because it was giving page errors
  ENDFOR


END