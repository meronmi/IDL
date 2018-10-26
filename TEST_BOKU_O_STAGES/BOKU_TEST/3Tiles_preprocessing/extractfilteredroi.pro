PRO extractFilteredROI
  base_dir_in = 'W:\boku'
  tile = 'H22V08'
  dir_stages = ['O0','O1','O2','O3','O4','OF']
  CASE tile OF
    'H22V08': te_option = '-te 40 0 50 10'
    ELSE: STOP
  ENDCASE
  FOR i = 0, N_ELEMENTS(dir_stages)-1 DO BEGIN
    dir_in = base_dir_in +'\'+dir_stages[i]
    PRINT, dir_in
    dir_out = 'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles'+'\'+tile+'ENVI' + '\'+dir_stages[i]
    FILE_MKDIR, dir_out
    files_in = FILE_SEARCH(dir_in,'*.' + dir_stages[i]+'.img')
    files_out = dir_out + '\'+FILE_BASENAME(files_in)
    FOR j = 0, N_ELEMENTS(files_in) - 1 DO BEGIN
      str1spawn = 'set path=C:\Program Files\EMC NetWorker\nsr\bin;C:\Python278\Scripts;C:\Python278\Lib\site-packages;C:\Python278;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\OSGeo4W64\bin;C:\Program Files\TortoiseGit\bin;C:\Program Files\Exelis\IDL82\bin\bin.x86_64;C:\Program Files (x86)\Skype\Phone\;C:\Program Files\MATLAB\R2016a\bin;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3\Scripts;X:\dev\gdal\2.2.1\bin;X:\dev\gdal\2.2.1\bin\gdal\apps;c:\MRT\bin;d:\Users\meronmi\AppData\Local\Microsoft\WindowsApps'
      str2spawn = 'gdalwarp ' + te_option + ' -of ENVI ' + files_in[j] + ' ' + files_out[j]
      ;  -t_srs EPSG:4326 -te -180 -56.00892859 180 75 -s_srs EPSG:6933 -tr 0.00892857143 0.00892857143 -of ENVI ' + base_fn_out + '.img ' + base_fn_out + 'BOKUgrid.img'
      SPAWN, str1spawn + ' & ' +str2spawn, /HIDE;, /NOWAIT
      ;PRINT, i, j
    ENDFOR
  ENDFOR

  
END