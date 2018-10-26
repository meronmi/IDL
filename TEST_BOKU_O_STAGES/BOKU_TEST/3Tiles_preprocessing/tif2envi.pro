PRO tif2envi
tile = 'H22V08'


base_dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles'
base_dir_in = base_dir + '\' + 'FromFTP'

dir_in = base_dir_in +'\'+tile
dir_out = base_dir+'\'+tile+'ENVI'
;CASE tile OF
;  'H22V08': te_option = '-te 40 0 50 10'
;  ELSE: STOP
;ENDCASE

FILE_MKDIR, dir_out
subdirs = FILE_SEARCH(dir_in+'\*',/TEST_DIRECTORY)
PRINT, subdirs
subdirs_out = [!NULL]
FOR i=0,N_ELEMENTS(subdirs)-1 DO BEGIN
  tmp = STRSPLIT(subdirs[i], '\', /EXTRACT)
  subdir_out = tmp[-1] 
  ;rename them
  IF (STRMID(subdir_out,0,1,/REVERSE_OFFSET) EQ 'U') THEN subdir_out = subdir_out + 'nfilte' ELSE subdir_out = subdir_out + 'Unconstr' 
  subdir_out = dir_out + '\'+subdir_out[-1] 
  FILE_MKDIR, subdir_out
  files_in = FILE_SEARCH(subdirs[i],'*.tif')
  files_out = subdir_out + '\'+FILE_BASENAME(files_in, '.tif')+'.img'
  FOR j = 0, N_ELEMENTS(files_in) - 1 DO BEGIN
    ;PRINT, files
    str1spawn = 'set path=C:\Program Files\EMC NetWorker\nsr\bin;C:\Python278\Scripts;C:\Python278\Lib\site-packages;C:\Python278;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\OSGeo4W64\bin;C:\Program Files\TortoiseGit\bin;C:\Program Files\Exelis\IDL82\bin\bin.x86_64;C:\Program Files (x86)\Skype\Phone\;C:\Program Files\MATLAB\R2016a\bin;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3\Scripts;X:\dev\gdal\2.2.1\bin;X:\dev\gdal\2.2.1\bin\gdal\apps;c:\MRT\bin;d:\Users\meronmi\AppData\Local\Microsoft\WindowsApps'
    str2spawn = 'gdal_translate -of ENVI ' + files_in[j] + ' ' + files_out[j]
  ;  -t_srs EPSG:4326 -te -180 -56.00892859 180 75 -s_srs EPSG:6933 -tr 0.00892857143 0.00892857143 -of ENVI ' + base_fn_out + '.img ' + base_fn_out + 'BOKUgrid.img'
    SPAWN, str1spawn + ' & ' +str2spawn, /HIDE;, /NOWAIT
    ;PRINT, i, j
  ENDFOR
ENDFOR

END