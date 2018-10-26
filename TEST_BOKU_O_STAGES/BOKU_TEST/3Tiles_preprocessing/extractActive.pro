PRO extractActive
  dir_in = '\\ies\h04\Foodsec\asap.2.0\data\phenology\active';'W:\boku'
  tile = 'H22V08'
  ;dir_stages = ['O0','O1','O2','O3','O4','OF']
  CASE tile OF
    'H22V08': te_option = '-te 40 0 50 10'
    ELSE: STOP
  ENDCASE
  
  
  
  dir_out = 'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles'+'\'+tile+'ENVI' + '\ACTIVE'
  FILE_MKDIR, dir_out
  files_in = FILE_SEARCH(dir_in,'*.img')
  files_out = dir_out + '\'+FILE_BASENAME(files_in)
  FOR j = 0, N_ELEMENTS(files_in) - 1 DO BEGIN
    str1spawn = 'set path=C:\Program Files\EMC NetWorker\nsr\bin;C:\Python278\Scripts;C:\Python278\Lib\site-packages;C:\Python278;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\OSGeo4W64\bin;C:\Program Files\TortoiseGit\bin;C:\Program Files\Exelis\IDL82\bin\bin.x86_64;C:\Program Files (x86)\Skype\Phone\;C:\Program Files\MATLAB\R2016a\bin;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3\Scripts;X:\dev\gdal\2.2.1\bin;X:\dev\gdal\2.2.1\bin\gdal\apps;c:\MRT\bin;d:\Users\meronmi\AppData\Local\Microsoft\WindowsApps'
    str2spawn = 'gdalwarp ' + te_option + ' -of ENVI ' + files_in[j] + ' ' + files_out[j]
    ;  -t_srs EPSG:4326 -te -180 -56.00892859 180 75 -s_srs EPSG:6933 -tr 0.00892857143 0.00892857143 -of ENVI ' + base_fn_out + '.img ' + base_fn_out + 'BOKUgrid.img'
    SPAWN, str1spawn + ' & ' +str2spawn, /HIDE;, /NOWAIT
    ;PRINT, i, j
  ENDFOR
  
  
  ;make the 504 band file
  test = ReadEnviWithHdr(files_out[0])
  map_info = read_info('map info', dir_out + '\' + FILE_BASENAME(files_out[0],'.img')+'.hdr')
  sz = SIZE(test)
  active = FLTARR(sz[1], sz[2], 36)
  dek_of_month = 1
  month = 1
  FOR i = 1, 36 DO BEGIN
    CASE dek_of_month OF
      1: day = '01'
      2: day = '11'
      3: day = '21'
    ENDCASE
       
    date = STRING(month, FORMAT = '(I02)')+day
    PRINT, STRING(month) +'  ' + STRING(dek_of_month) + '  --> ' + date
    ;tt = STRING(i, FORMAT = '(I02)')
    active[*,*,i-1] = FLOAT(ReadEnviWithHdr(dir_out + '\'+'active_1962'+date+'.img'))
    dek_of_month = dek_of_month + 1
    IF (dek_of_month EQ 4) THEN BEGIN
      dek_of_month = 1
      month = month +1
    ENDIF
  ENDFOR
  ind = WHERE(active NE 1)
  active[ind] = !VALUES.F_NAN
  ;concatenate 14 years on z axis, in this way I have a full year (36 deks), repeated for 14 year
  active =  [[[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]], $
    [[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]]]
  ind = 0
  ;I need it as a file
  sz = SIZE(active)
  res = write_envi_img(active, dir_out + '\all_504bands_active.img')
  res = write_envi_hdr(dir_out + '\all_504bands_active.hdr', sz[1], sz[2], 4,  NBANDS=sz[3], MAPINFO=map_info)

  
END