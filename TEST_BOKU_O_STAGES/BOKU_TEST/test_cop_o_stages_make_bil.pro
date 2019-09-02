PRO test_cop_O_stages_make_bil
  ;directory where meta files are store
  strPeriod = '1634-1831'
  dir = '\\ies\d5\foodsec\Share\COP_FAPAR\thinned'
  constages = ['RT6','RT2','RT1','RT0']
  FOR i = 0, N_ELEMENTS(constages)-1 DO BEGIN
    fn_meta = dir + '\' + constages[i] + '_' + strPeriod + '.mta'
    fn_bil = dir + '\' + constages[i] + '_' + strPeriod + '_bil'
    res = FILE_SEARCH(fn_bil)
    IF (res EQ '') THEN  ret = make_bil_from_meta(fn_meta, fn_bil)
  ENDFOR
  fn_meta = dir + '\' + 'RT6_9901-1831.mta'
  fn_bil = dir + '\' + 'RT6_9901-1831_bil'
  res = FILE_SEARCH(fn_bil)
  IF (res EQ '') THEN  ret = make_bil_from_meta(fn_meta, fn_bil)
END