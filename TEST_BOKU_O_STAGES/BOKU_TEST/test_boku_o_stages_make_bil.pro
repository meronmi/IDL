PRO test_boku_O_stages_make_bil
  ;directory where meta files are store
  strPeriod = '2003-2016'
  dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA'
  constages = ['OF','O0','O1','O2','O3','O4']
  FOR i = 0, 5 DO BEGIN
    fn_meta = dir + '\' + constages[i] + strPeriod + '.mta'
    fn_bil = dir + '\' + constages[i] + strPeriod + '_bil'
    res = FILE_SEARCH(fn_bil)
    IF (res EQ '') THEN  ret = make_bil_from_meta(fn_meta, fn_bil)
  ENDFOR
END