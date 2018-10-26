PRO make_all_bil_I_need_server
  ;make_bil_from_meta('\\ies\d5\asap\Pheno_year_by_year\Pheno\BBB_meta_kk0.mta', '\\ies\d5\asap\Pheno_year_by_year\Pheno\BBB_kk0.bil')
  dir = '\\ies\d5\asap\Pheno_year_by_year\Pheno\'
  suffix_in = 'BBB_meta_'
  suffix_out = 'BBB_'
  fns = ['m','s','e','l','ym']
  FOR f = 0, N_ELEMENTS(fns)-1 DO BEGIN
    FOR s = 1,2 DO BEGIN
      fn_in = dir + suffix_in + fns[f] + STRTRIM(s,2) + '.mta'
      PRINT, 'Processing file: ' + fn_in
      fn_out = dir + suffix_out + fns[f] + STRTRIM(s,2) + '.bil'
      PRINT, make_bil_from_meta(fn_in, fn_out)
    ENDFOR
  ENDFOR

END