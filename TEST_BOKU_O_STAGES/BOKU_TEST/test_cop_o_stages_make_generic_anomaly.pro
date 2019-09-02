PRO scri4w2cop
  test_cop_o_stages_make_generic_anomaly, 'z', '1'
  print, 'z done'
  test_cop_o_stages_make_generic_anomaly, 'v', '1'
  print, 'vci done'
  test_cop_o_stages_make_generic_anomaly, 'n', '1'
  print, 'nep done'
END


PRO test_cop_o_stages_make_generic_anomaly, var, version
  ;directory where bil stack files are store
  strPeriod = '1634-1831'
  dir = '\\ies\d5\foodsec\Share\COP_FAPAR\thinned'
  constages = ['RT0','RT1','RT2']
  product_gain = 0.005
  product_off = 0.0
  product_min = 0
  product_max = 200
  version = STRING(version) ;version of output (stored in directory 'variable'+version
  ;we have to compute
  ;all that use ltaF (0,1,2,3,4,F)
  ;all that use same stage for nrt and lta (0,1,2,3,4)

  ;first read all bil
  ns = FIX(read_info('samples', dir + '\' + constages[0] + '_' + strPeriod + '_bil.hdr'))
  nl = FIX(read_info('lines', dir + '\' + constages[0] + '_' + strPeriod + '_bil.hdr'))
  nb = FIX(read_info('bands', dir + '\' + constages[0] + '_' + strPeriod + '_bil.hdr'))
  dt = FIX(read_info('data type', dir + '\' + constages[0] + '_' + strPeriod + '_bil.hdr'))
  x = MAKE_ARRAY(ns, nl, nb, N_ELEMENTS(constages), TYPE = dt)
  FOR i = 0, N_ELEMENTS(constages)-1 DO BEGIN
    x[*,*,*,i] = ReadBilWithHdr(dir + '\' + constages[i] + '_' + strPeriod + '_bil')
  ENDFOR
  ;now the full time series
  
  xf = ReadBilWithHdr(dir + '\' + 'RT6' + '_' + strPeriod + '_bil')
  strPeriodTs = '9901-1831'
  xfhis = ReadBilWithHdr(dir + '\' + 'RT6' + '_' + strPeriodTs + '_bil')


  ;do anomaly families one by one or there will not be enough space
  sz = SIZE(xf)


  ;Final for both NRT and LTA
  PRINT, SYSTIME()
  ;aNfLf = a_by_dek_array3d(REFORM(xf), var, 1, 31, MINVAL = product_min, MAXVAL = product_max, GAIN = product_gain, OFFSET = product_off, STATFNAME=dir+'\LTA_STATS\OF_dek_', INCLUDECURRENTDATE = 1)
  PRINT, 'NfHf Var', var
  aNfLf = a_by_dek_array3d_arbitraryX_XHIS(REFORM(xf), var, 34, 31, 2016, XHISdata = REFORM(xfhis), XHIS_first_dek = 1, XHIS_last_dek = 31, $
    XHIS_first_year = 1999, MINVAL = product_min, MAXVAL = product_max, GAIN = product_gain, OFFSET = product_off, $
    INCLUDECURRENTDATE = 1)
  dir_out = dir + '\' + var.ToUpper()+version
  FILE_MKDIR, dir_out
  fn_out = var + 'NfLf_' + strPeriod + '_bsq'
  res = write_envi_img(aNfLf, dir_out + '\' + fn_out);dir + '\Z2\zNfLf_' + strPeriod + '_bsq')
  res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
  aNfLf = 0 ;free the memory allocated

  ;Consolidation X for NRT and Final for LTA
  ;zNxLf = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_ltaF, Z_nrt1_ltaF, .. Z_nrt4_ltaF
  FOR i = 0, N_ELEMENTS(constages)-1 DO BEGIN
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
    PRINT, SYSTIME()
    PRINT, 'NxHf Var', var, 'Conso stage', i
    aNxLf = a_by_dek_array3d_arbitraryX_XHIS(REFORM(x[*,*,*,i]), var, 34, 31, 2016, XHISdata = REFORM(xfhis), XHIS_first_dek = 1, XHIS_last_dek = 31, $
      XHIS_first_year = 1999, MINVAL = product_min, MAXVAL = product_max, GAIN = product_gain, OFFSET = product_off, $
      INCLUDECURRENTDATE = 0)
    ;aNxLf = a_by_dek_array3d(REFORM(x[*,*,*,i]), var, 1, 36, XHIS = xf, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, INCLUDECURRENTDATE = 0)
    PRINT, SYSTIME()
    fn_out = var + 'N' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq'
    res = write_envi_img(aNxLf, dir_out + '\' + fn_out)
    res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
    aNxLf = 0 ;free the memory allocated
  ENDFOR


; The following is not needed for cop (we do not have full time series of unconsolidated stages)
;  ;Consolidation X for NRT and LTA
;  ;zNxLx = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_lta0, Z_nrt1_lta1, .. Z_nrt4_lta4
;  FOR i = 0, 4 DO BEGIN
;    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
;    PRINT, SYSTIME()
;    aNxLx = a_by_dek_array3d(REFORM(x[*,*,*,i]), var, 1, 36, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, STATFNAME=dir+'\LTA_STATS\O'+STRTRIM(i,2)+'_dek_', INCLUDECURRENTDATE = 1)
;    PRINT, SYSTIME()
;    fn_out = var + 'N' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) + '_' + strPeriod + '_bsq'
;    res = write_envi_img(aNxLx, dir_out + '\' + fn_out)
;    res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
;    aNxLx = 0
;  ENDFOR

END