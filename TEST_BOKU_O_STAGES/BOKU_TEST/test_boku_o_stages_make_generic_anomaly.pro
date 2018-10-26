PRO scri4w2
  test_boku_o_stages_make_generic_anomaly, 'z', '3'
  print, 'z done'
  test_boku_o_stages_make_generic_anomaly, 'v', '3'
  print, 'vci done'
  test_boku_o_stages_make_generic_anomaly, 'n', '3'
  print, 'nep done'
END


PRO test_boku_o_stages_make_generic_anomaly, var, version
  ;directory where bil stack files are store
  strPeriod = '2003-2016'
  dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA'
  constages = ['O0','O1','O2','O3','O4','OF']
  version = STRING(version) ;version of output (stored in directory 'variable'+version
  ;we have to compute
  ;all that use ltaF (0,1,2,3,4,F)
  ;all that use same stage for nrt and lta (0,1,2,3,4)

  ;first read all bil
  ns = FIX(read_info('samples', dir + '\' + constages[0] + strPeriod + '_bil.hdr'))
  nl = FIX(read_info('lines', dir + '\' + constages[0] + strPeriod + '_bil.hdr'))
  nb = FIX(read_info('bands', dir + '\' + constages[0] + strPeriod + '_bil.hdr'))
  dt = FIX(read_info('data type', dir + '\' + constages[0] + strPeriod + '_bil.hdr'))
  x = MAKE_ARRAY(ns, nl, nb, 5, TYPE = dt)
  x[*,*,*,0] = ReadBilWithHdr(dir + '\' + constages[0] + strPeriod + '_bil')
  x[*,*,*,1] = ReadBilWithHdr(dir + '\' + constages[1] + strPeriod + '_bil')
  x[*,*,*,2] = ReadBilWithHdr(dir + '\' + constages[2] + strPeriod + '_bil')
  x[*,*,*,3] = ReadBilWithHdr(dir + '\' + constages[3] + strPeriod + '_bil')
  x[*,*,*,4] = ReadBilWithHdr(dir + '\' + constages[4] + strPeriod + '_bil')
  xf = ReadBilWithHdr(dir + '\' + constages[5] + strPeriod + '_bil')


  ;do anomaly families one by one or there will not be enough space
  sz = SIZE(xf)


  ;Final for both NRT and LTA
  PRINT, SYSTIME()
  aNfLf = a_by_dek_array3d(xf, var, 1, 36, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, STATFNAME=dir+'\LTA_STATS\OF_dek_', INCLUDECURRENTDATE = 1)
  dir_out = dir + '\' + var.ToUpper()+version
  FILE_MKDIR, dir_out
  fn_out = var + 'NfLf_' + strPeriod + '_bsq'
  res = write_envi_img(aNfLf, dir_out + '\' + fn_out);dir + '\Z2\zNfLf_' + strPeriod + '_bsq')
  res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
  aNfLf = 0 ;free the memory allocated

  ;Consolidation X for NRT and Final for LTA
  ;zNxLf = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_ltaF, Z_nrt1_ltaF, .. Z_nrt4_ltaF
  FOR i = 0, 4 DO BEGIN
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
    PRINT, SYSTIME()
    aNxLf = a_by_dek_array3d(REFORM(x[*,*,*,i]), var, 1, 36, XHIS = xf, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, INCLUDECURRENTDATE = 0)
    PRINT, SYSTIME()
    fn_out = var + 'N' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq'
    res = write_envi_img(aNxLf, dir_out + '\' + fn_out)
    res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
    aNxLf = 0 ;free the memory allocated
  ENDFOR
  
;  ;additional test proposed by Dominique using the mean of f and as SD = SQRT(SDx^2+SDf^2)
;  FOR i = 0, 4 DO BEGIN
;    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
;    PRINT, SYSTIME()
;    zNxLfx = Z_by_dek_ARRAY3D(REFORM(x[*,*,*,i]), 1, 36, XHIS = xf, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, /MIXED_SD)
;    PRINT, SYSTIME()
;    res = write_envi_img(zNxLfx, dir + '\Z\zN' + STRTRIM(i,2) + 'Lf'+STRTRIM(i,2)+'_' + strPeriod + '_bsq')
;    res = write_envi_hdr(dir + '\Z\zN' + STRTRIM(i,2) + 'Lf'+STRTRIM(i,2)+'_' + strPeriod + '_bsq.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
;    zNxLfx = 0 ;free the memory allocated
;  ENDFOR
  
  ;Consolidation X for NRT and LTA
  ;zNxLx = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_lta0, Z_nrt1_lta1, .. Z_nrt4_lta4
  FOR i = 0, 4 DO BEGIN
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
    PRINT, SYSTIME()
    aNxLx = a_by_dek_array3d(REFORM(x[*,*,*,i]), var, 1, 36, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, STATFNAME=dir+'\LTA_STATS\O'+STRTRIM(i,2)+'_dek_', INCLUDECURRENTDATE = 1)
    PRINT, SYSTIME()
    fn_out = var + 'N' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) + '_' + strPeriod + '_bsq'
    res = write_envi_img(aNxLx, dir_out + '\' + fn_out)
    res = write_envi_hdr(dir_out + '\' + fn_out + '.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
    aNxLx = 0
  ENDFOR

END