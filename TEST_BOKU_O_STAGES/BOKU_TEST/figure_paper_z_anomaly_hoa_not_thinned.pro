PRO figure_paper_z_anomaly_HoA_not_thinned
  ;directory where bil stack files are store
  ;unscaled bil filemust be present
  
  strPeriod = '_2003-2016_HoA'
  dir = 'D:\HoA'
  dir_out = 'D:\HoA'
  constages = ['O0','O1','O2','O3','O4','OF']
  
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


  ;do z-core families one by one or there want be enough space
  sz = SIZE(xf)

  ;zNfLf = FLTARR(sz[1], sz[2], sz[3])       ;Z_nrtF_ltaF
  ;ZARRAY1D2D3D, Xin, XHIS = XhisIn, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset

  ;Final for both NRT and LTA
  PRINT, SYSTIME()
  zNfLf = a_by_dek_array3d(TEMPORARY(xf), 'z', 1, 36, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, STATFNAME=dir+'\LTA_STATS\OF_dek_', INCLUDECURRENTDATE = 1)
  res = write_envi_img(zNfLf, dir_out + '\Z2\zNfLf_' + strPeriod + '_bsq')
  res = write_envi_hdr(dir_out + '\Z2\zNfLf_' + strPeriod + '_bsq.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
  zNfLf = 0 ;free the memory allocated

  ;Consolidation X for NRT and Final for LTA
  ;zNxLf = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_ltaF, Z_nrt1_ltaF, .. Z_nrt4_ltaF
  FOR i = 0, 4 DO BEGIN
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
    PRINT, SYSTIME()
    zNxLf = a_by_dek_array3d(REFORM(x[*,*,*,i]), 'z', 1, 36, XHIS = TEMPORARY(xf), MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, INCLUDECURRENTDATE = 0)
    PRINT, SYSTIME()
    res = write_envi_img(zNxLf, dir_out + '\Z2\zN' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq')
    res = write_envi_hdr(dir_out + '\Z2\zN' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
    zNxLf = 0 ;free the memory allocated
  ENDFOR
  
  ;Consolidation X for NRT and LTA
  ;zNxLx = FLTARR(sz[1], sz[2], sz[3])      ;Z_nrt0_lta0, Z_nrt1_lta1, .. Z_nrt4_lta4
  FOR i = 0, 4 DO BEGIN
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
    PRINT, SYSTIME()
    zNxLx = a_by_dek_array3d(REFORM(x[*,*,*,i]), 'z', 1, 36, MINVAL = 0, MAXVAL = 250, GAIN = 0.0048, OFFSET = -0.2, STATFNAME=dir+'\LTA_STATS\O'+STRTRIM(i,2)+'_dek_', INCLUDECURRENTDATE = 1)
    PRINT, SYSTIME()
    res = write_envi_img(zNxLx, dir_out + '\Z2\zN' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) +'_' + strPeriod + '_bsq')
    res = write_envi_hdr(dir_out + '\Z2\zN' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) +'_' + strPeriod + '_bsq.hdr', ns, nl, 4, NBANDS=sz[3], INTERLEAVE='bsq')
    zNxLx = 0
  ENDFOR

END