PRO test_boku_make_p
  ;transform z into probability using GAUSSINT
  strPeriod = '2003-2016'
  dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA'
  zNfLf = ReadEnviWithHdr(dir+'\Z\zNfLf_2003-2016_bsq.img')
  sz = SIZE(zNfLf)
  ind = WHERE(FINITE(zNfLf))
  zNfLf[ind] = GAUSSINT(zNfLf[ind])
  res = write_envi_img(zNfLf, dir + '\Z\pNfLf_2003-2016_bsq.img')
  zNfLf = 0
  ind = 0
  res = write_envi_hdr(dir + '\Z\pNfLf_2003-2016_bsq.hdr', sz[1], sz[2], 4, NBANDS=sz[3], INTERLEAVE='bsq')


  fn_zNxLf = dir + '\Z\zN' + STRTRIM(INDGEN(5),2) + 'Lf_2003-2016_bsq.img'
  FOR i = 0, 4 DO BEGIN
    zNxLf = ReadEnviWithHdr(fn_zNxLf[i])
    ind = WHERE(FINITE(zNxLf))
    zNxLf[ind] = GAUSSINT(zNxLf[ind])
    res = write_envi_img(zNxLf, dir + '\Z\pN' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq')
    res = write_envi_hdr(dir + '\Z\pN' + STRTRIM(i,2) + 'Lf_' + strPeriod + '_bsq.hdr', sz[1], sz[2], 4, NBANDS=sz[3], INTERLEAVE='bsq')
    zNxLf = 0
    ind = 0
  ENDFOR

  fn_zNxLx = dir + '\Z\zN' + STRTRIM(INDGEN(5),2) + 'L'+ STRTRIM(INDGEN(5),2) + '_2003-2016_bsq.img'
  FOR i = 0, 4 DO BEGIN
    zNxLx = ReadEnviWithHdr(fn_zNxLx[i])
    ind = WHERE(FINITE(zNxLx))
    zNxLx[ind] = GAUSSINT(zNxLx[ind])
    res = write_envi_img(zNxLx, dir + '\Z\pN' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) +'_' + strPeriod + '_bsq')
    res = write_envi_hdr(dir + '\Z\pN' + STRTRIM(i,2) + 'L' + STRTRIM(i,2) +'_' + strPeriod + '_bsq.hdr', sz[1], sz[2], 4, NBANDS=sz[3], INTERLEAVE='bsq')
    zNxLx = 0
    ind = 0
  ENDFOR
END