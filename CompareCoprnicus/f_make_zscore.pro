PRO F_make_zscore
startup = C_start_up()

;FOR i = 0, 2 DO BEGIN
FOR i = 0, 2 DO BEGIN
  ;read the bil in one shot
  fn_base = FILE_BASENAME(startup.bil_fn[i],'.bil')
  dir = FILE_DIRNAME(startup.bil_fn[i])
  ns = FIX(read_info('samples', dir + '\' + fn_base + '.hdr'))
  nl = FIX(read_info('lines', dir + '\' + fn_base + '.hdr'))
  nb = FIX(read_info('bands', dir + '\' + fn_base + '.hdr'))
  dt = FIX(read_info('data type', dir + '\' + fn_base + '.hdr'))
  map_info = read_info('map info', dir + '\' + fn_base + '.hdr')
  x = MAKE_ARRAY(ns, nl, nb, TYPE = dt)
  
  x = ReadBilWithHdr(startup.bil_fn[i])

  ;compute the z with a_by_dek_array3d.pro
  z = a_by_dek_array3d(x, 'z', 1, 17, MINVAL = startup.minval[i], MAXVAL = startup.maxval[i], GAIN = startup.gain[i], OFFSET = startup.offset[i], STATFNAME=startup.stack_path+'\LTA_STATS\'+startup.ts_name_short[i]+'_dek', INCLUDECURRENTDATE = 0)
  fn_out_base = 'z'
  res = write_envi_img(z, startup.z_fn[i]);dir + '\Z2\zNfLf_' + strPeriod + '_bsq')
  res = write_envi_hdr(startup.stack_path + '\' + FILE_BASENAME(startup.z_fn[i],'.img') + '.hdr', ns, nl, 4, NBANDS=nb, INTERLEAVE='bsq', MAPINFO=map_info)
  
ENDFOR
END