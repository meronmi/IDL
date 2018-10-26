FUNCTION scale_MODIS, dir, fn, valNoDataModis , gain, offset
  ns = FIX(read_info('samples', dir + '\' + remove_ext_from_fn(fn) + '.hdr'))
  nl = FIX(read_info('lines', dir + '\' + remove_ext_from_fn(fn) + '.hdr'))
  dt = FIX(read_info('data type',  dir + '\' + remove_ext_from_fn(fn) + '.hdr'))
  tmp = MAKE_ARRAY(ns,nl,TYPE=dt)
  OPENR, lun, dir + '\' + fn, /GET_LUN
  READU, lun, tmp
  FREE_LUN, lun
  ind = WHERE(tmp GE valNoDataModis, count)
  tmp = FLOAT(tmp) * gain + offset
  IF (count GT 0) THEN tmp[ind] = !VALUES.F_NAN
  RETURN, tmp
END