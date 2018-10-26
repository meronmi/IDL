FUNCTION isLS7, in_dir, fn
  ;Determine if it's LS7 or LS8
  ;open the metadata file (csv on one column)
  fn_base = FILE_BASENAME(fn, '.tif') 
  mta_fn = fn_base + '.csv'
  mtdata = READ_CSV(in_dir + '\' + mta_fn, HEADER=hdr) 
  sensor = mtdata.(WHERE(hdr EQ 'SENSOR_ID')) ;can be 'ETM+' or 'OLI_TIRS'
  IF (sensor EQ 'ETM+') THEN RETURN, 1 ELSE RETURN, 0
END
