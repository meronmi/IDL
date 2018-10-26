FUNCTION read_kcs, ini_fn
tmp = read_info('kcs', ini_fn)
tmp = STRSPLIT(tmp, ',', /EXTRACT)
IF ((N_ELEMENTS(tmp) MOD 4) NE 0) THEN STOP ;they are povided in four: name, kini, kmid, kend
kcs = REPLICATE(CREATE_STRUCT('name', '', 'kini', 0.0, 'kmid', 0.0, 'kend', 0.0), N_ELEMENTS(tmp)/4)
FOR i = 0, (N_ELEMENTS(tmp)/4)-1 DO BEGIN
  kcs[i].name = tmp[i*4]
  kcs[i].kini = FLOAT(tmp[i*4+1])
  kcs[i].kmid = FLOAT(tmp[i*4+2])
  kcs[i].kend = FLOAT(tmp[i*4+3])
ENDFOR

RETURN, kcs
END