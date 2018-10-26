Function load_scale_flag_one_by_one, filein, ns, nl, gain, offset, minval, maxval
  ; Input files are opened by performing ASSOC to one line
  OPENR, R1, filein, /GET_LUN
  line_ass_data = ASSOC(R1, BYTARR(ns,nl))
  fileout = STRMID(filein, 0, STRLEN(filein)-4) + '_ScaledFloat.img'
  ; Create files for output
  IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
  OPENW, W1, fileout, /GET_LUN

  data=FLTARR(ns, nl)
  data=FLOAT(line_ass_data[0])
  ind=where((data GT maxval) OR (data LT minval), count)
  
  data = FLOAT(TEMPORARY(data)) * FLOAT(gain) + FLOAT(offset)
  if (count ne 0) then data[ind]= !VALUES.F_NAN;-999
  WRITEU, W1, data
  CLOSE, /ALL
  
  ;now modify the hdr
  hdrfilein =   STRMID(filein, 0, STRLEN(filein)-4) + '.hdr'
  hdrfileout = STRMID(filein, 0, STRLEN(filein)-4) + '_ScaledFloat.hdr'
  OPENR, R1, hdrfilein, /GET_LUN
  OPENW, W1, hdrfileout, /GET_LUN
  WHILE ~ EOF(R1) DO BEGIN
    line = ''
    READF, R1, line
    IF (line EQ 'data type = 1') THEN line = 'data type = 4'
    IF (line EQ 'flags = {0=nodata, 254=water}') THEN line = 'flags = {-999=nodata or outside limits}'
    IF (line EQ 'values = {NDVI, -, 1, 200, 92, 181, -1, 0.01}') THEN line = 'values = {NDVI, -, 0.0, 1.0, -, -, -, -}'
    PRINTF, W1, line
  ENDWHILE  
  CLOSE, /ALL
  return, 1
End

Function load_scale_flag_one_by_one_float, filein, ns, nl, gain, offset, minval, maxval
  ; Input files are opened by performing ASSOC to one line
  OPENR, R1, filein, /GET_LUN
  line_ass_data = ASSOC(R1, FLTARR(ns,nl))
  fileout = STRMID(filein, 0, STRLEN(filein)-4) + '_ScaledFloat.img'
  ; Create files for output
  IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
  OPENW, W1, fileout, /GET_LUN

  data=FLTARR(ns, nl)
  data=FLOAT(line_ass_data[0])
  ind=where((data GT maxval) OR (data LT minval), count)

  data = FLOAT(TEMPORARY(data)) * FLOAT(gain) + FLOAT(offset)
  if (count ne 0) then data[ind]= !VALUES.F_NAN;-999
  WRITEU, W1, data
  CLOSE, /ALL

  ;now modify the hdr
  hdrfilein =   STRMID(filein, 0, STRLEN(filein)-4) + '.hdr'
  hdrfileout = STRMID(filein, 0, STRLEN(filein)-4) + '_ScaledFloat.hdr'
  OPENR, R1, hdrfilein, /GET_LUN
  OPENW, W1, hdrfileout, /GET_LUN
  WHILE ~ EOF(R1) DO BEGIN
    line = ''
    READF, R1, line
    IF (line EQ 'data type = 1') THEN line = 'data type = 4'
    IF (line EQ 'flags = {0=nodata, 254=water}') THEN line = 'flags = {-999=nodata or outside limits}'
    IF (line EQ 'values = {NDVI, -, 1, 200, 92, 181, -1, 0.01}') THEN line = 'values = {NDVI, -, 0.0, 1.0, -, -, -, -}'
    PRINTF, W1, line
  ENDWHILE
  CLOSE, /ALL
  return, 1
End

Function load_scale_flag_one_by_one_byte, filein, ns, nl, gain, offset, minval, maxval
  ; Input files are opened by performing ASSOC to one line
  OPENR, R1, filein, /GET_LUN
  line_ass_data = ASSOC(R1, BYTARR(ns,nl))
  fileout = STRMID(filein, 0, STRLEN(filein)-4) + '_Scaled0_100byte.img'
  ; Create files for output
  IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
  OPENW, W1, fileout, /GET_LUN

  
  data = line_ass_data[0]
  ind=where((data GT maxval) OR (data LT minval), count)

  data = data - 100
  if (count ne 0) then data[ind]= 255
  data = BYTE(data)
  WRITEU, W1, data
  CLOSE, /ALL

  ;now modify the hdr
  hdrfilein =   STRMID(filein, 0, STRLEN(filein)-4) + '.hdr'
  hdrfileout = STRMID(filein, 0, STRLEN(filein)-4) + '_Scaled0_100byte.hdr'
  OPENR, R1, hdrfilein, /GET_LUN
  OPENW, W1, hdrfileout, /GET_LUN
  WHILE ~ EOF(R1) DO BEGIN
    line = ''
    READF, R1, line
    IF (line EQ 'data type = 1') THEN line = 'data type = 1'
    IF (line EQ 'flags = {0=nodata, 254=water}') THEN line = 'flags = {255=nodata or outside limits}'
    IF (line EQ 'values = {NDVI, -, 1, 200, 92, 181, -1, 0.01}') THEN line = 'values = {NDVI, -, 0, 100, -, -, -, -}'
    PRINTF, W1, line
  ENDWHILE
  CLOSE, /ALL
  return, 1
End

PRO scale_one_by_one_puntland_chirps
  dir = 'S:\Actions\FOODSEC\projects\EU_del_SOMALIA\CHIRPS\year_sum'
  ns = 232
  nl = 290
  gain=1.0  & offset=0.0  & maxval=1e+030 & minval=0

  f_list = FILE_SEARCH(dir, 'som*.img')

  ;FOR i = 0, N_ELEMENTS(f_list) DO PRINT, i*(load_scale_flag_one_by_one(f_list[i], ns, nl, gain, offset, minval, maxval))
  FOR i = 0, N_ELEMENTS(f_list)-1 DO PRINT, i*(load_scale_flag_one_by_one_float(f_list[i], ns, nl, gain, offset, minval, maxval))

  PRINT, 'ENDED'
END


PRO scale_one_by_one_puntland
  dir = 'S:\Actions\FOODSEC\projects\EU_del_SOMALIA\eMODIS_som\Max_annual_comp_on_is\AAA for arcgis\LTA';'S:\Actions\FOODSEC\projects\EU_del_SOMALIA\eMODIS_som\Max_annual_comp_on_is'
  ns = 4800
  nl = 6000
  gain=1/100.0  & offset=-1.0  & maxval=200 & minval=101

  f_list = FILE_SEARCH(dir, 'som*.img')

  ;FOR i = 0, N_ELEMENTS(f_list) DO PRINT, i*(load_scale_flag_one_by_one(f_list[i], ns, nl, gain, offset, minval, maxval))
  FOR i = 0, N_ELEMENTS(f_list)-1 DO PRINT, i*(load_scale_flag_one_by_one(f_list[i], ns, nl, gain, offset, minval, maxval))

  PRINT, 'ENDED'
END

PRO scale_one_by_one
dir = 'S:\Actions\FOODSEC\temporary_share\4 Anne\SG_GGW\ACT_M10'
ns = 2363
nl = 1079
gain=1/100.0  & offset=-1.0  & maxval=200 & minval=100

f_list = FILE_SEARCH(dir, '*.img')

;FOR i = 0, N_ELEMENTS(f_list) DO PRINT, i*(load_scale_flag_one_by_one(f_list[i], ns, nl, gain, offset, minval, maxval))
FOR i = 0, N_ELEMENTS(f_list) DO PRINT, i*(load_scale_flag_one_by_one_byte(f_list[i], ns, nl, gain, offset, minval, maxval))

PRINT, 'ENDED'
END