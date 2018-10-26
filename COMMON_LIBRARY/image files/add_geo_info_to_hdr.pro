PRO add_geo_info_to_hdr
geo_line_2_add = 'map info = {Geographic Lat/Lon, 1, 1, -180, 75, 0.1875, 0.1875, WGS-84, units=Degrees}'; 'map info = {Geographic Lat/Lon, 1, 1, -18.0044643, 38.0044643, 0.00892857143, 0.00892857143, WGS-84}'
res = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.hdr', GET_PATH = dir)
n = N_ELEMENTS(res)
FOR i=0, n-1 DO BEGIN
  OPENW, lun, res[i], /GET_LUN, APPEND=1
  PRINTF, lun,geo_line_2_add
  FREE_LUN, lun
ENDFOR
END