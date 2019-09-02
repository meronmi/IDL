FUNCTION define_site_lat_crop_type
unit = CREATE_STRUCT('IGBP_CODE', '', 'Site_Code', '', 'Lat', 0.0, 'Lon', 0.0, 'years', INDGEN(17)+2000, 'crop_type', STRARR(17))
site_lat_crop_type = REPLICATE(unit, 57)
OPENR, lun, 'D:\SimMod_data\Eddy_Data\fromJOSH\ALL_site_used_2018_with_lat_and_crop_type.csv', /GET_LUN
tmp = ''
READF, lun, tmp
FOR i = 0, 56 DO BEGIN
  tmp = ''
  READF, lun, tmp
  res = STRSPLIT(tmp, ',',/EXTRACT, /PRESERVE_NULL)
  site_lat_crop_type[i].IGBP_CODE = res[0]
  site_lat_crop_type[i].Site_Code = res[1]
  site_lat_crop_type[i].lat = FLOAT(res[2])
  site_lat_crop_type[i].lon = FLOAT(res[3])
  site_lat_crop_type[i].crop_type = res[5:-1]
  
ENDFOR
FREE_LUN, lun
RETURN, site_lat_crop_type
END