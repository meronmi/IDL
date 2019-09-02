Pro make_all
 ;res = FILE_SEARCH('X:\Active Projects\FAO SOFI\ASAP20data\Phenology', '*.img')
 res = FILE_SEARCH('X:\Active Projects\FAO SOFI\ASAP20data\Crop and Rangeland masks', '*.img')
 FOR i = 0, N_ELEMENTS(res)-1 DO BEGIN
  PRINT, fromENVI2netcdf(res[i], -180, 75, 0.00892857143)
 ENDFOR
END

;make one PRINT, fromENVI2netcdf_multiband('\\ies\d5\foodsec\Users\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\processed\global_NDVI3g_filteredSG_monthlyMean.img', -179.958333333333333333333, 82.1250, 0.08333333333333333333)
PRO make_matteo_zampieri
file = '\\ies\d5\asap\users_data\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\up to 2015\global_NDVI3gV1_filteredSG_monthlyMean_up_to_2015.img'
lonLeft = -179.9583333333333
latUp = 82.20833333333333
pxsz = 0.08333333333333333333
PRINT, fromENVI2netcdf_multiband(file, lonLeft, latUp, pxsz)

END

Function fromENVI2netcdf_multiband, file, lonLeft, latUp, pxsz
  ; res =  fromENVI2netcdf('X:\Active Projects\FAO SOFI\ASAP20data\Phenology\phenoe1.img', -180, 75, 0.00892857143)
  ;file = 'X:\Active Projects\FAO SOFI\ASAP20data\Phenology\phenoe1.img'
  data  = ReadEnviWithHdr(file)
  ;   Set up the file & handler
  ;n_profiles = n_elements(lat) ; just pick any of your data arrays
  fn_base  = FILE_BASENAME(file, '.img')
  fn_dir = FILE_DIRNAME(file) ;+ 'NC_files'
  FILE_MKDIR, fn_dir

  sz = SIZE(data)
  lon = DOUBLE( LINDGEN(sz[1]) * pxsz  + (lonLeft + pxsz/2.0))
  lat = DOUBLE(- LINDGEN(sz[2]) * pxsz + (latUp - pxsz/2.0))
  months = INDGEN(sz[3])
  ;   Set Dimensions
  ;d[0]=ncdf_dimdef(fid,'n_profiles',n_profiles)   ; length 'j'
  ;d[1]=ncdf_dimdef(fid,'n_bins',n_bins)           ; length 'i'
  fid=ncdf_create(fn_dir+'\'+fn_base+'NETCDF.nc',/CLOBBER,/NETCDF4_FORMAT)
  xid=ncdf_dimdef(fid,'lon',sz[1])
  yid=ncdf_dimdef(fid,'lat',sz[2])
  zid=ncdf_dimdef(fid,'month',sz[3])
  ;zid = NCDF_DIMDEF(fid, 'z', /UNLIMITED) ; Define the Z dimension.


  ;   Define variables to be stored
  var_id=ncdf_vardef(fid,'Longitude',xid,/DOUBLE)
  var_id=ncdf_vardef(fid,'Latitude',yid,/DOUBLE)
  var_id=ncdf_vardef(fid,'Months_from_1981_07',zid,/SHORT)

  ;var_id=ncdf_vardef(fid,'data',[yid,xid],/BYTE)
  var_id=ncdf_vardef(fid,'data',[xid,yid,zid],/SHORT)
  ;var_id=ncdf_vardef(fid,'Pressure',[yid,xid],/double)

  ;   Change modes
  ncdf_control,fid,/ENDEF

  ;   Write the data
  ncdf_varput,fid,'Longitude',lon
  ncdf_varput,fid,'Latitude',lat
  ncdf_varput,fid,'Months_from_1981_07',months
  ncdf_varput,fid,'data',FLOAT(data)


  ;   Close the file & release the handler
  ncdf_close, fid

  print,' > Writing complete... Closing...'
  RETURN, 0
END

Function fromENVI2netcdf, file, lonLeft, latUp, pxsz
; res =  fromENVI2netcdf('X:\Active Projects\FAO SOFI\ASAP20data\Phenology\phenoe1.img', -180, 75, 0.00892857143)
;file = 'X:\Active Projects\FAO SOFI\ASAP20data\Phenology\phenoe1.img'
data  = ReadEnviWithHdr(file)
;   Set up the file & handler
;n_profiles = n_elements(lat) ; just pick any of your data arrays
fn_base  = FILE_BASENAME(file, '.img')
fn_dir = FILE_DIRNAME(file) + 'NC_files'
FILE_MKDIR, fn_dir

sz = SIZE(data)
lon = DOUBLE( LINDGEN(sz[1]) * pxsz  + (lonLeft + pxsz/2.0)) 
lat = DOUBLE(- LINDGEN(sz[2]) * pxsz + (latUp - pxsz/2.0))
 
;   Set Dimensions
;d[0]=ncdf_dimdef(fid,'n_profiles',n_profiles)   ; length 'j'
;d[1]=ncdf_dimdef(fid,'n_bins',n_bins)           ; length 'i'
fid=ncdf_create(fn_dir+'\'+fn_base+'NETCDF.nc',/CLOBBER,/NETCDF4_FORMAT)
xid=ncdf_dimdef(fid,'lon',sz[1]) 
yid=ncdf_dimdef(fid,'lat',sz[2])
;zid = NCDF_DIMDEF(fid, 'z', /UNLIMITED) ; Define the Z dimension.
          

;   Define variables to be stored
var_id=ncdf_vardef(fid,'Longitude',xid,/DOUBLE)
var_id=ncdf_vardef(fid,'Latitude',yid,/DOUBLE)

;var_id=ncdf_vardef(fid,'data',[yid,xid],/BYTE)
var_id=ncdf_vardef(fid,'data',[xid,yid],/FLOAT)
;var_id=ncdf_vardef(fid,'Pressure',[yid,xid],/double)

;   Change modes
ncdf_control,fid,/ENDEF

;   Write the data
ncdf_varput,fid,'Longitude',lon
ncdf_varput,fid,'Latitude',lat

ncdf_varput,fid,'data',FLOAT(data)


;   Close the file & release the handler
ncdf_close, fid

print,' > Writing complete... Closing...'
RETURN, 0 
END