PRO multi_tiff_from_USGS_sr_TMPWFP
fn = DIALOG_PICKFILE(FILTER='*.tif', GET_PATH = dir, TITLE="Choose band1 file file", $
                     PATH = 'T:\Projects\GGW_monitoring\WFP\Rasters\LC08\usgs FORMAT');'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\USGS')
fn_base = STRMID(FILE_BASENAME(fn, '.tif'), 0, 40)

;year of acq
yyyy = FIX(STRMID(fn_base,17, 4))
mm = FIX(STRMID(fn_base,21, 2))
dd = FIX( STRMID(fn_base,23, 2))
;;julian date
;ddd = FIX(STRMID(fn_base,13, 3))
;jd = ddd + JULDAY(1,1,yyyy) - 1
;CALDAT, jd, MM, DD, YYYY
;determine if is LS7 or LS8
sensor = STRMID(fn_base,0, 4)
If (sensor EQ 'LC08') THEN sensor = 'LC8'
If (sensor EQ 'LC07') OR (sensor EQ 'LT05') THEN sensor = 'LE7'
PRINT, 'USG SR file: ' + fn_base
PRINT, 'Sensor: ' + sensor
CASE sensor OF
  'LC8': BEGIN
    bands_base = [fn_base + '_sr_band1', fn_base + '_sr_band2', $
                  fn_base + '_sr_band3', fn_base + '_sr_band4', $
                  fn_base + '_sr_band5', fn_base + '_sr_band6', $
                  fn_base + '_sr_band7', fn_base + '_sr_cloud', $
                  fn_base + '_cfmask', fn_base + '_cfmask_conf']
    END
  'LE7': BEGIN
    bands_base = [fn_base + '_sr_band1', fn_base + '_sr_band2', $
                  fn_base + '_sr_band3', fn_base + '_sr_band4', $
                  fn_base + '_sr_band5', $
                  fn_base + '_sr_band7', fn_base + '_sr_atmos_opacity', $
                  fn_base + '_sr_fill_qa', fn_base + '_sr_ddv_qa', $
                  fn_base + '_sr_cloud_qa', fn_base + '_sr_cloud_shadow_qa', $
                  fn_base + '_sr_snow_qa', fn_base + '_sr_land_water_qa', $
                  fn_base + '_sr_adjacent_cloud_qa', $
                  fn_base + '_cfmask', fn_base + '_cfmask_conf']
    END
  ELSE:STOP
ENDCASE

;get info
res = QUERY_TIFF(fn, Info, GEOTIFF=geoinfo)
data = INTARR(N_ELEMENTS(bands_base),info.DIMENSIONS[0], info.DIMENSIONS[1])
FOR i = 0, N_ELEMENTS(bands_base)-1 DO BEGIN
  tmp = READ_TIFF(dir + '\' + bands_base[i]+'.tif')
  data[i,*,*] = FIX(tmp)
ENDFOR
WRITE_TIFF, dir + '\' + fn_base + '_multi_' + $
            STRTRIM(DD,2) + '_' + STRTRIM(MM,2) + '_' + STRTRIM(YYYY,2) + $  
            '.tif', data, /SHORT, /SIGNED, GEOTIFF = geoinfo  


END
