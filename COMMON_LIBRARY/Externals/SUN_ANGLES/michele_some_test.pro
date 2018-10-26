;zensun,day,time,lat,lon,zenith,azimuth,solfac,sunrise,sunset,local=local
envi
dd = 26
mm = 07
yy = 2017
day = ddmmyyyy2doy(dd, mm, yy)
lat = 45.8
lon = 8.6
PRINT, 'hh','SZA_zensun', 'SAA_zensun', 'SZA_envi', 'SAA_envi'  
FOR h = 6, 18 DO BEGIN
  time = h - lon/360.0*24 ;my calculation of UTC from solar local
  timeenvi = FIX((h-1) * 100 + 60-lon/360.0*24*60)  
  ;time = h - ((lon + 7.5)/15.0)
 ; zensun,day,h,lat,lon,zenith,azimuth,solfac,sunrise,sunset,/local
  zensun,day,time,lat,lon,zenith,azimuth,solfac,sunrise,sunset
  ;res = ENVI_COMPUTE_SUN_ANGLES(dd, mm, yy, (h-1)*100, lat, lon)
  res = ENVI_COMPUTE_SUN_ANGLES(dd, mm, yy, timeenvi, lat, lon)
  esaa = res[1]
  IF (esaa GT 180) THEN esaa = - (360-esaa)
  PRINT, h, zenith, azimuth, 90-res[0], esaa 
ENDFOR
PRINT, 'Rise     Set'
PRINT, sunrise,sunset


;lst2ct, 12,lon,-1,07,07,2017

END