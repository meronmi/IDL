PRO fromBilToSingleBsq

dir = 'E:\WA\EWP\EWP_images\standardize with spirits'
fn = dir + '\cwp_hl2000_bil'
fnbsq = dir + '\cwp_hl2000_bsq'
base_out = 'cwp2000_'
;uncomment to have the bil -> bsq
;e= ENVI()
;raster1 = e.OpenRaster(fn)
;raster1.Export, fnbsq, 'envi', INTERLEAVE='bsq'
samples = 1867
lines = 348
bands = 900
dt = 4
;just take the last 540


dates = ['0101', '0111', '0121', $
         '0201', '0211', '0221', $
         '0301', '0311', '0321', $
         '0401', '0411', '0421', $
         '0501', '0511', '0521', $
         '0601', '0611', '0621', $
         '0701', '0711', '0721', $
         '0801', '0811', '0821', $
         '0901', '0911', '0921', $
         '1001', '1011', '1021', $
         '1101', '1111', '1121', $
         '1201', '1211', '1221']
        
OPENR,lun, fnbsq, /GET_LUN
ass_image = ASSOC(lun, FLTARR(samples, lines))
yyyy = 1989
TT = 1
FOR i = 0, bands-1 DO BEGIN
  tmp = ass_image[i]
  indNan = WHERE(~FINITE(tmp), countNan)
  IF (countNan GT 0) THEN tmp[indNan] = -99
  tmp=FIX(tmp)
  IF (TT LT 10) THEN strTT = '0' + STRTRIM(TT,2) ELSE strTT = STRTRIM(TT,2) 
  fn = dir + '\' + base_out + STRMID(STRTRIM(yyyy,2),2,2) + strTT
  OPENW, lun, fn+'.img', /GET_LUN
  WRITEU, lun, tmp
  FREE_LUN, lun
  OPENW, lun, fn+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'description = cwp'
  PRINTF, lun, 'samples = ' + STRTRIM(samples)
  PRINTF, lun, 'lines = ' + STRTRIM(lines)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'header offset = 0'
  PRINTF, lun, 'file type = ENVI Standard'
  PRINTF, lun, 'data type = 2'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'byte order = 0'
  PRINTF, lun, 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
  PRINTF, lun, 'values = {rainfall, mm, 0, 2000, 0, 93, 0, 1}'
  PRINTF, lun, 'flags = {-99=nodata}'
  
  PRINTF, lun, 'date = ' + STRTRIM(yyyy,2) + dates[TT-1]
  PRINTF, lun, 'days = 10'
  PRINTF, lun, 'sensor type = TAMSAT'
  PRINTF, lun, 'comment = {made by Mic}'
  PRINTF, lun, 'program = {IDL}'
  FREE_LUN, lun
  TT = TT + 1
  IF (TT EQ 37) THEN BEGIN
    TT = 1
    yyyy = yyyy + 1
  ENDIF
ENDFOR
FREE_LUN, lun 

END