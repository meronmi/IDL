FUNCTION write_envi_img, data, fullPath
fn = fullPath
tmp = STRSPLIT(fn, '.', /EXTRACT)
IF N_ELEMENTS(tmp) GT 1 THEN BEGIN
  IF tmp[1] NE 'img' THEN BEGIN
    PRINT, 'ERROR: write_envi_img.pro called with wrong extention'
    PRINT, tmp[1]
    STOP
  ENDIF
ENDIF ELSE BEGIN
  fn = fn +'.img'
ENDELSE
OPENW, lun, fn, /GET_LUN
WRITEU, lun, data
FREE_LUN, lun
RETURN, 0
END