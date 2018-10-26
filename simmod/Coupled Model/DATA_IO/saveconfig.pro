FUNCTION saveconfig, fg, parinfo, fn
OPENW, lun, fn, /GET_LUN
PRINTF, lun, '#####Parinfo and Fitst guesses'
FOR i = 0, N_ELEMENTS(parinfo)-1 DO BEGIN
  PRINTF, lun, parinfo[i].PARNAME
  PRINTF, lun, 'Fixed: ' + STRTRIM(parinfo[i].FIXED,2)
  PRINTF, lun, 'Limited: ' + STRTRIM(parinfo[i].LIMITED[0],2) + ' - ' + STRTRIM(parinfo[i].LIMITED[1],2)
  PRINTF, lun, 'Limits: ' + STRTRIM(parinfo[i].LIMITS[0],2) + ' - ' + STRTRIM(parinfo[i].LIMITS[1],2) 
  PRINTF, lun, 'MPFIT Step: ' + STRTRIM(parinfo[i].STEP,2)
  PRINTF, lun, 'First Guess: ' + STRTRIM(fg[i],2)
  PRINTF, lun, ''
ENDFOR

FREE_LUN, lun
RETURN,0
END