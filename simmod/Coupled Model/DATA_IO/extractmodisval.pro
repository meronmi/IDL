FUNCTION extractModisVal, modisJd, modisVal, tJD2extract
;give time in JD and associated var, it extract the var for the requested time grid (tJD2extract)
varOut = FLTARR(N_ELEMENTS(tJD2extract))
FOR i = 0, N_ELEMENTS(tJD2extract)-1 DO BEGIN
  deltaTime =  modisJd - tJD2extract[i]
  res = MIN(ABS(deltaTime), ind)
  varOut[i] = modisVal[ind]
ENDFOR
RETURN, varOut
END