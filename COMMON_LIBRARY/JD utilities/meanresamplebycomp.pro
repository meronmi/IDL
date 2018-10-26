FUNCTION meanResampleByComp, var, tComp;, days
;the mean is computed only if at least one value is present
IF (N_ELEMENTS(var) NE N_ELEMENTS(tComp)) THEN STOP
;serialize tComp
serialComp = !NULL
c = 0
FOR i = 0, N_ELEMENTS(tComp)-1 DO BEGIN
  IF (i EQ 0) THEN BEGIN
    serialComp = [serialComp, c] 
  ENDIF ELSE BEGIN
    IF (tComp[i] NE tComp[i-1]) THEN c = c + 1 
    serialComp = [serialComp, c] 
  ENDELSE
ENDFOR
;compute the averages 
avgByComp = FLTARR(MAX(serialComp))
FOR i = 0, MAX(serialComp) -1 DO BEGIN
  ind = WHERE(serialComp EQ i)
  indFin = WHERE(FINITE(var[ind]), count)
  IF (count EQ 0) THEN avgByComp[i] = !VALUES.F_NAN ELSE avgByComp[i] = MEAN(var[ind[indFin]])
ENDFOR

RETURN, avgByComp
END
