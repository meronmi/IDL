FUNCTION removeNodataCouplesFrom2Array, x, y, nodataVal
IF (N_ELEMENTS(x) NE N_ELEMENTS(y)) THEN STOP
IF (FINITE(nodataVal) EQ 1) THEN BEGIN
  ;remove anyhow possible true NaN
  ind = WHERE(FINITE(x) AND FINITE(y), count)
  IF (count EQ 0) THEN RETURN,  {x:!NULL, y:!NULL, subOriginalKept:!NULL}
  x = x[ind]
  y = y[ind]
  ind = WHERE((x NE nodataVal) AND (y NE nodataVal), count)
ENDIF ELSE BEGIN
  ind = WHERE(FINITE(x) AND FINITE(y), count)
ENDELSE

IF (count GT 0) THEN BEGIN
  RETURN, {x:x[ind], y:y[ind], subOriginalKept:ind}
ENDIF ELSE BEGIN
  RETURN, {x:!NULL, y:!NULL, subOriginalKept:!NULL}
ENDELSE
END