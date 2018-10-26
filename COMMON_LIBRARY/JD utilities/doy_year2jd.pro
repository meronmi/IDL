FUNCTION DOY_YEAR2JD, DOY, YEAR
; return the JD of a given DOY and YEAR, accept scalars and vectors

; manage NaN or failures
indN=WHERE((FINITE(DOY) NE 1) OR (DOY EQ -999), countN)
IF (countN EQ 0) THEN BEGIN
  RETURN, JULDAY(1, 1, YEAR)+(DOY-1)
ENDIF ELSE BEGIN
  ind=WHERE((FINITE(DOY) EQ 1) AND (DOY NE -999), count)
  IF (count EQ 0)THEN RETURN, DOY
  JD = FLOAT(DOY)
  JD[ind] = JULDAY(1, 1, YEAR[ind])+(DOY[ind]-1)
  RETURN, JD
ENDELSE  
END 