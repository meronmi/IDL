FUNCTION JD2DOY, JD
; return the DOY of a given Julian day, accept scalars and vectors
 
; x = JULDAY(12,31,1999)
; doy = JD2DOY(x)

; manage NaN or failures
indNJD=WHERE((FINITE(JD) NE 1) OR (JD EQ -999), countNJD)
IF (countNJD EQ 0) THEN BEGIN
  CALDAT, JD, Month, Day, Year
  RETURN, JD - JULDAY(12, 31, Year-1)
ENDIF ELSE BEGIN
  indJD=WHERE((FINITE(JD) EQ 1) AND (JD NE -999), countJD)
  IF (countJD EQ 0)THEN RETURN, JD
  CALDAT, JD[indJD], Month, Day, Year
  DOY = JD
  DOY[indJD] = JD[indJD] - JULDAY(12, 31, Year-1)
  RETURN, DOY
ENDELSE  
END 