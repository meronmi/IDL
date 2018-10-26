FUNCTION JD2YEAR, JD
; return the YEAR of a given Julian day, accept scalars and vectors
 
; manage NaN or failures
indNJD=WHERE((FINITE(JD) NE 1) OR (JD EQ -999), countNJD)
IF (countNJD EQ 0) THEN BEGIN
  CALDAT, JD, Month, Day, Year
  RETURN, Year
ENDIF ELSE BEGIN
  indJD=WHERE((FINITE(JD) EQ 1) AND (JD NE -999), countJD)
  IF (countJD EQ 0)THEN RETURN, JD
  CALDAT, JD[indJD], Month, Day, Year
  YEARvec = JD
  YEARvec[indJD] = Year
  RETURN, YEARvec
ENDELSE  
END 