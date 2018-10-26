FUNCTION JD2DDMMYYYY, JD
; return the the date (DD MM YYYY) of a given Julian day, accept scalars and vectors
 
; manage NaN or failures
indNJD=WHERE((FINITE(JD) NE 1) OR (JD EQ -999), countNJD)
IF (countNJD EQ 0) THEN BEGIN
  CALDAT, JD, Month, Day, Year
  RETURN, [Day, Month, Year]
ENDIF ELSE BEGIN
  indJD=WHERE((FINITE(JD) EQ 1) AND (JD NE -999), countJD)
  IF (countJD EQ 0)THEN RETURN, JD
  CALDAT, JD[indJD], Month, Day, Year
  YYvec = JD & MMvec = JD & DDvec = JD 
  YYvec[indJD] = Year & MMvec[indJD] = Month & DDvec[indJD] = Day
  RETURN, [DDvec, MMvec, YYvec]
ENDELSE  
END 