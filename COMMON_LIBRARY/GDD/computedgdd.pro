FUNCTION computedGDD, T, Tb, CUMULATIVE = cumulative
  ;USAGE:
  ; computedGDD(T, Tb) To get the total sum
  ; computedGDD(T, Tb, /CUMULATIVE) If this keyword is set, the result is an array of the same size as the input, with each element, i, containing the sum of the input array elements 0 to i.
  sz = SIZE(Tb)
  IF (sz[0] NE 0) THEN BEGIN
    PRINT, 'GDD compuatation required with non-scalar Tb: ', Tb
    STOP
  ENDIF
  sz = SIZE(T)
  IF (sz[0] NE 1) THEN BEGIN
    PRINT, 'GDD compuatation required with non-1D vector T: ', T
    STOP
  ENDIF
  Tb = FLOAT(Tb)
  ;avoid making operations on NaN if no data are available
  indNaN = WHERE(~FINITE(T), countNaN)
  ; if all are Nan it will return Nan
  IF (countNaN EQ N_ELEMENTS(T)) THEN BEGIN
    IF KEYWORD_SET(cumulative) THEN RETURN, MAKE_ARRAY (N_ELEMENTS(T), VALUE=!VALUES.F_NAN, /FLOAT) $
    ELSE RETURN, !VALUES.F_NAN
  ENDIF
  ;if only some are Nan, make so that this are ignored
  IF (countNaN GT 0) THEN t[indNaN] = FLOAT(Tb)
  IF KEYWORD_SET(cumulative) THEN $
    ;SPEED
    ;RETURN, TOTAL(MAX([[T-Tb],[MAKE_ARRAY(N_ELEMENTS(T),VALUE=0)]], DIMENSION=2),/CUMULATIVE, /DOUBLE) $
    RETURN, TOTAL((T-Tb) > 0.0, /CUMULATIVE, /DOUBLE) $
  ELSE $
    ;SPEED
    ;RETURN, TOTAL(MAX([[T-Tb],[MAKE_ARRAY(N_ELEMENTS(T),VALUE=0)]], DIMENSION=2),/DOUBLE)
    RETURN, TOTAL((T-Tb) > 0.0, /DOUBLE)
END