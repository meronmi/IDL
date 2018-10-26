FUNCTION computedGDD_abs, T, Tb, CUMULATIVE = cumulative
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
IF KEYWORD_SET(cumulative) THEN $
  ;SPEED
  ;RETURN, TOTAL(MAX([[T-Tb],[MAKE_ARRAY(N_ELEMENTS(T),VALUE=0)]], DIMENSION=2),/CUMULATIVE, /DOUBLE) $
   RETURN, TOTAL(ABS(T-Tb), /CUMULATIVE, /DOUBLE) $
ELSE $
  ;SPEED
  ;RETURN, TOTAL(MAX([[T-Tb],[MAKE_ARRAY(N_ELEMENTS(T),VALUE=0)]], DIMENSION=2),/DOUBLE)
  RETURN, TOTAL(ABS(T-Tb) > 0.0, /DOUBLE)
END