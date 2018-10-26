FUNCTION slow_directional_one2one_match, what2find, where2find
;returne the indixes of where2find having what2find values
;work with any numerical datatype
indOut = FLTARR(N_ELEMENTS(what2find))
FOR i = 0, N_ELEMENTS(what2find) - 1 DO BEGIN
  ind = WHERE(where2find EQ what2find[i], count) 
  IF (count EQ 0) THEN indOut[i] = !VALUES.F_NAN
  IF (count EQ 1) THEN indOut[i] = ind[0]
  IF (count GT 1) THEN STOP
ENDFOR


RETURN, indOut
END