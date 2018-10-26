FUNCTION ddmm2dek, dd, mm
;return the dekad in which (dd, mm) falls
;work with vectors
IF (N_ELEMENTS(dd) NE N_ELEMENTS(mm)) THEN RETURN, -1
ret = !NULL
FOR i = 0, N_ELEMENTS(dd) - 1 DO BEGIN
  IF ((dd[i] GT 31) OR (dd[i] LT 0) OR (mm[i] GT 12) OR (mm[i] LT 0)) THEN RETURN, -2
  CASE 1 OF
    (dd[i] GE 1) AND (dd[i] LE 10): dek = 1 + (mm[i]-1) * 3
    (dd[i] GE 11) AND (dd[i] LE 20): dek = 2 + (mm[i]-1) * 3
    (dd[i] GE 21) AND (dd[i] LE 31): dek = 3 + (mm[i]-1) * 3  
  ENDCASE
  ret = [ret, dek] 
ENDFOR
IF (N_ELEMENTS(ret) EQ 1) THEN ret = REFORM(ret)
RETURN, ret
END