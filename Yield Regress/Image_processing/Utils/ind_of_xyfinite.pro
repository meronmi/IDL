FUNCTION ind_of_XYfinite, x, y, dep, prod, verbose
;  Purpose:
;     To return the indexes where both x and y are finite

;Check for missing data
ind_x_finite=WHERE(FINITE(x) EQ 1, count_x_finite) 
ind_y_finite=WHERE(FINITE(y) EQ 1, count_y_finite)

IF (dep EQ 0) THEN dep = 'all'

IF ((count_x_finite NE N_ELEMENTS(x)) OR (count_y_finite NE N_ELEMENTS(y))) THEN BEGIN
  IF (verbose EQ 1) THEN PRINT, 'Warning: missing data at dep anlyisis, dep '+strtrim(dep,2)+', product '+strtrim(prod,2)
ENDIF
IF (count_x_finite LT 3) OR (count_x_finite LT 3) THEN STOP
ind_finite=SetIntersection(ind_x_finite, ind_y_finite)
   
RETURN,  ind_finite
END