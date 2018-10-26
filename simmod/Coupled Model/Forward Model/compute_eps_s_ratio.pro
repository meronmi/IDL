FUNCTION compute_eps_s_ratio, nom, denom, method
;method can be 'casa' or 'ratio'
IF (N_ELEMENTS(nom) NE N_ELEMENTS(denom)) THEN STOP
IF (TOTAL(~FINITE(denom)) GT 0) THEN STOP
CASE method OF
  'ratio': RETURN, ((nom / DOUBLE(denom)) < 1.0) > 0.5                             ;limite to a max of 1 and a min of 0.5
  'casa': RETURN, (0.5 + nom / DOUBLE(denom)) < 1.0 ;limite to a max of 1
  ELSE: stop
ENDCASE
END