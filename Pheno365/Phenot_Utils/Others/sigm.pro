PRO sigm, x, a, f, parder

;  Purpose:
;     Procedure to compute the value f of a parametric sigmoid at
;     point x, given four model parameters:
;        a [0] = amplitude of the curve
;        a [1] = slope of the curve
;        a [2] = horizontal shift (in x)
;        a [3] = base value
;        and the partial derivatives of that function with respect
;        to the 4 parameters at that same point.

f = a[0] / (1.0 + EXP(-a[1] * (x - a[2]))) + a[3]

; If the procedure is called with 4 parameters, calculate the partial derivatives:
IF N_PARAMS () GE 4 THEN BEGIN
   parder = [[1.0 / (1.0 + EXP(-a[1] * (-a[2] + x)))], $
      [-(a[0] * EXP(-a[1] * (-a[2] + x)) * (a[2] - x)) / (1.0 + EXP(-a[1] * (-a[2] + x)))^2], $
      [-(a[0] * a[1] * EXP(-a[1] * (-a[2] + x))) / (1.0 + EXP(-a[1] * (-a[2] + x)))^2], $
      [REPLICATE (1.0, N_ELEMENTS(X))]]
ENDIF

END