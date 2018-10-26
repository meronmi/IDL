PRO pdhtf, x, a, f

;  Purpose:
;     To compute the value of the parametric double hyperbolic tangent
;     function (PDHTF) at a point x, knowing the 7 function parameters:
;     a [0] = base value of the PDHTF
;     a [1] = amplitude of the growing phase
;     a [2] = shift along the x axis for the inflexion point of the
;        growing phase of the PDHTF
;     a [3] = slope of the growing phase
;     a [4] = amplitude of the decay phase
;     a [5] = shift along the x axis for the inflexion point of the
;        decay phase of the PDHTF
;     a [6] = slope of the decay phase

;  Remark:
;     This routine does not provide the partial derivatives of the
;     function.

ygrow = a [1] * (TANH((x - a [2]) * a [3]) + 1) / 2.0
ydecy = a [4] * (TANH((x - a [5]) * a [6]) + 1) / 2.0
f = a [0] + ygrow + ydecy - a [4]

END