Function pdhtf4mpfit, x, p

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

;ygrow = p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2
;ydecy = p [4] * (TANH((x - p [5]) * p [6]) + 1) / 2
;f = p [0] + ygrow + ydecy - p [4]
;return, f


return, p [0] + p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2.0 + p [4] * (TANH((x - p [5]) * p [6]) + 1) / 2.0 - p [4]

END