Function DoublePdhtf4mpfit, x, p

;  Purpose:
;     To compute the value of the double parametric double hyperbolic tangent
;     function (PDHTF) at a point x, knowing the 13 function parameters:
;     a [0] = base value of the PDHTF
;     FIRST SEASON:
;     a [1] = amplitude of the growing phase
;     a [2] = shift along the x axis for the inflexion point of the
;        growing phase of the PDHTF
;     a [3] = slope of the growing phase
;     a [4] = amplitude of the decay phase
;     a [5] = shift along the x axis for the inflexion point of the
;        decay phase of the PDHTF
;     a [6] = slope of the decay phase
;     SECOND SEASON:
;     a [7] = amplitude of the growing phase
;     a [8] = shift along the x axis for the inflexion point of the
;        growing phase of the PDHTF
;     a [9] = slope of the growing phase
;     a [10] = amplitude of the decay phase
;     a [11] = shift along the x axis for the inflexion point of the
;        decay phase of the PDHTF
;     a [12] = slope of the decay phase

;  Remark:
;     This routine does not provide the partial derivatives of the
;     function.

ygrow1 = p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2.0
ydecy1 = p [4] * ((TANH((x - p [5]) * p [6]) + 1) / 2.0 -1)
ygrow2 = p [7] * (TANH((x - p [8]) * p [9]) + 1) / 2.0
ydecy2 = p [10] * ((TANH((x - p [11]) * p [12]) + 1) / 2.0 -1)

f = p [0] + ygrow1 + ydecy1 + ygrow2 + ydecy2
return, f

END