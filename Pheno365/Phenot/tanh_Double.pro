; tanh_Double: basically same function as Michele's pdhtf4mpfit
; a function defining the phenological model (here a double hyperbolic tangent model)
; The interpretation of the parameters is as follows:
;   P[0] is the overall minimum value
;   P[1] is the amplitude of the growth phase
;   P[2] shifts the inflexion point of the growth phase along the x-axis (approximate by mid-point in time between local maximum and preceding minimum)
;   P[3] is the slope of the growth phase
;   P[4] is the amplitude of the decay phase
;   P[5] shifts the inflexion point of the decay phase along the x-axis (approximate by mid-point in time between local minimum and preceding maximum)
;   P[6] is the slope of the decay phase

;  Remark:
;     This routine does not provide the partial derivatives of the
;     function.

;ygrow = p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2
;ydecy = p [4] * (TANH((x - p [5]) * p [6]) + 1) / 2
;f = p [0] + ygrow + ydecy - p [4]
;return, f

FUNCTION tanh_Double, x, p
  RETURN, p [0] + p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2.0 + p [4] * (TANH((x - p [5]) * p [6]) + 1) / 2.0 - p [4]
END
