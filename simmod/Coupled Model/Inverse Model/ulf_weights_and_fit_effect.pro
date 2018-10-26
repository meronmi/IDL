PRO UEF_weights_and_fit_effect, yobs, yfit, fit_effect
;compute weights and fitting effect for upper envelope adaptation
;note that N_ELEMENTS(weights) can be >  N_ELEMENTS(yobs) as some obs may be missing
;the subscripts of weights for the valid obs are in indobs:
;N_ELEMENTS(weights[indobs]) =  N_ELEMENTS(yobs)
weights = FLTARR(N_ELEMENTS(yobs)) + 1.
dif = yobs - yfit
abs_dif  = ABS(dif)
max_abs_dif   = MAX(abs_dif)
;for those obs where the fit > obs
index = WHERE(dif LE 0)
IF index[0] NE -1 THEN weights[index] = 1 - (abs_dif[index]/max_abs_dif)
;compute fitting effect
fit_effect = TOTAL((abs_dif * weights), /DOUBLE)
END