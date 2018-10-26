FUNCTION computeRMSE, yobs, ymod
ss = TOTAL((yobs-ymod)^2, /DOUBLE)
RETURN, SQRT(ss/DOUBLE(N_ELEMENTS(yobs)))
END

FUNCTION computeRRMSE, yobs, ymod
RETURN, computeRMSE(yobs, ymod)/MEAN(yobs,/DOUBLE)*100.0
END

FUNCTION computeBIAS, yobs, ymod
s = TOTAL((yobs-ymod), /DOUBLE)
RETURN, s/DOUBLE(N_ELEMENTS(yobs))
END







PRO stats
END