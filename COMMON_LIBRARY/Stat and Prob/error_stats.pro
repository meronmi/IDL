FUNCTION error_stats, x0, y0
;compute RMSE, ME and MAE
;be care this assumes that nan are !VALUR.F_NAN
res = CREATE_STRUCT('rmsd', 0.0, 'mae', 0.0, 'me', 0.0)

IF (N_ELEMENTS(x0) NE N_ELEMENTS(y0)) THEN STOP
indFin = WHERE((FINITE(x0) AND FINITE(y0)), countFin)
x = x0[indFin]
y = y0[indFin]
;rmsd
res.rmsd = SQRT(TOTAL((y-x)^2)/FLOAT(countFin))
;mae
res.mae = TOTAL(ABS(y-x))/FLOAT(countFin)
;me
res.me = TOTAL(y-x)/FLOAT(countFin)
RETURN, res
END
