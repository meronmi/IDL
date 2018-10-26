FUNCTION gmrf, X, Y
; it returns [b0,b1,AC,ACsys,ACuns,d,du,ds,FasbenderAC]
; b0 and b1 are the regression coefficients in the symmetric GMFR
; Yhat = b0 + b1 x X
Xm = MEAN(X, /DOUBLE)
Ym = MEAN(Y, /DOUBLE)
Xs = TOTAL((X-Xm)^2, /DOUBLE)
Ys = TOTAL((Y-Ym)^2, /DOUBLE)
n = N_ELEMENTS(X)

unsigned_b1 = SQRT(Ys/Xs)
r = CORRELATE(X,Y)
IF (r LT 0) THEN b1 = - unsigned_b1 ELSE b1 = unsigned_b1   ;this was neglected in the Clement's code
b0 = Ym - b1 * Xm

SSD = TOTAL((X - Y)^2, /DOUBLE)
SPOD = TOTAL( (ABS(Xm-Ym) + ABS(X - Xm)) * (ABS(Xm-Ym) + ABS(Y - Ym)), /DOUBLE);

AC = 1 - (SSD/SPOD)

Xhat = (Y - b0) / b1
Yhat = b0 + b1 * X

SPDu = TOTAL( (ABS(X - Xhat)) * (ABS(Y - Yhat)) , /DOUBLE)
SPDs = SSD - SPDu

ACsys = 1.0 - (SPDs/SPOD)
ACuns = 1.0 - (SPDu/SPOD)


d = MEAN((X-Y)^2, /DOUBLE)      ;this was strange in the Clement's code if the array was a row
du = (MEAN( ABS(X-Xhat) * ABS(Y-Yhat) , /DOUBLE))
ds = d - du;

;Coefficient of Dominique
NN = TOTAL((x-y)^2)
;  D <- sum((x-Xm)^2) + sum((y-Ym)^2) + n*(Xm - Ym)^2 + 2*abs(sum((x-Xm)*(y-Ym)))
dd = TOTAL((X-Xm)^2, /DOUBLE) + TOTAL((Y-Ym)^2, /DOUBLE) + n *(Xm - Ym)^2 + 2*ABS(TOTAL((X-Xm)*(Y-Ym), /DOUBLE))
dfAC = 1.0 - SSD/dd
RETURN, [b0,b1,AC,ACsys,ACuns,d,du,ds,dfAC]
END