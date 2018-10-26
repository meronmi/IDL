FUNCTION r2cv, x, y
;Purpose:
;     To compute the cross validate leave one out r2
;  Return values:
;     -20: Arrays contain missing data
;     -10: Arrays have different dimensio
;     >0:  R2cv 
;
n = N_ELEMENTS(x)

;check that arrays have the same dimension
IF (n NE N_ELEMENTS(y)) THEN RETURN, -10
;check that they do not contain NaN
ind = WHERE(FINITE(x) EQ 0, count) 
IF (count NE 0) THEN RETURN, -20
ind = WHERE(FINITE(y) EQ 0, count) 
IF (count NE 0) THEN RETURN, -20

pos = INDGEN(n)
yp = DBLARR(n)  ;y predicted
;loo loop
FOR i = 0, n-1 DO BEGIN
  ind = WHERE(pos NE i)   ;indexes of the kept
  indoo = WHERE(pos EQ i) ;index of the one out
  xloo = x[ind]
  yloo = y[ind]
  b1 = REGRESS(xloo, yloo, CONST=b0, CORRELATION = corr, /DOUBLE)
  yp[indoo] = b0 + b1[0] * x[indoo]
ENDFOR


SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE)
SSerrloo = TOTAL((y - yp)^2 , /DOUBLE)

cvr2 = 1.0 - (SSerrloo/SStot)
;overall regression
;b1 = REGRESS(x, y, CONST=b0, CORRELATION= corr, YFIT = yfit, /DOUBLE)
;yf = b0 + b1[0] * x
;SSerr = TOTAL((y - yf)^2 , /DOUBLE)
;r2 =  1.0 - (SSerr/SStot)
RETURN, cvr2
END
