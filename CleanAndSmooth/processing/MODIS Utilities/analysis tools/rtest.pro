FUNCTION RTEST, r1, r2, n1, n2
;Fsher's test on r
z1 = 0.5d * ALOG((1+r1)/(1-r1))
z2 = 0.5d * ALOG((1+r2)/(1-r2))

num = z1 - z2
den =SQRT(1.0d/(n1-3)+1.0d/(n2-3))
zz = num / den
;PRINT, zz
p = 2* GAUSS_PDF(-ABS(zz))



RETURN, p

END