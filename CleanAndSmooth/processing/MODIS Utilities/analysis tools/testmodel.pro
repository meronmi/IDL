FUNCTION testModel, x1, x2
;variance test on the residuals according to
;http://www.tandfonline.com/doi/abs/10.1080/00031305.1989.10475665#preview
;remove final NAN if any
ind = WHERE(FINITE(x1))
x1 = x1(ind)
ind = WHERE(FINITE(x2))
x2 = x2(ind)
d = x1 - x2
s = x1 + x2
rp = LINCORR(d, s, DOUBLE=double)

RETURN, rp[1]
END