;example
PRO exampleInv
  a = FLTARR(3,3,10)
  FOR i = 0, 2 DO FOR j = 0, 2 DO a[i,j,*] = FINDGEN(10)+1
  a[0,0,*] = [1,1,1,1,1, 5,5,5,5,!VALUES.F_NAN]

  res = invPercZof3DMatrix(a, 2)
  PRINT, res
END
;end of example

FUNCTION invPercZof3DMatrix, mat, val
RETURN,  CMAPPLY('USER:invPercentile', mat, 3,  TYPE=4, FUNCTARGS={VAL: val})
END

FUNCTION invPercentile, x, VAL = val
; given a set of data x it computed the empirical CDF (ECDF) of the value val
;to be used with cmapply.pro
;debug
;IF (FINITE(val) NE 1) THEN BEGIN
;  indFin = WHERE(FINITE(x), countFin)
;  IF (countFin GT 0) THEN STOP
;ENDIF
IF (FINITE(val) NE 1) THEN RETURN, !VALUES.F_NAN
;the inverse percentile of a value within a distribution represents
;the percentage of dtata in the distribution that falls below this value 
indFin = WHERE(FINITE(x), countFin)
IF (countFin GT 0) THEN BEGIN
  IF (countFin NE N_ELEMENTS(x)) THEN x = x[indFin]
  x = x[SORT(x)]
  ind = WHERE(x LE val, count)
  IF (count EQ 0) THEN RETURN, 0
  RETURN, (ind[-1] + 1) / FLOAT(countFin) * 100
ENDIF ELSE RETURN, !VALUES.F_NAN
END


