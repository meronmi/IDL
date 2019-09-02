FUNCTION linregstat, x0, y0

  ;perfrom linear regression managing NaN and returns
  ;RETURN, [offset, gain, corr, P-value for gain]
  
  ; ZAR example x=FLOAT([3,4,5,6,8,9,10,11,12,14,15,16,17])
  ; y=[1.4,1.5,2.2,2.4,3.1,3.2,3.2,3.9,4.1,4.7,4.5,5.2,5.0]
  N = N_ELEMENTS(x0)
  ; chek for -999 data
  ind999 = WHERE([x0, y0] EQ -999, count999)
  IF (count999 GT 0) THEN STOP
  ; remove points where one of the two variable is NaN
  indFinX = WHERE(FINITE(x0), countFinX)
  indFinY = WHERE(FINITE(y0), countFinY)
  indFin = SetIntersection(indFinX, indFinY)
  IF (N_ELEMENTS(indFin) LT 3) THEN RETURN, [!VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN]
  x = x0[indFin]
  y = y0[indFin]
  N = N_ELEMENTS(x)
  ;perform regression
  gain = REGRESS( x, y, CONST = offset, CORRELATION = corr, FTEST = ft, YFIT = yfit, /DOUBLE)
  ;PRINT, gain, offset, corr, ft
  
  
  
  
  
  ; Test of the slope
  Df=N-2
  pgain = MPFTEST(ft, 1, Df)
  IF ((pgain GT 1.0) OR (pgain LT 0.0)) THEN STOP
  ;rDf = 1   ;regressione degrees of freedom
  ;avgy =  MEAN(y, /DOUBLE)
  ;regSS = TOTAL((yfit-avgy)^2,/DOUBLE)
  ;totSS = TOTAL((y-avgy)^2,/DOUBLE)
  ;resMS= (totSS-regSS) /Df
  ;F = regSS/resMS
  ;PRINT, 'F for slope test', F
  ;PRINT, 'P? ', PROB
  RETURN, [offset, gain, corr, pgain]
  
END