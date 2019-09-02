FUNCTION linregstat, x, y, NODATAVAL = nodataval, MINSAMPLESIZE = minsamplesize

  ;be care this assumes that nan are !VALUR.F_NAN

  ;perfrom linear regression managing NaN and returns
  ;RETURN, [offset, gain, corr, P-value for gain]

  ; ZAR example x=FLOAT([3,4,5,6,8,9,10,11,12,14,15,16,17])
  ; y=[1.4,1.5,2.2,2.4,3.1,3.2,3.2,3.9,4.1,4.7,4.5,5.2,5.0]

  
  IF  N_ELEMENTS(nodataval) GT 0 THEN BEGIN
   x0 = FLOAT(x)
   y0 = FLOAT(y)
   indNaN= WHERE(x EQ nodataval, countNaN)
   IF (countNaN) THEN x0[indNaN] = !VALUES.F_NAN
   indNaN= WHERE(y EQ nodataval, countNaN)
   IF (countNaN) THEN y0[indNaN] = !VALUES.F_NAN
  ENDIF ELSE BEGIN
    x0 = x
    y0 = y 
  ENDELSE
  
  IF  N_ELEMENTS(minsamplesize) EQ 0 THEN minsamplesize = 3
  
  
  ;ind999 = WHERE([x0, y0] EQ -999, count999)
  ;IF (count999 GT 0) THEN STOP

  ;check equal dimensions
  IF (N_ELEMENTS(x0) NE N_ELEMENTS(y0)) THEN STOP

  indFin = WHERE((FINITE(x0) AND FINITE(y0)), countFin)
  IF (countFin LT minsamplesize) THEN RETURN, [!VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN]

  ;x = x0[indFin]
  ;y = y0[indFin]
  ;N = N_ELEMENTS(x0[indFin])
  ;perform regression
  gain = REGRESS( x0[indFin], y0[indFin], CONST = offset, CORRELATION = corr, FTEST = ft, YFIT = yfit, /DOUBLE)
  ;PRINT, gain, offset, corr, ft
  ; Test of the slope
  Df = N_ELEMENTS(x0[indFin])-2
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