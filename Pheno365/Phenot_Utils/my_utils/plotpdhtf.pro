FUNCTION plotPdhtf, a, x, der
;plot PDHT function and compnents

ygrow = a [1] * (TANH((x - a [2]) * a [3]) + 1) / 2.0
ydecy = a [4] * (TANH((x - a [5]) * a [6]) + 1) / 2.0
f = a [0] + ygrow + ydecy - a [4]

WINDOW, /FREE
yr = [MIN([ygrow, ydecy, f]), MAX([ygrow, ydecy, f])]
PLOT, x, f, YRANGE = yr 
c = 30000 
OPLOT, x, x*0 + a[0], color = c
XYOUTS, (MAX(x)-MIN(x))/10 + MIN(X), (MAX(f)-MIN(f))/10*9+MIN(f), 'Base', color = c
c = 90000
OPLOT, x, ygrow, color = c
XYOUTS, (MAX(x)-MIN(x))/10 + MIN(X), (MAX(f)-MIN(f))/10*7+MIN(f), 'Growth', color = c
c = 120000
OPLOT, x, ydecy, color = c
XYOUTS, (MAX(x)-MIN(x))/10 + MIN(X), (MAX(f)-MIN(f))/10*5+MIN(f), 'Decay', color = c
c = 16777215
IF (der NE 0) THEN BEGIN
  xhr = FLOAT(INDGEN(1000))*(MAX(x)-MIN(x))/1000.0+MIN(x)
  ygrow = a [1] * (TANH((xhr - a [2]) * a [3]) + 1) / 2.0
  ydecy = a [4] * (TANH((xhr - a [5]) * a [6]) + 1) / 2.0
  fhr = a [0] + ygrow + ydecy - a [4]
  WINDOW, /FREE
  IF (der GE 1) THEN BEGIN
    fhr1=SMOOTH(DERIV(fhr),20) ;brown
    yr = [MIN(fhr1), MAX(fhr1)]
    PLOT, xhr, xhr*0, YRange = yr
    XYOUTS, (MAX(xhr)-MIN(xhr))/15 + MIN(xhr), (MAX(fhr1)-MIN(fhr1))/10*9+MIN(fhr1), 'Scaled f'
    OPLOT, x, (f-MIN(fhr))/(MAX(fhr)-MIN(fhr))*(MAX(fhr1)-MIN(fhr1))+MIN(fhr1), LINESTYLE=1
    OPLOT, x, (f-MIN(fhr))/(MAX(fhr)-MIN(fhr))*(MAX(fhr1)-MIN(fhr1))+MIN(fhr1), PSYM=1
    OPLOT, xhr, (fhr-MIN(fhr))/(MAX(fhr)-MIN(fhr))*(MAX(fhr1)-MIN(fhr1))+MIN(fhr1)
    c = 30000 
    OPLOT, xhr, fhr1, color = c ;green
    XYOUTS, (MAX(xhr)-MIN(xhr))/15 + MIN(xhr), (MAX(fhr1)-MIN(fhr1))/10*7+MIN(fhr1), 'F1', color = c 
  ENDIF
  IF (der GE 2) THEN BEGIN
    fhr2=SMOOTH(DERIV(fhr1),20)
    c = 90000
    OPLOT, xhr, fhr2*10, color = c  ;brown
    XYOUTS, (MAX(xhr)-MIN(xhr))/15 + MIN(xhr), (MAX(fhr1)-MIN(fhr1))/10*5+MIN(fhr1), 'F2', color = c
  ENDIF
  IF (der GE 2) THEN BEGIN
    fhr3=SMOOTH(DERIV(fhr2),20)
    c = 120000  
    OPLOT, xhr, fhr3*100, color = c ;yellow 
    XYOUTS, (MAX(xhr)-MIN(xhr))/15 + MIN(xhr), (MAX(fhr1)-MIN(fhr1))/10*3+MIN(fhr1), 'F3', color = c
  ENDIF
ENDIF


RETURN, 0
END