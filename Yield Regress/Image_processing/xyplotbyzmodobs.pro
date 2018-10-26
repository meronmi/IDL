FUNCTION XYplotByZmodobs, x, y, z, wt, pt, xt, yt, doplot, legend
;usage: c=XYplotByZ(x, y, id, 'plot_title', 'xtitle', 'ytitle')

;plot x vs. y with datapoints coloured according to z and return the coefficients 
;of the linear regression and correlation coeff
;check for NaN and remove them if present
ind_x_finite=WHERE(FINITE(x) EQ 1, count_x_finite)
ind_y_finite=WHERE(FINITE(y) EQ 1, count_y_finite)
IF (count_x_finite LT 3) OR (count_x_finite LT 3) THEN STOP
ind_finite=SetIntersection(ind_x_finite, ind_y_finite)
x=x[ind_finite]
y=y[ind_finite]
n=N_ELEMENTS(Y)

coeff = REGRESS(x, y, CORRELATION=corr, CONST=b0, /DOUBLE)
pt = pt + ', R2=' +string(corr^2, FORMAT='(f4.2)')
;x and y ranges
xr=[min(x)-(max(x)-min(x))/10.0, max(x)+(max(x)-min(x))/10.0]
yr=[min(y)-(max(y)-min(y))/10.0, max(y)+(max(y)-min(y))/10.0]
;x position for legend
;xp1=max(x)+(max(x)-min(x))/40.0
xp2=min(xr)+(max(x)-min(x))/30.0
xp1=xp2+(max(x)-min(x))/30.0
;find the list of unique z identificators
zlist=z(SORT(z))
zlist=zlist[UNIQ(zlist)]
n=N_ELEMENTS(zlist)
legend=legend[zlist-1]
IF (doplot EQ 1) THEN BEGIN
  WINDOW, /free, TITLE=wt
  
  ;base plot  
  range=[min([xr[0],yr[0]]),max([xr[1],yr[1]])]
  PLOT, x, y, TITLE=pt, XTITLE=xt, YTITLE=yt, XSTYLE=1, CHARSIZE=1.2, $
        PSYM=1, /NODATA, XRANGE=range, YRANGE=range, BACKGROUND = 16777215, COLOR = 0 
  usepsym=[1,2,4,5,6,7]
  p=-1
  DEVICE, DECOMPOSED=0 
  LOADCT, 38, /SILENT
  FOR d=0,  n-1 DO BEGIN
    p=p+1
    IF p GT N_ELEMENTS(usepsym)-1 THEN p=0
    clr=FIX((d+1)/double(n)*253)
    ind=WHERE(z eq zlist[d], count)
    IF count GT 0 THEN BEGIN
      OPLOT, x[ind], y[ind], color=clr, PSYM=usepsym[p]
      XYOUTS, xp1, yr[1]-((yr[1]-yr[0])/double(2*n))*(d+1), 'dep. '+strtrim(FIX(zlist[d]),2)+' '+legend[d], COLOR=clr, CHARSIZE=1.25 
      OPLOT, [xp2], [yr[1]-((yr[1]-yr[0])/double(2*n))*(d+1)],  PSYM=usepsym[p], COLOR=clr
    ENDIF
  ENDFOR   
  DEVICE, DECOMPOSED=1
  OPLOT, xr, b0+coeff[0]*xr, COLOR=3000  
ENDIF
RETURN, [b0, coeff[0], corr, N]
END