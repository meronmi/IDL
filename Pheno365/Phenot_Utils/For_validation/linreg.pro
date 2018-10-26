FUNCTION LinReg, xx, yy, doPlot, title, xtitle, ytitle
  ;Compute and optionally plots the fitting stats of the Y vs X scatterplot
  ;Resisteat to NaN (they are excluded)
  ;Return -1 if not enough obs are present (at least 3) 
  
  
  res = FLTARR(3)
  ;remove NaN
  indFinX = WHERE(FINITE(xx) EQ 1, countFinX)
  indFinY = WHERE(FINITE(yy) EQ 1, countFinY)
  ;if one of the two has no finite value return -1
  IF ((countFinX EQ 0) OR (countFinY EQ 0)) THEN RETURN, -1
  ;find the where both arrays are finite
  indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
  IF (countFin LT 3) THEN RETURN, -1
  x = xx[indFinXAndY]
  y = yy[indFinXAndY]
  
  gain = REGRESS(REFORM(x), REFORM(y), CONST=offset, CORRELATION=r)
  res = [r^2, gain, offset]
  IF (doPlot EQ 1) THEN BEGIN
  
    ;xrange = [FLOOR(MIN([x,y]) - (MAX([x,y])-MIN([x,y]))/10.0), CEIL(MAX([x,y]) + (MAX([x,y])-MIN([x,y]))/10.0)]
    ;yrange = xrange
    
    
    gh =  plot(REFORM(x), REFORM(y), FONT_SIZE=16, FONT_STYLE=1,  TITLE=' R2 = ' + STRTRIM(res[0],2), $ ;ASPECT_RATIO=1,
      LINESTYLE="none", SYMBOL="o", XTITLE=xtitle, YTITLE=ytitle);,  XRANGE=xrange, YRANGE=yrange)
    gh.SYM_SIZE = 0.75
    gh.SYM_FILLED=1
    
    
    ;gh = plot([0,1000000],[0,1000000],  COLOR = "blue",  LINESTYLE=2,  XRANGE=xrange, YRANGE=yrange, /OVERPLOT)
    ;gh = plot(xrange, xrange*gain[0]+offset, color = "red", LINESTYLE=0,  XRANGE=xrange, YRANGE=yrange, /OVERPLOT)
    xrange = [min(x),max(x)]
    gh = plot(xrange, xrange*gain[0]+offset, color = "red", LINESTYLE=0, /OVERPLOT)
    
    ;gh.save, 'K:\ILRI\Comparison results\aggregate files\Results\'+title+'_'+ytitle+'.png'
  ENDIF
  RETURN, res
END