FUNCTION LinRegMulti, xx, yy, doPlot, title, xtitle, ytitle, $
                      layoutVec=layoutVec, colorInd = colorInd, _EXTRA=extra

;Compute and optionally plots the fitting stats of the Y vs X scatterplot

;Optional parameters
;layoutVec: is a 3 elements vector for the layout [n columns, n row, position of the graph]
;           if a multiplot is required a graphic window ha to be open on calling routin before calling LinRegMulti, e.g.:
;           wh = WINDOW(WINDOW_TITLE='Multiple plot')
;colorInd:  a vector indexes used to plot each observation with a given color 

;Resistant to NaN (they are excluded)

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
IF (N_ELEMENTS(colorInd) GT 0) THEN colorInd = colorInd[indFinXAndY]

gain = REGRESS(REFORM(x), REFORM(y), CONST=offset, CORRELATION=r)
res2 = linregstat(REFORM(x), REFORM(y))
pValGain = res2[3]
res = [r^2, gain, offset, pValGain, r]
IF (doPlot EQ 1) THEN BEGIN

  ;xrange = [FLOOR(MIN([x,y]) - (MAX([x,y])-MIN([x,y]))/10.0), CEIL(MAX([x,y]) + (MAX([x,y])-MIN([x,y]))/10.0)]
  ;yrange = xrange
  IF (N_ELEMENTS(layoutVec) GT 0) THEN BEGIN
    fontsize = 7.5
    fontstyle = 0 
    gtitle = title +', R2=' + STRING(FORMAT='(F4.2)',res[0]) 
    symsize = 0.4
    xminor = 0
  ENDIF ELSE BEGIN
    fontsize = 16
    fontstyle = 1
    gtitle = 'r = ' + STRING((res[4]), FORMAT='(f5.2)')+ '; R$^2$ = ' + STRING(res[0], FORMAT='(f4.2)')
    symsize = 0.75
    xminor = -1
  ENDELSE
  
  IF (N_ELEMENTS(colorInd) GT 0) THEN $   ;plot each point according to the color index and add a legend
    indexList = UNIQLIST(colorInd) $
  ELSE indexList = 0
  ;prepare single or multi plot
  IF (N_ELEMENTS(layoutVec) GT 0) THEN BEGIN
    gh0 =  PLOT(REFORM(x), REFORM(y),  FONT_SIZE=fontsize, FONT_STYLE=fontstyle, $
          TITLE=gtitle, XMINOR = xminor, XTICKVALUES=[MIN(x),MAX(x)],  YTICKVALUES=[MIN(y),MAX(y)], $
          LINESTYLE="none", SYMBOL="o", XTITLE=xtitle, YTITLE=ytitle, LAYOUT = layoutVec, /CURRENT, /NODATA)  ;ASPECT_RATIO=1,
  ENDIF ELSE BEGIN
    gh0 =  PLOT(REFORM(x), REFORM(y), FONT_SIZE=fontsize, FONT_STYLE=fontstyle,  $
          TITLE=gtitle, XMINOR = xminor, $
          LINESTYLE="none", SYMBOL="o", XTITLE=xtitle, YTITLE=ytitle, /NODATA) ;ASPECT_RATIO=1,
  ENDELSE

  
  FOR i = 0, N_ELEMENTS(indexList)- 1 DO BEGIN
    ;gh = 
    ctable = COLORTABLE(34, NCOLORS=N_ELEMENTS(indexList))
    ctable[0,*] = [0,0,0]
    IF (N_ELEMENTS(colorInd) GT 0) THEN ind = WHERE(colorInd EQ indexList[i]) ELSE ind = INDGEN(N_ELEMENTS(y))
    IF (N_ELEMENTS(layoutVec) GT 0) THEN BEGIN
      gh =  PLOT(REFORM(x[ind]), REFORM(y[ind]),  FONT_SIZE=fontsize, FONT_STYLE=fontstyle, $
        TITLE=gtitle, XMINOR = xminor, XTICKVALUES=[MIN(x),MAX(x)],  YTICKVALUES=[MIN(y),MAX(y)], $
        LINESTYLE="none", SYMBOL="o", XTITLE=xtitle, YTITLE=ytitle, LAYOUT = layoutVec, /CURRENT, $
        SYM_FILL_COLOR = REFORM(ctable[i,*]), OVERPLOT = 1, NAME = STRTRIM(indexList[i],2))  ;ASPECT_RATIO=1,
      gh0=[gh0,gh]
    ENDIF ELSE BEGIN
      gh =  PLOT(REFORM(x[ind]), REFORM(y[ind]), FONT_SIZE=fontsize, FONT_STYLE=fontstyle,  $
        TITLE=gtitle, XMINOR = xminor, $
        LINESTYLE="none", SYMBOL="o", XTITLE=xtitle, YTITLE=ytitle, $
        SYM_FILL_COLOR = REFORM(ctable[i,*]), OVERPLOT = 1, NAME = STRTRIM(indexList[i],2)) ;ASPECT_RATIO=1,
      gh0=[gh0,gh]
    ENDELSE
    gh.SYM_SIZE = symsize
    gh.SYM_FILLED = 1
  ENDFOR
  IF (N_ELEMENTS(colorInd) GT 0) THEN BEGIN
    xr = gh.XRANGE
    yr = gh.YRANGE
    l = LEGEND(TARGET=gh0, POSITION=[xr[0], yr[1]], $
    HORIZONTAL_ALIGNMENT='LEFT', VERTICAL_ALIGNMENT='TOP',/DATA,/AUTO_TEXT_COLOR, $
    SHADOW = 0)
  ENDIF
    
  
  
  xrange = [min(x),max(x)]
  gh = PLOT(xrange, xrange*gain[0]+offset, color = "red", LINESTYLE=0, OVERPLOT = 1)
  
  
  ;gh.save, 'K:\ILRI\Comparison results\aggregate files\Results\'+title+'_'+ytitle+'.png'
ENDIF
RETURN, res
END

;code for figure scatterplot
;gh0 =  PLOT(REFORM(x), REFORM(y), FONT_SIZE=fontsize, FONT_STYLE=fontstyle,  $
;  XMINOR = xminor, $
;  LINESTYLE="none", SYMBOL="o", XTITLE='CFAPAR', YTITLE='Measured biomass (kg ha$^-^1$)')
;gh0.SYM_SIZE = 0.75
;gh0.SYM_FILLED = 1
;xrange = [min(x),max(x)]
;gh = PLOT(xrange, xrange*gain[0]+offset, color = "black", LINESTYLE=0, OVERPLOT = 1)
;text0 = TEXT(0.2,0.8,'y = 104.06 + 30.33 x', /CURRENT, FONT_SIZE=15)
;line = '$\it r$ $\rm$= ' + STRING((res[4]), FORMAT='(f5.2)')+ ', R$^2$ = ' + STRING(res[0], FORMAT='(f4.2)')
;
;  text1 = TEXT(0.2,0.75, line, /CURRENT, FONT_SIZE=15)
;text2 = TEXT(0.2,0.7,  'P<0.001', /CURRENT, FONT_SIZE=15)