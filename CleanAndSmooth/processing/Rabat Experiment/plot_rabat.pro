Function plot_rabat, x, y, ind90, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90
symbolSize = 1.2
lineThickness = 2
textFontSize = 15

;check NaN
indFinX = WHERE(FINITE(x))
indFinY = WHERE(FINITE(y))
indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
x = x[indFinXAndY]
y = y[indFinXAndY]
indFinXAndYAndind90 = cgSetIntersection(indFinXAndY, ind90, count=countFin)
IF (N_ELEMENTS(indFinXAndY) NE N_ELEMENTS(x)) THEN BEGIN
  ind90tmp = !NULL
  FOR i=0, N_ELEMENTS(indFinXAndYAndind90)-1 DO BEGIN
    ind = WHERE(indFinXAndY EQ indFinXAndYAndind90[i], count)
    IF (count NE 0) THEN ind90tmp = [ind90tmp, ind]
    ENDFOR
ENDIF
;h0 = PLOT(x, y, LINESTYLE = 'none', SYMBOL='o', COLOR='grey', SYM_FILL_COLOR= 'grey', XRANGE=rr, YRANGE=rr, $
;  TITLE = tit, XTITLE=xtit, YTITLE=ytit, WINDOW_TITLE = tit + '_' + xtit + 'VS' + ytit, $
;  DIMENSIONS=[800,800], SYM_SIZE=symbolSize, SYM_FILLED=1, FONT_SIZE=fs, Name=' All_units')
h0 = PLOT(x, y, LINESTYLE = 'none', SYMBOL='o', COLOR='black', SYM_FILLED= 0, XRANGE=rr, YRANGE=rr, $
  TITLE = tit, XTITLE=xtit, YTITLE=ytit, WINDOW_TITLE = tit + '_' + xtit + 'VS' + ytit, $
  DIMENSIONS=[800,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, Name=' All_units')
h0.TITLE.FONT_SIZE =fs+10 
slope = REGRESS(x, y, CONST=offset, CORRELATION=r, FTEST = ft)
; Test of the slope
Df=N_ELEMENTS(x)-2
pgain = MPFTEST(ft, 1, Df)
;trick to avoid plotting outside
x0=(0.0-offset)/slope[0]
IF (x0 LT 0) THEN x0 = 0.0
x1=(rr[1]-offset)/slope[0]
IF (x1 GT rr[1]) THEN x1 = rr[1]
yreg = offset+slope[0]*[x0,x1]
h0r = PLOT([x0,x1], yreg, COLOR='black', OVERPLOT = 1, XRANGE=rr, YRANGE=rr, THICK = lineThickness, Name = 'OLS lin. reg.')
ht = TEXT(rr[1]*3/5.0,rr[1]*6.5/30.0, STRING(slope[0], offset, FORMAT='("y =", f7.4, " x ", f+7.4)', /PRINT) , /DATA, COLOR='black', FONT_SIZE=textFontSize)
ht2 = TEXT(rr[1]*3/5.0,rr[1]*5/30.0, '$R^2$ = ' + STRING(r^2,FORMAT='(f4.2)', /PRINT), /DATA, COLOR='black', FONT_SIZE=textFontSize)
;ht = TEXT(rr[1]*3/5,7.5, STRING(slope[0], offset, FORMAT='("y =", f7.4, " x ", f+7.4)', /PRINT) , /DATA, COLOR='grey', FONT_SIZE=textFontSize)
;ht2 = TEXT(rr[1]*3/5,6, '$R^2$ = ' + STRING(r^2,FORMAT='(f4.2)', /PRINT), /DATA, COLOR='grey', FONT_SIZE=textFontSize)
h11 = PLOT(rr, rr, COLOR='blue', OVERPLOT = 1, LINESTYLE='--', NAME=' 1:1 line', THICK = lineThickness)
IF (plot_90 EQ 1) THEN BEGIN
  h2 = PLOT(x[ind90], y[ind90], LINESTYLE = 'none', SYMBOL='o', COLOR='black', SYM_FILL_COLOR= 'green',$
            SYM_SIZE=symbolSize, SYM_FILLED=1, FONT_SIZE=fs, Name=' 90% production units', OVERPLOT=1)
ENDIF
slope90 = REGRESS(x[ind90], y[ind90], CONST=offset90, CORRELATION=r90, FTEST = ft)
; Test of the slope
Df=N_ELEMENTS(x[ind90])-2
pgain90 = MPFTEST(ft, 1, Df)
IF (plot_90 EQ 1) THEN BEGIN
  ;trick to avoid plotting outside
  x0=(0.0-offset90)/slope90[0]
  x1=(rr[1]-offset90)/slope90[0]
  IF (x1 GT rr[1]) THEN x1 = rr[1]
  IF (x0 LT 0) THEN x0=0.0
  yreg = offset90+slope90[0]*[x0,x1]
  h0r = PLOT([x0,x1], yreg, COLOR='green', OVERPLOT = 1, XRANGE=rr, YRANGE=rr, THICK = lineThickness)
  ht = TEXT(rr[1]*3/5.0,rr[1]*3/30.0, STRING(slope90[0], offset90, FORMAT='("y =", f7.4, " x ", f+7.4)', /PRINT) , /DATA, COLOR='green', FONT_SIZE=textFontSize)
  ht2 = TEXT(rr[1]*3/5.0,rr[1]*1.5/30.0, '$R^2$ = ' + STRING(r90^2,FORMAT='(f4.2)', /PRINT), /DATA, COLOR='green', FONT_SIZE=textFontSize)
  !null = LEGEND(target=[h0, h2, h11], /AUTO_TEXT_COLOR, FONT_SIZE = fs-3, POSITION=lp, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, /DATA)
ENDIF ELSE BEGIN
  !null = LEGEND(target=[h0r, h11], /AUTO_TEXT_COLOR, FONT_SIZE = fs-3, POSITION=lp, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, /DATA)
ENDELSE
IF save_out EQ 1 THEN h0.Save, out_fn

;[slope[0], offset, pgain, slope90[0], offset90, pgain90]
res = DBLARR(4,2)
res[*, 0] = [slope[0], offset, pgain, r^2]
res[*, 1] = [slope90[0], offset90, pgain90, r90^2]
RETURN, res
END