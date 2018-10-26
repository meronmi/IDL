PRO time_plot_X1X2X3X4vsY1Y2Y3Y4_and_pheno, jd1, y1,  jd2, y2, jd3, y3, jd4, y4, starts, ends, title


dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
fs = 12
ss = 0.7
largerDimX = 250
mm = [0.08, 0.22, 0.18, 0.1]
lp = [900, 580] ;legend position
;start = MIN(jd)+364.5*10+8
;res = MIN(ABS(jd-start), ind_start) 
rrx = [MIN(jd1-60), MAX(jd1)+60]

;rfe
yrange = [0, 200]
hb = BARPLOT(jd4, y4, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  COLOR = 'royal blue', FILL_COLOR='royal blue', $
  MARGIN = mm, AXIS_STYLE = 0, $
  DIMENSIONS=[1000+largerDimX,600],  $
  XRANGE = rrx, YRANGE = yrange, NAME = 'RFE')
a_hb = AXIS('y', TARGET = hb, LOCATION = [max(hb.xrange)+700,0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'RFE (mm)', YRANGE=yrange, COLOR = 'royal blue')
;ndvi
h0 = PLOT(jd1, y1, LINESTYLE = '-', SYMBOL='none', COLOR='black', SYM_FILLED= 0, $; XRANGE=rr, YRANGE=rr, TITLE = tit, $
  XTITLE='Time', YTITLE='NDVI (-)', TITLE = title, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  Name='NDVI', XRANGE=rrx, THICK = 2, YTICKDIR = 1, XTICKDIR = 1,$
  AXIS_STYLE = 1, SYM_SIZE=ss,  FONT_SIZE=fs,$
  MARGIN = mm, /CURRENT)
;ndvi lta
h1 = PLOT(jd2, y2, LINESTYLE = '-', SYMBOL='none', COLOR='green', NAME = 'NDVI_LTA', $
  OVERPLOT = 1, THICK = 2); AXIS_STYLE = 0



;plot the starts of Season
indSos = WHERE(starts EQ 1, countSos)
;FOR i = 0, countSos-1 DO BEGIN
;  hh = PLOT ([jd2[indSos[i]],[jd2[indSos[i]]]], [min(h0.yrange),max(h0.yrange)],  LINESTYLE = '-', OVERPLOT = 1, COLOR = 'grey')
;ENDFOR
;plot the ends of Season
indEos = WHERE(ends EQ 1, countEos)
;FOR i = 0, countEos-1 DO BEGIN
;  hh = PLOT ([jd2[indEos[i]],[jd2[indEos[i]]]], [min(h0.yrange),max(h0.yrange)],  LINESTYLE = '--', OVERPLOT = 1, COLOR = 'grey')
;ENDFOR
;insted: grey out the area when out of season
;if the first event is a start grey form origin
IF (jd2[indSos[0]] LT jd2[indEos[0]]) THEN $
  hh = PLOT ([jd2[0],jd2[indSos[0]]], [max(h0.yrange),max(h0.yrange)],  LINESTYLE = 'none', OVERPLOT = 1, FILL_BACKGROUND = 1, COLOR = 'grey', FILL_LEVEL = min(h0.yrange), FILL_TRANSPARENCY = 80)
;if the last event is a stop grey to end
IF (jd2[indEos[-1]] GT jd2[indSos[-1]]) THEN $
  hh = PLOT ([jd2[indEos[-1]],jd2[-1]], [max(h0.yrange),max(h0.yrange)],  LINESTYLE = 'none', OVERPLOT = 1, FILL_BACKGROUND = 1, COLOR = 'grey', FILL_LEVEL = min(h0.yrange), FILL_TRANSPARENCY = 80)
;in between:
FOR i = 0, countEos-1 DO BEGIN
  ind = WHERE (jd2[indSos] GT jd2[indEos[i]], count)
  IF (count GT 0) THEN BEGIN
    hh = PLOT ([jd2[indEos[i]],[jd2[indSos[ind[0]]]]], [max(h0.yrange),max(h0.yrange)],  LINESTYLE = 'none', OVERPLOT = 1, FILL_BACKGROUND = 1, COLOR = 'grey', FILL_LEVEL = min(h0.yrange), FILL_TRANSPARENCY = 80)
  ENDIF
ENDFOR


;mortrate
yrange = [0, 0.35];MAX(y3, /NAN) + 0.1]
h2 = PLOT(jd3, y3, LINESTYLE = '-', SYMBOL='o', COLOR='red', SYM_FILLED= 1, NAME = 'Tot_mort', $
   XTITLE='Time', YTITLE='NDVI (-)', YRANGE = yrange, XRANGE = rrx, /CURRENT, $
   MARGIN = mm, AXIS_STYLE = 0, SYM_SIZE = 0.8); AXIS_STYLE = 0
   
a_h2 = AXIS('y', TARGET = h2, LOCATION = [max(h0.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'Mortality rate (-)', YRANGE=yrange, COLOR = 'red')
!null = LEGEND(target=[h0, h1, hb, h2], /AUTO_TEXT_COLOR, FONT_SIZE = fs, POSITION=lp, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, ORIENTATION = 1, /DEVICE)
;h0.Save, 'X:\works\pubblicazioni\in preparazione\2015 Frontiers\vgt smoothing\different smoothing graph.png'
;h0['axis0'].TITLE.FONT_SIZE = fs+5

;h0.AXES.XTITLE.FONT_SIZE=fs+5
;PRINT, 'FINITO'
END