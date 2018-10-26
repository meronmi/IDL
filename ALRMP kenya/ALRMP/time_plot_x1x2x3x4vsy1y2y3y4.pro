PRO time_plot_X1X2X3X4vsY1Y2Y3Y4, jd1, y1,  jd2, y2, jd3, y3, jd4, y4,title


dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
fs = 12
ss = 0.7

lp = [150, 580] ;legend position
;start = MIN(jd)+364.5*10+8
;res = MIN(ABS(jd-start), ind_start) 
rrx = [MIN(jd1-60), MAX(jd1)+60]

;rfe
yrange = [0, 200]
hb = BARPLOT(jd4, y4, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  COLOR = 'royal blue', FILL_COLOR='royal blue', $
  MARGIN = [0.15, 0.15, 0.20, 0.15], AXIS_STYLE = 0, $
  DIMENSIONS=[1000,600],  $
  XRANGE = rrx, YRANGE = yrange, NAME = 'RFE')
a_hb = AXIS('y', TARGET = hb, LOCATION = [max(hb.xrange)+700,0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'RFE (mm)', YRANGE=yrange)
;ndvi
h0 = PLOT(jd1, y1, LINESTYLE = '-', SYMBOL='o', COLOR='black', SYM_FILLED= 0, $; XRANGE=rr, YRANGE=rr, TITLE = tit, $
  XTITLE='Time', YTITLE='NDVI (-)', TITLE = title, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  Name='NDVI', XRANGE=rrx, THICK = 2, $
  AXIS_STYLE = 1, SYM_SIZE=ss,  FONT_SIZE=fs,$
  MARGIN = [0.15, 0.15, 0.20, 0.15], /CURRENT)
;ndvi lta
h1 = PLOT(jd2, y2, LINESTYLE = '-', SYMBOL='none', COLOR='green', NAME = 'NDVI_LTA', $
  OVERPLOT = 1, THICK = 2); AXIS_STYLE = 0

;FOR i = 0, 400, 50 DO BEGIN
;  hh = PLOT ([jd2[i],[jd2[i]]], [0.0,max(h0.yrange)],  LINESTYLE = '--', OVERPLOT = 1)
;ENDFOR


;mortrate
yrange = [0, 0.35];MAX(y3, /NAN) + 0.1]
h2 = PLOT(jd3, y3, LINESTYLE = '-', SYMBOL='o', COLOR='red', SYM_FILLED= 1, NAME = 'Tot_mort', $
   XTITLE='Time', YTITLE='NDVI (-)', YRANGE = yrange, XRANGE = rrx, /CURRENT, $
   MARGIN = [0.15, 0.15, 0.20, 0.15], AXIS_STYLE = 0); AXIS_STYLE = 0
   
a_h2 = AXIS('y', TARGET = h2, LOCATION = [max(h0.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'Mortality rate (-)', YRANGE=yrange)
!null = LEGEND(target=[h0, h1, hb, h2], /AUTO_TEXT_COLOR, FONT_SIZE = fs, POSITION=lp, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, /DEVICE)
;h0.Save, 'X:\works\pubblicazioni\in preparazione\2015 Frontiers\vgt smoothing\different smoothing graph.png'
;h0['axis0'].TITLE.FONT_SIZE = fs+5

;h0.AXES.XTITLE.FONT_SIZE=fs+5
;PRINT, 'FINITO'
END