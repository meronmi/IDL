PRO time_plot_X1X2vsY1Y2, jd1, y1, jd2, y2, title


dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
fs = 12
ss = 0.7

;lp = [rrx[1]/2, rry[1]/15.0*14.0] ;legend position
;start = MIN(jd)+364.5*10+8
;res = MIN(ABS(jd-start), ind_start) 
rrx = [MIN(jd1-60), MAX(jd1)+60]

h0 = PLOT(jd1, y1, LINESTYLE = '-', SYMBOL='o', COLOR='black', SYM_FILLED= 0, $; XRANGE=rr, YRANGE=rr, TITLE = tit, $
  XTITLE='Time', YTITLE='NDVI (-)', TITLE = title, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  DIMENSIONS=[1000,600], SYM_SIZE=ss,  FONT_SIZE=fs, Name='NDVI', XRANGE=rrx, THICK = 2, $
  AXIS_STYLE = 1, $
  MARGIN = [0.15, 0.15, 0.20, 0.15])

yrange = [0, 0.35];MAX(y2, /NAN) + 0.1]
h1 = PLOT(jd2, y2, LINESTYLE = '-', SYMBOL='o', COLOR='red', SYM_FILLED= 1, NAME = 'Tot_mortrate', $
   XTITLE='Time', YTITLE='NDVI (-)', YRANGE = yrange, XRANGE = rrx, /CURRENT, $
   MARGIN = [0.15, 0.15, 0.20, 0.15], AXIS_STYLE = 0); AXIS_STYLE = 0
   
a_h1 = AXIS('y', TARGET = h1, LOCATION = [max(h0.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'Mortrate', YRANGE=yrange)
!null = LEGEND(target=[h0, h1], /AUTO_TEXT_COLOR, FONT_SIZE = fs, POSITION=[750, 520], $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, /DEVICE)
;h0.Save, 'X:\works\pubblicazioni\in preparazione\2015 Frontiers\vgt smoothing\different smoothing graph.png'
;h0['axis0'].TITLE.FONT_SIZE = fs+5

;h0.AXES.XTITLE.FONT_SIZE=fs+5
PRINT, 'FINITO'
END