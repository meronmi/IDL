PRO time_plot_XvsY, jd, y, title


dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
fs = 12
ss = 0.7

;lp = [rrx[1]/2, rry[1]/15.0*14.0] ;legend position
;start = MIN(jd)+364.5*10+8
;res = MIN(ABS(jd-start), ind_start) 
rrx = [MIN(jd-30), MAX(jd)+30]

h0 = PLOT(jd, y, LINESTYLE = '-', SYMBOL='o', COLOR='black', SYM_FILLED= 0, $; XRANGE=rr, YRANGE=rr, TITLE = tit, $
  XTITLE='Time', YTITLE='NDVI (-)', TITLE = title, $
  XTICKUNITS = ['Time', 'Time'], XTICKFORMAT='LABEL_DATE', XMINOR=6, $
  DIMENSIONS=[1000,600], SYM_SIZE=ss,  FONT_SIZE=fs, Name='raw', XRANGE=rrx, THICK = 2)

!null = LEGEND(target=[h0], /AUTO_TEXT_COLOR, FONT_SIZE = fs, POSITION=[750, 520], $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, /DEVICE)
;h0.Save, 'X:\works\pubblicazioni\in preparazione\2015 Frontiers\vgt smoothing\different smoothing graph.png'
;h0['axis0'].TITLE.FONT_SIZE = fs+5

;h0.AXES.XTITLE.FONT_SIZE=fs+5
PRINT, 'FINITO'
END