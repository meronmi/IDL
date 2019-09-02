FUNCTION Raphael_getInfoById, id
  fn_sav = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190328.sav'
  doPlotS1 = 1
  RESTORE, fn_sav
  ind = WHERE(x.pointID EQ id) ;
  ind = ind[0]
  tn = [TAG_NAMES(x), 'Coord String']
  strVal = !NULL
  FOR i = 0, N_TAGS(x)-1 DO strVal = [strVal, STRTRIM(x.(i)[ind],2)]
  strVal = [strVal, STRTRIM(STRING(x.x[ind]) + ' E '+STRTRIM(x.y[ind],2)+' N',2)]
  IF (doPlotS1 EQ 1) THEN BEGIN
    index_names = ['VH_mean', 'VV_mean', 'VH_VV_ratio']
    FOR i = 0, 2 DO BEGIN
      ind = WHERE((x.pointID EQ id) AND (x.index_name EQ index_names[i]), count)
      date = x.date[ind]
      dateJd = YYYYbMMbDD2jd(date)
      ;order date ascending
      subAsc = SORT(dateJd)
      dateJd = dateJd[subAsc]
      y = x.index_value[ind]
      y = y[subAsc]
      ;now remove possible NaN (important, it may cause troubles later when fitting)
      indFin = WHERE(FINITE(y))
      dateJd = dateJd[indFin]
      y = y[indFin]
      PRINT, index_names[i]
      PRINT, y
      CASE i OF
        0: h1 = PLOT(dateJd, y, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=index_names[i], MARGIN = [0.15, 0.15, 0.20, 0.15], $
          TITLE= ' ID ' + STRTRIM(id,2), LINESTYLE='-', SYMBOL='o', YTITLE='VV, VH', DIMENSIONS=[1200,600], XMINOR=5, AXIS_STYLE = 1)
        1: BEGIN
          h2 = PLOT(dateJd, y, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=index_names[i], COLOR= 'red',$
            LINESTYLE='-', SYMBOL='+', /OVERPLOT)
          tmp = h2.YRANGE
          h2.YRANGE = [-0.025, tmp[1]]
          htmp = PLOT(h2.XRANGE,[0,0], LINESTYLE='--', COLOR='grey', /OVERPLOT)
        END
        2: BEGIN
          h3 = PLOT(dateJd, y, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=index_names[i], COLOR='green', $
            LINESTYLE='', SYMBOL='tu', MARGIN = [0.15, 0.15, 0.20, 0.15], AXIS_STYLE = 0, /CURRENT)
          ax_h3 = AXIS('y', TARGET = h3, LOCATION = [max(h3.xrange),0,0], TEXTPOS = 1, TITLE = 'VH/VV', TICKDIR=1)
          tmp = h3.YRANGE
          h3.YRANGE = [tmp[0], 1]
          htmp = PLOT(h3.XRANGE,[0,0], LINESTYLE='--', COLOR='grey', /OVERPLOT)
        END
      ENDCASE
    ENDFOR
    hl = LEGEND(TARGET=[h1,h2,h3], POSITION=[0.99,0.85], LINESTYLE='', TRANSPARENCY = 75)
  ENDIF
  RETURN,TRANSPOSE([[tn],[strVal]])
END