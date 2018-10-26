Function hovmoller, data, y, x, yname, xname, graph_title, fn_base, FN_OUT = fn_out
;ret = hovmoller(count_miss_avg_per_dek, y, x, 'Lat', 'Dek', fnout_path + '\' + fn_base)
mrgn = [0.1,0.2,0.1,0.1]
ct = COLORTABLE(72, /reverse)
c = CONTOUR(data, x, y,  /FILL, RGB_TABLE=ct, TITLE=graph_title, XTITLE= xname, YTITLE=yname, XRANGE=[MIN(x),MAX(x)], YRANGE=[MIN(y),MAX(y)], MARGIN=mrgn)
cb = COLORBAR(TITLE=graph_title, ORIENTATION=0, POSITION=[0.35,0.07,0.65,0.12])
IF N_ELEMENTS(fn_out GT 0) THEN BEGIN
  c.save, fn_out
ENDIF

RETURN, 0
END