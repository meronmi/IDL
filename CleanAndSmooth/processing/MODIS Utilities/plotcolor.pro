PRO plotcolor, xmat, ymat, title
;xmat is  REFORM(archive_mat_Short[indS, *]/n_obs_short_archive[j], dims[1]*dims[2], 1)
;ymat is REFORM(gimms_mat_Short[indS, *]/n_obs_short_gimms[j], dims[1]*dims[2], 1)
!P.Multi=[0,2,1]
xrange = [MIN([xmat,ymat]) - (MAX([xmat,ymat])-MIN([xmat,ymat]))/10.0, MAX([xmat,ymat]) + (MAX([xmat,ymat])-MIN([xmat,ymat]))/10.0]
yrange = xrange

;color by division
window, /FREE, XSIZE = 800, YSIZE = 400
;make the plot
plot, xrange, yrange, PSYM = 3, $
          TITLE = title, CHARSIZE=1, BACKGROUND = 16777215, COLOR = 0, XRANGE=xrange, YRANGE=yrange,  /NODATA
;plot by division
mypsym = 0          
FOR c = 0, N_ELEMENTS(xmat[0,*])-1 DO BEGIN
  IF (c MOD 10 EQ 0) THEN  mypsym = mypsym + 1
  IF mypsym GT 7 THEN mypsym = 1
  colr=LONG(c)*1500
  oplot, REFORM(xmat[*,c]), REFORM(ymat[*,c]), PSYM = mypsym, COLOR = colr, SYMSIZE=0.4
  ;PRINT, c+1 
END

dims = SIZE(xmat)
x = REFORM(xmat, dims[1]*dims[2], 1)
y = REFORM(ymat, dims[1]*dims[2], 1)
gainS =  REGRESS(REFORM(x), REFORM(y), CONST=offsetS, CORRELATION=rS)
oplot, [0,1000000],[0,1000000],  COLOR = 0,  LINESTYLE=2
oplot, xrange, xrange*gainS[0]+offsetS, color = 255L + 256*0L + (256^2)*0L, LINESTYLE=0

;make the centred plot
;center
x_div_avg = MEAN(xmat, DIMENSION = 1)
y_div_avg = MEAN(ymat, DIMENSION = 1)
x_centred = xmat *!VALUES.F_NAN
y_centred = xmat *!VALUES.F_NAN
FOR c = 0, N_ELEMENTS(xmat[0,*])-1 DO BEGIN
  x_centred[*,c] = REFORM(xmat[*,c])-x_div_avg[c]
  y_centred[*,c] = REFORM(ymat[*,c])-y_div_avg[c]
ENDFOR
xrange_centred = [MIN([x_centred,y_centred]) - (MAX([x_centred,y_centred])-MIN([x_centred,y_centred]))/10.0, MAX([x_centred,y_centred]) + (MAX([x_centred,y_centred])-MIN([x_centred,y_centred]))/10.0]
yrange_centred = xrange_centred
;color by division
;make the plot
plot, xrange_centred, yrange_centred, PSYM = 3, $
          TITLE = title + ' CENTRED', CHARSIZE=1, BACKGROUND = 16777215, COLOR = 0, XRANGE=xrange_centred, YRANGE=yrange_centred,  /NODATA
;plot by division
mypsym = 0          
FOR c = 0, N_ELEMENTS(xmat[0,*])-1 DO BEGIN
  IF (c MOD 10 EQ 0) THEN  mypsym = mypsym + 1
  IF mypsym GT 7 THEN mypsym = 1
  colr=LONG(c)*1500
  oplot, REFORM(x_centred[*,c]), REFORM(y_centred[*,c]), PSYM = mypsym, COLOR = colr, SYMSIZE=0.4
END

dims = SIZE(xmat)
x = REFORM(x_centred, dims[1]*dims[2], 1)
y = REFORM(y_centred, dims[1]*dims[2], 1)
gain =  REGRESS(REFORM(x), REFORM(y), CONST=offset, CORRELATION=r)
oplot, [-1000000,1000000],[-1000000,1000000],  COLOR = 0,  LINESTYLE=2
oplot, xrange_centred, xrange_centred*gain[0]+offset, color = 255L + 256*0L + (256^2)*0L, LINESTYLE=0
PRINT, 'gain offset r^2'
PRINT, gain[0], offset, r^2
END