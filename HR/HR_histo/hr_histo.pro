PRO hr_histo
;path_in = 'X:\Active Projects\HR and Hitograms\free_state\feb_01_21' 
;path_in = 'X:\Active Projects\HR and Hitograms\free_state\dec_01_21'
path_in = 'X:\Active Projects\HR and Hitograms\free_state\aug_10_21'
fns = ['free_state_ndvi2013.tif','free_state_ndvi2014.tif','free_state_ndvi2015.tif']
;fns = ['ndvi2014.tif','ndvi2015.tif','ndvi2016.tif']
;path_in = 'X:\Active Projects\HR and Hitograms\west_cape\feb21-mar10';'X:\Active Projects\HR and Hitograms\west_cape\june_01_21'
;;fns = ['west_cape_ndvi2013.tif','west_cape_ndvi2014.tif','west_cape_ndvi2015.tif']
;fns = ['west_cape_ndvi2014.tif','west_cape_ndvi2015.tif']


labels = FILE_BASENAME(fns, '.tif')
plot_cdf = 0

;1 Read the tif
tmp = READ_TIFF(path_in + '\' + fns[0])
res = QUERY_TIFF(path_in + '\' + fns[0], GEOTIFF=geoinfo)
sz = SIZE(tmp)
;PRINT, 'Bands: ' + STRTRIM(sz[1],2)
data = FLTARR(sz[1], sz[2], N_ELEMENTS(fns))
data[*,*, 0] = tmp
FOR i = 1, N_ELEMENTS(fns)- 1 DO BEGIN
  data[*,*, i] = READ_TIFF(path_in + '\' + fns[i])
ENDFOR

pdf_list = LIST()
;2 Make histos
maxPdf = !NULL
FOR i = 0, N_ELEMENTS(fns)-1 DO BEGIN
  tmp = data[*,*, i]
  indFin = WHERE(FINITE(tmp))
  PRINT, fns[i] + ' mean NDVI = ' + STRTRIM(MEAN(tmp[indFin]),2)
  xr = [0.0, 1.0]
  pdf= HISTOGRAM(tmp[indFin], BINSIZE=0.01, LOCATIONS=xbin, MIN = xr[0], MAX = xr[1])
  pdf_list.add, pdf
  cdf = TOTAL(pdf, /CUMULATIVE) / N_ELEMENTS(indFin)
  maxPdf = MAX([pdf/TOTAL(pdf)*100, maxPdf])
  IF (i EQ 0) THEN BEGIN
    h = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[xr[0],xr[1]], TITLE='NDVI PDF', XTITLE='NDVI', YTITLE='FREQ. %', $
      COLOR='green', NAME = fns[0], THICK = 2, AXIS_STYLE=1)
    IF (plot_cdf) THEN $
    hc = PLOT(xbin, cdf, XRANGE=[xr[0],xr[1]], TITLE='NDVI CDF', XTITLE='NDVI', YTITLE='Cum FREQ.', $
      COLOR='green', NAME = fns[0], THICK = 2, AXIS_STYLE=1)
  ENDIF ELSE BEGIN
    IF (i EQ 1) THEN clr = 'blue' ELSE clr = 'red'
    h[0].select
    h = [h, PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[xr[0],xr[1]], $
      COLOR=clr, NAME = fns[i], /OVERPLOT)]
    IF (plot_cdf) THEN BEGIN
      hc[0].select
      hc = [hc, PLOT(xbin, cdf, XRANGE=[xr[0],xr[1]], $
        COLOR=clr, NAME = fns[i], /OVERPLOT)]
    ENDIF
  ENDELSE
  
ENDFOR
h[0].select
legh = LEGEND(TARGET=h, POSITION=[1.0,maxPdf ], /DATA, /AUTO_TEXT_COLOR, LINESTYLE='', SHADOW=0, VERTICAL_ALIGNMENT='BOTTOM')
h[0].select, /UNSELECT
IF (plot_cdf) THEN BEGIN
  hc[0].select
  leghc = LEGEND(TARGET=hc, POSITION=[0.5,1.0], /DATA, /AUTO_TEXT_COLOR, LINESTYLE='', SHADOW=0 )
  hc[0].select, /UNSELECT
ENDIF
;;histo of 2016 divided by histo of 2014
;hrat = PLOT(xbin, pdf_list[2]/FLOAT(pdf_list[0]), XRANGE=[xr[0],xr[1]], TITLE='NDVI PDF ratio', XTITLE='NDVI',  YTITLE='ratio', $
;      COLOR='red', NAME = 'ratio 16div14' , THICK = 2, AXIS_STYLE=1, YRANGE=[0,3])
;hrat2 = PLOT(xbin, pdf_list[1]/FLOAT(pdf_list[0]), XRANGE=[xr[0],xr[1]], OVERPLOT = 1, $
;      COLOR='blue', NAME = 'ratio 15div14' , THICK = 2, AXIS_STYLE=1, YRANGE=[0,3])
;hrat3 = PLOT([0,1], [1,1], OVERPLOT=1, COLOR = 'black', LINESTYLE='--')
;leghc = LEGEND(TARGET=[hrat, hrat2], POSITION=[1.0,2.5], /DATA, /AUTO_TEXT_COLOR, LINESTYLE='', SHADOW=0 )
CLOSE, /ALL
END