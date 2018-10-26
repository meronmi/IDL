PRO compare_with_cloudiness
fn_cloud = 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\cloudiness\MODCF_meanannual_BOKU_GRID_THINNED.img'
fn_ndvi = 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\Results\OFmeanNDVI_veg_active.img'
var = 'mean(abs(zN0H0-zNfHf))'  
dir_out  = 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\cloudiness\corr with mean abs'
fn = dir_out+'\'+'zN0H0_mean_delta_veg.img'

;open cloudiness file and scale it
cloud = ReadEnviWithHdr(fn_cloud)
indFill = WHERE((cloud GT 10000) OR (cloud LT 0), count)
cloud = cloud * 0.01
IF count gT 0 THEN cloud[indFill] = !VALUES.F_NAN

data = ReadEnviWithHdr(fn)
ndvi = ReadEnviWithHdr(fn_ndvi)
szC = SIZE(cloud)
szD = SIZE(data)
IF (TOTAL(szC-szD) NE 0) THEN STOP
indFin = WHERE(FINITE(cloud) AND FINITE(data), count)

ols_stat = 1

DensityAndFit_log_scale, REFORM(cloud[indFin]), REFORM(data[indFin]), 'Cloudiness (%)', var, dir_out, [0,100], [MIN(data[indFin]),MAX(data[indFin])], $
  100, 30, TITLE='Mean annual cloudiness vs. ' + var, $ ;ngrid, nlevels, TITLE=title, $
  DOFIT = 1, logfit= 0, OLS_STAT = ols_stat, SIMPLE = 1, DOLOG = 0, PLOT1TO1 = 0;FILESUFFIX = filesuffix, NOWIN=nowin, RGBTAB = rgbtab, SIMPLE = simple, DOLOG = dolog, $
  
DensityAndFit_log_scale, REFORM(ndvi[indFin]), REFORM(data[indFin]), 'NDVI', var, dir_out, [0,1.0], [MIN(data[indFin]),MAX(data[indFin])], $
  100, 30, TITLE='Mean NDVI during season vs. ' + var, $ ;ngrid, nlevels, TITLE=title, $
  DOFIT = 1, logfit= 0, OLS_STAT = ols_stat, SIMPLE = 1, DOLOG = 0, PLOT1TO1 = 0


;apply the regression to x, compoute y_est and the residuals y-y_est, save it
y_est = cloud*!VALUES.F_NAN
y_est[indFin] = ols_stat.offset + cloud[indFin] * ols_stat.slope
;now reuse the var for residulas
y_est[indFin] = data[indFin] - y_est[indFin]
fnRes = FILE_BASENAME(fn, '.img')
res = write_envi_img(y_est, dir_out+'\'+fnRes+'_RESIDUALS_OLS_with_cloud_cover.img')  
res = write_envi_hdr(dir_out+'\'+fnRes+'_RESIDUALS_OLS_with_cloud_cover.hdr', szD[1], szD[2], 4)

;check
;h = plot(REFORM(cloud[indFin]), REFORM(data[indFin]), XRANGE=[0.001,100], LINESTYLE=' ', SYMBOL='.', DIMENSIONS=[600,600], YRANGE= [MIN(data[indFin]),MAX(data[indFin])])
;slope=REGRESS(REFORM(cloud[indFin]),REFORM(data[indFin]),const=const,correlation=corr)
;yy=slope[0]*[0,100]+const
;hh = plot([0,100],yy, color='red', overplot = 1)
END