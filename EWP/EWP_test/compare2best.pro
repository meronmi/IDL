PRO compare2best
doplot = 1
prefix = ''
xlabel = prefix + 'SPI7'
ylabel = prefix  + 'zCWP2000'
ns = 1867
nl = 348
dir = 'E:\WA\EWP\CROSSCORR'
corr_suffix = '_ZFAPAR_bestCrossCorr_up2eos'
lag_suffix = '_ZFAPAR_bestLag_up2eos'
pval_suffix = '_ZFAPAR_bestPval_up2eos'

variable = 'corr'

CASE variable OF
  'corr': BEGIN
    x_fn = dir + '\' + xlabel + corr_suffix
    y_fn = dir + '\' + ylabel + corr_suffix
  END
  'lag':BEGIN
    x_fn = dir + '\' + xlabel + lag_suffix
    y_fn = dir + '\' + ylabel + lag_suffix
  END
  'pval':BEGIN
    x_fn = dir + '\' + xlabel + pval_suffix
    y_fn = dir + '\' + ylabel + pval_suffix
  END
  ELSE: STOP
ENDCASE

mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
fn_intersection = 'eco_crop_pasture_res_TAMSAT.img' ;pasrure = 1, crop = 2

mask = BYTARR(ns,nl)*0B
OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
READU, lun, mask
FREE_LUN, lun

x = FLTARR(ns,nl)
y = FLTARR(ns,nl)
OPENR, lun, x_fn, /GET_LUN
READU, lun, x
FREE_LUN, lun

OPENR, lun, y_fn, /GET_LUN
READU, lun, y
FREE_LUN, lun

;mask them
ind2mask = WHERE(mask LT 1, count2mask)
x[ind2mask] = !VALUES.F_NAN
y[ind2mask] = !VALUES.F_NAN
PRINT, xlabel+ ' Mean r =', MEAN(x, /NAN)
PRINT, ylabel+ ' Mean r =', MEAN(y, /NAN)
delta = y-x
;hh = PLOT(x, delta, LINESTYLE='', SYMBOL='.')
deltaPrct = delta/x*100.0
PRINT, 'Mean delta r =', MEAN(delta, /NAN)
PRINT, 'Max delta r =', MAX(delta, /NAN)
PRINT, 'Min delta r =', MIN(delta, /NAN)
PRINT, 'Mean delta as prct = ', MEAN(deltaPrct, /NAN)
ind_pos_deltaPrct = WHERE(FINITE(deltaPrct) AND (deltaPrct GT 0), count_pos_deltaPrct)
ind_neg_deltaPrct = WHERE(FINITE(deltaPrct) AND (deltaPrct LE 0), count_neg_deltaPrct)
PRINT, '% having positive increase = ', count_pos_deltaPrct/FLOAT(TOTAL(FINITE(deltaPrct)))*100
PRINT, '% having negative or null increase = ', count_neg_deltaPrct/FLOAT(TOTAL(FINITE(deltaPrct)))*100
ind_pos10_deltaPrct = WHERE(FINITE(deltaPrct) AND (deltaPrct GT 10), count_pos10_deltaPrct)
PRINT, '% having an increase GT 10%= ', count_pos10_deltaPrct/FLOAT(TOTAL(FINITE(deltaPrct)))*100
IF (doPlot EQ 1) THEN BEGIN
   ind_fin_delta = WHERE(FINITE(delta))
   ind_fin_deltaPrct = WHERE(FINITE(deltaPrct))
   pdf_delta = HISTOGRAM(delta[ind_fin_delta], BINSIZE=0.01, LOCATIONS=x_delta, MIN = -0.4, MAX = 0.4)
   pdf_deltaPrct = HISTOGRAM(deltaPrct[ind_fin_deltaPrct], BINSIZE=5.0, LOCATIONS=x_deltaPrct, MIN = -100, MAX = +100)
   h0 = PLOT(x_delta, pdf_delta/TOTAL(pdf_delta)*100, XRANGE=[-0.5,1,0], TITLE= 'r '+ ylabel + ' - r '+ xlabel, XTITLE='Delta corr', $
     YTITLE='Frequency %', AXIS_STYLE=1, COLOR='red', NAME = 'delta', /STAIRSTEP)
   h0b = PLOT([0,0],[0,100], COLOR='black', OVERPLOT = 1, YRANGE = h0.YRANGE)
   h1 = PLOT(x_deltaPrct, pdf_deltaPrct/TOTAL(pdf_deltaPrct)*100, XRANGE=[-0.5,1,0], TITLE= '% increase in r', XTITLE='% increase', $
     YTITLE='Frequency %', AXIS_STYLE=1, COLOR='red', NAME = 'deltaPrct', /STAIRSTEP)
   h1b = PLOT([0,0],[0,100], COLOR='black', OVERPLOT = 1, YRANGE = h1.YRANGE)
ENDIF
  

END