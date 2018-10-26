Pro corr_r_and_rain
ns = 1402
nl = 262
;fn_r = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps\SPI18_atProg50-corr_with-zCFAPAR'
fn_r = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps\SPI18_atProg50-cod_with-zCFAPAR'
dt_r = 4
fn_mask = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\eco_crop_pasture_res_CHIRPS.img' 
dt_mask = 1
fn_var_mean = 'E:\WA\EWP\CHIRPS ROI WA\mean_yearly_rain\LTA\chrps_ys_lta1962wa.img'
fn_var_sd = 'E:\WA\EWP\CHIRPS ROI WA\mean_yearly_rain\LTA\chrps_ys_lta1963wa.img'
dt_var = 4
;attenzione ai -9999
r = FLTARR(ns, nl)
OPENR, lun, fn_r, /GET_LUN
READU, lun, r
FREE_LUN, lun
ind = WHERE(r LT -1.0, count)
IF (count GT 0) THEN r[ind]=!VALUES.F_NAN

var = FLTARR(ns, nl)
OPENR, lun, fn_var_mean, /GET_LUN
READU, lun, var
FREE_LUN, lun 
ind = WHERE(var LT 0.0, count)
IF (count GT 0) THEN var[ind]=!VALUES.F_NAN

varsd = FLTARR(ns, nl)
OPENR, lun, fn_var_sd, /GET_LUN
READU, lun, varsd
FREE_LUN, lun
ind = WHERE(varsd LT 0.0, count)
IF (count GT 0) THEN varsd[ind]=!VALUES.F_NAN


mask = BYTARR(ns, nl)
OPENR, lun, fn_mask, /GET_LUN
READU, lun, mask
FREE_LUN, lun


;to look at cv
var = varsd/var

ind_all = WHERE(FINITE(r) AND FINITE(var) AND (mask GT 0), count_can)
ind_rangelnd = WHERE(FINITE(r) AND FINITE(var) AND (mask EQ 1), count1)
ind_croplnd =   WHERE(FINITE(r) AND FINITE(var) AND (mask EQ 2), count2)
PRINT, count_can, count1, count2

PRINT, 'r all = ', CORRELATE(r[ind_all], var[ind_all])
PRINT, linregstat(r[ind_all], var[ind_all])
PRINT, 'r rangeland = ', CORRELATE(r[ind_rangelnd], var[ind_rangelnd])
PRINT, linregstat(r[ind_rangelnd], var[ind_rangelnd])
PRINT, 'r cropland = ', CORRELATE(r[ind_croplnd], var[ind_croplnd])
PRINT, linregstat(r[ind_croplnd], var[ind_croplnd])
hh = PLOT(r[ind_rangelnd], var[ind_rangelnd], LINESTYLE='', SYMBOL='+', SYM_SIZE=0.05)
hh2 = PLOT(r[ind_croplnd], var[ind_croplnd], LINESTYLE='', SYMBOL='+', OVERPLOT=1, COLOR='red', SYM_SIZE=0.05)
END