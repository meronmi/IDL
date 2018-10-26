Pro corr_r_and_whc
ns = 562
nl = 106
fn_r = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps\BEST_resampled_to_WSI_soil_whc\SPI18_atProg50-corr_with-zCFAPAR_res_at_WSI_whc.img'
dt_r = 4
fn_mask = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\eco_crop_pasture_res_WSI_wch.img' 
dt_mask = 1
fn_whc = 'E:\WA\SOIL-ATERRA\fao_soil_whc_0-125_nodata-9999_WA_win.img'
dt_whc = 4
;attenzione ai -9999
r = FLTARR(ns, nl)
OPENR, lun, fn_r, /GET_LUN
READU, lun, r
FREE_LUN, lun
ind = WHERE(r LT -1.0, count)
IF (count GT 0) THEN r[ind]=!VALUES.F_NAN

whc = FLTARR(ns, nl)
OPENR, lun, fn_whc, /GET_LUN
READU, lun, whc
FREE_LUN, lun 
ind = WHERE(whc LT 0.0, count)
IF (count GT 0) THEN whc[ind]=!VALUES.F_NAN

mask = BYTARR(ns, nl)
OPENR, lun, fn_mask, /GET_LUN
READU, lun, mask
FREE_LUN, lun



ind_all = WHERE(FINITE(r) AND FINITE(whc) AND (mask GT 0), count_can)
ind_rangelnd = WHERE(FINITE(r) AND FINITE(whc) AND (mask EQ 1))
ind_croplnd =   WHERE(FINITE(r) AND FINITE(whc) AND (mask EQ 2))

PRINT, 'r all = ', CORRELATE(r[ind_all], whc[ind_all])
PRINT, 'r rangeland = ', CORRELATE(r[ind_rangelnd], whc[ind_rangelnd])
PRINT, 'r cropland = ', CORRELATE(r[ind_croplnd], whc[ind_croplnd])
END