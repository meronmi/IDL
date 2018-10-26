PRO stats_without_irrigated

;all at CHIRPS res
ns = 1402
nl = 262
eco_crop_pasture = BYTARR(ns, nl)
eco_irri = BYTARR(ns, nl)
p = FLTARR(ns, nl)
cod = FLTARR(ns, nl)
;map of crop-rangeland
;CHIRPS RESOLUTION
mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
fn_intersection = 'eco_crop_pasture_res_CHIRPS.img' ;pasrure = 1, crop = 2
OPENR, lun, mask_dir + '\' + fn_intersection, /GET_LUN
READU, lun, eco_crop_pasture
FREE_LUN, lun
;map of irri
fn_irri = 'eco_crop_irri_MARS_CropMask_res_CHIRPS.img'
OPENR, lun, mask_dir + '\' + fn_irri, /GET_LUN
READU, lun, eco_irri
FREE_LUN, lun
;map of best R2 up to 50%
dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps'
fn = 'best_cod_up_to_prog50.img'
OPENR, lun, dir + '\' + fn, /GET_LUN
READU, lun, cod
FREE_LUN, lun
;maps of best P up to 50%
fn = 'best_p_up_to_prog50.img'
OPENR, lun, dir + '\' + fn, /GET_LUN
READU, lun, p
FREE_LUN, lun

;first show % of irri over total, crop and pasture
indecocp = WHERE((eco_crop_pasture EQ 1) OR (eco_crop_pasture EQ 2), counteco)
indecoirri = WHERE((eco_irri EQ 1) , countecoirri)
PRINT, '% Fraction of irr over crop+pasture = ' + STRTRIM(countecoirri/FLOAT(counteco)*100,2)
 
indecoc = WHERE((eco_crop_pasture EQ 2), countecoc)
indecoirric = WHERE((eco_irri EQ 1) AND (eco_crop_pasture EQ 2), countecoirric)
PRINT, '% Fraction of irr over crop = ' + STRTRIM(countecoirric/FLOAT(countecoc)*100,2)

indecop = WHERE((eco_crop_pasture EQ 1), countecop)
indecoirrip = WHERE((eco_irri EQ 1) AND (eco_crop_pasture EQ 1), countecoirrip)
PRINT, '% Fraction of irr over pasture = ' + STRTRIM(countecoirrip/FLOAT(countecop)*100,2)



;second compute the R2 and fract as standard
indf = WHERE((FINITE(p)) AND ((eco_crop_pasture EQ 1) OR (eco_crop_pasture EQ 2)), countf) 
inds = WHERE(p[indf] LE 0.05, counts)
PRINT, 'Fraction of sig = ' + STRTRIM(counts/FLOAT(countf)*100.0,2) 
PRINT, 'AvG R2 = ' + STRTRIM(MEAN(cod[indf]),2)
PRINT, 'AvG R2 of sig = ' + STRTRIM(MEAN(cod[indf[inds]]),2)
indfc = WHERE((FINITE(p)) AND (eco_crop_pasture EQ 2), countfc)
PRINT, 'AvG R2 of crops= ' + STRTRIM(MEAN(cod[indfc]),2)

;third recompute the above excluding irri from MARS JRC mask (note that is not on GLC2000)
indf_non_irri = WHERE((FINITE(p)) AND ((eco_crop_pasture EQ 1) OR (eco_crop_pasture EQ 2)) AND (eco_irri NE 1), countf_non_irri)
inds_non_irri = WHERE(p[indf_non_irri] LE 0.05, counts_non_irri)
PRINT, '** Same stats excluding irrigated pixels from Christelle mask:'
PRINT, 'Fraction of sig = ' + STRTRIM(counts_non_irri/FLOAT(countf_non_irri)*100.0,2)
PRINT, 'AvG R2 = ' + STRTRIM(MEAN(cod[indf_non_irri]),2)
PRINT, 'AvG R2 of sig = ' + STRTRIM(MEAN(cod[indf_non_irri[inds_non_irri]]),2)
indf_non_irric = WHERE((FINITE(p)) AND (eco_crop_pasture EQ 2) AND (eco_irri NE 1), countf_non_irri)
PRINT, 'AvG R2 of crops= ' + STRTRIM(MEAN(cod[indf_non_irric]),2)
END