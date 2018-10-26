PRO C_anomaly_agreement_rabat
; compute and classify the difference in anomalies between PV and VGT, VGT is the "truth"
;Classification of anomaly agreement by categories (assuming VGT as the truth):
;
;- Perfect agreement: % of PV pixels having the same category of VGT
;- Minor mismatch: % of PV pixels where both sensors have the same sign of the anomaly (“increase” or “decrease”) but different magnitude (“small” vs. “large” and the other way around)
;- Mismatch: % of PV pixels where one sensor indicates “no change” and the other indicates “small” change (can be either “increase” or “decrease”)
;- Serious mismatch: % of PV pixels where sensor indicates “no change” and the other indicates “large” change  (can be either “increase” or “decrease”
;- Unacceptable mismatch: % of PV pixels where sensors indicates anomaly with opposite sign (“increase” vs. “decrease”, no matter magnitude)
;- Missing: % of PV pixels that doesn’t have a valid observation when VGT is valid (expressed in % of total number of valid VGT observations)


base_dir = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\NDVI\ENVI FORMAT\ROI North Arcica Reduced for Anomalies\ABS_DIF_ANOMALIES_OVERLAP'
pv_dir = base_dir + '\PV'
vt_dir = base_dir + '\VGT'
fn_gaul0_image = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\NDVI\ENVI FORMAT\ROI North Arcica Reduced for Anomalies\ABS_DIF_ANOMALIES_OVERLAP\CROP-MASK\gaul0.img'
fn_CropMask_image = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\NDVI\ENVI FORMAT\ROI North Arcica Reduced for Anomalies\ABS_DIF_ANOMALIES_OVERLAP\CROP-MASK\CM_4_anomalies.img'
ns = 2689
nl = 1009
maxval = 250
offset = -1.25
gain = 0.01
pv_read = BYTARR(ns,nl)
vt_read = BYTARR(ns,nl)
gaul0 = INTARR(ns,nl)
cropMask =  BYTARR(ns,nl)
n_files = 15
stats = FLTARR(6, n_files) 
deks = STRTRIM(INDGEN(15)+1, 2)
FOR i = 0, n_files - 1 DO IF (STRLEN(deks[i]) EQ 1) THEN deks[i] = '0'+deks[i] 
base_names = '14' + deks + 'i0.img'
PRINT, ['Dekad', 'Perf   ', 'Minor_Mis', 'Mismatch', 'serious_Mis', 'Unaccpet_Mis', 'Missing']

;pixel to be considered (crop mask = 1 and one of the three countries, algeria, marocco, tunisia = 164, 4, 248
OPENR, LUN, fn_gaul0_image, /GET_LUN
READU, LUN, gaul0
FREE_LUN, LUN
OPENR, LUN, fn_CropMask_image, /GET_LUN
READU, LUN, cropMask
FREE_LUN, LUN
roi_ind = WHERE((cropMask EQ 1) AND ((gaul0 EQ 169) OR (gaul0 EQ 4) OR (gaul0 EQ 248)), count)
PRINT, 'Number of pixels considered: ' + STRTRIM(count,2)
all_pv = !NULL
all_vgt = !NULL
;perform pairwise comparison
FOR i = 0, n_files - 1 DO BEGIN
  ;OPEN and SCALE the two
  OPENR, LUN, pv_dir + '\pv' + base_names[i], /GET_LUN
  READU, LUN, pv_read
  FREE_LUN, LUN
  pv_float = FLOAT(pv_read)
  
  OPENR, LUN, vt_dir + '\vt' + base_names[i], /GET_LUN
  READU, LUN, vt_read
  FREE_LUN, LUN
  vt_float = FLOAT(vt_read)
  
  ;scale 
  ind=where(pv_float gt maxval, count)
  if (count ne 0) then pv_float[ind]=!VALUES.F_NAN
  pv_float = FLOAT(TEMPORARY(pv_float)) * FLOAT(gain) + FLOAT(offset)
  
  ind=where(vt_float gt maxval, count)
  if (count ne 0) then vt_float[ind]=!VALUES.F_NAN
  vt_float = FLOAT(TEMPORARY(vt_float)) * FLOAT(gain) + FLOAT(offset)
  
  ;consider only where crop mask and country
  pv_float = pv_float[roi_ind]
  vt_float = vt_float[roi_ind]
  all_pv = [all_pv, pv_float]
  all_vgt = [all_vgt, vt_float] 

  ; classify
  ;classes 
  ; 0: < -.125          ;large decrease
  ; 1: -0.125 : -0.05   ;small decrease
  ; 2: -0.05 : 0.05     ;no cahnge
  ; 3: 0.05 : 0.125     ;small increase
  ; 4: >0.125           ;large incerase
  ; 9: missing
  tmp = pv_float
  class = BYTARR(ns,nl) * 0 + 9     ;set all to missing
  indFin = WHERE(FINITE(tmp))
  ind0 = WHERE(tmp[indFin] LT -0.125, count0) 
  IF (count0 GT 0) THEN class[indFin[ind0]] = 0
  ind1 = WHERE((tmp[indFin] GE -0.125) AND (tmp[indFin] LT -0.05), count1)
  IF (count1 GT 0) THEN class[indFin[ind1]] = 1
  ind2 = WHERE((tmp[indFin] GE -0.05) AND (tmp[indFin] LT 0.05), count2)
  IF (count2 GT 0) THEN class[indFin[ind2]] = 2
  ind3 = WHERE((tmp[indFin] GE 0.05) AND (tmp[indFin] LT 0.125), count3)
  IF (count3 GT 0) THEN class[indFin[ind3]] = 3
  ind4 = WHERE(tmp[indFin] GE 0.125, count4)
  IF (count4 GT 0) THEN class[indFin[ind4]] = 4
  pv_class = class
  
  tmp = vt_float
  class = BYTARR(ns,nl) * 0 + 9     ;set all to missing
  indFin = WHERE(FINITE(tmp))
  ind0 = WHERE(tmp[indFin] LT -0.125, count0)
  IF (count0 GT 0) THEN class[indFin[ind0]] = 0
  ind1 = WHERE((tmp[indFin] GE -0.125) AND (tmp[indFin] LT -0.05), count1)
  IF (count1 GT 0) THEN class[indFin[ind1]] = 1
  ind2 = WHERE((tmp[indFin] GE -0.05) AND (tmp[indFin] LT 0.05), count2)
  IF (count2 GT 0) THEN class[indFin[ind2]] = 2
  ind3 = WHERE((tmp[indFin] GE 0.05) AND (tmp[indFin] LT 0.125), count2)
  IF (count3 GT 0) THEN class[indFin[ind3]] = 3
  ind4 = WHERE(tmp[indFin] GE 0.125, count4)
  IF (count4 GT 0) THEN class[indFin[ind4]] = 4
  vt_class = class
  
  indFinVt = WHERE(vt_class NE 9, count_fin_vt)
  indFinpv = WHERE(pv_class NE 9, count_fin_pv)
  indFinBoth = WHERE((pv_class NE 9) AND (vt_class NE 9), count_fin_both)
   
  ;- Perfect agreement (% of PV pixels having the same category of  VGT)
  ind_0 = WHERE(((vt_class - pv_class) EQ 0) AND (pv_class NE 9), count_0)
  stats[0, i] = count_0/FLOAT(count_fin_both) * 100
  ;- Mismatch (% of PV pixels having the same sign, increase or decrease)
  ind_1 = WHERE(((vt_class EQ 0) AND (pv_class EQ 1)) OR $
                ((vt_class EQ 1) AND (pv_class EQ 0)) OR $
                ((vt_class EQ 3) AND (pv_class EQ 4)) OR $
                ((vt_class EQ 4) AND (pv_class EQ 3)), count_1)
  stats[1, i] = count_1/FLOAT(count_fin_both) * 100
  ;- Mismatch (% of PV pixels where PV is no change and VGT is small change OR PV is small change and VGT is no change)
  ind_2 = WHERE(((vt_class EQ 1) AND (pv_class EQ 2)) OR $
                ((vt_class EQ 3) AND (pv_class EQ 2)) OR $
                ((vt_class EQ 2) AND (pv_class EQ 1)) OR $
                ((vt_class EQ 2) AND (pv_class EQ 3)), count_2)
  stats[2, i] = count_2/FLOAT(count_fin_both) * 100
  ;- Serious mismacth (% of PV pixels where one is no change and the other is large decrease/increase)
  ind_3 = WHERE(((vt_class EQ 2) AND (pv_class EQ 0)) OR $
                ((vt_class EQ 2) AND (pv_class EQ 4)) OR $
                ((vt_class EQ 0) AND (pv_class EQ 2)) OR $
                ((vt_class EQ 4) AND (pv_class EQ 2)), count_3)
  stats[3, i] = count_3/FLOAT(count_fin_both) * 100
  ;- Unacceptable mismatch (% of PV pixels having opposite sign)
  ind_4 = WHERE(((vt_class EQ 0) AND (pv_class EQ 3)) OR $
                ((vt_class EQ 0) AND (pv_class EQ 4)) OR $
                ((vt_class EQ 1) AND (pv_class EQ 3)) OR $
                ((vt_class EQ 1) AND (pv_class EQ 4)) OR $
                ((vt_class EQ 3) AND (pv_class EQ 0)) OR $
                ((vt_class EQ 3) AND (pv_class EQ 1)) OR $
                ((vt_class EQ 4) AND (pv_class EQ 0)) OR $
                ((vt_class EQ 4) AND (pv_class EQ 1)), count_4)
  stats[4, i] = count_4/FLOAT(count_fin_both) * 100
  ;- Missing (% of PV pixels that are nodata when VGT is valid)
  ind_5 = WHERE((pv_class EQ 9) AND (vt_class NE 9), count_5)
  stats[5, i] = count_5/FLOAT(count_fin_vt) * 100
  
  
  PRINT, STRTRIM(deks[i],2), STRTRIM(stats[*,i],2)
;  PRINT, 'here'
ENDFOR

;plot anomalies scatter plot
xr = [-0.6,0.6]
yr = xr
h0 = PLOT(all_vgt, all_pv, LINESTYLE = 'none', SYMBOL='o', XRANGE=xr, YRANGE=yr, $ 
          XTITLE='VGT abs diff anomaly', YTITLE='PV abs diff anomaly', DIMENSIONS=[800,800], SYM_SIZE=0.5, SYM_TRANSPARENCY=99, SYM_FILLED=1, SYM_THICK=0, FONT_SIZE=20)
h1 = PLOT(xr, yr, COLOR='blue', OVERPLOT = 1, LINESTYLE=0)
x = all_vgt[WHERE(FINITE(all_pv) AND FINITE(all_vgt))]
y = all_pv[WHERE(FINITE(all_pv) AND FINITE(all_vgt))]
slope = REGRESS(x, y, CONST=offset, CORRELATION=r)
h2 = PLOT(xr, offset+yr*slope[0], COLOR='red', OVERPLOT = 1)
h3 = TEXT(-0.55,0.45, 'y = '+STRTRIM(slope[0],2)+' x + '+STRTRIM(offset,2) , /DATA, COLOR='red', FONT_SIZE=20)
h4 = TEXT(-0.55,0.38, '$R^2$ = ' + STRTRIM(r^2,2) , /DATA, COLOR='red', FONT_SIZE=20)
PRINT, 'Mean bias = ' + STRTRIM(MEAN(y-x),2)
;WRITE_CSV, 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\DOCS\paired_anomal.csv', x, y, HEADER=['pv', 'vgt']
PRINT, 'Done.'
END