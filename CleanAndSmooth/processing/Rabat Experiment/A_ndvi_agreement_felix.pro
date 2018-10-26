PRO A_ndvi_agreement_felix
; compute and classify the difference in anomalies between PV and VGT, VGT is the "truth"
;Classification of anomaly agreement by categories (assuming VGT as the truth):
;
;- Perfect agreement: % of PV pixels having the same category of VGT
;- Minor mismatch: % of PV pixels where both sensors have the same sign of the anomaly (“increase” or “decrease”) but different magnitude (“small” vs. “large” and the other way around)
;- Mismatch: % of PV pixels where one sensor indicates “no change” and the other indicates “small” change (can be either “increase” or “decrease”)
;- Serious mismatch: % of PV pixels where sensor indicates “no change” and the other indicates “large” change  (can be either “increase” or “decrease”
;- Unacceptable mismatch: % of PV pixels where sensors indicates anomaly with opposite sign (“increase” vs. “decrease”, no matter magnitude)
;- Missing: % of PV pixels that doesn’t have a valid observation when VGT is valid (expressed in % of total number of valid VGT observations)

 

base_dir = 'S:\Actions\FOODSEC\base_data\remote_sensing\ROI IGA per Felix'
;s1 = 'ot'
;s1_dir = base_dir + '\METOP\ACT\S10s2'
;s2 = 'ea'
;s2_dir = base_dir + '\EMDODIS\ACT\Resample1km'
s1 = 'ea'
s1_dir = base_dir + '\EMDODIS\ACT\Resample1km'
s2 = 'pv'
s2_dir = base_dir + '\VGT-PV\ACT\S10s2\METOP_WINDOW'

fn_gaul0_image = 'S:\Actions\FOODSEC\base_data\remote_sensing\ROI IGA per Felix\HoAcountries_mask.img'
fn_CropMask_image = 'S:\Actions\FOODSEC\base_data\remote_sensing\ROI IGA per Felix\HoAvegetation_mask.img'
ns = 3809
nl = 3809
n_files = 36
maxval = 250
offset = -0.08; -1.25
gain = 0.004
offset_emodis = -1; -1.25
gain_emodis = 0.01
s1_read = BYTARR(ns,nl)
s2_read = BYTARR(ns,nl)
gaul0 = INTARR(ns,nl)
PRINT, 'Care, mask read as INT'
cropMask =  INTARR(ns,nl)

stats = FLTARR(9, n_files) 
deks = STRTRIM(INDGEN(n_files)+1, 2)
FOR i = 0, n_files - 1 DO IF (STRLEN(deks[i]) EQ 1) THEN deks[i] = '0'+deks[i] 
base_names = '14' + deks + 'is.img'
strStats = ['Dekad', 'Perf   ', 'Minor_Mis', 'Mismatch', 'serious_Mis', 'Unaccpet_Mis', 'Missing', 'AC', 'dfAC', 'R2']
PRINT, strStats

;pixel to be considered (crop mask = 1 and one of the three countries, algeria, marocco, tunisia = 164, 4, 248
OPENR, LUN, fn_gaul0_image, /GET_LUN
READU, LUN, gaul0
FREE_LUN, LUN

IF (fn_CropMask_image EQ 'none') THEN BEGIN
  cropMask[*,*] = 1
ENDIF ELSE BEGIN
  OPENR, LUN, fn_CropMask_image, /GET_LUN
  READU, LUN, cropMask
  ind = WHERE((cropMask EQ 1) OR (cropMask EQ 2))
  cropMask[ind] = 1
  FREE_LUN, LUN
ENDELSE

roi_ind = WHERE((cropMask EQ 1) AND (gaul0 NE 0), count)

PRINT, 'Number of pixels considered: ' + STRTRIM(count,2)
all_s1 = !NULL
all_s2 = !NULL
;perform pairwise comparison
FOR i = 0, n_files - 1 DO BEGIN
  ;OPEN and SCALE the two
  OPENR, LUN, s1_dir + '\' +s1 + base_names[i], /GET_LUN
  READU, LUN, s1_read
  FREE_LUN, LUN
  s1_float = FLOAT(s1_read)
  
  OPENR, LUN, s2_dir + '\' + s2 + base_names[i], /GET_LUN
  READU, LUN, s2_read
  FREE_LUN, LUN
  s2_float = FLOAT(s2_read)
  
  ;scale 
  ind=where(s1_float gt maxval, count)
  if (count ne 0) then s1_float[ind]=!VALUES.F_NAN
  IF (s1 EQ 'ea') THEN BEGIN
    s1_float = FLOAT(TEMPORARY(s1_float)) * FLOAT(gain_emodis) + FLOAT(offset_emodis)
  ENDIF ELSE BEGIN
    s1_float = FLOAT(TEMPORARY(s1_float)) * FLOAT(gain) + FLOAT(offset)
  ENDELSE
  
  ind=where(s2_float gt maxval, count)
  if (count ne 0) then s2_float[ind]=!VALUES.F_NAN
  IF (s2 EQ 'ea') THEN BEGIN
    s2_float = FLOAT(TEMPORARY(s2_float)) * FLOAT(gain_emodis) + FLOAT(offset_emodis)
  ENDIF ELSE BEGIN
    s2_float = FLOAT(TEMPORARY(s2_float)) * FLOAT(gain) + FLOAT(offset)
  ENDELSE
  ;consider only where crop mask and country
  s1_float = s1_float[roi_ind]
  s2_float = s2_float[roi_ind]
;  all_s1 = [all_s1, s1_float]
;  all_s2 = [all_s2, s2_float] 
  indFinBoth = WHERE(FINITE(s1_float) AND FINITE(s2_float))
  all_s1 = [all_s1, s1_float[indFinBoth]]
  all_s2 = [all_s2, s2_float[indFinBoth]]
  ;compute GMRF AC
  ;IF (i EQ 20) THEN STOP
  res = gmrf(s1_float[indFinBoth], s2_float[indFinBoth])
  stats[6,i] = res[2]
  stats[7,i] = res[8]
  stats[8,i] = (CORRELATE(s1_float[indFinBoth], s2_float[indFinBoth]))^2 
  ;scatter_plotDataXY, s1_float[indFinBoth], s2_float[indFinBoth], 'nd_dek_'+deks[i]+'_'+s1, s2, base_dir, [0.0,1.0], [0.0,1.0] 
  
  PRINT, STRTRIM(deks[i],2), STRTRIM(stats[*,i],2)
;  PRINT, 'here'
ENDFOR

;plot evolution of AC
hh = PLOT(INDGEN(n_files)+1, REFORM(stats[6,*]), XTITLE='Dekads of 2014', YTITLE = 'AC ('+ s1 +' Vs. '+ s2+')', XRANGE=[0,(n_files)+1], YRANGE=[0,1], NAME = 'AC')
hh2 = PLOT(INDGEN(n_files)+1, REFORM(stats[7,*]), OVERPLOT = 1, COLOR = 'red', NAME = 'dfAC')
hh3 = PLOT(INDGEN(n_files)+1, REFORM(stats[8,*]), OVERPLOT = 1, COLOR = 'green', NAME = 'R2')
hl = LEGEND(TARGET=[hh,hh2,hh3], POSITION=[35, 1], /DATA, LINESTYLE='none', SHADOW=0, TRANSPARENCY=100) 
hh.save, base_dir + '\NDtimeAC_' + s1 + 'VS' + s2 + '.png'
;plot anomalies scatter plot
xr = [-0.6,0.6]
yr = xr
y = all_s2[WHERE(FINITE(all_s1) AND FINITE(all_s2))]
x = all_s1[WHERE(FINITE(all_s1) AND FINITE(all_s2))]
;ind_tot_opposite_sign = WHERE(((x GT 0) AND (y LT 0)) OR ((x LT 0) AND (y GT 0)), count_tot_opposite_sign)
;PRINT, '% of pixel with opposite sign (% of total number of points) ', count_tot_opposite_sign/FLOAT(N_ELEMENTS(x))*100
;ind_plus_minus = WHERE(((x GT 0) AND (y LT 0)), count_plus_minus)
;PRINT, '% of pixel where X is + and Y is – (% of pixels having opposite sign) ', count_plus_minus/FLOAT(count_tot_opposite_sign)*100
;ind_minus_plus = WHERE(((x LT 0) AND (y GT 0)), count_minus_plus)
;PRINT, '% of pixel where X is - and Y is + (% of pixels having opposite sign) ', count_minus_plus/FLOAT(count_tot_opposite_sign)*100
scatter_plotDataXY, x, y, 'nd_'+s1, 'nd_'+s2, base_dir, [0.0,1.0], [0.0,1.0] 
;h0 = PLOT(x, y, LINESTYLE = 'none', SYMBOL='o', XRANGE=xr, YRANGE=yr, $ 
;          XTITLE='s2 abs diff anomaly', YTITLE='s1 abs diff anomaly', DIMENSIONS=[800,800], SYM_SIZE=0.5, SYM_TRANSPARENCY=99, SYM_FILLED=1, SYM_THICK=0, FONT_SIZE=20)
;h1 = PLOT(xr, yr, COLOR='blue', OVERPLOT = 1, LINESTYLE=0)
;
;slope = REGRESS(x, y, CONST=offset, CORRELATION=r)
;h2 = PLOT(xr, offset+yr*slope[0], COLOR='red', OVERPLOT = 1)
;h3 = TEXT(-0.55,0.45, 'y = '+STRTRIM(slope[0],2)+' x + '+STRTRIM(offset,2) , /DATA, COLOR='red', FONT_SIZE=20)
;h4 = TEXT(-0.55,0.38, '$R^2$ = ' + STRTRIM(r^2,2) , /DATA, COLOR='red', FONT_SIZE=20)
PRINT, 'Mean bias = ' + STRTRIM(MEAN(y-x),2)
;WRITE_CSV, base_dir + '\' + s1 + 'VS' + s2 + '_paired_anomal.csv', x, y, HEADER=['s1', 's2']
stats2 = FLTARR(10, n_files)
stats2[1:*,*] = stats
stats2[0,*] = deks
WRITE_CSV, base_dir + '\' + 'ND_' + s1 + 'VS' + s2 + '_agreement_stats.csv', stats2, HEADER=strStats
PRINT, 'Done.'
END