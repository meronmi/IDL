PRO rainfall_stats

  ;all at CHIRPS res
  ns = 1402
  nl = 262
  eco_crop_pasture = BYTARR(ns, nl)
  avgRFE = FLTARR(ns, nl)
  sdRFE  = FLTARR(ns, nl)

  ;map of crop-rangeland
  ;CHIRPS RESOLUTION
  mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
  fn_intersection = 'eco_crop_pasture_res_CHIRPS.img' ;pasrure = 1, crop = 2
  OPENR, lun, mask_dir + '\' + fn_intersection, /GET_LUN
  READU, lun, eco_crop_pasture
  FREE_LUN, lun
  ;map of mean rain
  rfe_dir = 'E:\WA\EWP\CHIRPS ROI WA\mean_yearly_rain\LTA'
  fn_avg = 'chrps_ys_lta1962wa.img'
  OPENR, lun, rfe_dir + '\' + fn_avg, /GET_LUN
  READU, lun, avgRFE
  FREE_LUN, lun
  
  ;map of SD
  fn = 'chrps_ys_lta1963wa.img'
  OPENR, lun, rfe_dir + '\' + fn, /GET_LUN
  READU, lun, sdRFE
  FREE_LUN, lun
  

  
  ;crops
  indecoc = WHERE((eco_crop_pasture EQ 2), countecoc)
  
  PRINT, 'Spatial Mean RFE crop ' + STRTRIM(MEAN(FLOAT(avgRFE[indecoc])),2)
  PRINT, 'Spatial SD RFE crop ' + STRTRIM(STDDEV(FLOAT(avgRFE[indecoc])),2)
  PRINT, 'Mean SD RFE crop ' + STRTRIM(MEAN(FLOAT(sdRFE[indecoc])),2)
  ;pastures
  indecop = WHERE((eco_crop_pasture EQ 1), countecop)
  PRINT, 'Spatial Mean RFE rangeland ' + STRTRIM(MEAN(FLOAT(avgRFE[indecop])),2)
  PRINT, 'Spatial SD RFE rangeland ' + STRTRIM(STDDEV(FLOAT(avgRFE[indecop])),2)
  PRINT, 'Mean SD RFE rangeland ' + STRTRIM(MEAN(FLOAT(sdRFE[indecop])),2)
  PRINT, 'Finito'
END