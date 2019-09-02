FUNCTION sentinel_phenoRes_CREATE_STRUCT
na = !VALUES.F_NAN
phenoRes = CREATE_STRUCT($
  'Index', '', $ 
  'CROP_TYPE', '',$ 
  'ID', 0L, $
  'NUTS0', '', $
  'Lat', na, $
  'Lon', na, $
  'nImages_input', 0, $
  'mean_cv_varrat_inSeason', [na, na], $ ;here I store CV for S2 and the ration bewteen actual and therotical variance for s1
  'prctHeterogObs_inSeason', [na, na], $ here is % of time when the above exceeds a threshold 
  'prctSnow_inSeason', [na, na], $ here is % of time when snow was cover was above threshold
  'SD_in_season', [na, na], $ ;can be NDVI or VH
  'SOS20', [na, na], $
  'EOS20', [na, na], $
  'LGS20', [na, na], $
  'SOS50', [na, na], $
  'EOS50', [na, na], $
  'LGS50', [na, na], $
  'PS90', [na, na], $
  'maxFit', [na, na], $
  'Slope_growth', [na, na], $
  'Slope_decay', [na, na], $
  'r_fit', [na, na], $
  'RMSD_fit', [na, na], $
  'Pval_fit', [na, na], $
  'SOSunrel', [na, na], $
  'EOSunrel', [na, na], $
  'SOSfractUnrel', [na, na], $
  'EOSfractUnrel', [na, na], $
  'nImages_fit', [0, 0], $
  'maxGap', [0, 0], $
  'p0', [na, na], 'p1', [na, na], 'p2', [na, na], 'p3', [na, na], 'p4', [na, na], 'p5', [na, na], 'p6', [na, na],  $
  'p0_err', [na, na], 'p1_err', [na, na], 'p2_err', [na, na], 'p3_err', [na, na], 'p4_err', [na, na], 'p5_err', [na, na], 'p6_err', [na, na],  $
  'LocateSeason_code', [0, 0], $
  'LocateSeason_msg', ['', ''], $
  'PhenoFit_msg', ['', ''])
RETURN, phenoRes
END