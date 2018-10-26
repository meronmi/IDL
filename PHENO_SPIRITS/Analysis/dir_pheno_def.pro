PRO dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion, devDir
  run = 'boku_v1_2018_03';'boku_v2';'vgt_preMaxGapOn' ;'vgt_preMaxGapOff'
  ;after 2017, the bats files can be launched ONLY from the dev directory
  devDir = 'X:\dev'
  CASE run OF
    'boku_v1_2018_03': BEGIN
      runVersion = '1_2018';'2'
      prefix = 'OF_LTA'
      suffix = '""'
      dateformat = '5'
      dir_lta = '\\ies\d5\asap\boku\V1\pheno\LTA2002-2016';'Y:\remote_sensing\boku\Test Michele\LTA\OF'
      dir_PHENOdef = '\\ies\d5\asap\boku\V1\pheno'
      ;New thresholds 0.25-0.35
      phenoDefFullPath = {normal:  dir_PHENOdef + '\' + 'pheno_025035_boku_v1_2018_SOS', $
        sen:  dir_PHENOdef + '\' +                      'pheno_025035_boku_v1_2018_SEN', $
        max:  dir_PHENOdef + '\' +                      'pheno_025035_boku_v1_2018_MAX', $    ;note that max is the same of sos
        range:  dir_PHENOdef + '\' +                    'pheno_025035_boku_v1_2018_RNG'}
      ;                                                pheno_SOS025range_boku_v1_2018
      base_dir_pheno_out =  dir_PHENOdef + '\' + 'PhenoV' + runVersion
      phenoDirOut = {normal:  base_dir_pheno_out + '\' + 'pheno_SOS_eos', $
        dir_out: base_dir_pheno_out, $
        sen:  base_dir_pheno_out + '\' + 'pheno_SOS_sen', $
        max:  base_dir_pheno_out + '\' + 'pheno_SOS_max', $
        range:  base_dir_pheno_out + '\' + 'pheno_SOS_range'}
      merged_fixed_dir = base_dir_pheno_out + '\merged_fixed'
      target_area_fn = '\\ies\d5\asap\boku\V1\pheno\suppression_second_season\Gauls_where_to_suppress_boku_v1_2018.img';'Y:\remote_sensing\boku\Pheno_17-01-2017\raster for second season\Gauls_where_to_suppress_bk.img'
      cm_fn = '\\ies\d5\asap\boku\V1\pheno\cropAFI\mask_crop_afi_v2.2.img';'Y:\remote_sensing\boku\ref\crop_mask\mask_crop_afi_bk.img'

    END
    'boku_v2': BEGIN
      runVersion = '2'
      prefix = 'OF_LTA'
      suffix = '""'
      dateformat = '5'
      dir_PHENOdef = 'Y:\remote_sensing\boku\Pheno_17-01-2017'
      dir_lta = 'Y:\remote_sensing\boku\Test Michele\LTA\OF'
      ;New thresholds 0.25-0.35
      phenoDefFullPath = {normal:  dir_PHENOdef + '\' + 'pheno_SOS025_bk_v2', $
        sen:  dir_PHENOdef + '\' + 'pheno_SOS025_sen_bk_v2', $
        max:  dir_PHENOdef + '\' + 'pheno_SOS025max03_bk_v2', $
        range:  dir_PHENOdef + '\' + 'pheno_SOS025range018_bk_v2'}
      base_dir_pheno_out =  dir_PHENOdef + '\' + 'PhenoV' + runVersion
      phenoDirOut = {normal:  base_dir_pheno_out + '\' + 'pheno_SOS025', $
        dir_out: base_dir_pheno_out, $
        sen:  base_dir_pheno_out + '\' + 'pheno_SOS025_sen', $
        max:  base_dir_pheno_out + '\' + 'pheno_SOS025max03', $
        range:  base_dir_pheno_out + '\' + 'pheno_SOS025range018'}
      merged_fixed_dir = base_dir_pheno_out + '\merged_fixed'
      target_area_fn = 'Y:\remote_sensing\boku\Pheno_17-01-2017\raster for second season\Gauls_where_to_suppress_bk.img'
      cm_fn = 'Y:\remote_sensing\boku\ref\crop_mask\mask_crop_afi_bk.img'
    END
    'boku': BEGIN
      runVersion = '1'
      prefix = 'OF_LTA'
      suffix = '""'
      dateformat = '5'
      dir_PHENOdef = 'Y:\remote_sensing\boku\Pheno_17-01-2017'
      dir_lta = 'Y:\remote_sensing\boku\Test Michele\LTA\OF'
      ;New thresholds 0.25-0.35
      phenoDefFullPath = {normal:  dir_PHENOdef + '\' + 'pheno_SOS025_bk', $
        sen:  dir_PHENOdef + '\' + 'pheno_SOS025_sen_bk', $
        max:  dir_PHENOdef + '\' + 'pheno_SOS025max03_bk', $
        range:  dir_PHENOdef + '\' + 'pheno_SOS025range018_bk'}
      base_dir_pheno_out =  dir_PHENOdef + '\' + 'PhenoV' + runVersion
      phenoDirOut = {normal:  base_dir_pheno_out + '\' + 'pheno_SOS025', $
        sen:  base_dir_pheno_out + '\' + 'pheno_SOS025_sen', $
        max:  base_dir_pheno_out + '\' + 'pheno_SOS025max03', $
        range:  base_dir_pheno_out + '\' + 'pheno_SOS025range018'}
      merged_fixed_dir = base_dir_pheno_out + '\merged_fixed'
      target_area_fn = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\raster for second season\Gauls_where_to_suppress.img'
      cm_fn = 'Y:\remote_sensing\vgt\ref\crop_mask\mask_crop_afi_vgt.img'
    END
    'vgt_preMaxGapOn': BEGIN
      prefix = 'vt'
      suffix = 'i_s'
      dateformat = '1'
      dir_PHENOdef = 'Y:\remote_sensing\vgt\2017-01-12\Pheno_on_smoothed'
      dir_lta = 'Y:\remote_sensing\vgt\2017-01-12\lta_on_smoothed'
      ;New thresholds 0.25-0.35
      phenoDefFullPath = {normal:  dir_PHENOdef + '\' + 'pheno_SOS025', $
        sen:  dir_PHENOdef + '\' + 'pheno_SOS025_sen', $
        max:  dir_PHENOdef + '\' + 'pheno_SOS025max03', $
        range:  dir_PHENOdef + '\' + 'pheno_SOS025range018'}
      base_dir_pheno_out =  dir_PHENOdef + '\' + 'PhenoV1'
      phenoDirOut = {normal:  base_dir_pheno_out + '\' + 'pheno_SOS025', $
        sen:  base_dir_pheno_out + '\' + 'pheno_SOS025_sen', $
        max:  base_dir_pheno_out + '\' + 'pheno_SOS025max03', $
        range:  base_dir_pheno_out + '\' + 'pheno_SOS025range018'}
      merged_fixed_dir = base_dir_pheno_out + '\merged_fixed'
      target_area_fn = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\raster for second season\Gauls_where_to_suppress.img'
      cm_fn = 'Y:\remote_sensing\vgt\ref\crop_mask\mask_crop_afi_vgt.img'
    END
    'vgt_preMaxGapOff': BEGIN
      prefix = 'vt'
      suffix = 'i_s'
      dateformat = '1'
      dir_PHENOdef = 'Y:\remote_sensing\vgt\Pheno_Oct_2016'
      dir_lta = 'Y:\remote_sensing\vgt\NDVI\S10\lta'
      phenoDefFullPath = {normal:  dir_PHENOdef + '\' + 'pheno_SOS025', $
        sen:  dir_PHENOdef + '\' + 'pheno_SOS025_sen', $
        max:  dir_PHENOdef + '\' + 'pheno_SOS025max03', $
        range:  dir_PHENOdef + '\' + 'pheno_SOS025range018'}
      base_dir_pheno_out =  dir_PHENOdef + '\' + 'PhenoV2'
      phenoDirOut = {normal:  base_dir_pheno_out + '\' + 'pheno_SOS025', $
        sen:  base_dir_pheno_out + '\' + 'pheno_SOS025_sen', $
        max:  base_dir_pheno_out + '\' + 'pheno_SOS025max03', $
        range:  base_dir_pheno_out + '\' + 'pheno_SOS025range018'}
      merged_fixed_dir = base_dir_pheno_out + '\merged_fixed'
      target_area_fn = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\raster for second season\Gauls_where_to_suppress.img'
      cm_fn = 'Y:\remote_sensing\vgt\ref\crop_mask\mask_crop_afi_vgt.img'
    END
  ENDCASE

END