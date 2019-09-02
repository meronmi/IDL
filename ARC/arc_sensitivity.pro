PRO ARC_sensitivity
lta_dir = '\\ies\d5\asap\boku\V1\pheno\LTA2002-2016'
base_dir = '\\ies\d5\asap\boku\V1\pheno\ARC_sensitivity'
;for run moreEOS
collection_dir = '\\ies\d5\asap\boku\V1\pheno\ARC_sensitivity\Pheno_map_4_sensitivity_moreEOS'
FILE_MKDIR, collection_dir
;original run
;collection_dir = '\\ies\d5\asap\boku\V1\pheno\ARC_sensitivity\Pheno_map_4_sensitivity'
;compute pheno fo all sos and eos combinatiojn
;avevamo pensato a 7 livelli per SOS e 7 per EOS partenedo da 20 fino a 50 con tintervallo di 5. 20, 25, 30, 35, 40, 45, 50
sos_thresholds = (FINDGEN(7) * 5 + 20.0)*0.01; STRING(, FORMAT='(F6.2)')
sos_thresholds = [0.25];, 0.5]; STRING(, FORMAT='(F6.2)')
;for run moreEOS
eos_thresholds =  [0.6,0.7,0.8,0.9,1.0]
;original run
;eos_thresholds =  (FINDGEN(7) * 5 + 20.0)*0.01
FOR s = 0, N_ELEMENTS(sos_thresholds)-1 DO BEGIN
  FOR e = 0, N_ELEMENTS(eos_thresholds)-1 DO BEGIN
    ;make it
    ret = write_pheno_spf_file(base_dir, sos_thresholds[s], eos_thresholds[e], 0.75)
    fn_base_name = 'S_' + STRTRIM(ROUND(sos_thresholds[s] * 100),2) + 'E_' + STRTRIM(ROUND(eos_thresholds[e] * 100),2)
    PRINT, '******************************************************************************'
    PRINT, fn_base_name
    PRINT, '******************************************************************************'
    fns = [fn_base_name + '_for_SOS_EOS', fn_base_name + '_for_SEN', fn_base_name + '_for_MAX', fn_base_name + '_for_RNG']
    ;make the big structure
    all_in_a_STRUCTURE = CREATE_STRUCT($
      'runVersion', fn_base_name, $
      'prefix', 'OF_LTA', $ ;the prefix is for lta
      'suffix', '""', $
      'dateformat', '5', $
      'dir_lta', '\\ies\d5\asap\boku\V1\pheno\LTA2002-2016', $ 
      'dir_PHENOdef', base_dir, $
      'phenoDefFullPath', {normal: base_dir + '\' + fns[0], $
                           sen:    base_dir + '\' + fns[1], $
                           max:    base_dir + '\' + fns[2], $    ;note that max is the same of sos
                           range:  base_dir + '\' + fns[3]}, $
      'base_dir_pheno_out', base_dir + '\' + 'Pheno_' + fn_base_name, $
      'phenoDirOut', {dir_out: base_dir + '\' + 'Pheno_' + fn_base_name, $
                      normal:  base_dir + '\' + 'Pheno_' + fn_base_name + '\' + 'pheno_SOS_eos', $ 
                      sen:     base_dir + '\' + 'Pheno_' + fn_base_name + '\' + 'pheno_SOS_sen', $
                      max:     base_dir + '\' + 'Pheno_' + fn_base_name + '\' + 'pheno_SOS_max', $
                      range:   base_dir + '\' + 'Pheno_' + fn_base_name + '\' + 'pheno_SOS_range'}, $
      'merged_fixed_dir', base_dir + '\' + 'Pheno_' + fn_base_name + '\merged_fixed', $
      'target_area_fn', '\\ies\d5\asap\boku\V1\pheno\suppression_second_season\Gauls_where_to_suppress_boku_v1_2018.img', $
      'cm_fn', '\\ies\d5\asap\boku\V1\pheno\cropAFI\mask_crop_afi_v2.2.img', $
      'devDir', 'C:\dev' $
      )
    ;send it to A_run_pheno that makes the bat
    A_run_pheno, all_in_a_STRUCTURE = all_in_a_STRUCTURE
    B_harvest_pheno_and_correct, all_in_a_STRUCTURE = all_in_a_STRUCTURE   
    Bbis_suppress_2nd_seas, all_in_a_STRUCTURE = all_in_a_STRUCTURE
    C_check_pheno, all_in_a_STRUCTURE = all_in_a_STRUCTURE
    ;do some file delete
    ;do some file delete
    FILE_DELETE, all_in_a_STRUCTURE.phenoDirOut.normal, /RECURSIVE
    FILE_DELETE, all_in_a_STRUCTURE.phenoDirOut.sen, /RECURSIVE
    FILE_DELETE, all_in_a_STRUCTURE.phenoDirOut.max, /RECURSIVE
    FILE_DELETE, all_in_a_STRUCTURE.phenoDirOut.range, /RECURSIVE
    FILE_DELETE, all_in_a_STRUCTURE.merged_fixed_dir, /RECURSIVE
    ;zip copy relevant file for arc, FILE_GZIP, File [, FileOut]
    fn_list = FILE_SEARCH(all_in_a_STRUCTURE.phenoDirOut.dir_out, '*.img')
    FILE_GZIP, fn_list, /DELETE
    
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'s1.img.gz', collection_dir + '\' + fn_base_name + 's1.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'s2.img.gz', collection_dir + '\' + fn_base_name + 's2.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'e1.img.gz', collection_dir + '\' + fn_base_name + 'e1.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'e2.img.gz', collection_dir + '\' + fn_base_name + 'e2.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'m1.img.gz', collection_dir + '\' + fn_base_name + 'm1.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'m2.img.gz', collection_dir + '\' + fn_base_name + 'm2.img.gz'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'s1.hdr', collection_dir + '\' + fn_base_name + 's1.hdr'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'s2.hdr', collection_dir + '\' + fn_base_name + 's2.hdr'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'e1.hdr', collection_dir + '\' + fn_base_name + 'e1.hdr'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'e2.hdr', collection_dir + '\' + fn_base_name + 'e2.hdr'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'m1.hdr', collection_dir + '\' + fn_base_name + 'm1.hdr'
    FILE_COPY, all_in_a_STRUCTURE.phenoDirOut.dir_out[0] + '\Filtered_seasonality\'+all_in_a_STRUCTURE.prefix+'m2.hdr', collection_dir + '\' + fn_base_name + 'm2.hdr'
  
    ;copy hdrs
    PRINT, 'debug'
  ENDFOR
ENDFOR


END