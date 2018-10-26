PRO D_suppress_pheno_problem
dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion
dir = phenoDirOut.dir_out + '\Filtered_seasonality'
;dir = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\merged_fixed'
check = ReadEnviWithHdr(dir + '\pheno_check.img')
;keep the last band only (any problem)
check = check[*,*,8]
ind = WHERE(check EQ 1, count)
PRINT, 'Number of pix with at least one problem (they will be removed):'
PRINT, count
IF (count GT 0) THEN BEGIN
  fn_list = FILE_SEARCH(dir, prefix + '*.img')
  FOR i = 0, N_ELEMENTS(fn_list)-1 DO BEGIN
    ;suppress the pixels
    tmp = ReadEnviWithHdr(fn_list[i])
    tmp[ind] = 252
    res = write_envi_img(tmp, fn_list[i])
  ENDFOR
ENDIF
END