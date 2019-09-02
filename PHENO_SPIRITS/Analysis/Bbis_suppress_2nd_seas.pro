PRO Bbis_suppress_2nd_seas, all_in_a_STRUCTURE = all_in_a_STRUCTURE
TIC
IF KEYWORD_SET(all_in_a_STRUCTURE) EQ 1 THEN BEGIN
  ;"decompress" the big structure if it was sent here
  runVersion = all_in_a_STRUCTURE.runVersion
  prefix = all_in_a_STRUCTURE.prefix
  suffix = all_in_a_STRUCTURE.suffix
  dateformat = all_in_a_STRUCTURE.dateformat
  dir_lta = all_in_a_STRUCTURE.dir_lta
  dir_PHENOdef = all_in_a_STRUCTURE.dir_PHENOdef
  ;New thresholds 0.25-0.35
  phenoDefFullPath = all_in_a_STRUCTURE.phenoDefFullPath
  base_dir_pheno_out =  all_in_a_STRUCTURE. base_dir_pheno_out
  phenoDirOut = all_in_a_STRUCTURE.phenoDirOut
  merged_fixed_dir = all_in_a_STRUCTURE.merged_fixed_dir
  target_area_fn = all_in_a_STRUCTURE.target_area_fn
  cm_fn = all_in_a_STRUCTURE.cm_fn
  devDir = all_in_a_STRUCTURE.devDir
ENDIF ELSE BEGIN
  dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion, devDir
ENDELSE
;dir_pheno = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\merged_fixed'
dir_pheno = merged_fixed_dir
dir_out = phenoDirOut.dir_out + '\Filtered_seasonality'
out_fn = dir_out + '\10_two_w_seas-11_one_first-12_one_second'


;code taken from  X:\pheno_spirits_test_over_europe\filter_sesonality.pro
;revised and updated
;
;ns = 40320
;nl = 14673

fn_hdr = FILE_SEARCH(dir_lta+'\*.hdr')
fn_hdr = fn_hdr[0]
ns = read_info('samples', fn_hdr)
nl = read_info('lines', fn_hdr)


geo_line1 = 'map info = ' + read_info('map info', dir_pheno + '\' + prefix + 'KK0.hdr')

ngspy_fn = prefix + 'KK0.img'
tm1_fn = prefix + 'm1.img'
tm2_fn = prefix + 'm2.img'


FILE_MKDIR, dir_out

cm = ReadEnviWithHdr(cm_fn)
ta = ReadEnviWithHdr(target_area_fn) 

ngspy = ReadEnviWithHdr(dir_pheno + '\' + ngspy_fn)
;reformat ngspy 
ind1 =  WHERE((ngspy GE 100) AND (ngspy LT 200))
ind2 =  WHERE((ngspy GE 200) AND (ngspy LT 250))
ngspy = ngspy * 0B
ngspy[ind1] = 1
ngspy[ind2] = 2

tm1 = ReadEnviWithHdr(dir_pheno + '\' + tm1_fn)
tm2 = ReadEnviWithHdr(dir_pheno + '\' + tm2_fn)
tm1 = SPIRITS_from_108_to_36_ARRAY(tm1)
tm2 = SPIRITS_from_108_to_36_ARRAY(tm2)

;discard any second season occurring in winter, where there is crop, in the selected area
;focus on bimodal
indBi = WHERE(ngspy EQ 2)
ind1 = WHERE(((tm1[indBi] GE 25) AND (tm1[indBi] LE 36)) $
          OR ((tm1[indBi] GE 1) AND (tm1[indBi] LE 7)) AND $
          (ta[indBi] EQ 1) AND $
          (cm[indBi] GT 0), count1)
ind2 = WHERE(((tm2[indBi] GE 25) AND (tm2[indBi] LE 36)) $
          OR ((tm2[indBi] GE 1) AND (tm2[indBi] LE 7)) AND $
              (ta[indBi] EQ 1) AND $
              (cm[indBi] GT 0), count2)
IF ((count1 EQ 0) OR (count2 EQ 0)) THEN BEGIN
  ind1AND2 = -1
ENDIF ELSE BEGIN
  ;update indexes to that they refer to the image directly
  ind1 = indBi[ind1]
  ind2 = indBi[ind2]

  ;remove the season
  ;first check that if there are cases where we remove both first and second
  ind1AND2 = SetIntersection(ind1, ind2)
ENDELSE


out_check = ngspy * 0B
file_list = prefix + $
  ['s1.img', 'e1.img', 'l1.img', 'm1.img', 'sen1.img', $
  's2.img', 'e2.img', 'l2.img', 'm2.img', 'sen2.img', $
  'KK0.img']
file_list_in  = dir_pheno + '\' + file_list
file_list_out  = dir_out + '\' + file_list
IF (ind1AND2[0] NE -1) THEN BEGIN
  ;save the check file
  out_check[ind1AND2] = 10
  res = write_envi_img(out_check, out_fn + '.img')
  res = write_envi_hdr(out_fn + '.hdr', ns, nl, 1)
  PRINT, 'Pixels that have two seasons in winter will be set to 252 error.'
  PRINT, 'N = ' + STRTRIM(N_ELEMENTS(ind1AND2),2)
  ;remove these pixels and assign no seasonality
 
  FOR i = 0, N_ELEMENTS(file_list)-1 DO BEGIN
    tmp = ReadEnviWithHdr(file_list_in[i])
    tmp[ind1AND2] = 252
    res = write_envi_img(tmp, file_list_out[i])
    fn = FILE_BASENAME(file_list[i],'.img')
    FILE_COPY, dir_pheno + '\' + fn +'.hdr', dir_out + '\' + fn +'.hdr', /OVERWRITE
  ENDFOR
  ;recompute ngspy, ind1 and ind2
  ngspy = ReadEnviWithHdr(dir_out + '\' + ngspy_fn)
  ;reformat ngspy
  ind1 =  WHERE((ngspy GE 100) AND (ngspy LT 200))
  ind2 =  WHERE((ngspy GE 200) AND (ngspy LT 250))
  ngspy = ngspy * 0B
  ngspy[ind1] = 1
  ngspy[ind2] = 2

  tm1 = ReadEnviWithHdr(dir_out + '\' + tm1_fn)
  tm2 = ReadEnviWithHdr(dir_out + '\' + tm2_fn)
  tm1 = SPIRITS_from_108_to_36_ARRAY(tm1)
  tm2 = SPIRITS_from_108_to_36_ARRAY(tm2)

  ;discard any second season occurring in winter, where there is crop, in the selected area
  ;focus on bimodal
  indBi = WHERE(ngspy EQ 2)
  ind1 = WHERE(((tm1[indBi] GE 25) AND (tm1[indBi] LE 36)) $
    OR ((tm1[indBi] GE 1) AND (tm1[indBi] LE 7)) AND $
    (ta[indBi] EQ 1) AND $
    (cm[indBi] GT 0), count1)
  ind2 = WHERE(((tm2[indBi] GE 25) AND (tm2[indBi] LE 36)) $
    OR ((tm2[indBi] GE 1) AND (tm2[indBi] LE 7)) AND $
    (ta[indBi] EQ 1) AND $
    (cm[indBi] GT 0), count2)

  ;update indexes to that they refer to the image directly
  ind1 = indBi[ind1]
  ind2 = indBi[ind2]

  ;remove the season
  ;first check that if there are cases where we remove both first and second
  ind1AND2 = SetIntersection(ind1, ind2) 
  IF (ind1AND2[0] NE -1) THEN STOP
ENDIF ELSE BEGIN
  res = write_envi_img(out_check, out_fn + '.img')
  res = write_envi_hdr(out_fn + '.hdr', ns, nl, 1)
  FOR i = 0, N_ELEMENTS(file_list)-1 DO BEGIN
    FILE_COPY, file_list_in[i], file_list_out[i], /OVERWRITE
    fn = FILE_BASENAME(file_list[i],'.img')
    FILE_COPY, dir_pheno + '\' + fn +'.hdr', dir_out + '\' + fn +'.hdr', /OVERWRITE
  ENDFOR
ENDELSE
;note: if the first season is removed but the second is left, the second must become the first
;treat first season of bimodal where the second is left there, all of 
;ind1 as those having both seasons to be remove are already flagged out
PRINT, 'Pixels with season 1 to be removed.'
PRINT, 'N = ' + STRTRIM(N_ELEMENTS(ind1),2)  
PRINT, 'Pixels with season 2 to be removed.'
PRINT, 'N = ' + STRTRIM(N_ELEMENTS(ind2),2)
file_list1 = dir_out + '\' + prefix + ['s1.img', 'e1.img', 'l1.img', 'm1.img', 'sen1.img']
file_list2 = dir_out + '\' + prefix + ['s2.img', 'e2.img', 'l2.img', 'm2.img', 'sen2.img']
FOR i = 0, N_ELEMENTS(file_list1) -1 DO BEGIN
  ;apro la seconda e le copio nella prima
  tmp1 = ReadEnviWithHdr(file_list1[i])
  tmp2 = ReadEnviWithHdr(file_list2[i])
  tmp1[ind1] = tmp2[ind1]
  ;nella second metto no season (251)
  tmp2[ind1] = 251
  res = write_envi_img(tmp1, file_list1[i])
  res = write_envi_img(tmp2, file_list2[i])
ENDFOR


;treat second season (always bimodal  
FOR i = 0, N_ELEMENTS(file_list2) -1 DO BEGIN
  ;apro la seconda  
  tmp2 = ReadEnviWithHdr(file_list2[i])
  ;metto no season (251)
  tmp2[ind2] = 251
  res = write_envi_img(tmp2, file_list2[i])
ENDFOR
           
;treat kk
fn = dir_out + '\' + prefix + 'KK0.img'
tmp = ReadEnviWithHdr(fn)
tmp[ind1] = 100 ;set it to one season only
tmp[ind2] = 100 ;set it to one season only
res = write_envi_img(tmp, fn)

;save the check filr
out_check = ReadEnviWithHdr(out_fn + '.img')
out_check[ind1] = 11
out_check[ind2] = 12
res = write_envi_img(out_check, out_fn + '.img')


TOC
END