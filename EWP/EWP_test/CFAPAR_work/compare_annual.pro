PRO compare_annual

saveDataForTest = 1
out_of = 1
;**************************************************************************
rfe_dataset = 'chirps';'tamsat' ;'chirps'
CASE rfe_dataset OF
  'tamsat': BEGIN
    ;TAMSAT RESOLUTION
    mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
    fn_intersection = 'eco_crop_pasture_res_TAMSAT.img' ;pasrure = 1, crop = 2
    ns = 1867
    nl = 348
    dir = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\correlation'
  END
  'chirps': BEGIN
    ;CHIRPS RESOLUTION
    mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
    fn_intersection = 'eco_crop_pasture_res_CHIRPS.img' ;pasrure = 1, crop = 2
    ns = 1402
    nl = 262
    dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps'
  END
  ELSE: STOP
ENDCASE
;**************************************************************************

suffixes = ['_atProg0-3','_atProg0-2','_atProg0-1',$
  '_atProg0','_atProg25','_atProg50','_atProg75','_atProg100']
prefixes = !NULL
;hl = half_life_define_all()
;FOR i = 0, N_ELEMENTS(hl)- 1 DO prefixes = [prefixes, 'standardized_cwp_hl'+STRTRIM(hl[i],2)]
;prefixes=[prefixes,'SPI3','SPI4','SPI5','SPI6','SPI7','SPI8','SPI9','SPI10','SPI11','SPI12','SPI13','SPI14','SPI15']
;now only on monthly to make it more understandable:
prefixes=[prefixes,'SPI3','SPI6','SPI9','SPI12','SPI15','SPI18']
;prefixes=['standardized_cwp_hl250','standardized_cwp_hl500','standardized_cwp_hl1000',$
;    'standardized_cwp_hl2000','standardized_cwp_hl3000','SPI3', $
;    'SPI4','SPI5','SPI6','SPI7','SPI8','SPI9','SPI10','SPI12']
fnames = !NULL
FOR i= 0, N_ELEMENTS(prefixes)-1 DO BEGIN
  FOR j= 0, N_ELEMENTS(suffixes)-1 DO BEGIN
    fnames = [fnames, prefixes[i] + suffixes[j] + '-corr_with-zCFAPAR']
  ENDFOR
ENDFOR


corr_mat = FLTARR(N_ELEMENTS(fnames)+1, 3)  ;overall, crop, pastures

mask = BYTARR(ns,nl)*0B
OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
READU, lun, mask
FREE_LUN, lun
IF (saveDataForTest EQ 1) THEN BEGIN
  dirT = dir + '\FOR TUKEY test'
  fn_data_for_test = dirT + '\data.csv'
  ;if present delete
  res = FILE_SEARCH(fn_data_for_test) 
  IF (res NE '') THEN FILE_DELETE, fn_data_for_test
  OPENW, lunD, fn_data_for_test, /GET_LUN
  PRINTF, lunD, 'Id,r' 
  fn_reduced_data_for_test = dirT + '\reduced_data.csv'
  ;if present delete
  res = FILE_SEARCH(fn_reduced_data_for_test)
  IF (res NE '') THEN FILE_DELETE, fn_reduced_data_for_test
  OPENW, lunRD, fn_reduced_data_for_test, /GET_LUN
  PRINTF, lunRD, 'Id,r'
  fn_ids = dirT + '\id.csv'
  res = FILE_SEARCH(fn_ids)
  IF (res NE '') THEN FILE_DELETE, fn_ids
  OPENW, lunI, fn_ids, /GET_LUN
  PRINTF, lunI, 'Id,Name'
ENDIF
ids = INDGEN(N_ELEMENTS(fnames))
r = FLTARR(ns,nl)
FOR i = 0, N_ELEMENTS(fnames) -1 DO BEGIN
  OPENR, lun, dir +  '\' + fnames[i], /GET_LUN
  READU, lun, r
  FREE_LUN, lun
  ind_all = WHERE(FINITE(r) AND (mask GT 0), count_can)
  ind_rangelnd = WHERE(FINITE(r) AND (mask EQ 1))
  ind_croplnd =   WHERE(FINITE(r) AND (mask EQ 2))
  corr_mat[i,*] = [Mean(r[ind_all]), Mean(r[ind_croplnd]), Mean(r[ind_rangelnd])]
  IF (saveDataForTest EQ 1) THEN BEGIN
    PRINTF, lunI, STRTRIM(ids[i],2) + ',' +  STRTRIM(fnames[i],2)
    FOR p = 0, N_ELEMENTS(ind_all)-1 DO BEGIN
      PRINTF, lunD, STRTRIM(i,2) + ',' +  STRTRIM(r[ind_all[p]],2)
      IF (p MOD out_of) EQ 0 THEN PRINTF, lunRD, STRTRIM(i,2) + ',' +  STRTRIM(r[ind_all[p]],2)
    ENDFOR
  ENDIF
ENDFOR

ind_sorted = REVERSE(SORT(corr_mat[*,0]))
OPENW, lun, dir + '\AAA_correlation.csv', /GET_LUN
PRINTF, lun,'Id,File,Overall r,Crop r, Rangeland r'
FOR i = 0, N_ELEMENTS(fnames) -1 DO BEGIN 
  PRINTF, lun, STRTRIM(ids[ind_sorted[i]],2)+ ',' +STRTRIM(fnames[ind_sorted[i]],2) + ',' + $
         STRTRIM(corr_mat[ind_sorted[i],0],2) + ', ' + $
         STRTRIM(corr_mat[ind_sorted[i],1],2) + ', ' + $
         STRTRIM(corr_mat[ind_sorted[i],2],2)
ENDFOR
FREE_LUN, lun
;plot istogram of best
fn_best = fnames[ind_sorted[0]]
OPENR, lun, dir +  '\' + fn_best, /GET_LUN
READU, lun, r
FREE_LUN, lun
ind_all = WHERE(FINITE(r) AND (mask GT 0), count_can)
pdf= HISTOGRAM(r[ind_all], BINSIZE=0.01, LOCATIONS=xbin, MIN = -1.0, MAX = 1.0)
h0 = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[-1.0,1,0], TITLE=fn_best+' r', XTITLE='Correlation with zCFAPAR', $
  YTITLE=' Frequency ', AXIS_STYLE=1, COLOR='black', NAME = fn_best)
pdf= HISTOGRAM(r[ind_all], BINSIZE=0.05, LOCATIONS=xbin, MIN = -1.0, MAX = 1.0)
h0bis = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[-1.0,1,0],  XTITLE='Correlation', $
    YTITLE=' Frequency (%)', AXIS_STYLE=1, COLOR='black', NAME = fn_best)
ind = WHERE(xbin GE 0.5)
PRINT, 'Fraction with r GT 0.5 =', TOTAL(pdf[ind[0]:*])/TOTAL(pdf)
IF (saveDataForTest EQ 1) THEN BEGIN
  FREE_LUN, lunD
  FREE_LUN, lunRD
  FREE_LUN, lunI
ENDIF
END