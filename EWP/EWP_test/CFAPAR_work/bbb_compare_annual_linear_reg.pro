PRO RUN_BBB_compare_annual_linear_reg

  fn_afi_crops = '\\ies\d5\asap\TEST_PREDICTORS\Masks\mask_crop_afi_asap2_0.img' 
  fn_afi_range = '\\ies\d5\asap\TEST_PREDICTORS\Masks\mask_rangeland_afi_asap2_0.img'
  afi_thresold = 100 ; remember tha afi is 0-200 values, so 100 is 50%
  correlation_dir = '\\ies\d5\asap\TEST_PREDICTORS\YX_correlation'
  out_dir = '\\ies\d5\asap\TEST_PREDICTORS\YX_correlation_summary'
  vars=['SPI1','SPI3','chirpsSPI1','chirpsSPI3','zSWI015','zSWI040','zSWI060','zBECsm'] 
  BBB_compare_annual_linear_reg, correlation_dir, out_dir, fn_afi_crops, fn_afi_range, afi_thresold, vars
  
END


PRO BBB_compare_annual_linear_reg, correlation_dir, out_dir, fn_afi_crops, fn_afi_range, afi_thresold, vars
; here i have all statistic (r2, r, sig 0/1, gain) for season 1 and season 2 and I have to extract statistics 
; for crops and rangelands  
;13 apr 2016, modified to compute r2 instead of r, indicator before sos dropped
;14 sep 2017 modified for the new study

saveDataForTest = 1 ;save csv for Tuckey test
out_of = 5  ;subsampling for tuckey test

suffixes = ['_atProg0','_atProg25','_atProg50','_atProg75','_atProg100']
fnames_r2 = [!NULL]
fnames_sig01 = [!NULL]
seasN = [!NULL] ;this is to keep track of how many season
varName = [!NULL] ; keep track of the variable
prog = [!NULL]
fileId = [!NULL]

FOR s = 1, 2 DO BEGIN ;season
  FOR i = 0, N_ELEMENTS(vars)-1 DO BEGIN
    FOR j = 0, N_ELEMENTS(suffixes)-1 DO BEGIN
      fnames_r2 = [fnames_r2, correlation_dir + '\' + vars[i] + '\season' + STRTRIM(s,2) + vars[i] + suffixes[j] + '_bil-cod_with-zcNDVI']
      fnames_sig01 = [fnames_sig01, correlation_dir + '\' + vars[i] + '\season' + STRTRIM(s,2) + vars[i] + suffixes[j] + '_bil-sig_with-zcNDVI']
      seasN = [seasN, s] ;1 or 2
      varName = [varName, vars[i]] ; string var
      prog = [prog, suffixes[j]]  ; 0 to 4 indicating 0. 25 50, 75, 100
    ENDFOR
  ENDFOR
ENDFOR

r2_mat = FLTARR(N_ELEMENTS(fnames_r2)/2, 3, 3) ;d1 vars*progress, d2 overall/crop/pasture, d3 season1/season2/both merged) 
;r2_mat_s1_s2 = FLTARR(N_ELEMENTS(fnames_r2), 3)  ;overall, crop, pastures
;r2_mat_season_merged = FLTARR(N_ELEMENTS(fnames_r2)/2, 3)  ;overall, crop, pastures

;build teh mask. binary. rangeland = 1, crop = 2
afiCrops = ReadEnviWithHdr(fn_afi_crops)
afiRange = ReadEnviWithHdr(fn_afi_range)
mask = afiRange * 0B 
;sz = SIZE(afiRange)
;ns = sz[1]
;nl = sz[2]
;mask = BYTARR(ns,nl)
;thrshold the afis
indCrop = WHERE(afiCrops GT afi_thresold, countCrop)
IF (countCrop GT 0) then mask[indCrop] = 2
indRang = WHERE(afiRange GT afi_thresold, countRang)
IF (countRang GT 0) then mask[indRang] = 1

;perpare csvs for Tuckey test if requested
IF (saveDataForTest EQ 1) THEN BEGIN
  dirT = out_dir + '\FOR_TUKEY_test'
  fn_data_for_test = dirT + '\data_cod.csv'
  ;if present delete
  res = FILE_SEARCH(fn_data_for_test) 
  IF (res NE '') THEN FILE_DELETE, fn_data_for_test
  OPENW, lunD, fn_data_for_test, /GET_LUN
  PRINTF, lunD, 'Id,r2' 
  fn_reduced_data_for_test = dirT + '\reduced_data_cod.csv'
  ;if present delete
  res = FILE_SEARCH(fn_reduced_data_for_test)
  IF (res NE '') THEN FILE_DELETE, fn_reduced_data_for_test
  OPENW, lunRD, fn_reduced_data_for_test, /GET_LUN
  PRINTF, lunRD, 'Id,r2' 
;  fn_ids = dirT + '\id_cod.csv'
;  res = FILE_SEARCH(fn_ids)
;  IF (res NE '') THEN FILE_DELETE, fn_ids
;  OPENW, lunI, fn_ids, /GET_LUN
;  PRINTF, lunI, 'Id,Name'
ENDIF

;save a csv with code and meaning
OPENW, lunCode, out_dir + '\IdCodes.csv', /GET_LUN
PRINTF, lunCode, 'Id,Var,Prog' 

;ids = INDGEN(N_ELEMENTS(fnames_r2))
;r2 = FLTARR(ns,nl)
;sig = r2
PRINT, 'var prog seas %fraction of sig (p<0.05)'
strVarProg = STRARR(2,N_ELEMENTS(fnames_r2)/2)
c = 0
codeIds = [!NULL]
FractSigS1 = [!NULL]
FractSigS2 =[!NULL]
FractSigS1and2 = [!NULL]

FOR i = 0, N_ELEMENTS(vars)-1 DO BEGIN  
  FOR j = 0, N_ELEMENTS(suffixes)-1 DO BEGIN
    codeIds = [codeIds, c]
    strVarProg[*,c] = [vars[i], suffixes[j]]
    PRINTF, lunCode, STRTRIM(c,2)+','+vars[i]+','+ suffixes[j];'Id,Var,Prog' 
    ;select a variable , a progress, and both season
    indFileS1 = WHERE((varName EQ vars[i]) AND (prog EQ suffixes[j] AND (seasN EQ 1)), count1)
    indFileS2 = WHERE((varName EQ vars[i]) AND (prog EQ suffixes[j] AND (seasN EQ 2)), count2)
    IF (count1 + count2 NE 2) THEN STOP
    ;now i have two files per variable, one for season 1 and 1 for season 2
    r2S1 = ReadEnviWithHdr(fnames_r2[indFileS1])
    sigS1 = ReadEnviWithHdr(fnames_sig01[indFileS1])
    ;find where data are available and under the mask (both crop and range)
    ind_allS1 = WHERE(FINITE(r2S1) AND (mask GT 0), count_allS1)  ;count_allS1: valid and crop or range
    indsignS1 = WHERE(FINITE(r2S1) AND (mask GT 0) AND (sigS1 EQ 1.0), countSignS1)   ; valid as above nut also signifcant 
    ;then only crops and range
    ind_rangelndS1 = WHERE(FINITE(r2S1) AND (mask EQ 1))
    ind_croplndS1 =   WHERE(FINITE(r2S1) AND (mask EQ 2))
    
    r2S2 = ReadEnviWithHdr(fnames_r2[indFileS2])
    sigS2 = ReadEnviWithHdr(fnames_sig01[indFileS2])
    ind_allS2 = WHERE(FINITE(r2S2) AND (mask GT 0), count_allS2)
    indsignS2 = WHERE(FINITE(r2S2) AND (mask GT 0) AND (sigS2 EQ 1.0), countSignS2)
    ind_rangelndS2 = WHERE(FINITE(r2S2) AND (mask EQ 1))
    ind_croplndS2 =   WHERE(FINITE(r2S2) AND (mask EQ 2))
    
    ;store the mean of S1 for overall, crop , rangelnd  ;note r2_mat = FLTARR(N_ELEMENTS(fnames_r2)/2, 3, 3) ;d1 vars*progress, d2 overall/crop/pasture, d3 season1/season2/both merged) 
    r2_mat[c,*,0] =   [MEAN(r2S1[ind_allS1]), MEAN(r2S1[ind_croplndS1]), MEAN(r2S1[ind_rangelndS1])]
    ;store the mean of S2 for overall, crop , rangelnd
    r2_mat[c,*,1] =   [MEAN(r2S2[ind_allS2]), MEAN(r2S2[ind_croplndS2]), MEAN(r2S2[ind_rangelndS2])]
    ;store the mean of S1 and S2 merged for overall, crop , rangelnd
    r2_mat[c,*,2] =   [MEAN([r2S1[ind_allS1],r2S2[ind_allS2]]), $
                       MEAN([r2S1[ind_croplndS1],r2S2[ind_croplndS2]]), $
                       MEAN([r2S1[ind_rangelndS1],r2S2[ind_rangelndS2]])]
    
    ;print the signifcant fraction of season 1, 2 and merged
    FractSigS1 = [FractSigS1, countSignS1/FLOAT(count_allS1)*100]
    FractSigS2 = [FractSigS2, countSignS2/FLOAT(count_allS2)*100]
    FractSigS1and2 = [FractSigS1and2, (countSignS1+countSignS2)/FLOAT(count_allS1+count_allS2)*100]
    PRINT, STRJOIN([strVarProg[*,c], 'S1', STRTRIM(FractSigS1[-1],2), $
                                     'S2', STRTRIM(FractSigS2[-1],2), $
                                     'Merged', STRTRIM(FractSigS1and2[-1],2)], ' ')
    
    ;save data for test, only the two seasons pooled  
    IF (saveDataForTest EQ 1) THEN BEGIN
      FOR p = 0, count_allS1-1 DO BEGIN
        PRINTF, lunD, STRTRIM(r2S1[ind_allS1[p]],2)
        IF (p MOD out_of) EQ 0 THEN PRINTF, lunRD, STRTRIM(r2S1[ind_allS1[p]],2)
      ENDFOR
      FOR p = 0, count_allS2-1 DO BEGIN
        PRINTF, lunD, STRTRIM(r2S2[ind_allS2[p]],2)
        IF (p MOD out_of) EQ 0 THEN PRINTF, lunRD, STRTRIM(r2S2[ind_allS2[p]],2)
      ENDFOR
    ENDIF
    c = c + 1
  ENDFOR
ENDFOR

ind_sorted = REVERSE(SORT(r2_mat[*,0,2])) ; this is to orber dy r2 descending (r2 of crop+range and the two seasons together 
OPENW, lun, out_dir + '\AAA_cod_sorted_table.csv', /GET_LUN
PRINTF, lun,'Id, Var,Prog,Overall r2,Crop r2,Rangeland r2,S1andS2 FractSig,S1 FractSig,S2 FractSig'
FOR i = 0, N_ELEMENTS(ind_sorted) -1 DO BEGIN
  PRINTF, lun, STRTRIM(codeIds[ind_sorted[i]],2) + ',' + $
               STRTRIM(strVarProg[0,ind_sorted[i]],2) + ',' + STRTRIM(strVarProg[1,ind_sorted[i]],2) + ',' + $
               STRTRIM(r2_mat[ind_sorted[i],0,2],2) + ',' + $
               STRTRIM(r2_mat[ind_sorted[i],1,2],2) + ',' + $
               STRTRIM(r2_mat[ind_sorted[i],2,2],2) + ',' + $
               STRTRIM(FractSigS1and2[ind_sorted[i]],2) + ',' + STRTRIM(FractSigS1[ind_sorted[i]],2) + ',' + STRTRIM(FractSigS2[ind_sorted[i]],2)
ENDFOR
FREE_LUN, lun
FREE_LUN, lunCode

;plot istogram of best
;HERE ALSO PLOT CORRELATION HISTOGRAM
; I have tro retrive two files (on per season) of the the best var and prog

fn_bestS1 = correlation_dir + '\' + strVarProg[0,ind_sorted[0]] + '\season' + STRTRIM(1,2) + strVarProg[0,ind_sorted[0]] + strVarProg[1,ind_sorted[0]] + '_bil-cod_with-zcNDVI'
fn_bestS2 = correlation_dir + '\' + strVarProg[0,ind_sorted[0]] + '\season' + STRTRIM(2,2) + strVarProg[0,ind_sorted[0]] + strVarProg[1,ind_sorted[0]] + '_bil-cod_with-zcNDVI'
r2S1 = ReadEnviWithHdr(fn_bestS1)
r2S2 = ReadEnviWithHdr(fn_bestS2)
ind_allS1 = WHERE(FINITE(r2S1) AND (mask GT 0), count_allS1)
ind_allS2 = WHERE(FINITE(r2S2) AND (mask GT 0), count_allS2)
pdf= HISTOGRAM([r2S1[ind_allS1], r2S2[ind_allS2]], BINSIZE=0.1, LOCATIONS=xbin, MIN = 0.0, MAX = 1.0)
b1 = BARPLOT(xbin,  pdf/TOTAL(pdf)*100, XRANGE=[0,1], XMAJOR=11, XMINOR=0, YMINOR=0, HISTOGRAM=1, $
   XTICKDIR=1, YTICKDIR=1, AXIS_STYLE=1, FILL_COLOR=[230,230,230], TITLE= strVarProg[0,ind_sorted[0]]+' '+strVarProg[1,ind_sorted[0]]+' $R^2$', $
   XTITLE='$R^2$ with zCFAPAR', YTITLE=' Frequency (%)')
ind = WHERE(xbin GE 0.5)
PRINT, 'Fraction with R2 GT 0.5 =', TOTAL(pdf[ind[0]:*])/TOTAL(pdf)
r2S1 = 0
reS2 = 0
;do the same for correlation


fn_bestS1 = correlation_dir + '\' + strVarProg[0,ind_sorted[0]] + '\season' + STRTRIM(1,2) + strVarProg[0,ind_sorted[0]] + strVarProg[1,ind_sorted[0]] + '_bil-corr_with-zcNDVI'
fn_bestS2 = correlation_dir + '\' + strVarProg[0,ind_sorted[0]] + '\season' + STRTRIM(2,2) + strVarProg[0,ind_sorted[0]] + strVarProg[1,ind_sorted[0]] + '_bil-corr_with-zcNDVI'
rS1 = ReadEnviWithHdr(fn_bestS1)
rS2 = ReadEnviWithHdr(fn_bestS2)
ind_allS1 = WHERE(FINITE(rS1) AND (mask GT 0), count_allS1)
ind_allS2 = WHERE(FINITE(rS2) AND (mask GT 0), count_allS2)
pdf= HISTOGRAM([rS1[ind_allS1], rS2[ind_allS2]], BINSIZE=0.05, LOCATIONS=xbin, MIN = -1.0, MAX = 1.0)
h0 = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[-1.0,1.0], TITLE=strVarProg[0,ind_sorted[0]]+' '+strVarProg[1,ind_sorted[0]]+' r', XTITLE='Correlation with zCFAPAR', $
  YTITLE=' Frequency ', AXIS_STYLE=1, COLOR='black', NAME = fn_best)
ind = WHERE(xbin GE 0.5)
PRINT, 'Fraction with r GT 0.5 =', TOTAL(pdf[ind[0]:*])/TOTAL(pdf)
IF (saveDataForTest EQ 1) THEN BEGIN
  FREE_LUN, lunD
  FREE_LUN, lunRD
  ;FREE_LUN, lunI
ENDIF
END