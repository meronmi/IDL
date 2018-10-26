PRO test_boku_O_stages_old
TIC  
platform = !version.os_family
;  Set the screen device and the directory separator:
IF (platform EQ 'unix') THEN BEGIN
  screen = 'X'
  dirsep = '/'
  main_dir = '/eos/jeodpp/home/users/mmeroni/data'
ENDIF ELSE BEGIN
  screen = 'Win'
  dirsep = '\'
  main_dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE'
ENDELSE
  
; Consolidation stages:
; O0, O1, O2, O3, O4, and finally OF
; Motivation of the study:
; When computing anomalies in NRT we compare the most recent product O0 with a reference LTA. We are
; intersted in understanding which consolidation stage should be used for the LTA
; Assumption:
; - focus on Z score anomaly of a dekad with respect to LTA
; - the Z obtained by comparing OF and lta_OF is the best achievable and serves as reference
; - we use the following convention Z_nrtX_ltaY to indicate that Z is computed using X consolidation stage for nrt data and Y cons. stage for lta dat
;   X and Y = 0,1,2,3,4,F
; Method:
; with SPIRITS make a thinned image 21x21, of boku NDVI and of pheno, then make meta files
; 0 make bil stack files
; 1 Compute 
; - Z_nrtF_ltaF
; - Z_nrt0_ltaF, Z_nrt0_lta0
; - Z_nrt1_ltaF, Z_nrt1_lta1
; - Z_nrt2_ltaF, Z_nrt2_lta2
; - Z_nrt3_ltaF, Z_nrt3_lta3
; - Z_nrt4_ltaF, Z_nrt4_lta4
; 2 mask them for vegetated areas using phenology
; 3 Make the following graph:
; - X axis is consolidation level (0,1,2,3,4,5; where 5 is F)
; - Y axis is  (Z_nrtX_ltaX - Z_nrtF_ltaF)
; - plot  (Z_nrtX_ltaX - Z_nrtF_ltaF) and ABS(Z_nrtX_ltaX - Z_nrt5_lta5), with x=1,5, again 5 is F
; 
; 4 Summarise by latitude and time (deks) with Hovmöller diagram (It is a density scatter). over vegetated areas, during gorwing season only?

; Make the bils from meta
;test_boku_O_stages_make_bil

;make the z files (z score)
;test_boku_O_stages_make_Zfiles 

;make simple difference files (value - lta)
;test_boku_O_stages_make_DiffFiles

;make probability out of Z-score (Gaussint)
;test_boku_make_p


;build the confusion matrixes over z classification in categorial classes
;only z can analysed by confision matrix



;open pheno map for defining vegetated areas
tmp = ReadEnviWithHdr(main_dir+dirsep+'THINNED_DATA'+dirsep+'Pheno'+dirsep+'phenoKK0.img')
;make it 1 where is veg and NaN where is not, so I multiply for the various input to exclude non veg areas
ind = WHERE((tmp GE 100) AND (tmp LE 250))
veg = FLOAT(tmp)*!VALUES.F_NAN
veg[ind] = 1.0
tmp = 0
ind = 0

variable = 'z' ; can be either 'z' or 'p', indicating Z-score or Probability (GAUSSINT(z)), or 'd' indicating the simple difference
;set the breaks according tp WMO SPI
z_breaks = [-!VALUES.F_INFINITY,-2,-1.5,-1,0.99999,1.49999,1.99999, !VALUES.F_INFINITY]
z_cat_str    = ['extr. bad','very bad','moder. bad','near norm.','moder. good', 'very good', 'extr. good']
z_cat_num_str    = ['z<=-2','-2<z<=-1.5','-1.5<z<=-1','-1<z<1','1<=z<1.5', '1.5<=z<2', 'z>=2']
z_cat = INDGEN(N_ELEMENTS(z_cat_str))


;open reference
dir = main_dir+dirsep+'THINNED_DATA'+dirsep+'Z'
dir_results = main_dir+dirsep+'Results'
varNfLf = ReadEnviWithHdr(dir+dirsep+ variable + 'NfLf_2003-2016_bsq.img')
PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
sz = SIZE(varNfLf)


;open the active pixel to select only timings when there is a growing season
;start with one year
active = FLTARR(sz[1], sz[2], 36)
FOR i = 1, 36 DO BEGIN
  tt = STRING(i, FORMAT = '(I02)')
  active[*,*,i-1] = FLOAT(ReadEnviWithHdr(main_dir+dirsep+'THINNED_DATA'+dirsep+'Pheno'+dirsep+'active'+dirsep+'active1962'+tt+'.img'))
ENDFOR
ind = WHERE(active NE 1)
active[ind] = !VALUES.F_NAN
;concatenate 14 years on z axis, in this way I have a full year (36 deks), repeated for 14 year
active =  [[[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]], $
           [[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]]]
ind = 0

IF (variable EQ 'z') THEN BEGIN
  ;DATA 4 CONFUSION MATRIX
  NfLF_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
  ;reclassify varNfLf in the WMO cathegories
  indFin = WHERE(FINITE(varNfLf))
  FOR i = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
    indCat = WHERE((varNfLf[indFin] GT z_breaks[i]) AND (varNfLf[indFin] LE z_breaks[i+1]),countCat) 
    NfLF_cat[indFin[indCat]] = z_cat[i]
  ENDFOR
  DELVAR, indFin, indCat
ENDIF

;now varNxLf. So: given variable (can be z or p), work on the product obtained by using lelev x for NRT and level f (final) for LTA 
dvarNxLf_varNfLf = FLTARR(5)
dvarNxLf_varNfLf_veg = FLTARR(5)
dvarNxLf_varNfLf_veg_active = FLTARR(5)
fn_varNxLf = dir + dirsep + variable + 'N' + STRTRIM(INDGEN(5),2) + 'Lf_2003-2016_bsq.img'
FOR i = 0, 4 DO BEGIN
  varNxLf = ReadEnviWithHdr(fn_varNxLf[i])
  absDelta = ABS((varNxLf - varNfLf))
  dvarNxLf_varNfLf[i] = MEAN(absDelta, /NAN)
  ;only those vegetated
  dvarNxLf_varNfLf_veg[i] = MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]), /NAN)
  ;only those vegetated and at time when they are active 
  dvarNxLf_varNfLf_veg_active[i] = MEAN(absDelta*active, /NAN)
  IF (i EQ 0) THEN BEGIN
    !except=0 
    res = write_envi_img(MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]),DIMENSION=3, /NAN), dir_results + dirsep + variable + 'N0Lf_mean_delta_veg')
    res = write_envi_hdr(dir_results + dirsep + variable + 'N0Lf_mean_delta_veg.hdr', sz[1], sz[2], 4)
    res = write_envi_img(MEAN(absDelta*active,DIMENSION=3, /NAN), dir_results + dirsep + variable + 'N0Lf_mean_delta_veg_active')
    res = write_envi_hdr(dir_results + dirsep + variable + 'N0Lf_mean_delta_veg_active.hdr', sz[1], sz[2], 4)
    clear = CHECK_MATH() ;set back to normal
    !except=1
    ;test with ecdf (just to see how it looks like)
    ;regular subsample (evry 200)
    indFin = WHERE(FINITE(varNfLf))
    XYecdfVarNfLf = ecdf(varNfLf[indfin[0:-1:200]])
    IF (variable EQ 'z') THEN BEGIN
      ;DATA 4 CONFUSION MATRIX
      N0LF_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
      ;reclassify varNfLf in the WMO cathegories
      indFin = WHERE(FINITE(varNxLf))
      FOR j = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
        indCat = WHERE((varNxLf[indFin] GT z_breaks[j]) AND (varNxLf[indFin] LE z_breaks[j+1]),countCat)
        N0LF_cat[indFin[indCat]] = z_cat[j]
      ENDFOR
      DELVAR, indFin, indCat
    ENDIF
  ENDIF
  absDelta = 0
  varNxLf = 0
  PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
ENDFOR

;now varNxLx
;varNxLx = MAKE_ARRAY(sz[1], sz[2], sz[3], 5, TYPE = 4)
dvarNxLx_varNfLf = FLTARR(5)
dvarNxLx_varNfLf_veg = FLTARR(5)
dvarNxLx_varNfLf_veg_active = FLTARR(5)
fn_varNxLx = dir + dirsep+ variable + 'N' + STRTRIM(INDGEN(5),2) + 'L'+ STRTRIM(INDGEN(5),2) + '_2003-2016_bsq.img'
FOR i = 0, 4 DO BEGIN
  varNxLx = ReadEnviWithHdr(fn_varNxLx[i])
  absDelta = ABS((varNxLx - varNfLf))
  dvarNxLx_varNfLf[i] = MEAN(absDelta, /NAN)
  ;only those vegetated
  dvarNxLx_varNfLf_veg[i] = MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]), /NAN)
  ;only those vegetated and at time when they are active
  dvarNxLx_varNfLf_veg_active[i] = MEAN(absDelta*active, /NAN)
  IF (i EQ 0) THEN BEGIN
    !except=0 
    res = write_envi_img(MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]),DIMENSION=3, /NAN), dir_results + dirsep + variable + 'N0L0_mean_delta_veg')
    res = write_envi_hdr(dir_results + dirsep + variable + 'N0L0_mean_delta_veg.hdr', sz[1], sz[2], 4)
    res = write_envi_img(MEAN(absDelta*active,DIMENSION=3, /NAN), dir_results + dirsep + variable + 'N0L0_mean_delta_veg_active')
    res = write_envi_hdr(dir_results + dirsep + variable + 'N0L0_mean_delta_veg_active.hdr', sz[1], sz[2], 4)
    clear = CHECK_MATH() ;set back to normal
    !except=1
    ;test with ecdf (just to see how it looks like)
    ;regular subsample (evry 200)
    indFin = WHERE(FINITE(varNxLx))
    XYecdfVarN0L0 = ecdf(varNxLx[indfin[0:-1:200]])
    IF (variable EQ 'z') THEN BEGIN
      ;DATA 4 CONFUSION MATRIX
      N0L0_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
      ;reclassify varNfLf in the WMO cathegories
      indFin = WHERE(FINITE(varNxLx))
      FOR j = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
        indCat = WHERE((varNxLx[indFin] GT z_breaks[j]) AND (varNxLx[indFin] LE z_breaks[j+1]),countCat)
        N0L0_cat[indFin[indCat]] = z_cat[j]
      ENDFOR
      DELVAR, indFin, indCat
    ENDIF
  ENDIF
  absDelta = 0
  varNxLx = 0
  PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
ENDFOR

;CONFUSION MATRIX
IF (variable EQ 'z') THEN BEGIN
  ;now i have NfLF_cat, N0LF_cat, N0L0_cat. Marixes with z catheogarized in the 7 WMO cathegories (plus -999 that is no data)
  ;I have to build (for veg and veg active), the confusion matrix
  
  ;matrixes veg and active are 1 when veg and veg/active and NaN elsewhere
  
  
  ;veg
  cm = FLTARR(N_ELEMENTS(z_cat), N_ELEMENTS(z_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
  FOR i = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
    ;located indexes of category i on reference
    indCatI = WHERE((NfLF_cat*REBIN(veg, sz[1], sz[2], sz[3])) EQ z_cat[i], countCatI)
    ;quick chec
    IF (countCatI EQ 0) THEN STOP
    ;now see how were these pixels classified in the estimated z
    cm[i,*] = HISTOGRAM(REFORM(N0LF_cat[indCatI]), MIN=z_cat[0], MAX=z_cat[-1], BINSIZE = 1, LOCATIONS = loc)
    ;print, 'debug'
  ENDFOR
  res = confusion_mat_stats(cm, variable + '_' + 'N0LF', z_cat_str, z_cat_num_str, dir_results + dirsep + variable + '_' + 'N0LF_veg_confusion_matrix_stat.csv')
  ;veg & act
  cm = FLTARR(N_ELEMENTS(z_cat), N_ELEMENTS(z_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
  FOR i = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
    ;located indexes of category i on reference
    indCatI = WHERE((NfLF_cat*active) EQ z_cat[i], countCatI)
    ;quick chec
    IF (countCatI EQ 0) THEN STOP
    ;now see how were these pixels classified in the estimated z
    cm[i,*] = HISTOGRAM(REFORM(N0LF_cat[indCatI]), MIN=z_cat[0], MAX=z_cat[-1], BINSIZE = 1, LOCATIONS = loc)
  ENDFOR
  res = confusion_mat_stats(cm, variable + '_' + 'N0LF', z_cat_str, z_cat_num_str, dir_results + dirsep + variable + '_' + 'N0LF_veg_active_confusion_matrix_stat.csv')
  
  
  ; now N0L0
  ;veg
  cm = FLTARR(N_ELEMENTS(z_cat), N_ELEMENTS(z_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
  FOR i = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
    ;located indexes of category i on reference
    indCatI = WHERE((NfLF_cat*REBIN(veg, sz[1], sz[2], sz[3])) EQ z_cat[i], countCatI)
    ;quick chec
    IF (countCatI EQ 0) THEN STOP
    ;now see how were these pixels classified in the estimated z
    cm[i,*] = HISTOGRAM(REFORM(N0L0_cat[indCatI]), MIN=z_cat[0], MAX=z_cat[-1], BINSIZE = 1, LOCATIONS = loc)
  ENDFOR
  res = confusion_mat_stats(cm, variable + '_' + 'N0L0', z_cat_str, z_cat_num_str, dir_results + dirsep + variable + '_' + 'N0L0_veg_confusion_matrix_stat.csv')
  ;veg
  cm = FLTARR(N_ELEMENTS(z_cat), N_ELEMENTS(z_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
  FOR i = 0, N_ELEMENTS(z_cat)-1 DO BEGIN
    ;located indexes of category i on reference
     indCatI = WHERE((NfLF_cat*active) EQ z_cat[i], countCatI)
    ;quick chec
    IF (countCatI EQ 0) THEN STOP
    ;now see how were these pixels classified in the estimated z
    cm[i,*] = HISTOGRAM(REFORM(N0L0_cat[indCatI]), MIN=z_cat[0], MAX=z_cat[-1], BINSIZE = 1, LOCATIONS = loc)
  ENDFOR
  res = confusion_mat_stats(cm, variable + '_' + 'N0L0', z_cat_str, z_cat_num_str, dir_results + dirsep + variable + '_' + 'N0L0_veg_active_confusion_matrix_stat.csv')
  DELVAR, NfLF_cat, N0LF_cat, N0L0_cat, cm
ENDIF


SAVE, dvarNxLf_varNfLf, dvarNxLx_varNfLf, dvarNxLf_varNfLf_veg, dvarNxLx_varNfLf_veg, dvarNxLf_varNfLf_veg_active, dvarNxLx_varNfLf_veg_active, variable, $
      XYecdfVarNfLf, XYecdfVarN0L0, FILENAME = dir + '\savevar.sav'

;make the simple graph
;plot the two ECDF
ghE1 = PLOT(XYecdfVarNfLf[*,0], XYecdfVarNfLf[*,1], SYMBOL = 'dot', LINESTYLE='', YTITLE = 'ECDF', XTITLE = variable ,XRANGE=[-3,3], NAME=variable+'NfLf')
ghE2 = PLOT(XYecdfVarN0L0[*,0], XYecdfVarN0L0[*,1], SYMBOL = 'dot', LINESTYLE='', COLOR='red', NAME=variable+'N0L0', /OVERPLOT)
ghl2 = LEGEND(TARGET=[ghE1,ghE2], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.9,0.85])
ghE1.save, dir_results + dirsep + variable + '_ecdfs.tif'

;RESTORE, FILENAME='\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\Z\savevar.sav'
gh1 = PLOT(dvarNxLf_varNfLf, SYMBOL= 'o', LINESTYLE = ':', NAME = variable + 'NxLf', XTITLE='Consolidation Stage x', YTITLE = 'Mean of ABS('+ variable + 'NxLy - '+ variable + 'NfLf)', XRANGE=[-0.5, 4.5])
gh2 = PLOT(dvarNxLx_varNfLf, SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxLx', /OVERPLOT)
gh1veg = PLOT(dvarNxLf_varNfLf_veg, SYMBOL= 'o', LINESTYLE = ':', COLOR= 'red', NAME =  variable + 'NxLf_veg', /OVERPLOT)
gh2veg = PLOT(dvarNxLx_varNfLf_veg, SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxLx_veg', /OVERPLOT)
gh1vegact = PLOT(dvarNxLf_varNfLf_veg_active, SYMBOL= 'o', LINESTYLE = ':', COLOR= 'green', NAME = variable + 'NxLf_actVeg', /OVERPLOT)
gh2vegact = PLOT(dvarNxLx_varNfLf_veg_active, SYMBOL= 'o', LINESTYLE = '-', COLOR= 'green', NAME = variable + 'NxLx_actVeg', /OVERPLOT)
ghl = LEGEND(TARGET=[gh1,gh1veg,gh1vegact,gh2,gh2veg, gh2vegact], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.9,0.85])
gh1.save, dir_results + dirsep + variable + '_delta.tif'
PRINT,'ok'
TOC
END