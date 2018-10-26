PRO test_handler_year_by_year
truncation2rangeTeor = [0];[0,1]
variables = ['z'];,'n','v']
version = '2' ;version of anonaly to be used

;can be:
;'z': z-score
;'d': simple difference x - historical mean
;'p': Probability (GAUSSINT(z))
;'v': vci (Vegetation Condition Index)
;'n': non exceedance probability (robust version of vci)
;'s': modified z-score (robust version of z made with median and MAD)
FOR i = 0, N_ELEMENTS(variables)-1 Do BEGIN
  FOR j = 0, N_ELEMENTS(truncation2rangeTeor)-1 Do BEGIN
    test_boku_O_stages_map_year_by_year, variables[i], truncation2rangeTeor[j], version
  ENDFOR
ENDFOR
END

PRO test_boku_O_stages_map_year_by_year, variable, trunc2rangeTeor, version
TIC  
platform = !version.os_family
;  Set the screen device and the directory separator:
IF (platform EQ 'unix') THEN BEGIN
  screen = 'X'
  dirsep = '/'
  main_dir = '/eos/jeodpp/data/users/mmeroni/data'
ENDIF ELSE BEGIN
  screen = 'Win'
  dirsep = '\'
  main_dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE'
ENDELSE
plot_only_act_veg = 1  
; Consolidation stages:
; O0, O1, O2, O3, O4, and finally OF
; Motivation of the study:
; When computing anomalies in NRT we compare the most recent product O0 with a reference LTA. We are
; intersted in understanding which consolidation stage should be used for the LTA
; Assumption:
; - focus on Z score and vci anomaly of a dekad with respect to LTA. Hereafter only Z is mentioned.
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
;only z and v can be analysed by confision matrix


target = 'all' ;can be: all, cro, range 
test_dom_variation = 0 ;temporay test (using a mixed variance), put it at 0
dir = main_dir + dirsep + 'THINNED_DATA' + dirsep + STRUPCASE(variable) + version
dir_results = main_dir + dirsep + 'Results_year_by_year' + dirsep + target + version + dirsep + variable
IF (trunc2rangeTeor EQ 1) THEN dir_results = dir_results + '_trunc_2_f_range'
FILE_MKDIR, dir_results

;open pheno map for defining vegetated areas
tmp = ReadEnviWithHdr(main_dir+dirsep+'THINNED_DATA'+dirsep+'pheno'+dirsep+'phenoKK0.img')
;make it 1 where is veg and NaN where is not, so I multiply for the various input to exclude non veg areas
ind = WHERE((tmp GE 100) AND (tmp LE 250))
veg = FLOAT(tmp)*!VALUES.F_NAN
veg[ind] = 1.0
tmp = 0
ind = 0

;;define cathegories for matrix assesment
;CASE 1 OF
;  (variable EQ 'z') OR (variable EQ 's'): BEGIN
;    ;set the breaks according to WMO SPI
;    anom_breaks = [-!VALUES.F_INFINITY,-2,-1.5,-1,0.99999,1.49999,1.99999, !VALUES.F_INFINITY]
;    anom_cat_str    = ['extr. bad','very bad','moder. bad','near norm.','moder. good', 'very good', 'extr. good']
;    anom_cat_num_str    = ['z<=-2','-2<z<=-1.5','-1.5<z<=-1','-1<z<1','1<=z<1.5', '1.5<=z<2', 'z>=2']
;    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
;  END
;  (variable EQ 'n'): BEGIN
;    ;set the breaks according to WMO SPI
;    anom_breaks = [-!VALUES.F_INFINITY, 6.6807,15.8655,84.1344,93.3192, !VALUES.F_INFINITY]
;    anom_cat_str    = ['vert to extr. bad','moder. bad','near norm.','moder. good', 'very to extr. good']
;    anom_cat_num_str    = ['n<=6.6807','6.6807<n<=15.8655','15.8655<n<84.1345','84.1345<=n<93.3193', 'n>=93.3193']
;    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
;  END
;  (variable EQ 'v'): BEGIN
;    ;set the breaks according to Klish and Atzberger, doi:10.3390/rs8040267
;    anom_breaks = [-!VALUES.F_INFINITY,9.99999,19.99999,34.99999,49.99999,!VALUES.F_INFINITY]
;    anom_cat_str    = ['Extr. Drought','Severe Drought','Moder. Drought','Norm.','Wet']
;    anom_cat_num_str    = ['VCI<10','10<=VCI<20','20<=VCI<35','35<=VCI<50','VCI>=50']
;    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
;  END
;ENDCASE 

;open the reference anomaly NfLf
varNfLf = ReadEnviWithHdr(dir + dirsep + variable + 'NfLf_2003-2016_bsq.img')
;set inf to Nan
indInf = WHERE(FINITE(varNfLf, /INFINITY), countInf)
IF (countInf GT 0) THEN varNfLf[indInf] = !VALUES.F_NAN
indInf = 0
PRINT, 'Mem: ' + STRTRIM(MEMORY(/Current)/1000000.0,2)
sz = SIZE(varNfLf)
;apply the finite mask to remove 18 pixels where OF time series has missing values
fn = main_dir + dirsep + 'THINNED_DATA' + dirsep + 'masks' + dirsep + 'OF_finiteness.img'
finOfMask = ReadEnviWithHdr(fn)
ind2Bmasked = WHERE(finOfMask EQ 1)
finOfMask = FLOAT(finOfMask)*0+1  ;set it to 1
finOfMask[ind2Bmasked] = !VALUES.F_NAN
varNfLf = TEMPORARY(varNfLf) * REBIN(finOfMask, sz[1], sz[2], sz[3])
finOfMask = 0 & ind2Bmasked = 0


;Open the crop or range AFIs and make it a mask (GT 25%), note that this is 0/1
IF (target NE 'all') THEN BEGIN
  ;we are analysing crops or rangeland, open the correct file
  CASE target OF
    'cro': fn = main_dir + dirsep + 'THINNED_DATA' + dirsep + 'masks' + dirsep + 'mask_crop_afi_asap2.img'
    'range': fn = main_dir + dirsep + 'THINNED_DATA' + dirsep + 'masks' + dirsep + 'mask_pasture_afi_asap2.img'
    ELSE: stop
  ENDCASE
  maskafi = ReadEnviWithHdr(fn)
  ;select those with afi greater than 25% (note that 50 dn is 25)
  ind = WHERE(maskafi GT 50)
  mask = maskafi*0
  mask[ind] = 1
  maskafi = 0
ENDIF

;open the active pixel to select only timings when there is a growing season
;first make the avg year
active = FLTARR(sz[1], sz[2], 36)
FOR i = 1, 36 DO BEGIN
  tt = STRING(i, FORMAT = '(I02)')
  active[*,*,i-1] = FLOAT(ReadEnviWithHdr(main_dir+dirsep+'THINNED_DATA'+dirsep+'pheno'+dirsep+'active'+dirsep+'active1962'+tt+'.img'))
  IF (target NE 'all') THEN active[*,*,i-1] = active[*,*,i-1] * mask
ENDFOR
ind = WHERE(active NE 1)
active[ind] = !VALUES.F_NAN
;make the full time series:
;concatenate 14 years on z axis, in this way I have a full year (36 deks), repeated for 14 year
active =  [[[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]], $
           [[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]]]
ind = 0

;;catheogorize the reference NfLf anomaly values 
;IF (variable EQ 'z') OR (variable EQ 'v') OR (variable EQ 's') OR (variable EQ 'n') THEN BEGIN
;  ;DATA 4 CONFUSION MATRIX
;  ;NfLF_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
;  NfLF_cat = INTARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
;  ;reclassify varNfLf in the WMO cathegories
;  indVarNfLfFin = WHERE(FINITE(varNfLf), countVarNfLfFin)
;  FOR i = 0, N_ELEMENTS(anom_cat)-1 DO BEGIN
;    indCat = WHERE((varNfLf[indVarNfLfFin] GT anom_breaks[i]) AND (varNfLf[indVarNfLfFin] LE anom_breaks[i+1]),countCat) 
;    NfLF_cat[indVarNfLfFin[indCat]] = anom_cat[i]
;  ENDFOR
;  indCat = 0
;  ;check that the numebr of finite pixels in the anomalies is equal to the number of pixels with cat GE 0
;  ind = WHERE(NfLF_cat GE anom_cat[0], countCatFin)
;  IF (countVarNfLfFin NE countCatFin) THEN STOP 
;  ind  = 0
;ENDIF


;;test with ecdf (just to see how it looks like)
;;regular subsample (every 200)
;indFin = WHERE(FINITE(varNfLf))
;XYecdfVarNfLf = ecdf(varNfLf[indfin[0:-1:200]])

;;In a loop
;;variable to store the difference between anonalies at different consolidation stage and for different targets
;;var for the mean abs diff 
;mnAbsDiffVarNxLxf_varNfLf = FLTARR(5,2,3) ;row: 0 = NxLf, 1 = NxLx. Dimension 3 is for: all pixels/vegetated/vegetated and active 
;;var for mean difference
;mnDiffVarNxLxf_varNfLf = FLTARR(5,2,3) ;row: 0 = NxLf, 1 = NxLx. Dimension 3 is for: all pixels/vegetated/vegetated and active
;;of an anomoly made with NxLy, the percent of pixels missing (they are NaN) with respect of NfLf. It is the % of all valid NfLf pixels 
;prctMissInNiLjWrNfLf = FLTARR(5,2,3)
;;of an anomoly made with NxLy, the percent of pixels missing (they are NaN) in NfLf. It is the % of all valid NxLy pixels
;prctMissInNfLfWrNiLj = FLTARR(5,2,3)

;files to read
;files are [N0Lf, .., N4Lf]
;          [N0L0, .., N4L4]
fn_varNxLxf = [[dir + dirsep + variable + 'N' + STRTRIM(INDGEN(5),2) + 'Lf_2003-2016_bsq.img'],$
              [dir + dirsep+ variable + 'N' + STRTRIM(INDGEN(5),2) + 'L'+ STRTRIM(INDGEN(5),2) + '_2003-2016_bsq.img']]     ;5 colums, 2 rows
NL_names =[['N' + STRTRIM(INDGEN(5),2) + 'Hf'],['N' + STRTRIM(INDGEN(5),2) + 'H'+ STRTRIM(INDGEN(5),2)]]
IF (test_dom_variation EQ 1) THEN BEGIN
  fn_varNxLxf = [[dir + dirsep + variable + 'N' + STRTRIM(INDGEN(5),2) + 'Lf_2003-2016_bsq.img'],$
    [dir + dirsep+ variable + 'N' + STRTRIM(INDGEN(5),2) + 'Lf'+ STRTRIM(INDGEN(5),2) + '_2003-2016_bsq.img']]     ;5 colums, 2 rows
  NL_names =[['N' + STRTRIM(INDGEN(5),2) + 'Hf'],['N' + STRTRIM(INDGEN(5),2) + 'Hf'+ STRTRIM(INDGEN(5),2)]]
ENDIF

histoNfLfdone = 0     ;to make the histogram only once for NfLf
;row: 0 = NxLf, 1 = NxLx
;declere a structure where to store the dichot. matrix results
;mc is multi-cathegory, d is dichotomous
;veg_act_stats = CREATE_STRUCT('mc_a',FLTARR(5,2),'mc_HSS',FLTARR(5,2),'mc_rank_a',FLTARR(5,2),'mc_rank_mm',FLTARR(5,2),'mc_rank_m',FLTARR(5,2),'mc_rank_sm',FLTARR(5,2),'mc_rank_um',FLTARR(5,2),$
;                              'd_a',FLTARR(5,2),'d_bias',FLTARR(5,2), 'd_DetRate', FLTARR(5,2), 'd_FalseAlRate', FLTARR(5,2), 'd_HSS', FLTARR(5,2)) ;row: 0 = NxLf, 1 = NxLx
FOR jLTA = 0, 1 DO BEGIN ;loop on consolidation stagle of LTA: either f (jLTA = 0) or x (jLTA = 1)
  FOR iNRT = 0, 4 DO BEGIN ; loop on consolidation stage of NRT, five in total, 0 to 4
    ;read the variable with NiLj
    varNiLj = ReadEnviWithHdr(fn_varNxLxf[iNRT,jLTA])
    ;deal with truncation to theoretical range
    ;trunc2rangeTeor
    ;it happens only with stats from Hf
    IF ((jLTA EQ 0) AND (trunc2rangeTeor EQ 1)) THEN BEGIN
      CASE variable OF
        'v': rangeT = [0,100]
        'n': rangeT = [0,100]
        'z': rangeT = [-3.4744,3.4744]
        ELSE: STOP
      ENDCASE
      ind2trunc = WHERE(varNiLj LT rangeT[0], count2t)
      IF (count2t GT 0) THEN varNiLj[ind2trunc] = rangeT[0]
      ind2trunc = WHERE(varNiLj GT rangeT[1], count2t)
      IF (count2t GT 0) THEN varNiLj[ind2trunc] = rangeT[1]
      ind2trunc = 0
    ENDIF
    ;set inf to Nan
    indInf = WHERE(FINITE(varNiLj, /INFINITY), countInf)
    IF (countInf GT 0) THEN varNiLj[indInf] = !VALUES.F_NAN
    indInf = 0
    ;make the avg diff
    Delta = varNiLj - varNfLf
    absDelta = ABS((Delta))
    ;only those veg and active
    Delta = Delta*active
    absDelta = absDelta*active
    DeltaByYear = FLTARR(sz[1],sz[2],14)
    absDeltaByYear = FLTARR(sz[1],sz[2],14)       
    IF (iNRT EQ 0) THEN BEGIN
      ;we are looking at N0. L can be 0 or f. This is a special interesting case in NRT, so we save more info
      !except=0
      ;differently from standard test_boku_o_stage, save it year by year
      band0 = 0
      FOR y = 0, 13 DO  BEGIN
        fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_MAE_abs_delta_veg_active' + STRTRIM(2003+y,2)
        absDeltaByYear[*,*,y] = MEAN(absDelta[*,*,band0:band0+35],DIMENSION=3, /NAN)
        res = write_envi_img(absDeltaByYear[*,*,y], fn_base)
        res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
        
        fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_ME_delta_veg_active' + STRTRIM(2003+y,2)
        DeltaByYear[*,*,y] = MEAN(Delta[*,*,band0:band0+35],DIMENSION=3, /NAN)
        res = write_envi_img(DeltaByYear[*,*,y], fn_base)
        res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
        band0 = band0+35+1
        clear = CHECK_MATH() ;set back to normal
        !except=1
      ENDFOR
      ;2 Hovmöller diagram of the difference
      
      Hov_absDelta = TRANSPOSE(REFORM(MEAN(absDelta, DIMENSION=1, /NAN))) ; now it should be sz[2], sz[3]
      nactive = FINITE(absDelta)
      Hov_nactive = TRANSPOSE(REFORM(TOTAL(nactive, 1))) ; now it should be sz[2], sz[3]
      ;Hov_nactive = Hov_nactive[0:35,*]
      ct = COLORTABLE(72, /reverse)
      lat = REVERSE(75 - FINDGEN(sz[2]) * 0.1875)
      ;gh = CONTOUR(REVERSE(Hov_absDelta,2), INDGEN(sz[3]), INDGEN(sz[2]), /FILL, RGB_TABLE=ct, MIN_VALUE=0.0, MAX_VALUE=3.0,XRANGE=[0,503],YRANGE=[0,697])
      mrgn = [0.05, 0.2, 0.05, 0.05]
      indexes = FINDGEN(6)*0.5;[0.01, 1.0, 2.0, 3.0]
;      gh = CONTOUR(REVERSE(Hov_absDelta,2), INDGEN(sz[3]), lat, /FILL, RGB_TABLE=ct, AXIS_STYLE=2, C_VALUE = indexes,$
;                   XRANGE=[0,503],YRANGE=[MIN(lat),MAX(lat)], $ ;MIN_VALUE=0.0, MAX_VALUE=3.0,
;                   XTITLE = 'Time', YTITLE = 'Latitude (deg)', MARGIN = mrgn)
;      cb = COLORBAR(TITLE='MAE', POSITION=[0.3,0.07,0.7,0.12]) ;[X1, Y1, X2, Y2], defining the lower left and upper right corners of the image portion of the colorbar
      ;last three years
      time = 2014+FINDGEN(108)/36.0
      n_land_by_lat = REFORM(TOTAL(MAX(FINITE(active[*,*,0:35]), DIMENSION=3), 1))
      fract_active = FLTARR(108,sz[2])
      FOR i = 0, 107 DO fract_active[i,*] = FLOAT(Hov_nactive[i,*]) / FLOAT(n_land_by_lat)*100
      ;fract_active = FLOAT(Hov_nactive[-108:-1,*])/[[[n_land_by_lat]],[[n_land_by_lat]],[[n_land_by_lat]]]*100
      
      ;ind = WHERE(Hov_nactive LE 
      Hov_absDeltaLast3yrs = Hov_absDelta[-108:-1,*]
      indless5prtc= WHERE((fract_active LT 5) AND (Hov_nactive[-108:-1,*] LT 50))
      Hov_absDeltaLast3yrs[indless5prtc] = !VALUES.F_NAN
      gh = CONTOUR(REVERSE(Hov_absDeltaLast3yrs,2), time, lat, /FILL, RGB_TABLE=ct, AXIS_STYLE=2, C_VALUE = indexes, $
                    XRANGE=[2014,time[-1]],YRANGE=[MIN(lat),MAX(lat)], ZRANGE=[0,3], $ ;MIN_VALUE=0.0, MAX_VALUE=3.0,
                    XTITLE = 'Time', YTITLE = 'Latitude (deg)', MARGIN = mrgn, XTICKINTERVAL=1)
      cb = COLORBAR(TITLE='MAE (act>5%)', POSITION=[0.3,0.07,0.7,0.12])
      
      ;nactive
      indexes2 = FINDGEN(10)*2;[0.01, 1.0, 2.0, 3.0]
      gh = CONTOUR(REVERSE(Hov_nactive[-108:-1,*],2), time, lat, /FILL, RGB_TABLE=ct, AXIS_STYLE=2, C_VALUE = indexes2, $
        XRANGE=[2014,time[-1]],YRANGE=[MIN(lat),MAX(lat)], ZRANGE=[0,3], $ ;MIN_VALUE=0.0, MAX_VALUE=3.0,
        XTITLE = 'Time', YTITLE = 'Latitude (deg)', MARGIN = mrgn, XTICKINTERVAL=1)
      cb = COLORBAR(TITLE='nActive', POSITION=[0.3,0.07,0.7,0.12])
      
      ;fract active
      indexes2 = FINDGEN(10)*10;[0.01, 1.0, 2.0, 3.0]
      gh = CONTOUR(REVERSE(fract_active,2), time, lat, /FILL, RGB_TABLE=ct, AXIS_STYLE=2, C_VALUE = indexes2, $
        XRANGE=[2014,time[-1]],YRANGE=[MIN(lat),MAX(lat)], ZRANGE=[0,3], $ ;MIN_VALUE=0.0, MAX_VALUE=3.0,
        XTITLE = 'Time', YTITLE = 'Latitude (deg)', MARGIN = mrgn, XTICKINTERVAL=1)
      cb = COLORBAR(TITLE='FRACT ACT %', POSITION=[0.3,0.07,0.7,0.12])
      PRINT, 'qui'
    ENDIF
    

    absDelta = 0
    varNxLf = 0
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
  ENDFOR
ENDFOR


PRINT,'ok'
TOC
END

