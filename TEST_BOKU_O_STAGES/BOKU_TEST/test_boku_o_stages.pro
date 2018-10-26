PRO test_handler
truncation2rangeTeor = [0];[0,1]
variables = ['z','n','v']
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
    test_boku_O_stages, variables[i], truncation2rangeTeor[j], version
  ENDFOR
ENDFOR
END

PRO test_boku_O_stages, variable, trunc2rangeTeor, version
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
  main_dir = '\\ies\d5\asap\users_data\meronmi\TEST_BOKU_CONSOLIDATION_STAGE'
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
dir_results = main_dir + dirsep + 'Results' + dirsep + target + version + dirsep + variable
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

;define cathegories for matrix assesment
CASE 1 OF
  (variable EQ 'z') OR (variable EQ 's'): BEGIN
    ;set the breaks according to WMO SPI
    anom_breaks = [-!VALUES.F_INFINITY,-2,-1.5,-1,0.99999,1.49999,1.99999, !VALUES.F_INFINITY]
    anom_cat_str    = ['extr. bad','very bad','moder. bad','near norm.','moder. good', 'very good', 'extr. good']
    anom_cat_num_str    = ['z<=-2','-2<z<=-1.5','-1.5<z<=-1','-1<z<1','1<=z<1.5', '1.5<=z<2', 'z>=2']
    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
  END
  (variable EQ 'n'): BEGIN
    ;set the breaks according to WMO SPI
    anom_breaks = [-!VALUES.F_INFINITY, 6.6807,15.8655,84.1344,93.3192, !VALUES.F_INFINITY]
    anom_cat_str    = ['vert to extr. bad','moder. bad','near norm.','moder. good', 'very to extr. good']
    anom_cat_num_str    = ['n<=6.6807','6.6807<n<=15.8655','15.8655<n<84.1345','84.1345<=n<93.3193', 'n>=93.3193']
    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
  END
  (variable EQ 'v'): BEGIN
    ;set the breaks according to Klish and Atzberger, doi:10.3390/rs8040267
    anom_breaks = [-!VALUES.F_INFINITY,9.99999,19.99999,34.99999,49.99999,!VALUES.F_INFINITY]
    anom_cat_str    = ['Extr. Drought','Severe Drought','Moder. Drought','Norm.','Wet']
    anom_cat_num_str    = ['VCI<10','10<=VCI<20','20<=VCI<35','35<=VCI<50','VCI>=50']
    anom_cat = INDGEN(N_ELEMENTS(anom_cat_str))
  END
ENDCASE 

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

;catheogorize the reference NfLf anomaly values 
IF (variable EQ 'z') OR (variable EQ 'v') OR (variable EQ 's') OR (variable EQ 'n') THEN BEGIN
  ;DATA 4 CONFUSION MATRIX
  ;NfLF_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
  NfLF_cat = INTARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
  ;reclassify varNfLf in the WMO cathegories
  indVarNfLfFin = WHERE(FINITE(varNfLf), countVarNfLfFin)
  FOR i = 0, N_ELEMENTS(anom_cat)-1 DO BEGIN
    indCat = WHERE((varNfLf[indVarNfLfFin] GT anom_breaks[i]) AND (varNfLf[indVarNfLfFin] LE anom_breaks[i+1]),countCat) 
    NfLF_cat[indVarNfLfFin[indCat]] = anom_cat[i]
  ENDFOR
  indCat = 0
  ;check that the numebr of finite pixels in the anomalies is equal to the number of pixels with cat GE 0
  ind = WHERE(NfLF_cat GE anom_cat[0], countCatFin)
  IF (countVarNfLfFin NE countCatFin) THEN STOP 
  ind  = 0
ENDIF


;;test with ecdf (just to see how it looks like)
;;regular subsample (every 200)
;indFin = WHERE(FINITE(varNfLf))
;XYecdfVarNfLf = ecdf(varNfLf[indfin[0:-1:200]])

;In a loop
;variable to store the difference between anonalies at different consolidation stage and for different targets
;var for the mean abs diff 
mnAbsDiffVarNxLxf_varNfLf = FLTARR(5,2,3) ;row: 0 = NxLf, 1 = NxLx. Dimension 3 is for: all pixels/vegetated/vegetated and active 
;var for mean difference
mnDiffVarNxLxf_varNfLf = FLTARR(5,2,3) ;row: 0 = NxLf, 1 = NxLx. Dimension 3 is for: all pixels/vegetated/vegetated and active
;of an anomoly made with NxLy, the percent of pixels missing (they are NaN) with respect of NfLf. It is the % of all valid NfLf pixels 
prctMissInNiLjWrNfLf = FLTARR(5,2,3)
;of an anomoly made with NxLy, the percent of pixels missing (they are NaN) in NfLf. It is the % of all valid NxLy pixels
prctMissInNfLfWrNiLj = FLTARR(5,2,3)

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
veg_act_stats = CREATE_STRUCT('mc_a',FLTARR(5,2),'mc_HSS',FLTARR(5,2),'mc_rank_a',FLTARR(5,2),'mc_rank_mm',FLTARR(5,2),'mc_rank_m',FLTARR(5,2),'mc_rank_sm',FLTARR(5,2),'mc_rank_um',FLTARR(5,2),$
                              'd_a',FLTARR(5,2),'d_bias',FLTARR(5,2), 'd_DetRate', FLTARR(5,2), 'd_FalseAlRate', FLTARR(5,2), 'd_HSS', FLTARR(5,2)) ;row: 0 = NxLf, 1 = NxLx
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
        
    ;all pixels
    mnAbsDiffVarNxLxf_varNfLf[iNRT,jLTA,0] = MEAN(absDelta, /NAN)
    mnDiffVarNxLxf_varNfLf[iNRT,jLTA,0] = MEAN(Delta, /NAN)
    ;see how many are present in NfLf but missing in NiLj
    prctMissInNiLjWrNfLf[iNRT,jLTA,0] = TOTAL(~FINITE(varNiLj[indVarNfLfFin]))/FLOAT(countVarNfLfFin)*100.
    PRINT, 'Present in NfHf but missing in ' + NL_names[iNRT,jLTA] + '(all pixels): ' + STRTRIM(TOTAL(~FINITE(varNiLj[indVarNfLfFin])),2)
    ;see how many are present in NiLj but missing in NfLf
    indVarNiLjFin = WHERE(FINITE(varNiLj), countVarNiLjFin)
    prctMissInNfLfWrNiLj[iNRT,jLTA,0] = TOTAL(~FINITE(varNfLf[indVarNiLjFin]))/FLOAT(countVarNiLjFin)*100.0
    indVarNiLjFin = 0
    
    
    ;only those vegetated
    mnAbsDiffVarNxLxf_varNfLf[iNRT,jLTA,1] = MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]), /NAN)
    mnDiffVarNxLxf_varNfLf[iNRT,jLTA,1] = MEAN(Delta*REBIN(veg, sz[1], sz[2], sz[3]), /NAN)
    indVarNfLfVegFin = WHERE(FINITE(varNfLf*REBIN(veg, sz[1], sz[2], sz[3])), countVarNfLfVegFin)
    prctMissInNiLjWrNfLf[iNRT,jLTA,1] = TOTAL(~FINITE(varNiLj[indVarNfLfVegFin]))/FLOAT(countVarNfLfVegFin)*100.0
    PRINT, 'Present in NfHf but missing in ' + NL_names[iNRT,jLTA] + '(Veg): ' + STRTRIM(TOTAL(~FINITE(varNiLj[indVarNfLfVegFin])),2)
    indVarNiLjVegFin = WHERE(FINITE(varNiLj*REBIN(veg, sz[1], sz[2], sz[3])), countVarNiLjVegFin)
    prctMissInNfLfWrNiLj[iNRT,jLTA,1] = TOTAL(~FINITE(varNfLf[indVarNiLjVegFin]))/FLOAT(countVarNiLjVegFin)*100.0
    indVarNiLjVegFin = 0
    
        
    ;only those vegetated and at time when they are active
    mnAbsDiffVarNxLxf_varNfLf[iNRT,jLTA,2] = MEAN(absDelta*active, /NAN)
    mnDiffVarNxLxf_varNfLf[iNRT,jLTA,2] = MEAN(Delta*active, /NAN)
    Delta = 0
    indVarNfLfVegActFin = WHERE(FINITE(varNfLf*active), countVarNfLfVegActFin)
    prctMissInNiLjWrNfLf[iNRT,jLTA,2] = TOTAL(~FINITE(varNiLj[indVarNfLfVegActFin]))/FLOAT(countVarNfLfVegActFin)*100.0
    PRINT, 'Present in NfHf but missing in ' + NL_names[iNRT,jLTA] + '(Veg Act): ' + STRTRIM(TOTAL(~FINITE(varNiLj[indVarNfLfVegActFin])),2)
    indVarNiLjVegActFin = WHERE(FINITE(varNiLj*active), countVarNiLjVegActFin)
    prctMissInNfLfWrNiLj[iNRT,jLTA,2] = TOTAL(~FINITE(varNfLf[indVarNiLjVegActFin]))/FLOAT(countVarNiLjVegActFin)*100.0
    indVarNiLjVegActFin  = 0
    
    CASE variable OF    ;decide binsize
      'z': BEGIN
        bnsz = 0.5
        minmaxX =  [MIN(varNiLj*active,/NAN), MAX(varNiLj*active,/NAN)]
        minmaxY = [MIN(varNfLf*active,/NAN), MAX(varNfLf*active,/NAN)]
        nbins = 50
        END
      'v': BEGIN
        bnsz = 10
        minmaxX =  [MIN(varNiLj*active,/NAN), MAX(varNiLj*active,/NAN)]
        minmaxY = [MIN(varNfLf*active,/NAN), MAX(varNfLf*active,/NAN)]
        nbins = 50
        END
      'n': BEGIN
        bnsz = 10
        minmaxX =  [0,100]
        minmaxY = [0,100]
        nbins = 10
        END
    ENDCASE
    
    pdf = HISTOGRAM(varNiLj*active, BINSIZE=bnsz, LOCATIONS = xbin, /NAN)
    locLT0 = WHERE(varNiLj*active LT 0, nLT0)
    locGT100 = WHERE(varNiLj*active GT 100, nGT100)
    locLT0 = 0 & locGT100 = 0
    tmp = WHERE(FINITE(varNiLj*active), n)
    tmp = 0
    mystring = ', %LT0 = ' + STRING(nLT0/FLOAT(n)*100, FORMAT='(F6.3)') +  ', %GT100 = ' + STRING(nGT100/FLOAT(n)*100, FORMAT='(F6.3)')
    ghHisto = PLOT(xbin, pdf, TITLE = 'Act. veg., Hist. of ' + variable + NL_names[iNRT,jLTA]+ mystring, $
      XTITLE=variable + NL_names[iNRT,jLTA], YTITLE='Frequency', /BUFFER)
    ghHisto.save, dir_results + dirsep + variable + '_'+NL_names[iNRT,jLTA]+'_histo.png'
    ghHisto.close
    ;make a density plot of vNfLf agains vNiLj-vNfLf
;    IF (variable EQ 'v') THEN BEGIN
;      ;only for veg act for thr moment
;      DensityAndFit_log_scale, varNfLf*active, varNiLj*active-varNfLf*active, 'vNfLf', 'v'+NL_names[iNRT,jLTA]+' - vNfLf', dir_results, $
;                               [MIN(varNfLf*active,/NAN), MAX(varNfLf*active,/NAN)], [MIN(varNiLj*active-varNfLf*active,/NAN), MAX(varNiLj*active-varNfLf*active,/NAN)], $;data1_range, data2_range, 
;                               50, 30, TITLE='Act.veg', $;ngrid, nlevels, TITLE=title, $
;                               DOFIT=0, SIMPLE=1, DOLOG=1 ;DOFIT = dofit, FILESUFFIX = filesuffix, NOWIN=nowin, RGBTAB = rgbtab, SIMPLE = simple, DOLOG = dolog, $
;                               ;RANGE_IN = range_in, $ ;used with simple graph, set the min max values to be used (min max stretch), 2 elements vector
;                               ;RANGEINLOW = rangeinlow, $ ; ;used with simple graph, set only the min value (to one normally)
;                               ;RANGEOUT = rangeout, $; used with simple graph, store the used range for z, 2 elements vector
;                               ;SAVECSV = savecsv, $ ;1 to save a csv with data
;                               ;TOTALN = totaln ;total number of obs to be used in the csv (made for area and pheno)
;    ENDIF
    ;make the scatterplot ref vs NxLy
    ;only for veg and active
    ;test
    ;FOR nbins = 20, 45, 5 DO $
    nbins=50
    DensityAndFit_log_scale, REFORM(varNiLj*active, N_ELEMENTS(varNiLj*active)), REFORM(varNfLf*active, N_ELEMENTS(varNfLf*active)), variable+NL_names[iNRT,jLTA], variable+'NfHf', dir_results, $
      minmaxX, minmaxY,  $;data1_range, data2_range,
      nbins, 30, $;TITLE='Act.veg', $;ngrid, nlevels, TITLE=title, $
      DOFIT=1, SIMPLE=1, DOLOG=1, PLOT1TO1=1, FILESUFFIX = '_act_veg_nbins' + STRTRIM(nbins,2), NOWIN=1
;    DensityAndFit_log_scale, REFORM(varNiLj*active, N_ELEMENTS(varNiLj*active)),  REFORM(varNfLf*active, N_ELEMENTS(varNfLf*active)), variable+NL_names[iNRT,jLTA], variable+'NfHf', dir_results, $
;      [MIN(varNfLf*active,/NAN), MAX(varNfLf*active,/NAN)], [MIN(varNfLf*active,/NAN), MAX(varNfLf*active,/NAN)], $;data1_range, data2_range,
;      50, 30, TITLE='Act.veg', $;ngrid, nlevels, TITLE=title, $
;      DOFIT=1, SIMPLE=1, DOLOG=1, PLOT1TO1=1, FILESUFFIX = '_act_veg_ref_range', NOWIN=1
    IF (iNRT EQ 0) THEN BEGIN
      ;we are looking at N0. L can be 0 or f. This is a special interesting case in NRT, so we save more info
      !except=0
      ;IF (jLTA EQ 0) THEN N_L_name = 'N'+STRTRIM(iNRT,2)+'Lf' ELSE N_L_name = 'N'+STRTRIM(iNRT,2)+'L'+STRTRIM(iNRT,2)   
      fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_MAE_mean_abs_delta_veg'                
      res = write_envi_img(MEAN(absDelta*REBIN(veg, sz[1], sz[2], sz[3]),DIMENSION=3, /NAN), fn_base)
      res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
      fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_MAE_mean_abs_delta_veg_active'
      res = write_envi_img(MEAN(absDelta*active,DIMENSION=3, /NAN), fn_base)
      res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
      
      fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_ME_mean_delta_veg'
      res = write_envi_img(MEAN(Delta*REBIN(veg, sz[1], sz[2], sz[3]),DIMENSION=3, /NAN), fn_base)
      res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
      fn_base = dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_ME_mean_abs_delta_veg_active'
      res = write_envi_img(MEAN(Delta*active,DIMENSION=3, /NAN), dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_mean_delta_veg_active')
      res = write_envi_hdr(fn_base+'.hdr', sz[1], sz[2], 4)
      clear = CHECK_MATH() ;set back to normal
      !except=1
    ENDIF
    
    IF (variable EQ 'z') OR (variable EQ 'v') OR (variable EQ 'n') OR (variable EQ 's')THEN BEGIN
      ;DATA 4 CONFUSION MATRIX
      ;NiLj_cat = LONARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
      NiLj_cat = INTARR(sz[1], sz[2], sz[3]) - 999  ;set it -999 that will stay with NAN
      ;reclassify varNfLf in the WMO cathegories, wher it si FINITE
      indFin = WHERE(FINITE(varNiLj))
      FOR k = 0, N_ELEMENTS(anom_cat)-1 DO BEGIN
        indCat = WHERE((varNiLj[indFin] GT anom_breaks[k]) AND (varNiLj[indFin] LE anom_breaks[k+1]),countCat)
        NiLj_cat[indFin[indCat]] = anom_cat[k]
      ENDFOR
      indFin = 0 & indCat = 0
      ;NiLj veg
      cm = FLTARR(N_ELEMENTS(anom_cat), N_ELEMENTS(anom_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
      ;TEST
      PRINT, variable + '_' + NL_names[iNRT,jLTA]
      tmpI = WHERE(NfLF_cat*active GE 0, countTmpI)
      PRINT, '(Act Vegetated) Number of pixel with NFHF in any cathegory: ' + STRTRIM(countTmpI,2)
      tmpI = WHERE(NiLj_cat*active GE 0, countTmpI)
      PRINT, '(Act Vegetated) Number of pixel with ' + NL_names[iNRT,jLTA] + ' in any cathegory: ' + STRTRIM(countTmpI,2)
      ;END TEST
      IF (plot_only_act_veg  NE 1) THEN BEGIN
        FOR k = 0, N_ELEMENTS(anom_cat)-1 DO BEGIN
          ;located indexes of category k on reference
          indCatI = WHERE((NfLF_cat*REBIN(veg, sz[1], sz[2], sz[3])) EQ anom_cat[k], countCatI)
          ;quick chec
          ;IF (countCatI EQ 0) THEN STOP
          ;now see how were these pixels classified in the estimated z, note that I might have a number of missing values
          cm[k,*] = HISTOGRAM(REFORM(NiLj_cat[indCatI]), MIN=anom_cat[0], MAX=anom_cat[-1], BINSIZE = 1, LOCATIONS = loc)
          ;print, 'debug'
        ENDFOR
        res = confusion_mat_stats(cm, variable, variable + '_' + NL_names[iNRT,jLTA], anom_cat_str, anom_cat_num_str, dir_results + dirsep + variable + '_' + NL_names[iNRT,jLTA] + '_veg_confusion_matrix_stat.csv')
      ENDIF
      ;veg & act
      cm = FLTARR(N_ELEMENTS(anom_cat), N_ELEMENTS(anom_cat)) ;on columns I have the ref dataset (NfLf) and on rows the estimation (N0Lf)
      FOR k = 0, N_ELEMENTS(anom_cat)-1 DO BEGIN
        ;located indexes of category k on reference
        indCatI = WHERE((NfLF_cat*active) EQ anom_cat[k], countCatI)
        ;quick chec
        ;IF (countCatI EQ 0) THEN STOP
        ;now see how were these pixels classified in the estimated z
        cm[k,*] = HISTOGRAM(REFORM(NiLj_cat[indCatI]), MIN=anom_cat[0], MAX=anom_cat[-1], BINSIZE = 1, LOCATIONS = loc)
      ENDFOR
      IF (iNRT EQ 0)THEN BEGIN
        CASE variable OF
          'z': an_cat = anom_cat[2]
          'n': an_cat = anom_cat[1]
          'v': an_cat = anom_cat[2]
        ENDCASE
        ;save hss, detection rate and false alarm rate by pixel
        tmp_Hits = NiLj_cat * !VALUES.F_NAN
        tmp_Miss = NiLj_cat * !VALUES.F_NAN
        tmp_CN = NiLj_cat * !VALUES.F_NAN
        tmp_FA = NiLj_cat * !VALUES.F_NAN
        ;I have bad event when cat le 2 
        ;compute hits (make a matrix where I have 1 in case of hit, 0 otherwise 
        ind_Hits = WHERE(((NfLF_cat*active) LE an_cat) AND ((NfLF_cat*active) GE 0) AND (NiLj_cat LE an_cat))
        tmp_Hits[ind_Hits] = 1
        ;compute miss
        ind_Miss = WHERE(((NfLF_cat*active) LE an_cat) AND ((NfLF_cat*active) GE 0) AND (NiLj_cat GT an_cat))
        tmp_Miss[ind_Miss] = 1
        ;compute correct neg
        ind_CN = WHERE(((NfLF_cat*active) GT an_cat) AND ((NfLF_cat*active) GE 0) AND (NiLj_cat GT an_cat))
        tmp_CN[ind_CN] = 1
        ;compute false alarms
        ind_FA = WHERE(((NfLF_cat*active) GT an_cat) AND ((NfLF_cat*active) GE 0) AND (NiLj_cat LE an_cat))
        tmp_FA[ind_FA] = 1   
        ;detection rate
        nHits = TOTAL(tmp_Hits, 3, /NAN) 
        nMiss = TOTAL(tmp_Miss, 3, /NAN)
        nFA = TOTAL(tmp_FA, 3, /NAN)
        nCN = TOTAL(tmp_CN, 3, /NAN)
        
        det_Rate =   nHits / FLOAT(nHits + nMiss)*100
        false_Rate = nFA   / FLOAT(nFA + nHits)*100
        n = FLOAT(nHits + nMiss + nFA + nCN)
        ecr = 1.0/n *((nHits+nMiss)*(nHits+nFA) + (nCN+nMiss)*(nCN+nFA))
        hss = ((nHits+nCN)-ecr)/FLOAT(n-ecr)
        
        res = write_envi_img(det_Rate, dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_DETECTION_RATE.img')
        res = write_envi_hdr(dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_DETECTION_RATE.hdr', sz[1], sz[2], 4)
                
        res = write_envi_img(false_Rate, dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_FALSE_ALARM_RATE.img')
        res = write_envi_hdr(dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_FALSE_ALARM_RATE.hdr', sz[1], sz[2], 4)
        
        res = write_envi_img(hss, dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_HSS.img')
        res = write_envi_hdr(dir_results + dirsep + variable + NL_names[iNRT,jLTA] + '_veg_active_HSS.hdr', sz[1], sz[2], 4)
        ind_Hits = 0
        tmp_Hits =  0
        ind_Miss =  0
        tmp_Miss =  0
        ind_CN =  0
        tmp_CN =  0
        ind_FA =  0
        tmp_FA =  0
        nHits = 0
        nMiss = 0
        nFA = 0
        nCN = 0
        det_Rate =  0
        false_Rate = 0
        n = 0
        ecr = 0
        hss = 0
      ENDIF
      res = 0 
      res = confusion_mat_stats(cm, variable, variable + '_' + NL_names[iNRT,jLTA], anom_cat_str, anom_cat_num_str, dir_results + dirsep + variable + '_' + NL_names[iNRT,jLTA] + '_veg_active_confusion_matrix_stat.csv')
      veg_act_stats.mc_a[iNRT,jLTA] = res.mc_oa
      veg_act_stats.mc_rank_a[iNRT,jLTA] = res.mc_rank_a
      veg_act_stats.mc_rank_mm[iNRT,jLTA] = res.mc_rank_mm
      veg_act_stats.mc_rank_m[iNRT,jLTA] = res.mc_rank_m
      veg_act_stats.mc_rank_sm[iNRT,jLTA] = res.mc_rank_sm
      veg_act_stats.mc_rank_um[iNRT,jLTA] = res.mc_rank_um
      veg_act_stats.mc_HSS[iNRT,jLTA] = res.mc_hss
      veg_act_stats.d_a[iNRT,jLTA] = res.d_a
      veg_act_stats.d_bias[iNRT,jLTA] = res.d_bias
      veg_act_stats.d_DetRate[iNRT,jLTA] = res.d_dr
      veg_act_stats.d_FalseAlRate[iNRT,jLTA] = res.d_far
      veg_act_stats.d_HSS[iNRT,jLTA] = res.d_hss
      N0LF_cat = 0 & cm = 0
    ENDIF
    absDelta = 0
    varNxLf = 0
    PRINT, 'Mem: ' + STRTRIM(memory(/Current)/1000000.0,2)
  ENDFOR
ENDFOR



;SAVE, mnAbsDiffVarNxLxf_varNfLf, fn_varNxLxf, variable, $
;      FILENAME = dir + '\savevar.sav'

;RESTORE, FILENAME='\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\Z\savevar.sav'

;make the simple graph
;plot_only_act_veg = 1
targetList = [!NULL]
fs_axis = 18
fs_legend = 12
marg = [0.2,0.2,0.1,0.1]
;abs diff
;gh1vegact = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,0,2], SYMBOL= 'o', NAME = variable + 'NxHf_actVeg', $ ; LINESTYLE = ':', COLOR= 'green',
gh1vegact = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,0,2], SYMBOL= 'o', NAME = variable + 'NxHf', $ ; LINESTYLE = ':', COLOR= 'green',
                 XTITLE='Consolidation Stage x', YTITLE = 'MAE ('+ variable + 'NxHy - '+ variable + 'NfHf)', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, MARGIN = marg)
targetList = [targetList, gh1vegact]
gh2vegact = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,1,2], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx', /OVERPLOT)
targetList = [targetList, gh2vegact]
IF (plot_only_act_veg EQ 0) THEN BEGIN 
  gh1 = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,0,0], SYMBOL= 'o', LINESTYLE = ':', NAME = variable + 'NxHf_all', /OVERPLOT)
  targetList = [targetList, gh1]
  gh2 = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,1,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHx_all', /OVERPLOT)
  targetList = [targetList, gh2]
  gh1veg = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,0,1], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'red', NAME =  variable + 'NxHf_veg', /OVERPLOT)
  targetList = [targetList, gh1veg]
  gh2veg = PLOT(mnAbsDiffVarNxLxf_varNfLf[*,1,1], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx_veg', /OVERPLOT)
  targetList = [targetList, gh2veg]
ENDIF
ghl = LEGEND(TARGET=targetList, SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.85,0.85], FONT_SIZE = fs_legend)
gh1vegact.save, dir_results + dirsep + variable + '_mean_abs_delta.png'
gh1vegact.close
;diff
targetList = [!NULL]
;gh1vegact = PLOT(mnDiffVarNxLxf_varNfLf[*,0,2], SYMBOL= 'o', NAME = variable + 'NxHf_actVeg', $ ;, LINESTYLE = ':', COLOR= 'green'
  gh1vegact = PLOT(mnDiffVarNxLxf_varNfLf[*,0,2], SYMBOL= 'o', NAME = variable + 'NxHf', $ ;, LINESTYLE = ':', COLOR= 'green'
                 XTITLE='Consolidation Stage x', YTITLE = 'ME ('+ variable + 'NxHy - '+ variable + 'NfHf)', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, MARGIN =marg)
targetList = [targetList, gh1vegact]
gh2vegact = PLOT(mnDiffVarNxLxf_varNfLf[*,1,2], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx', /OVERPLOT)
targetList = [targetList, gh2vegact]
IF (plot_only_act_veg EQ 0) THEN BEGIN 
  gh1 = PLOT(mnDiffVarNxLxf_varNfLf[*,0,0], SYMBOL= 'o', LINESTYLE = ':', NAME = variable + 'NxHf_all', /OVERPLOT)
  targetList = [targetList, gh1]
  gh2 = PLOT(mnDiffVarNxLxf_varNfLf[*,1,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHx_all', /OVERPLOT)
  targetList = [targetList, gh2]
  gh1veg = PLOT(mnDiffVarNxLxf_varNfLf[*,0,1], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'red', NAME =  variable + 'NxHf_veg', /OVERPLOT)
  targetList = [targetList, gh1veg]
  gh2veg = PLOT(mnDiffVarNxLxf_varNfLf[*,1,1], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx_veg', /OVERPLOT)
  targetList = [targetList, gh2veg]
ENDIF
ghl = LEGEND(TARGET=targetList, SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.9,0.85], FONT_SIZE = fs_legend)
gh1vegact.save, dir_results + dirsep + variable + '_mean_delta.png'
gh1vegact.close

;make the graph of missing (missing in NiLj and present in NfLf)
gh1 = PLOT(prctMissInNiLjWrNfLf [*,0,0], SYMBOL= 'o', LINESTYLE = ':', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', YTITLE = '% missing in NxHy but present in NfHf', XRANGE=[-0.5, 4.5])
gh2 = PLOT(prctMissInNiLjWrNfLf [*,1,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHx', /OVERPLOT)
gh1veg = PLOT(prctMissInNiLjWrNfLf [*,0,1], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'red', NAME =  variable + 'NxHf_veg', /OVERPLOT)
gh2veg = PLOT(prctMissInNiLjWrNfLf [*,1,1], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx_veg', /OVERPLOT)
gh1vegact = PLOT(prctMissInNiLjWrNfLf [*,0,2], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'green', NAME = variable + 'NxHf_actVeg', /OVERPLOT)
gh2vegact = PLOT(prctMissInNiLjWrNfLf [*,1,2], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'green', NAME = variable + 'NxHx_actVeg', /OVERPLOT)
ghl = LEGEND(TARGET=[gh1,gh1veg,gh1vegact,gh2,gh2veg, gh2vegact], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.9,0.85])
gh1.save, dir_results + dirsep + variable + '_miss_in_NxHy_pres_in_NfHf.png'
gh1.close

;make the graph of missing (missing in NfLf and present in NiLj)
gh1 = PLOT(prctMissInNiLjWrNfLf [*,0,0], SYMBOL= 'o', LINESTYLE = ':', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', YTITLE = '% missing in NxHy but present in NfHf', XRANGE=[-0.5, 4.5])
gh2 = PLOT(prctMissInNiLjWrNfLf [*,1,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHx', /OVERPLOT)
gh1veg = PLOT(prctMissInNiLjWrNfLf [*,0,1], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'red', NAME =  variable + 'NxHf_veg', /OVERPLOT)
gh2veg = PLOT(prctMissInNiLjWrNfLf [*,1,1], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'red', NAME = variable + 'NxHx_veg', /OVERPLOT)
gh1vegact = PLOT(prctMissInNiLjWrNfLf [*,0,2], SYMBOL= 'o', LINESTYLE = ':', COLOR= 'green', NAME = variable + 'NxHf_actVeg', /OVERPLOT)
gh2vegact = PLOT(prctMissInNiLjWrNfLf [*,1,2], SYMBOL= 'o', LINESTYLE = '-', COLOR= 'green', NAME = variable + 'NxHx_actVeg', /OVERPLOT)
ghl = LEGEND(TARGET=[gh1,gh1veg,gh1vegact,gh2,gh2veg, gh2vegact], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.9,0.85])
gh1.save, dir_results + dirsep + variable + '_miss_in_NfHf_pres_in_NfHf.png'
gh1.close

;make the dichotomous estimation graphics
IF (variable EQ 'z') OR (variable EQ 'v') OR (variable EQ 'n') OR (variable EQ 's')THEN BEGIN
  CASE variable OF
    'z': titleString = 'class z<=-1 (moderately to extremely bad)'
    'v': titleString = 'Class VCI<=35 (moderate to extreme drought)'
    's': titleString = 'Class S<=-1 (moderately to extremely bad)'
    'n': titleString = 'Class NEP<=15.8655 (moderately to extremely bad)'
  ENDCASE
  
  posleg = [0.5,0.94]
  ;dichotomous and multicat estimation graphics, now one by one
  gh1 =  PLOT(veg_act_stats.d_DetRate[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
              YTITLE = 'Detection Rate [0,100]', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis);, LAYOUT=[3,2,1]
  gh1b = PLOT(veg_act_stats.d_DetRate[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT) 
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0)
  ghl = LEGEND(TARGET=[gh1, gh1b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh1.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_detect_rate.png'
  gh1.close
  
  gh2 =  PLOT(veg_act_stats.d_FalseAlRate[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'False Alarm Rate [0,100]', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis); , LAYOUT=[3,2,2], /CURRENT
  gh2b = PLOT(veg_act_stats.d_FalseAlRate[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_falsAlarm_rate.png'
  gh2.close
  
  ;detection and false in one
  gh1 =  PLOT(veg_act_stats.d_DetRate[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf DR', XTITLE='Consolidation Stage x', $
    YTITLE = 'DR and FAR', XRANGE=[-0.5, 4.5], YRANGE=[0,100], FONT_SIZE = fs_axis);, LAYOUT=[3,2,1]
  gh1b = PLOT(veg_act_stats.d_DetRate[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx DR', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  gh2 =  PLOT(veg_act_stats.d_FalseAlRate[*,0], SYMBOL= 'Triangle', LINESTYLE = ':', COLOR='black',NAME = variable + 'NxHf FAR', XTITLE='Consolidation Stage x', $
    /OVERPLOT); , LAYOUT=[3,2,2], /CURRENT
  gh2b = PLOT(veg_act_stats.d_FalseAlRate[*,1], SYMBOL= 'Triangle', LINESTYLE = ':', COLOR = 'red', NAME = variable + 'NxHx FAR', /OVERPLOT)
  ghl = LEGEND(TARGET=[gh1, gh1b, gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', HORIZONTAL_SPACING=0.03,VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh1.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_falseANDdetect_rate.png'
  gh1.close
  
  gh2 =  PLOT(veg_act_stats.d_bias[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'Bias', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, YRANGE=[0,140]) ; , LAYOUT=[3,2,3], /CURRENT
  gh2b = PLOT(veg_act_stats.d_bias[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_bias.png'
  gh2.close
  
  gh2 =  PLOT(veg_act_stats.d_HSS[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'HSS', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, YRANGE=[0,1]) ; , LAYOUT=[3,2,4], /CURRENT
  gh2b = PLOT(veg_act_stats.d_HSS[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_hss.png'
  gh2.close
  
  gh2 =  PLOT(veg_act_stats.d_a[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'Overall Accuracy', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, YRANGE=[0,100]) ;, LAYOUT=[3,2,5], /CURRENT
  gh2b = PLOT(veg_act_stats.d_a[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_dicho_stats_OA.png'
  gh2.close
  
  gh2 =  PLOT(veg_act_stats.mc_a[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'Overall Accuracy', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, YRANGE=[0,100])  ;, LAYOUT=[3,2,6], /CURRENT
  gh2b = PLOT(veg_act_stats.mc_a[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_multicat_stats_OA.png'
  gh2.close
  
  gh2 =  PLOT(veg_act_stats.mc_HSS[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
    YTITLE = 'HSS', XRANGE=[-0.5, 4.5], FONT_SIZE = fs_axis, YRANGE=[0,1])  ;, LAYOUT=[3,2,6], /CURRENT
  gh2b = PLOT(veg_act_stats.mc_HSS[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
  ;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0, FONT_SIZE = fs_axis)
  ghl = LEGEND(TARGET=[gh2, gh2b], SHADOW = 0, TRANSPARENCY = 100, POSITION = posleg, ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  gh2.save, dir_results + dirsep + 'BB_' + variable + '_multicat_stats_hss.png'
  gh2.close
  
  
  ;now the mismatch graph
  ;make HxLf first
  a = veg_act_stats.mc_rank_a[*,0]
  mm = a + veg_act_stats.mc_rank_mm[*,0]
  m = mm + veg_act_stats.mc_rank_m[*,0]
  sm = m + veg_act_stats.mc_rank_sm[*,0]
  um = sm + veg_act_stats.mc_rank_um[*,0]
  marg = [0.15, 0.35, 0.05, 0.15]
  b1 = BARPLOT(a, LAYOUT=[1,2,1],  FILL_COLOR='#2c7bb6', YRANGE=[0,100], XRANGE=[-0.6,4.6], NAME = 'A', $
       MARGIN = marg, $;TITLE = 'Multi-category agreement, ' + variable + 'NxHf',  MARGIN = marg, $
       XTITLE='Consolidation stage x', YTITLE='%',FONT_SIZE = fs_axis)
  text = TEXT(0.55, 0.95, 'Multi-category agreement, ' + variable + 'NxHf', ALIGNMENT = 0.5, FONT_SIZE = 18)
  b2 = BARPLOT(mm, BOTTOM_VALUES=a, FILL_COLOR='#abd9e9', /OVERPLOT, NAME = 'MM')
  b3 = BARPLOT(m, BOTTOM_VALUES=mm, FILL_COLOR='#ffffbf', /OVERPLOT, NAME = 'M')
  b4 = BARPLOT(sm, BOTTOM_VALUES=m, FILL_COLOR='#fdae61', /OVERPLOT, NAME = 'SM')
  b5 = BARPLOT(um, BOTTOM_VALUES=sm, FILL_COLOR='#d7191c', /OVERPLOT, NAME = 'UM')
  ghl = LEGEND(TARGET=[b1, b2, b3, b4, b5], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.5,0.53], ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  ;now HxLx
  a = veg_act_stats.mc_rank_a[*,1]
  mm = a + veg_act_stats.mc_rank_mm[*,1]
  m = mm + veg_act_stats.mc_rank_m[*,1]
  sm = m + veg_act_stats.mc_rank_sm[*,1]
  um = sm + veg_act_stats.mc_rank_um[*,1]
  marg = [0.15, 0.33, 0.05, 0.17]
  b1b = BARPLOT(a, LAYOUT=[1,2,2],  FILL_COLOR='#2c7bb6', YRANGE=[0,100], XRANGE=[-0.6,4.6], /CURRENT, $
    MARGIN = marg, $;TITLE = 'Multi-category agreement, ' + variable + 'NxHx', MARGIN = marg, $
    XTITLE='Consolidation stage x', YTITLE='%', FONT_SIZE = fs_axis)
  text = TEXT(0.55, 0.44, 'Multi-category agreement, ' + variable + 'NxHx', ALIGNMENT = 0.5, FONT_SIZE = 18)
  b2b = BARPLOT(mm, BOTTOM_VALUES=a, FILL_COLOR='#abd9e9', /OVERPLOT)
  b3b = BARPLOT(m, BOTTOM_VALUES=mm, FILL_COLOR='#ffffbf', /OVERPLOT)
  b4b = BARPLOT(sm, BOTTOM_VALUES=m, FILL_COLOR='#fdae61', /OVERPLOT)
  b5b = BARPLOT(um, BOTTOM_VALUES=sm, FILL_COLOR='#d7191c', /OVERPLOT)
  b1.save, dir_results + dirsep + 'BB_' + variable + '_multicat_agreement.png'
  b1.close
ENDIF
PRINT,'ok'
TOC
END



;dico graph all toghether
;gh1 =  PLOT(veg_act_stats.d_DetRate[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'Detection Rate [0,100]', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,1])
;gh1b = PLOT(veg_act_stats.d_DetRate[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;CASE variable OF
;  'z': titleString = 'Active veg, class z<=-1 (moderately to extremely bad)'
;  'v': titleString = 'Active veg, class VCI<=35 (moderate to extreme drought)'
;  's': titleString = 'Active veg, class S<=-1 (moderately to extremely bad)'
;  'n': titleString = 'Active veg, class NEP<=15.8655 (moderately to extremely bad)'
;ENDCASE
;otitle = TEXT(0.5, 0.99, titleString, ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1.0)
;
;gh2 =  PLOT(veg_act_stats.d_FalseAlRate[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'False Alarm Rate [0,100]', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,2], /CURRENT)
;gh2b = PLOT(veg_act_stats.d_FalseAlRate[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;
;gh2 =  PLOT(veg_act_stats.d_bias[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'Bias [0,+Inf]', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,3], /CURRENT)
;gh2b = PLOT(veg_act_stats.d_bias[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;
;gh2 =  PLOT(veg_act_stats.d_HSS[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'Heidke skill (k)[-1,1]', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,4], /CURRENT)
;gh2b = PLOT(veg_act_stats.d_HSS[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;
;gh2 =  PLOT(veg_act_stats.d_a[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'Overall Accuracy (dicho)', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,5], /CURRENT)
;gh2b = PLOT(veg_act_stats.d_a[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;
;gh2 =  PLOT(veg_act_stats.mc_a[*,0], SYMBOL= 'o', LINESTYLE = '-', NAME = variable + 'NxHf', XTITLE='Consolidation Stage x', $
;  YTITLE = 'Overall Accuracy (multicat)', XRANGE=[-0.5, 4.5], LAYOUT=[3,2,6], /CURRENT)
;gh2b = PLOT(veg_act_stats.mc_a[*,1], SYMBOL= 'o', LINESTYLE = '-', COLOR = 'red', NAME = variable + 'NxHx', /OVERPLOT)
;
;ghl = LEGEND(TARGET=[gh1, gh1b], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.5,0.5], ORIENTATION = 1, HORIZONTAL_ALIGNMENT = 1, VERTICAL_ALIGNMENT = 1)
;gh1.save, dir_results + dirsep + variable + '_dicho_stats.png'
