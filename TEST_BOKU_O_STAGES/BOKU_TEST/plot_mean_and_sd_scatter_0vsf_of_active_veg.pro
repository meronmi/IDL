PRO Plot_mean_and_sd_scatter_0vsF_of_active_veg
meanF = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\OF_dek__mean.img')
sdF = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\OF_dek__sd.img')
mean0 = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\O0_dek__mean.img')
sd0 = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\O0_dek__sd.img')
sz = SIZE(meanF)
active = FLTARR(sz[1], sz[2], 36)
FOR i = 1, 36 DO BEGIN
  tt = STRING(i, FORMAT = '(I02)')
  active[*,*,i-1] = FLOAT(ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\pheno\active\'+'active1962'+tt+'.img'))
ENDFOR
ind = WHERE(active NE 1)
active[ind] = !VALUES.F_NAN
;concatenate 14 years on z axis, in this way I have a full year (36 deks), repeated for 14 year
active =  [[[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]], $
  [[active]],[[active]],[[active]],[[active]],[[active]],[[active]],[[active]]]
ind = 0

;remove non veg and not active
meanF =  meanF * active
sdF = sdF * active
mean0 = mean0 * active
sd0 = sd0 * active
;get the finite values and check they are all the same
ind_meanF =  WHERE(FINITE(meanF), countFin)
ind_sdF = WHERE(FINITE(sdF))
ind_mean0 = WHERE(FINITE(mean0))
ind_sd0 = WHERE(FINITE(sd0))

IF (TOTAL(ind_meanF-ind_mean0) + TOTAL(ind_sdF-ind_sd0)) NE 0 THEN STOP

;remove observations with 0 sd (it means that the value is constant for that deks (it happens)
ind0SD0 = WHERE(sd0[ind_meanF] EQ 0.0, count0SD0)
IF (count0SD0 GT 0) THEN BEGIN
  PRINT, '% of pixels O0 with SD = 0 (will be excluded) = ', STRTRIM(count0SD0/FLOAT(countFin)*100,2) 
ENDIF
ind0SDF = WHERE(sdF[ind_meanF] EQ 0.0, count0SDF)
IF (count0SDF GT 0) THEN BEGIN
  PRINT, '% of pixels OF with SD = 0 (will be excluded) = ', STRTRIM(count0SDF/FLOAT(countFin)*100,2)
ENDIF
indBothSDsGT0 = WHERE((sd0[ind_meanF] GT 0.0) AND (sdF[ind_meanF] GT 0))
indBothSDsGT0 = ind_meanF[indBothSDsGT0]

rangeMean = [MIN([meanF[indBothSDsGT0],mean0[indBothSDsGT0]]), MAX([meanF[indBothSDsGT0],mean0[indBothSDsGT0]])]         
DensityAndFit_log_scale, meanF[indBothSDsGT0], mean0[indBothSDsGT0], 'mean dekadal NDVI OF', 'mean dekadal NDVI O0', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results', $
                         rangeMean, rangeMean, 50, 30, TITLE='Vegetated and active', $;data1_range, data2_range, ngrid, nlevels, TITLE=title, $
                         DOFIT = 1, FILESUFFIX = 'dek_mean_scatter', NOWIN=0, RGBTAB = rgbtab, SIMPLE = 1, DOLOG = 1, PLOT1TO1 =1 ;DOFIT = dofit, FILESUFFIX = filesuffix, NOWIN=nowin, RGBTAB = rgbtab, SIMPLE = simple, DOLOG = dolog, $ 

rangeSD = [MIN([sdF[indBothSDsGT0],sd0[indBothSDsGT0]]), MAX([sdF[indBothSDsGT0],sd0[indBothSDsGT0]])]  
;rangeSD = [0,0.1]                       
DensityAndFit_log_scale, sdF[indBothSDsGT0], sd0[indBothSDsGT0], 'SD dekadal NDVI OF', 'SD dekadal NDVI O0', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results', $
                         rangeSD, rangeSD, 50, 30, TITLE='Vegetated and active', $;data1_range, data2_range, ngrid, nlevels, TITLE=title, $
                         DOFIT = 1, FILESUFFIX = 'dek_sd_scatter', NOWIN=0, RGBTAB = rgbtab, SIMPLE = 1, DOLOG = 1, PLOT1TO1 = 1;DOF

END


