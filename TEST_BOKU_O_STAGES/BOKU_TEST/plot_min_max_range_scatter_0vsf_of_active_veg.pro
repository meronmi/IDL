PRO Plot_min_max_range_scatter_0vsF_of_active_veg
minF = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\OF_dek__min.img')
maxF = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\OF_dek__max.img')
min0 = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\O0_dek__min.img')
max0 = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\LTA_STATS\O0_dek__max.img')
sz = SIZE(minF)
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
minF =  minF * active
maxF = maxF * active
min0 = min0 * active
max0 = max0 * active
;get the finite values and check they are all the same
ind_minF =  WHERE(FINITE(minF), countFin)
ind_maxF = WHERE(FINITE(maxF))
ind_min0 = WHERE(FINITE(min0))
ind_max0 = WHERE(FINITE(max0))

IF (TOTAL(ind_minF-ind_min0) + TOTAL(ind_maxF-ind_max0)) NE 0 THEN STOP
IF (TOTAL(ind_minF-ind_maxF) + TOTAL(ind_min0-ind_max0)) NE 0 THEN STOP
IF (MAX(3*ind_minF-ind_maxF-ind_min0-ind_max0)) NE 0 THEN STOP
minF =  minF[ind_minF]
min0 = min0[ind_min0]
maxF = maxF[ind_maxF]
max0 = max0[ind_max0]

; compute the range
rangeF = maxF - minF
range0 = max0 - min0


rangemin = [MIN([minF,min0]), MAX([minF,min0])]         
DensityAndFit_log_scale, minF, min0, 'min dekadal NDVI OF', 'min dekadal NDVI O0', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results', $
                         rangemin, rangemin, 50, 30, TITLE='Vegetated and active', $;data1_range, data2_range, ngrid, nlevels, TITLE=title, $
                         DOFIT = 1, FILESUFFIX = 'dek_min_scatter', NOWIN=0, RGBTAB = rgbtab, SIMPLE = 1, DOLOG = 1, PLOT1TO1 =1 ;DOFIT = dofit, FILESUFFIX = filesuffix, NOWIN=nowin, RGBTAB = rgbtab, SIMPLE = simple, DOLOG = dolog, $ 

rangemax = [MIN([maxF,max0]), MAX([maxF,max0])]  
;rangemax = [0,0.1]                       
DensityAndFit_log_scale, maxF, max0, 'max dekadal NDVI OF', 'max dekadal NDVI O0', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results', $
                         rangemax, rangemax, 50, 30, TITLE='Vegetated and active', $;data1_range, data2_range, ngrid, nlevels, TITLE=title, $
                         DOFIT = 1, FILESUFFIX = 'dek_max_scatter', NOWIN=0, RGBTAB = rgbtab, SIMPLE = 1, DOLOG = 1, PLOT1TO1 = 1;DOF

 rangemax = [MIN([rangeF,range0]), MAX([rangeF,range0])]
 ;rangemax = [0,0.1]
 DensityAndFit_log_scale, rangeF, range0, 'range dekadal NDVI OF', 'range dekadal NDVI O0', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results', $
                          rangemax, rangemax, 50, 30, TITLE='Vegetated and active', $;data1_range, data2_range, ngrid, nlevels, TITLE=title, $
                          DOFIT = 1, FILESUFFIX = 'dek_range_scatter', NOWIN=0, RGBTAB = rgbtab, SIMPLE = 1, DOLOG = 1, PLOT1TO1 = 1;DOF

END


