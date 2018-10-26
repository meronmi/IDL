PRO find_extreme_cases
; find whe z est is bad and z obs is good (saveit as 1) and the other way around (save it as -1), sum them by pixels
; two resulting files counting how many time it happens
zest = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\Z\zN0L0_2003-2016_bsq.img')
zobs = ReadEnviWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\Z\zNfLf_2003-2016_bsq.img')  
zestName = 'zN0L0'
zobsName = 'zNfLf'

sz = SIZE(zest)
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
;I need it as a file
res = write_envi_img(active, '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\pheno\all_504bands_active.img')
res = write_envi_hdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\pheno\all_504bands_active.hdr', $
  sz[1], sz[2], 4,  NBANDS=sz[3], MAPINFO='{Geographic Lat/Lon, 1, 1, -180, 75, 0.1875, 0.1875, WGS-84, units=Degrees}')



;only for veg abd active
zest = TEMPORARY(zest)*active
zobs = TEMPORARY(zobs)*active

active = 0

;find zest bad and zobs good
extrCases = INTARR(sz[1], sz[2], sz[3])
totExtrCases1 = INTARR(sz[1], sz[2])
ind = WHERE((zest LT -2.0) AND (zobs GT 2.0))
extrCases[ind] = 1
totExtrCases1 = TOTAL(extrCases, 3, /NAN)
;find zest good and zobs bad
extrCases = INTARR(sz[1], sz[2], sz[3])
totExtrCases2 = INTARR(sz[1], sz[2])
ind = WHERE((zest GT 2.0) AND (zobs LT -2.0))
extrCases[ind] = 1
totExtrCases2 = TOTAL(extrCases, 3, /NAN)


res = write_envi_img(totExtrCases1, '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results\nTimes_' + zestName +'_veryBad_WHILE_'+zobsName + '_veryGood.img')
res = write_envi_hdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results\nTimes_' + zestName +'_veryBad_WHILE_'+zobsName + '_veryGood.hdr', $ 
      sz[1], sz[2], 4,  MAPINFO='{Geographic Lat/Lon, 1, 1, -180, 75, 0.1875, 0.1875, WGS-84, units=Degrees}')
      
res = write_envi_img(totExtrCases2, '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results\nTimes_' + zestName +'_veryGood_WHILE_'+zobsName + '_veryBad.img')
res = write_envi_hdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\Results\nTimes_' + zestName +'_veryGood_WHILE_'+zobsName + '_veryBad.hdr', $
  sz[1], sz[2], 4,  MAPINFO='{Geographic Lat/Lon, 1, 1, -180, 75, 0.1875, 0.1875, WGS-84, units=Degrees}')
      
END


