PRO build_NDVI_mean_when_active
X = ReadBilWithHdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\OF2003-2016_bil')
GAIN = 0.0048
OFFSET = -0.2
MINVAL = 0
MAXVAL = 250
ind = WHERE(X LT minval, count)
IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
ind = WHERE(X GT maxval, count)
IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
X = TEMPORARY(X) * gain + offset
sz = SIZE(X)
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

xMeanAct = MEAN(X*active, DIMENSION=3, /NAN)
sz = SIZE(xMeanAct)
res = write_envi_img(xMeanAct, 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\Results\OFmeanNDVI_veg_active.img')
res = write_envi_hdr('X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\Results\OFmeanNDVI_veg_active.hdr', $ 
      sz[1], sz[2], 4,  MAPINFO='{Geographic Lat/Lon, 1, 1, -180, 75, 0.1875, 0.1875, WGS-84, units=Degrees}')
END


