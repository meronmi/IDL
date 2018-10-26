PRO extraction4Alan
;dir_mask = 'X:\Active Projects\FAO SOFI\pheno_copy_of_z\references\'
analyze_country_trend_v2_faster, 1, 'Africa'

END

PRO extract_from_ARC
threshMask = 50 ;minimum AFI value for consdering the pixel, 50 is 25 %
pLevel = 0.1

fn_africa_mask = '\\tsclient\X\Active Projects\FAO SOFI\pheno_copy_of_z\references\Africa_continent_raster.img'
gaulImage = ReadEnviWithHdr(fn_africa_mask)
fn_crop_mask = '\\ies\d5\asap\Pheno_year_by_year\References\mask_crop_afi_masked.img'
maskImage = ReadEnviWithHdr(fn_crop_mask)
;get the index on a 2D array to be analyzed
indTrgt = WHERE((gaulImage EQ 1) AND (maskImage GE threshMask), count)
countTarget = count
maskImage = 0
gaulImage = 0

;fn_rangeland_mask= 

;fn_season1_mask =
;fn_season2_mask =
fn_mkp1 =  '\\tsclient\X\Active Projects\FAO SOFI\pheno_copy_of_z\TREND_ADJ_4ARCGIS\BBB_l_adj_anomX100_1_MKp.img'
mkp = ReadEnviWithHdr(fn_mkp1)
indTargetFinite = WHERE(FINITE(mkp[indTrgt]))
indTargetFinite = indTrgt[indTargetFinite]
indTargetFiniteSig = WHERE(mkp[indTargetFinite] LE pLevel, countTargetFiniteSig)
indTargetFiniteSig = indTargetFinite[indTargetFiniteSig]
mkp = 0
;fn_mkp2_mask = 
fn_ts1 = '\\tsclient\X\Active Projects\FAO SOFI\pheno_copy_of_z\TREND_ADJ_4ARCGIS\BBB_l_adj_anomX100_1_TSslope.img'
ts = ReadEnviWithHdr(fn_ts1)
PRINT, 'AFRICA, crop, first season' 
PRINT, 'Mean of TS = ' + STRTRIM(MEAN(ts[indTargetFiniteSig], /NAN),2)
indPos = WHERE(ts[indTargetFiniteSig] GT 0, countPos)
indNeg = WHERE(ts[indTargetFiniteSig] LT 0, countNeg)
PRINT, '% of positve TS' + STRTRIM(countPos / FLOAT(countTargetFiniteSig) *100,2)
PRINT, '% of negative TS' + STRTRIM(countNeg / FLOAT(countTargetFiniteSig) *100,2)
;fn_ts2 = 
PRINT, 'Qui'
END
