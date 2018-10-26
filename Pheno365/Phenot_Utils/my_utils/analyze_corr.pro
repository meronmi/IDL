PRO analyze_corr
ns = 7841
nl = 1458
; corr = correlationb image
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_corr.img'
OPENR, lun, filein, /GET_LUN
corr=FLTARR(ns, nl)
READU, lun, corr
FREE_LUN, lun
; peak = maxval image
filein= 'X:\WA corr analyis GIS\sahel resuts\maxv\maxv1VSacc1_corr.img'
OPENR, lun, filein, /GET_LUN
peak=FLTARR(ns, nl)
READU, lun, peak
FREE_LUN, lun
; len = length image
filein= 'X:\WA corr analyis GIS\sahel resuts\len\len1VSacc1_corr.img'
OPENR, lun, filein, /GET_LUN
len=FLTARR(ns, nl)
READU, lun, len
FREE_LUN, lun
; pvalCorr = pvalCorr image
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalCorr=FLTARR(ns, nl)
READU, lun, pvalCorr
FREE_LUN, lun
; pvalPeak = pvalPeak image
filein= 'X:\WA corr analyis GIS\sahel resuts\maxv\maxv1VSacc1_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalPeak=FLTARR(ns, nl)
READU, lun, pvalPeak
FREE_LUN, lun
; pvalLen = pvalLen image
filein= 'X:\WA corr analyis GIS\sahel resuts\len\len1VSacc1_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalLen=FLTARR(ns, nl)
READU, lun, pvalLen
FREE_LUN, lun
; glc = reclass glc (1 grass, 2 crop, NaN are present)
filein= 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\glc_reclass.img'
OPENR, lun, filein, /GET_LUN
glc=BYTARR(ns, nl)
READU, lun, glc
FREE_LUN, lun
; eco = ecoregions (all ok, expect 255)
filein= 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\5ecoregions.img'
OPENR, lun, filein, /GET_LUN
eco=BYTARR(ns, nl)
READU, lun, eco
FREE_LUN, lun

indeco = WHERE(eco NE 255)
eco01 = eco * 0
eco01[indeco] = 1
eco01 = BYTE(eco01)

pval01Corr =  BYTE(eco*0)
pval01Peak =  BYTE(eco*0)
pval01Len =  BYTE(eco*0)
phenoExists = BYTE(eco*0)

ind0pvalCorr = WHERE(FINITE(pvalCorr) EQ 1)
ind0pvalPeak = WHERE(FINITE(pvalPeak) EQ 1)
ind0pvalLen = WHERE(FINITE(pvalLen) EQ 1)

phenoExists[ind0pvalCorr] = 1
phenoExists = BYTE(phenoExists)

indpvalCorr = WHERE(pvalCorr[ind0pvalCorr] LE 0.05)
pval01Corr[ind0pvalCorr[indpvalCorr]]=1
pval01Corr = BYTE(pval01Corr)
indpvalPeak = WHERE(pvalPeak[ind0pvalPeak] LE 0.05)
pval01Peak[ind0pvalPeak[indpvalPeak]]=1
pval01Peak = BYTE(pval01Peak)
indpvalLen = WHERE(pvalLen[ind0pvalLen] LE 0.05)
pval01Len[ind0pvalLen[indpvalLen]]=1
pval01Len = BYTE(pval01Len)

indGlcGRASS =WHERE((glc EQ 1) AND (eco01 EQ 1))
indGlcCROP = WHERE((glc EQ 2) AND (eco01 EQ 1))

;compute the fraction of significant

; total number of pixel with pehomology
PRINT, '*** SOS'
n = TOTAL(phenoExists[indGlcGRASS])
; number with significant relation
nos = TOTAL(pval01Corr[indGlcGRASS])
PRINT, 'GRASS % of significant = ', DOUBLE(nos)/DOUBLE(n)*100
; total number of pixel with pehomology
n = TOTAL(phenoExists[indGlcCROP])
; number with significant relation
nos = TOTAL(pval01Corr[indGlcCROP])
PRINT, 'CROP % of significant = ', DOUBLE(nos)/DOUBLE(n)*100

;over these, compute the mean of correlation for the significantly correlated
indGlcGRASSsc =WHERE((glc EQ 1) AND (eco01 EQ 1) AND (pval01Corr EQ 1))
indGlcCROPsc = WHERE((glc EQ 2) AND (eco01 EQ 1) AND (pval01Corr EQ 1))
PRINT, 'GRASS, mean correlation = ', MEAN(corr[indGlcGRASSsc])
PRINT, 'CROP, mean correlation = ', MEAN(corr[indGlcCROPsc])
PRINT, TM_TEST(corr[indGlcGRASSsc], corr[indGlcCROPsc])


PRINT, '*** Peak'
n = TOTAL(phenoExists[indGlcGRASS])
; number with significant relation
nos = TOTAL(pval01Peak[indGlcGRASS])
PRINT, 'GRASS % of significant = ', DOUBLE(nos)/DOUBLE(n)*100
; total number of pixel with pehomology
n = TOTAL(phenoExists[indGlcCROP])
; number with significant relation
nos = TOTAL(pval01Peak[indGlcCROP])
PRINT, 'CROP % of significant = ', DOUBLE(nos)/DOUBLE(n)*100

;over these, compute the mean of correlation for the significantly correlated
indGlcGRASSsc =WHERE((glc EQ 1) AND (eco01 EQ 1) AND (pval01Peak EQ 1))
indGlcCROPsc = WHERE((glc EQ 2) AND (eco01 EQ 1) AND (pval01Peak EQ 1))
PRINT, 'GRASS, mean correlation = ', MEAN(Peak[indGlcGRASSsc])
PRINT, 'CROP, mean correlation = ', MEAN(Peak[indGlcCROPsc])

PRINT, '*** Length'
n = TOTAL(phenoExists[indGlcGRASS])
; number with significant relation
nos = TOTAL(pval01Len[indGlcGRASS])
PRINT, 'GRASS % of significant = ', DOUBLE(nos)/DOUBLE(n)*100
; total number of pixel with pehomology
n = TOTAL(phenoExists[indGlcCROP])
; number with significant relation
nos = TOTAL(pval01Len[indGlcCROP])
PRINT, 'CROP % of significant = ', DOUBLE(nos)/DOUBLE(n)*100

;over these, compute the mean of correlation for the significantly correlated
indGlcGRASSsc =WHERE((glc EQ 1) AND (eco01 EQ 1) AND (pval01Len EQ 1))
indGlcCROPsc = WHERE((glc EQ 2) AND (eco01 EQ 1) AND (pval01Len EQ 1))
PRINT, 'GRASS, mean correlation = ', MEAN(Len[indGlcGRASSsc])
PRINT, 'CROP, mean correlation = ', MEAN(Len[indGlcCROPsc])
;fileout = 'X:\works\pubblicazioni\in preparazione\2013 Pheno2\post corr analysis\maskGrass_eco'
;tmp = glc * 0
;tmp[indGlcGRASS] = 1
;tmp=BYTE(tmp)
;OPENW, lun, fileout+'.img', /GET_LUN
;WRITEU, lun, tmp
;FREE_LUN, lun
;OPENW, lun, fileout+'.hdr', /GET_LUN
;PRINTF,lun,'ENVI'
;PRINTF,lun,'samples ='+STRCOMPRESS(ns)
;PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
;PRINTF,lun,'bands   ='+STRCOMPRESS(1)
;PRINTF,lun,'data type = 1'
;PRINTF,lun,'interleave = bsq'
;FREE_LUN, lun



END