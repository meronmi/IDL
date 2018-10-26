PRO analyze_corr_for_Rev2
ns = 7841
nl = 1458
; corr = correlationb image
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_corr.img'
OPENR, lun, filein, /GET_LUN
corrSOSvsACC=FLTARR(ns, nl)
READU, lun, corrSOSvsACC
FREE_LUN, lun
; pvalCorr = pvalCorr image
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalCorrSOSvsACC=FLTARR(ns, nl)
READU, lun, pvalCorrSOSvsACC
FREE_LUN, lun

; corr sos vs peak
filein= 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis\maxv\sos1dltVSmaxv11_corr.img'
OPENR, lun, filein, /GET_LUN
corrSOSvsPeak=FLTARR(ns, nl)
READU, lun, corrSOSvsPeak
FREE_LUN, lun

; pvalCorr of above
filein= 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis\maxv\sos1dltVSmaxv11_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalSOSvsPeak=FLTARR(ns, nl)
READU, lun, pvalSOSvsPeak
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

;pixel of ecoregions
indeco = WHERE(eco NE 255)
eco01 = eco * 0
eco01[indeco] = 1
eco01 = BYTE(eco01)
;pixel of the two classes and Eco
indGlcEco =WHERE(((glc EQ 1) OR (glc EQ 2))AND (eco01 EQ 1))
valid = BYTE(eco*0)
valid[indGlcEco] = 1
;pixel where the corr SOS vs CFAPAR is not significant
non_sig_SOSvsACC = BYTE(eco*0)
ind0 = WHERE((FINITE(pvalCorrSOSvsACC) EQ 1) AND (valid EQ 1))
ind1 = WHERE(pvalCorrSOSvsACC[ind0] GT 0.05, n_non_sig_SOSvsACC)
non_sig_SOSvsACC[ind0[ind1]] = 1
;ok
;over this pixels check the fraction of pixel which have a significant corr dltSOS vs Peak
ind_non_sig_SOSvsACC = WHERE(non_sig_SOSvsACC EQ 1)
ind_sig_SOSvsPeak = WHERE(pvalSOSvsPeak[ind_non_sig_SOSvsACC] LT 0.05, n_sig_SOSvsPeak)
sig_SOSvsPeak_ou_of_sig_SOSvsACC = BYTE(eco*0)
sig_SOSvsPeak_ou_of_sig_SOSvsACC[ind_non_sig_SOSvsACC[ind_sig_SOSvsPeak]]=1
PRINT, 'fraction sig SOSvsPeak among non_sig_SOSvsACC %=  ', n_sig_SOSvsPeak/DOUBLE(n_non_sig_SOSvsACC)*100.0
;and compute their mean corr dltSOS vs Peak
PRINT, 'Mean corr of such pixels =  ', MEAN(corrSOSvsPeak[ind_non_sig_SOSvsACC[ind_sig_SOSvsPeak]], /DOUBLE)


SAVE, /VARIABLES, FILENAME = 'N:\variables1.sav'
; RESTORE, 'N:\variables1.sav'




END