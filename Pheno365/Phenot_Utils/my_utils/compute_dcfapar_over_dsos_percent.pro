PRO compute_dcfapar_over_dsos_percent
ns = 7841
nl = 1458
; pvalCorr = pvalCorr image
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_Pval_gain.img'
OPENR, lun, filein, /GET_LUN
pvalCorr=FLTARR(ns, nl)
READU, lun, pvalCorr
FREE_LUN, lun
; a of: acc = a dSOS+ b
filein= 'X:\WA corr analyis GIS\sahel resuts\sos\sos1dltVSacc1_gain.img'
OPENR, lun, filein, /GET_LUN
gain=FLTARR(ns, nl)
READU, lun, gain
FREE_LUN, lun
; average acc1 (average nade exluding the zeros)
filein= 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\avg ACC without Zeros\A1sos-1997_acc1_dlt_TZPavg_no_zeros.img'
OPENR, lun, filein, /GET_LUN
avgAcc=FLTARR(ns, nl)
READU, lun, avgAcc
FREE_LUN, lun

; work only on pixels where Pval is finite and LE 0.05
indf = WHERE(FINITE(pvalCorr) EQ 1)
ind = WHERE(pvalCorr[indf] LE 0.05)
ind = indf[ind]

res = FLTARR(ns,nl) * 0.0-9999.0    ;set backgound to -9999 for arcgis
res[ind] = gain[ind] / avgAcc[ind] *100.0

fileout = 'X:\WA corr analyis GIS\sahel resuts\sos\Dacc1_over_Dsos_divby_mean_no_zeros_acc1'
OPENW, lun, fileout+'.img', /GET_LUN
WRITEU, lun, res
FREE_LUN, lun
;OPENW, lun, fileout+'.hdr', /GET_LUN
;PRINTF,lun,'ENVI'
;PRINTF,lun,'samples ='+STRCOMPRESS(ns)
;PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
;PRINTF,lun,'bands   ='+STRCOMPRESS(1)
;PRINTF,lun,'data type = 4'
;PRINTF,lun,'interleave = bsq'
;FREE_LUN, lun

END