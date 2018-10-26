PRO analyse_spirits_DB
;load__spirits_db
workpath = 'd:\Users\meronmi\Documents\IDL\ALRMP kenya\workdir' 
RESTORE, FILENAME = workpath + '\' + 'spiritsDB.sav'
dbMissingVal = -99999.999
; CLASS = Overall mean, no_veg, veg
; PERIODICITY_DAYS = 10, 30
; VARIABLE = NDVI_eMODIS, RFE_TARCAT, Mortality_rate_total, Mortality_rate_drought,
;            Mortality_rate_missing, Moratlity_rate_disease, Mortality_rate_conflict, Mortality_rate_other,
;            Mortality_rate_predation
; DATE = dd-mm-yyyy
; MEAN
; SD
; ADMIN = district
; REGION = IBLI unit

; fill vectors, ndvi
ind = WHERE((data.CLASS EQ 'veg') AND (data.VARIABLE EQ 'NDVI_eMODIS'), count)
ndvi = { $
  periodicity : 10, $
  date : STRARR(count), $
  dd : INTARR(count), $
  mm : INTARR(count), $
  yyyy : INTARR(count), $
  dek : INTARR(count), $
  JD : LONARR(count), $
  district : STRARR(count), $
  IBLIunit : STRARR(count), $
  avg : FLTARR(count), $
  sd : FLTARR(count) $
}
ndvi.district = data.ADMIN[ind]
ndvi.IBLIunit = data.REGION[ind]
ndvi.avg = checkMiss(FLOAT(data.MEAN[ind]), dbMissingVal)
ndvi.sd =  checkMiss(FLOAT(data.SD[ind]), dbMissingVal) 
ndvi.date = data.DATE[ind]
FOR i = 0, count-1 DO BEGIN
  tmp = STRSPLIT(data.DATE[ind[i]], /EXTRACT, '-')
  ndvi.dd[i] = FIX(tmp[2])
  ndvi.mm[i] = FIX(tmp[1])
  ndvi.yyyy[i] = FIX(tmp[0])
ENDFOR
ndvi.dek = ddmm2dek(ndvi.dd, ndvi.mm)
ndvi.jd = JULDAY(ndvi.mm, ndvi.dd, ndvi.yyyy)

; fill vectors, TAMSAT
ind = WHERE((data.CLASS EQ 'veg') AND (data.VARIABLE EQ 'RFE_TARCAT'), count)
rfe = { $
  periodicity : 10, $
  date : STRARR(count), $
  dd : INTARR(count), $
  mm : INTARR(count), $
  yyyy : INTARR(count), $
  dek : INTARR(count), $
  JD : LONARR(count), $
  district : STRARR(count), $
  IBLIunit : STRARR(count), $
  avg : FLTARR(count), $
  sd : FLTARR(count) $
}
rfe.district = data.ADMIN[ind]
rfe.IBLIunit = data.REGION[ind]
rfe.avg = checkMiss(FLOAT(data.MEAN[ind]), dbMissingVal)
rfe.sd =  checkMiss(FLOAT(data.SD[ind]), dbMissingVal)
rfe.date = data.DATE[ind]
FOR i = 0, count-1 DO BEGIN
  tmp = STRSPLIT(data.DATE[ind[i]], /EXTRACT, '-')
  rfe.dd[i] = FIX(tmp[2])
  rfe.mm[i] = FIX(tmp[1])
  rfe.yyyy[i] = FIX(tmp[0])
ENDFOR
rfe.dek = ddmm2dek(rfe.dd, rfe.mm)
rfe.jd = JULDAY(rfe.mm, rfe.dd, rfe.yyyy)

; fill vectors, mortality
ind = WHERE((data.CLASS EQ 'veg') AND (data.VARIABLE EQ 'Mortality_rate_total'), count)
mortrate_tot = { $
  periodicity : 30, $
  date : STRARR(count), $
  dd : INTARR(count), $
  mm : INTARR(count), $
  yyyy : INTARR(count), $
  dek : INTARR(count), $
  JD : LONARR(count), $
  district : STRARR(count), $
  IBLIunit : STRARR(count), $
  avg : FLTARR(count), $
  sd : FLTARR(count) $
}
mortrate_tot.district = data.ADMIN[ind]
mortrate_tot.IBLIunit = data.REGION[ind]
mortrate_tot.avg = checkMiss(FLOAT(data.MEAN[ind]), dbMissingVal)
mortrate_tot.sd =  checkMiss(FLOAT(data.SD[ind]), dbMissingVal)
mortrate_tot.date = data.DATE[ind]
FOR i = 0, count-1 DO BEGIN
  tmp = STRSPLIT(data.DATE[ind[i]], /EXTRACT, '-')
  mortrate_tot.dd[i] = FIX(tmp[2])
  mortrate_tot.mm[i] = FIX(tmp[1])
  mortrate_tot.yyyy[i] = FIX(tmp[0])
ENDFOR
mortrate_tot.dek = ddmm2dek(mortrate_tot.dd, mortrate_tot.mm)
mortrate_tot.jd = JULDAY(mortrate_tot.mm, mortrate_tot.dd, mortrate_tot.yyyy)

;load pheno by IBLI unit
inpath = 'E:\ILRI\Pheno_summary_Anton'
fn = 'ibliID_phenoSummary_eMODIS.csv'
tmp =  READ_CSV(inpath + '\' + fn, HEADER=hdr, MISSING_VALUE=-99999.999)
;PRINT, hdr
data1 = rename_tags(tmp, TAG_NAMES(tmp),  hdr)
;help, data1, /STRUCT
pheno = { $
  IBLIunit : STRARR(count), $ 
  avgSOS_2L_halfSD : FLTARR(count), $
  avgEOS_2L_halfSD : FLTARR(count), $
  avgSOS_2S_halfSD : FLTARR(count), $
  avgEOS_2S_halfSD : FLTARR(count) $
}
FOR i = 0, N_ELEMENTS(data1.ibliID)-1 DO BEGIN
  pheno.IBLIunit[i] = data1.IBLI_UNIT[i]
  pheno.avgSOS_2L_halfSD[i] = data1.avgSOS_2L_halfSD[i]
  pheno.avgEOS_2L_halfSD[i] = data1.avgEOS_2L_halfSD[i]
  pheno.avgSOS_2S_halfSD[i] = data1.avgSOS_2S_halfSD[i]
  pheno.avgEOS_2S_halfSD[i] = data1.avgEOS_2S_halfSD[i]
ENDFOR
;***********************************************************************************
target = 'MERTI'
ind_ndvi = WHERE(ndvi.IBLIunit EQ target)
ind_mort = WHERE(mortrate_tot.IBLIunit EQ target)
ind_rfe = WHERE(rfe.IBLIunit EQ target)
ind_pheno = WHERE(pheno.IBLIunit EQ target)
;build the appropriate repeated LTA 
n = N_ELEMENTS(ndvi.avg[ind_ndvi])
continuous_lta_ndvi = !NULL
lta_ndvi = compute_lta(ndvi.avg[ind_ndvi], ndvi.dek[ind_ndvi])
;build a vector that goes with ndvi.jd[ind_ndvi] saying when there's a start
starts = REFORM(ndvi.dek[ind_ndvi]) * 0.0 
ss = [pheno.avgSOS_2L_halfSD[ind_pheno], pheno.avgSOS_2S_halfSD[ind_pheno]]
ind_tmp = WHERE((ndvi.dek[ind_ndvi] EQ ss[0]) OR $
                (ndvi.dek[ind_ndvi] EQ ss[1]))
starts[ind_tmp] = 1
;same for end
ends = REFORM(ndvi.dek[ind_ndvi]) * 0.0
ee = [pheno.avgEOS_2L_halfSD[ind_pheno],pheno.avgEOS_2S_halfSD[ind_pheno]]
ind_tmp = WHERE((ndvi.dek[ind_ndvi] EQ ee[0]) OR $
  (ndvi.dek[ind_ndvi] EQ ee[1]))
ends[ind_tmp] = 1
FOR i = 0, n - 1 DO BEGIN
  continuous_lta_ndvi =[continuous_lta_ndvi, lta_ndvi[(ndvi.dek[ind_ndvi[i]]-1)]]
ENDFOR
;time_plot_X1X2X3vsY1Y2Y3, ndvi.jd[ind_ndvi], ndvi.avg[ind_ndvi], $
;                          ndvi.jd[ind_ndvi], continuous_lta_ndvi, $
;                          mortrate_tot.jd[ind_mort], mortrate_tot.avg[ind_mort],$
;                          ndvi.district[ind_ndvi[0]] + ' ' + ndvi.IBLIunit[ind_ndvi[0]]
;time_plot_X1X2X3X4vsY1Y2Y3Y4, ndvi.jd[ind_ndvi], ndvi.avg[ind_ndvi], $
;                              ndvi.jd[ind_ndvi], continuous_lta_ndvi, $
;                              mortrate_tot.jd[ind_mort], mortrate_tot.avg[ind_mort],$
;                              rfe.jd[ind_rfe], rfe.avg[ind_rfe],$
;                              ndvi.district[ind_ndvi[0]] + ' ' + ndvi.IBLIunit[ind_ndvi[0]] 
time_plot_X1X2X3X4vsY1Y2Y3Y4_and_pheno, ndvi.jd[ind_ndvi], ndvi.avg[ind_ndvi], $
                              ndvi.jd[ind_ndvi], continuous_lta_ndvi, $
                              mortrate_tot.jd[ind_mort], mortrate_tot.avg[ind_mort],$
                              rfe.jd[ind_rfe], rfe.avg[ind_rfe],$
                              starts, ends, $
                              ndvi.district[ind_ndvi[0]] + ' ' + ndvi.IBLIunit[ind_ndvi[0]]
PRINT, 'Finished'
END