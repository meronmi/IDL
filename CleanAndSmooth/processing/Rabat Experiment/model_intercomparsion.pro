PRO model_intercomparsion
rr_step = 5.0 ;step for the range
plot_90 = 0   ;set to 1 for plotting two regression, obe for all division and one for 90% production, 0 otherwise
outpath = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\ZZZ reply from partners\model_comparison_summary\without90prod'
;outpath = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\ZZZ reply from partners\model_comparison_summary'

save_out = 1
out_ext = '.png'

inpath = 'S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\ZZZ reply from partners\model_comparison_summary'
country = 'tunisia'
;country = 'morocco'
;country = 'algeria'

fn = 'model_output_evaluation_'+country+'.csv'


OPENW, lun, outpath + '\stats_'+fn, /GET_LUN
PRINTF, lun, 'Cereal,relationship,slope,offset,P,R2,rmse,mean_paired_t_P,RMSE_test_P,slope90,offset90,P90,R2_90,rmse90,mean_paired_t_P90,RMSE_test_P90,n,n90'

OPENW, lun2, outpath + '\VGT09-13_stats_'+fn, /GET_LUN
PRINTF, lun2, 'VGT STATS FOR 2009-2013 Scatter obs VS mod



tmp =  READ_CSV(inpath+'\'+fn, HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
data = rename_tags(tmp, TAG_NAMES(tmp),  hdr)
types = BYTARR(N_TAGS(data))
FOR i = 0, N_TAGS(data)-1 DO types[i] = SIZE(data.(i), /TYPE)
;PRINT, types
;replace missing in float and double
ind = WHERE((types EQ 4) OR (types EQ 5), count)
FOR i = 0, count-1 DO BEGIN
  iind = WHERE(data.(ind[i]) EQ -9999, count)
  IF (count GT 0) THEN data.(ind[i])[iind] = !VALUES.F_NAN
ENDFOR

;make a copy to vgt stats of all cereals, first field says if the data belong to 90% or not
mat2 = FLTARR(11, N_ELEMENTS(data.Cereal))*!VALUES.F_NAN
mat2[1,*] = data.obs_yield_2009
mat2[2,*] = data.obs_yield_2010
mat2[3,*] = data.obs_yield_2011
mat2[4,*] = data.obs_yield_2012
mat2[5,*] = data.obs_yield_2013
mat2[6,*] = data.mod_yield_2009
mat2[7,*] = data.mod_yield_2010
mat2[8,*] = data.mod_yield_2011
mat2[9,*] = data.mod_yield_2012
mat2[10,*] = data.mod_yield_2013 
IF (country EQ 'algeria') THEN mat2 = mat2 * 10.0
;prepare vectors for "overall plots"
obs_all = !NULL & ind90_all = !NULL
vgt_all = !NULL 
pv_all = !NULL 

cereal = ['Barley','Durum wheat','Soft wheat']
FOR i = 0, N_ELEMENTS(cereal)-1 DO BEGIN
  ind = WHERE(data.Cereal EQ cereal[i], count)
  ;extract the relevant variables
  district_names = data.Gaul1_Name[ind]
  ;put it in matrxi form, easier to manipulate
  mat = FLTARR(14, count)
  mat[0,*] = data.Observed_yield[ind] 
  mat[1,*] = data.mod_yield_VGT[ind]
  mat[2,*] = data.mod_yield_PV[ind]
  mat[3,*] = data.obs_yield_2009[ind]
  mat[4,*] = data.obs_yield_2010[ind]
  mat[5,*] = data.obs_yield_2011[ind]
  mat[6,*] = data.obs_yield_2012[ind]
  mat[7,*] = data.obs_yield_2013[ind]
  mat[8,*] = data.mod_yield_2009[ind]
  mat[9,*] = data.mod_yield_2010[ind]
  mat[10,*] = data.mod_yield_2011[ind]
  mat[11,*] = data.mod_yield_2012[ind]
  mat[12,*] = data.mod_yield_2013[ind]
  mat[13,*] = data.Mean_pro_03_12_1000ql[ind]
  iind = ind
  ;homogenise units (to 100 Kg / ha)
  IF (country EQ 'algeria') THEN mat = mat * 10.0
  ;create vector for "all_cereal" analysis
  obs_all = [obs_all, REFORM(mat[0,*])]
  vgt_all = [vgt_all,  REFORM(mat[1,*])]
  pv_all = [pv_all, REFORM(mat[2,*])]
  ;make a subset of the the db containing only the biggest admin unit summing up to 90 % production
  indsort = SORT(mat[13,*])
  indsortBig2Small = REVERSE(indsort)
  res = TOTAL(mat[13,indsortBig2Small], /CUMULATIVE)
  PRINT, 'Cereal: ' + cereal[i]
  PRINT, 'sorted prod'
  PRINT, REFORM(mat[13,indsortBig2Small])
  PRINT, 'sorted cum prod'
  PRINT, REFORM(res)
  ;take only the 90% prodcution, ind[0] is the first and refer to indsortBig2Small
  ind = WHERE(res GE TOTAL(mat[13,*])/100.0*90.0, count)
  ind90 = indsortBig2Small[0:ind[0]]
  mat2[0,iind[ind90]] = 1
  ;extract for overall plot
  ind90_all = [ind90_all, REFORM(ind90) + N_ELEMENTS(ind90_all)]
  PRINT, '90 % biggest'
  PRINT, REFORM(mat[13,ind90])
  PRINT, 'TOTAL PROD = ', TOTAL(mat[13,*])
  nn = N_ELEMENTS(mat[13,*])
  PRINT, 'n admin units = ', nn
  PRINT, '90% PROD = ', TOTAL(mat[13,*])/100.0*90.0
  PRINT, 'PROD OF SELECTED= ', TOTAL(mat[13,ind90])
  nn90 = N_ELEMENTS(mat[13,ind90])
  PRINT, 'n admin units = ', nn90
 
  
  ;compute stats of VGT estimates for the previous 5 years, 2009-2013
  VGT_stats = DBLARR(4,2,5) *!VALUES.F_NAN      ;slope, offset, r2, rmse
  FOR k = 0, 4 DO BEGIN
    x = REFORM(mat[3+k,*])  ;obs
    y = REFORM(mat[8+k,*])  ;mod
    indFinX = WHERE(FINITE(x))
    indFinY = WHERE(FINITE(y))
    indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
    IF (indFinXAndY[0] NE -1) THEN BEGIN ;they both have data
      xx = x[indFinXAndY]
      yy = y[indFinXAndY]   
      slope = REGRESS(xx, yy, CONST=offset, CORRELATION=r)
      VGT_stats[*,0,k] = [slope[0],offset, r^2, rmse(xx, yy)]
      x = REFORM(mat[3+k,ind90])  ;obs
      y = REFORM(mat[8+k,ind90])  ;mod
      indx = WHERE(FINITE(x))
      indFinX = WHERE(FINITE(y))
      indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
      xx = x[indFinXAndY]
      yy = y[indFinXAndY]
      slope = REGRESS(xx, yy, CONST=offset, CORRELATION=r)
      VGT_stats[*,1,k] = [slope[0],offset, r^2, rmse(xx, yy)]
    ENDIF ELSE BEGIN
      VGT_stats[*,1,k] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
  PRINTF, lun2, cereal[i]+',All admin,slope, offset, r2, rmse'
  PRINTF, lun2, cereal[i]+',MEAN,' + STRTRIM(MEAN(VGT_stats[0,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[1,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[2,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[3,0,*], /NAN),2)
  PRINTF, lun2, cereal[i]+',SD,' + STRTRIM(STDDEV(VGT_stats[0,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[1,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[2,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[3,0,*], /NAN),2)
  PRINTF, lun2, cereal[i]+',90%Prod,slope, offset, r2, rmse'
  PRINTF, lun2, cereal[i]+',MEAN,' + STRTRIM(MEAN(VGT_stats[0,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[1,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[2,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[3,1,*], /NAN),2)
  PRINTF, lun2, cereal[i]+',SD,' + STRTRIM(STDDEV(VGT_stats[0,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[1,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[2,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[3,1,*], /NAN),2)
  ;now plot the agreement between estimates
  fs = 20   ; font size
   
  x = REFORM(mat[1,*])
  y = REFORM(mat[2,*])
  rr = [0, CEIL(MAX([x,y], /NAN)/rr_step)*rr_step]
  lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
  xtit = 'VGT Mod Yield (100kg/ha)'
  ytit = 'PV Mod Yield (100kg/ha)'
  tit = cereal[i]
  rel = 'VGTvsPV'
  out_fn = outpath + '\' + cereal[i] + '_'+ rel + '_' + country + out_ext 
  res = plot_rabat(x, y, ind90, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
  paired_sample_t_all = TM_TEST( x, y, /PAIRED)
  paired_sample_t_90 = TM_TEST( x[ind90], y[ind90], /PAIRED)
  crop = cereal[i]
  tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],-999, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90], y[ind90]),paired_sample_t_90[1],-999,nn,nn90]),2)] + ',', /REMOVE_ALL)
  tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
  PRINTF, lun, tmp2 
  
  ;now plot the agreement against observations VGT
  x = REFORM(mat[0,*])
  y = REFORM(mat[1,*])
  rr = [0, CEIL(MAX([x,y], /NAN)/rr_step)*rr_step]
  lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
  xtit = 'Obs Yield (100kg/ha)'
  ytit = 'VGT Mod Yield (100kg/ha)'
  rel = 'OBSvsVGT'
  out_fn = outpath + '\' + cereal[i] + '_'+ rel + '_' + country + out_ext 
  res= plot_rabat(x, y, ind90, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
  paired_sample_t_all = TM_TEST( x, y, /PAIRED)
  paired_sample_t_90 = TM_TEST( x[ind90], y[ind90], /PAIRED)
  tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],-999, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90], y[ind90]),paired_sample_t_90[1],-999,nn,nn90]),2)] + ',', /REMOVE_ALL)
  tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
  PRINTF, lun, tmp2 
                                                                  
  ;now plot the agreement against observations PV
  x = REFORM(mat[0,*])
  y = REFORM(mat[2,*])
  ;rr = [0, CEIL(MAX([x,y], /NAN)/rr_step)*rr_step]
  lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
  xtit = 'Obs Yield (100kg/ha)'
  ytit = 'PV Mod Yield (100kg/ha)'
  rel = 'OBSvsPV'
  out_fn = outpath + '\' + cereal[i] + '_'+ rel + '_' + country + out_ext 
  res = plot_rabat(x, y, ind90, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
  paired_sample_t_all = TM_TEST( x, y, /PAIRED)
  paired_sample_t_90 = TM_TEST( x[ind90], y[ind90], /PAIRED)
  
  
  ;make the test about the RMSE difference
  mod1_y =  REFORM(mat[1,*])
  mod2_y = REFORM(mat[2,*])
  obs_y = REFORM(mat[0,*])
  pvalRMSE = pitman_morgan_test(mod1_y, mod2_y, obs_y)
  pvalRMSE90 = pitman_morgan_test(mod1_y[ind90], mod2_y[ind90], obs_y[ind90])
  tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],pvalRMSE, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90], y[ind90]),paired_sample_t_90[1],pvalRMSE90,nn,nn90]),2)] + ',', /REMOVE_ALL)
  tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
  PRINTF, lun, tmp2
                                                               
  PRINT, 'Finshed cereal: ' + cereal[i]
ENDFOR

;OVERALL plot (all cereals)

;now plot the agreement between estimates
fs = 20   ; font size

x = vgt_all
y = pv_all
rr = [0, CEIL(MAX([x,y])/rr_step)*rr_step]
lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
xtit = 'VGT Mod Yield (100kg/ha)'
ytit = 'PV Mod Yield (100kg/ha)'
tit = 'All cereals'
rel = 'VGTvsPV'

out_fn = outpath + '\All_crops_' + rel + '_' + country + out_ext 
res = plot_rabat(x, y, ind90_all, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
paired_sample_t_all = TM_TEST( x, y, /PAIRED)
paired_sample_t_90 = TM_TEST( x[ind90], y[ind90], /PAIRED)
crop = 'All_cereals'
tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],-999, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90], y[ind90]),paired_sample_t_90[1],-999,nn,nn90]),2)] + ',', /REMOVE_ALL)
tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
PRINTF, lun, tmp2                                
;now plot the agreement against observations VGT
x = obs_all
y = vgt_all
rr = [0, CEIL(MAX([x,y], /NAN)/rr_step)*rr_step]
lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
xtit = 'Obs Yield (100kg/ha)'
ytit = 'VGT Mod Yield (100kg/ha)'
rel = 'OBSvsVGT'
out_fn = outpath + '\All_crops_' + rel + '_' + country + out_ext 
res = plot_rabat(x, y, ind90_all, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
paired_sample_t_all = TM_TEST( x, y, /PAIRED)
paired_sample_t_90 = TM_TEST( x[ind90], y[ind90], /PAIRED)
tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],-999, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90], y[ind90]),paired_sample_t_90[1],-999,nn,nn90]),2)] + ',', /REMOVE_ALL)
tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
PRINTF, lun, tmp2
                                
;now plot the agreement against observations PV
x = obs_all
y = pv_all
rr = [0, CEIL(MAX([x,y], /NAN)/rr_step)*rr_step]
lp = [rr[1]/2, rr[1]/15.0*14.0] ;legend position
xtit = 'Obs Yield (100kg/ha)'
ytit = 'PV Mod Yield (100kg/ha)'
rel = 'OBSvsPV'
out_fn = outpath + '\All_crops_' + rel + '_' + country + out_ext 
res = plot_rabat(x, y, ind90_all, tit, xtit, ytit, save_out, out_fn, rr, fs, lp, plot_90)
paired_sample_t_all = TM_TEST( x, y, /PAIRED)
paired_sample_t_90 = TM_TEST( x[ind90_all], y[ind90_all], /PAIRED)
;make the test about the RMSE difference
mod1_y =  vgt_all
mod2_y = pv_all
obs_y = obs_all
pvalRMSE = pitman_morgan_test(mod1_y, mod2_y, obs_y)
pvalRMSE90 = pitman_morgan_test(mod1_y[ind90_all], mod2_y[ind90_all], obs_y[ind90_all])
tmp = STRCOMPRESS([crop,rel,STRTRIM(REFORM([res[0,0],res[1,0],res[2,0],res[3,0],rmse(x,y),paired_sample_t_all[1],pvalRMSE, res[0,1],res[1,1],res[2,1],res[3,1],rmse(x[ind90_all], y[ind90_all]),paired_sample_t_90[1],pvalRMSE90,nn,nn90]),2)] + ',', /REMOVE_ALL)
tmp2 = '' & FOR j=0, N_ELEMENTS(tmp)-1 DO tmp2=tmp2+tmp[j]
PRINTF, lun, tmp2

FREE_LUN, lun

;compute stats of VGT estimates for the previous 5 years, 2009-2013, all cerareal
VGT_stats = DBLARR(4,2,5) *!VALUES.F_NAN      ;slope, offset, r2, rmse
FOR k = 0, 4 DO BEGIN
  x = REFORM(mat2[1+k,*])  ;obs
  y = REFORM(mat2[6+k,*])  ;mod
  indFinX = WHERE(FINITE(x))
  indFinY = WHERE(FINITE(y))
  indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
  IF (indFinXAndY[0] NE -1) THEN BEGIN ;they both have data
    xx = x[indFinXAndY]
    yy = y[indFinXAndY]
    slope = REGRESS(xx, yy, CONST=offset, CORRELATION=r)
    VGT_stats[*,0,k] = [slope[0],offset, r^2, rmse(xx, yy)]
    x = REFORM(mat[3+k,ind90])  ;obs
    y = REFORM(mat[8+k,ind90])  ;mod
    indx = WHERE(FINITE(x))
    indFinX = WHERE(FINITE(y))
    indFinXAndY = cgSetIntersection(indFinX, indFinY, count=countFin)
    xx = x[indFinXAndY]
    yy = y[indFinXAndY]
    slope = REGRESS(xx, yy, CONST=offset, CORRELATION=r)
    VGT_stats[*,1,k] = [slope[0],offset, r^2, rmse(xx, yy)]
  ENDIF ELSE BEGIN
    VGT_stats[*,1,k] = !VALUES.F_NAN
  ENDELSE
ENDFOR
PRINTF, lun2, 'All_cereal,'+'All admin,slope, offset, r2, rmse'
PRINTF, lun2, 'All_cereal,'+'MEAN,' + STRTRIM(MEAN(VGT_stats[0,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[1,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[2,0,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[3,0,*], /NAN),2)
PRINTF, lun2, 'All_cereal,'+'SD,' + STRTRIM(STDDEV(VGT_stats[0,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[1,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[2,0,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[3,0,*], /NAN),2)
PRINTF, lun2, 'All_cereal,'+'90%Prod,slope, offset, r2, rmse'
PRINTF, lun2, 'All_cereal,'+'MEAN,' + STRTRIM(MEAN(VGT_stats[0,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[1,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[2,1,*], /NAN),2)+','+STRTRIM(MEAN(VGT_stats[3,1,*], /NAN),2)
PRINTF, lun2, 'All_cereal,'+'SD,' + STRTRIM(STDDEV(VGT_stats[0,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[1,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[2,1,*], /NAN),2)+','+STRTRIM(STDDEV(VGT_stats[3,1,*], /NAN),2)
FREE_LUN, lun2
PRINT, 'Completed'
END