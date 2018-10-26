PRO compute_trends_on_adj_pheno
;re use fn structure from align_seasos_of_year_by_year_pheno.pro
dir = '\\ies\d5\asap\Pheno_year_by_year\Pheno\'
fns = CREATE_STRUCT('k',dir + 'BBB_' +'kk0','s',dir + 'BBB_' +'s','m',dir + 'BBB_' +'m','e',dir + 'BBB_' +'e','l',dir + 'BBB_' +'l','y',dir + 'BBB_' +'ym')
;fns_adj_valid1_108 = [fns.s+'_adj_'+'1', fns.s+'_adj_'+'2', fns.m+'_adj_'+'1', fns.m+'_adj_'+'2', $
;                      fns.e+'_adj_'+'1', fns.e+'_adj_'+'2']
fns_adj_valid1_108 = [fns.e+'_adj_'+'1', fns.e+'_adj_'+'2'] 
fns_adj_valid1_36 = [fns.l+'_adj_'+'1', fns.l+'_adj_'+'2']
fns_adj_valid0_250 = [fns.y+'_adj_'+'1', fns.y+'_adj_'+'2'] 

dir = '\\ies\d5\asap\Pheno_year_by_year\Pheno'
;time_trend_stats, fnameY, inPath, outPath, outBaseName, validRange
FOR i = 0, N_ELEMENTS(fns_adj_valid1_108)-1 DO BEGIN
  PRINT, FILE_BASENAME(fns_adj_valid1_108[i])+'.bil'
  PRINT, time_trend_stats(FILE_BASENAME(fns_adj_valid1_108[i])+'.bil', dir, dir+'\TREND_ADJ', FILE_BASENAME(fns_adj_valid1_108[i]), [1,108])
ENDFOR
FOR i = 0, N_ELEMENTS(fns_adj_valid1_36)-1 DO BEGIN
  PRINT, FILE_BASENAME(fns_adj_valid1_36[i])+'.bil'
  PRINT, time_trend_stats(FILE_BASENAME(fns_adj_valid1_36[i])+'.bil', dir, dir+'\TREND_ADJ', FILE_BASENAME(fns_adj_valid1_36[i]), [1,36])
ENDFOR
FOR i = 0, N_ELEMENTS(fns_adj_valid0_250)-1 DO BEGIN
  PRINT, FILE_BASENAME(fns_adj_valid0_250[i])+'.bil'
  PRINT, time_trend_stats(FILE_BASENAME(fns_adj_valid0_250[i])+'.bil', dir, dir+'\TREND_ADJ', FILE_BASENAME(fns_adj_valid0_250[i]), [0,250], YSCALING = [0.2, 0.0048])
ENDFOR

END