PRO compute_trends_on_adj_pheno_v2
;re use fn structure from align_seasos_of_year_by_year_pheno.pro
dir = 'F:\Pheno_year_by_year\Pheno\consolidated_results'
fns = CREATE_STRUCT('k',dir + '\BBB_' +'kk0','s',dir + '\BBB_' +'s','m',dir + '\BBB_' +'m','e',dir + '\BBB_' +'e','l',dir + '\BBB_' +'l','y',dir + '\BBB_' +'ym')
anomStr = '_adj_anomX100_'
fns_adj_anom_valid_minus36_36 = [TRANSPOSE([fns.s+anomStr+'1', fns.m+anomStr+'1', fns.e+anomStr+'1', fns.l+anomStr+'1']), $
                                 TRANSPOSE([fns.s+anomStr+'2', fns.m+anomStr+'2', fns.e+anomStr+'2', fns.l+anomStr+'2'])]
fns_adj_anom_valid_minus1_1 = [fns.y+anomStr+'1', fns.y+anomStr+'2']

fns_time_deks = dir + '\BBB_adj_anom_prog_dek_from_first_year_minus1_' + ['1','2'] 

;dir = '\\ies\d5\asap\Pheno_year_by_year\Pheno'
;time_trend_stats, fnameY, inPath, outPath, outBaseName, validRange

;FOR i =0, N_ELEMENTS(fns_adj_anom_valid_minus36_36[0,*])-1 DO BEGIN
FOR i =1, N_ELEMENTS(fns_adj_anom_valid_minus36_36[0,*])-1 DO BEGIN
  FOR s = 0, 1 DO BEGIN
     PRINT, FILE_BASENAME(fns_adj_anom_valid_minus36_36[s,i])+'.bil', '    ',$
                                   FILE_BASENAME(fns_time_deks[s])+'.bil', '     ',dir, dir+'\TREND_ADJ', '    ',$
                                   FILE_BASENAME(fns_adj_anom_valid_minus36_36[s,i])
     
     PRINT, time_trend_stats_XandY(FILE_BASENAME(fns_adj_anom_valid_minus36_36[s,i])+'.bil', $
                                   FILE_BASENAME(fns_time_deks[s])+'.bil', dir, dir+'\TREND_ADJ', $
                                   FILE_BASENAME(fns_adj_anom_valid_minus36_36[s,i]), [-36*100,36*100], YSCALING=[0.0, 1/100.0], XSCALING=[0.0, 1/36.0])
  ENDFOR
ENDFOR

FOR s = 0, 1 DO BEGIN
  PRINT, FILE_BASENAME(fns_adj_anom_valid_minus1_1[s])+'.bil'
  PRINT, time_trend_stats_XandY(FILE_BASENAME(fns_adj_anom_valid_minus1_1[s])+'.bil', $
                                FILE_BASENAME(fns_time_deks[s])+'.bil', dir, dir+'\TREND_ADJ', $
                                FILE_BASENAME(fns_adj_anom_valid_minus1_1[s]), [-1*100,1*100], YSCALING=[0.0, 1/100.0], XSCALING=[0.0, 1/36.0])
ENDFOR

END