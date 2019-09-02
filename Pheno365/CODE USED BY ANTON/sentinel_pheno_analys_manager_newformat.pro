PRO Sentinel_pheno_analys_manager_newFormat
;use this to run the sequential caller
fn_sav_s1 = 'D:\RAPHAEL_pheno_test\LUCAS_S1_extraction_postprocess_20190704_v0.sav'
fn_sav_s2 = 'D:\RAPHAEL_pheno_test\LUCAS_S2_extraction_postprocess_20190703_v0.sav'

;make mean profiles
RAPHAEL_mean_profile_by_country_newFormat, fn_sav_s1, fn_sav_s2

;SEQUENTIAL CALLER
;Sentinel_pheno_sequential_caller_newFormat, fn_sav_s1, fn_sav_s2

;run the bridged pheno
dir  = Sentinel_pheno_bridge_caller_newFormat( fn_sav_s1, fn_sav_s2)






;gather the results
PRINT, Sentinel_pheno_bridge_gather_results(dir)

;make the histograms (here I exclude a point if it does not have sos OR EOS)
PRINT, Sentinel_pheno_histo_newFormat(dir) 

;make lat correlation (from here on I keep what is available) 
PRINT, Sentinel_pheno_lat_trends(dir)

;make correlation between pheno timing derived from different idexes
PRINT, Sentinel_pheno_compare_S2andS1_GS12_matching_v2(dir)

;plot map of variables 
PRINT, Sentinel_pheno_plot_by_grid(dir,  ['SOS50','EOS50'])
END


