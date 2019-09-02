PRO Sentinel_pheno_analys_manager
;run the bridged pheno
fn_sav = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190520_v3.sav'
dir  = Sentinel_pheno_bridge_caller(fn_sav)

;gather the results
PRINT, Sentinel_pheno_bridge_gather_results(dir)

;make the histograms (here I exclude a point if it does not have sos OR EOS)
PRINT, Sentinel_pheno_histo(dir) 

;make lat correlation (from here on I keep what is available) 
PRINT, Sentinel_pheno_lat_trends(dir)

;make correlation between pheno timing derived from different idexes
PRINT, Sentinel_pheno_compare_S2andS1_GS12_matching_v2(dir)

;plot map of variables 
PRINT, Sentinel_pheno_plot_by_grid(dir,  ['SOS50','EOS50'])
END


