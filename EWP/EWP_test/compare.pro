PRO compare

doplot = 0
dir = 'E:\WA\EWP\CROSSCORR'

prefix = '';'Fixed_lag_'; ''
up2 = 'eos';'tom'; 'eos'

;ref = prefix + 'SPI7'
ref = prefix + 'zCWP2000SP'

var_can = prefix + 'zCWP';'zEWP';'NzEWP';'zCWP'
list_candidates = var_can +STRTRIM(half_life_define_all(),2)
;add other SPIs
adds = prefix + ['SPI3','SPI4','SPI5','SPI6','SPI8','SPI9','SPI10','SPI12']
list_candidates = [list_candidates, adds]



mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
fn_intersection = 'eco_crop_pasture_res_TAMSAT.img' ;pasrure = 1, crop = 2
ns = 1867
nl = 348

CASE up2 OF
  'tom': BEGIN
    corr_suffix = '_ZFAPAR_bestCrossCorr_up2tom';
    lag_suffix = '_ZFAPAR_bestLag_up2tom'
    pval_suffix = '_ZFAPAR_bestPval_up2tom'
  END
  'eos': BEGIN
    corr_suffix = '_ZFAPAR_bestCrossCorr_up2eos'
    lag_suffix = '_ZFAPAR_bestLag_up2eos'
    pval_suffix = '_ZFAPAR_bestPval_up2eos'
  END 
  ELSE: STOP 
ENDCASE


corr_ref_fn = dir + '\' + ref + corr_suffix
lag_ref_fn =  dir + '\' + ref + lag_suffix
pval_ref_fn =  dir + '\' + ref + pval_suffix



corr_mat = FLTARR(N_ELEMENTS(list_candidates)+1, 3) ;reference and all the others
lag_mat = corr_mat
corr_mat_sig = corr_mat
lag_mat_sig = corr_mat

mask = BYTARR(ns,nl)*0B
OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
READU, lun, mask
FREE_LUN, lun


;open reference
corr_ref = FLTARR(ns,nl)
lag_ref = FLTARR(ns,nl)
pval_ref = FLTARR(ns,nl)

OPENR, lun, corr_ref_fn, /GET_LUN
READU, lun, corr_ref
FREE_LUN, lun

OPENR, lun, lag_ref_fn, /GET_LUN
READU, lun, lag_ref
FREE_LUN, lun

OPENR, lun, pval_ref_fn, /GET_LUN
READU, lun, pval_ref
FREE_LUN, lun

ind_ref = WHERE(FINITE(corr_ref) AND (mask GT 0), count_ref)
ind_ref_rangelnd = WHERE(FINITE(corr_ref) AND (mask EQ 1))
ind_ref_croplnd =   WHERE(FINITE(corr_ref) AND (mask EQ 2))

corr_mat[0,*] = [Mean(corr_ref[ind_ref]), Mean(corr_ref[ind_ref_croplnd]), Mean(corr_ref[ind_ref_rangelnd])]
lag_mat[0,*] = [Mean(lag_ref[ind_ref]), Mean(lag_ref[ind_ref_croplnd]), Mean(lag_ref[ind_ref_rangelnd])] 

ind_sig_ref = WHERE((pval_ref[ind_ref] LT 0.05), count_ref_sig)
ref_sig_fract = count_ref_sig / FLOAT(count_ref)  * 100
PRINT, ref + ' sign / Tot = ', ref_sig_fract
ind_sig_croplnd = WHERE(pval_ref[ind_ref_croplnd] LT 0.05, count_ref_croplnd_sig)
ind_sig_rangelnd = WHERE(pval_ref[ind_ref_rangelnd] LT 0.05, count_ref_rangelnd_sig)

corr_mat_sig[0,*] = [Mean(corr_ref[ind_ref[ind_sig_ref]]), Mean(corr_ref[ind_ref_croplnd[ind_sig_croplnd]]), Mean(corr_ref[ind_ref_rangelnd[ind_sig_rangelnd]])]
lag_mat_sig[0,*] = [Mean(lag_ref[ind_ref[ind_sig_ref]]), Mean(lag_ref[ind_ref_croplnd[ind_sig_croplnd]]), Mean(lag_ref[ind_ref_rangelnd[ind_sig_rangelnd]])]

can_sig_fract = FLTARR(N_ELEMENTS(list_candidates))
;loop on the candidates
FOR i = 0, N_ELEMENTS(list_candidates) -1 DO BEGIN
  corr_can_fn = dir + '\' + list_candidates[i] + corr_suffix
  lag_can_fn =  dir + '\' + list_candidates[i] + lag_suffix
  pval_can_fn =  dir + '\' + list_candidates[i] + pval_suffix
  corr_can = FLTARR(ns,nl)
  lag_can = FLTARR(ns,nl)
  pval_can = FLTARR(ns,nl)
  OPENR, lun, corr_can_fn, /GET_LUN
  READU, lun, corr_can
  FREE_LUN, lun
  
  ind_can = WHERE(FINITE(corr_can) AND (mask GT 0), count_can)
  ind_can_rangelnd = WHERE(FINITE(corr_can) AND (mask EQ 1))
  ind_can_croplnd =   WHERE(FINITE(corr_can) AND (mask EQ 2))

  OPENR, lun, lag_can_fn, /GET_LUN
  READU, lun, lag_can
  FREE_LUN, lun
  
  OPENR, lun, pval_can_fn, /GET_LUN
  READU, lun, pval_can
  FREE_LUN, lun
  
  corr_mat[i+1,*] = [Mean(corr_can[ind_can]), Mean(corr_can[ind_can_croplnd]), Mean(corr_can[ind_can_rangelnd])]
  lag_mat[i+1,*] = [Mean(lag_can[ind_can]), Mean(lag_can[ind_can_croplnd]), Mean(lag_can[ind_can_rangelnd])]
  
  ind_sig_can = WHERE((pval_can[ind_can] LT 0.05), count_can_sig)
  can_sig_fract[i] = count_can_sig / FLOAT(count_can) * 100
  PRINT, list_candidates[i] + ' sign / Tot = ', can_sig_fract[i]
  ind_sig_croplnd = WHERE(pval_can[ind_can_croplnd] LT 0.05, count_can_croplnd_sig)
  ind_sig_rangelnd = WHERE(pval_can[ind_can_rangelnd] LT 0.05, count_can_rangelnd_sig)

  corr_mat_sig[i+1,*] = [Mean(corr_can[ind_can[ind_sig_can]]), Mean(corr_can[ind_can_croplnd[ind_sig_croplnd]]), Mean(corr_can[ind_can_rangelnd[ind_sig_rangelnd]])]
  lag_mat_sig[i+1,*] = [Mean(lag_can[ind_can[ind_sig_can]]), Mean(lag_can[ind_can_croplnd[ind_sig_croplnd]]), Mean(lag_can[ind_can_rangelnd[ind_sig_rangelnd]])]
  
  IF (doplot EQ 1) THEN BEGIN
    pdf_ref = HISTOGRAM(corr_ref[ind_ref], BINSIZE=0.005, LOCATIONS=xbin_ref, MIN = -0.4, MAX = 1.0)
    cdf_ref = TOTAL(pdf_ref, /CUMULATIVE, /DOUBLE) / N_ELEMENTS(corr_ref[ind_ref])
    pdf_can = HISTOGRAM(corr_can[ind_can], BINSIZE=0.005, LOCATIONS=xbin_can, MIN = -0.4, MAX = 1.0)
    cdf_can = TOTAL(pdf_can, /CUMULATIVE, /DOUBLE) / N_ELEMENTS(corr_can[ind_can])
    ;  h0 = PLOT(xbin_ref, pdf_ref/TOTAL(pdf_ref)*100, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i], XTITLE='Correlation wiith zFAPAR', $
    ;       YTITLE='% of pixels', AXIS_STYLE=1, COLOR='red', NAME = ref)
    ;  h1 = PLOT(xbin_can, pdf_can/TOTAL(pdf_can)*100, COLOR='blue', OVERPLOT = 1, NAME = list_candidates[i])
    ;  !null = LEGEND(target=[h0, h1], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
    h0 = PLOT(xbin_ref, cdf_ref, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i] + ' Overall r', XTITLE='Correlation wiith zFAPAR', $
      YTITLE='Cumulative Frequency', AXIS_STYLE=1, COLOR='red', NAME = ref)
    h1 = PLOT(xbin_can, cdf_can, COLOR='blue', OVERPLOT = 1, NAME = list_candidates[i])
    !null = LEGEND(target=[h0, h1], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
    
    ;only sig
    pdf_ref = HISTOGRAM(corr_ref[ind_ref[ind_sig_ref]], BINSIZE=0.005, LOCATIONS=xbin_ref, MIN = -0.4, MAX = 1.0)
    cdf_ref = TOTAL(pdf_ref, /CUMULATIVE, /DOUBLE) / N_ELEMENTS(corr_ref[ind_ref[ind_sig_ref]])
    pdf_can = HISTOGRAM(corr_can[ind_can[ind_sig_can]], BINSIZE=0.005, LOCATIONS=xbin_can, MIN = -0.4, MAX = 1.0)
    cdf_can = TOTAL(pdf_can, /CUMULATIVE, /DOUBLE) / N_ELEMENTS(corr_can[ind_can[ind_sig_can]])
    ;  h0 = PLOT(xbin_ref, pdf_ref/TOTAL(pdf_ref)*100, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i], XTITLE='Correlation wiith zFAPAR', $
    ;       YTITLE='% of pixels', AXIS_STYLE=1, COLOR='red', NAME = ref)
    ;  h1 = PLOT(xbin_can, pdf_can/TOTAL(pdf_can)*100, COLOR='blue', OVERPLOT = 1, NAME = list_candidates[i])
    ;  !null = LEGEND(target=[h0, h1], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
    h0 = PLOT(xbin_ref, cdf_ref, XRANGE=[-0.5,1,0], TITLE=ref + ' & ' + list_candidates[i] + ' Sig r', XTITLE='Correlation wiith zFAPAR', $
      YTITLE='Cumulative Frequency', AXIS_STYLE=1, COLOR='red', NAME = ref)
    h1 = PLOT(xbin_can, cdf_can, COLOR='blue', OVERPLOT = 1, NAME = list_candidates[i])
    !null = LEGEND(target=[h0, h1], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
  ENDIF
ENDFOR
PRINT, '             Overall      Cropland     Rangeland'
PRINT, ref,  'r= ', STRTRIM(TRANSPOSE(corr_mat[0, *]),2)
PRINT, ref,  'lag = ', STRTRIM(TRANSPOSE(lag_mat[0, *]),2)
FOR i = 0, N_ELEMENTS(list_candidates) -1 DO BEGIN
  PRINT, list_candidates[i], 'r= ',  STRTRIM(TRANSPOSE(corr_mat[i+1, *]),2)
  PRINT, list_candidates[i], 'lag = ', STRTRIM(TRANSPOSE(lag_mat[i+1, *]),2)
ENDFOR
PRINT, ''
PRINT, 'ONLY sig     Overall      Cropland     Rangeland'
PRINT, ref,  'r= ', STRTRIM(TRANSPOSE(corr_mat_sig[0, *]),2)
PRINT, ref,  'lag = ', STRTRIM(TRANSPOSE(lag_mat_sig[0, *]),2)
FOR i = 0, N_ELEMENTS(list_candidates) -1 DO BEGIN
  PRINT, list_candidates[i], 'r= ',  STRTRIM(TRANSPOSE(corr_mat_sig[i+1, *]),2)
  PRINT, list_candidates[i], 'lag = ', STRTRIM(TRANSPOSE(lag_mat_sig[i+1, *]),2)
ENDFOR
PRINT, ''
ind = MAX(corr_mat[*,0], ind_max)
PRINT, 'Max overall correlation, all sign and not sign'
IF (ind_max EQ 0) THEN BEGIN
  PRINT, 'Best for ' + ref
ENDIF ELSE BEGIN
  PRINT, list_candidates[ind_max-1], 'r= ',  STRTRIM(TRANSPOSE(corr_mat[ind_max, *]),2)
  PRINT, list_candidates[ind_max-1], 'lag = ', STRTRIM(TRANSPOSE(lag_mat[ind_max, *]),2)
ENDELSE
PRINT, ''
ind = MAX(corr_mat_sig[*,0], ind_max)
PRINT, 'Max overall correlation, ONLY sign'
IF (ind_max EQ 0) THEN BEGIN
  PRINT, 'Best for ' + ref
ENDIF ELSE BEGIN
  PRINT, list_candidates[ind_max-1], 'r= ',  STRTRIM(TRANSPOSE(corr_mat_sig[ind_max, *]),2)
  PRINT, list_candidates[ind_max-1], 'lag = ', STRTRIM(TRANSPOSE(lag_mat_sig[ind_max, *]),2)
ENDELSE



;PRINT to csv
OPENW, lun, dir + '\AAA_comparison_' + ref +'_' + var_can + 'up2'+ STRTRIM(up2,2) + '.csv', WIDTH = 500, /GET_LUN
PRINTF, lun, ',r,,,lag,,,Sig/Total'
PRINTF, lun, ',Overall,Cropland,Rangeland,Overall,Cropland,Rangeland,Overall'
PRINTF, lun, ref, ',', STRTRIM([TRANSPOSE(corr_mat[0, *]),TRANSPOSE(lag_mat[0, *]), ref_sig_fract],2) + ','
FOR i = 0, N_ELEMENTS(list_candidates) -1 DO BEGIN
  PRINTF, lun, list_candidates[i], ',', STRTRIM([TRANSPOSE(corr_mat[i+1, *]),TRANSPOSE(lag_mat[i+1, *]), can_sig_fract[i]],2) + ','
ENDFOR
PRINTF, lun, 'ONLY sig,'
PRINTF, lun, ref, ',', STRTRIM([TRANSPOSE(corr_mat_sig[0, *]),TRANSPOSE(lag_mat_sig[0, *])],2)+','
;PRINTF, lun, ref,  ',r=, ',  STRTRIM(TRANSPOSE(corr_mat_sig[0, *]),2) + ',', $
;                   'lag =,', STRTRIM(TRANSPOSE(lag_mat_sig[0, *]),2) + ','
FOR i = 0, N_ELEMENTS(list_candidates) -1 DO BEGIN
  PRINTF, lun, list_candidates[i], ',', STRTRIM([TRANSPOSE(corr_mat_sig[i+1, *]),TRANSPOSE(lag_mat_sig[i+1, *])],2) + ','
;  PRINTF, lun, list_candidates[i] + ',r=,',  STRTRIM(TRANSPOSE(corr_mat_sig[i+1, *]),2) + ',', $
;                                    'lag =,', STRTRIM(TRANSPOSE(lag_mat_sig[i+1, *]),2) + ','
ENDFOR
ind = MAX(corr_mat[*,0], ind_max)
PRINTF, lun, 'Max overall correlation, all sign and not sign'
IF (ind_max EQ 0) THEN BEGIN
  PRINTF, lun, 'Best for ' + ref
ENDIF ELSE BEGIN
  PRINTF, lun, list_candidates[ind_max-1], ',', STRTRIM([TRANSPOSE(corr_mat[ind_max, *]),TRANSPOSE(lag_mat[ind_max, *])],2) + ','
;  PRINTF, lun, list_candidates[ind_max-1] + ',r=,',  STRTRIM(TRANSPOSE(corr_mat[ind_max, *]),2) + ',', $
;                                            'lag =,', STRTRIM(TRANSPOSE(lag_mat[ind_max, *]),2)  + ','
ENDELSE
ind = MAX(corr_mat_sig[*,0], ind_max)
PRINTF, lun, 'Max overall correlation, ONLY sign'
IF (ind_max EQ 0) THEN BEGIN
  PRINTF, lun, 'Best for ' + ref
ENDIF ELSE BEGIN
  PRINTF, lun, list_candidates[ind_max-1], ',', STRTRIM([TRANSPOSE(corr_mat_sig[ind_max, *]),TRANSPOSE(lag_mat_sig[ind_max, *])],2) + ','
;  PRINTF, lun, list_candidates[ind_max-1] + ',r=,',  STRTRIM(TRANSPOSE(corr_mat_sig[ind_max, *]),2)  + ',', $
;                                            'lag =,', STRTRIM(TRANSPOSE(lag_mat_sig[ind_max, *]),2) + ','
ENDELSE
FREE_LUN, lun

END


;PRINT, '************************************************'
;PRINT, 'Mean Corr'
;PRINT, '             Overall      Cropland     Rangeland'
;
;PRINT, ref + '    ', Mean(corr_ref[ind_ref]), Mean(corr_ref[ind_ref_croplnd]), Mean(corr_ref[ind_ref_rangelnd])
;PRINT, list_candidates[i], Mean(corr_can[ind_can]), Mean(corr_can[ind_can_croplnd]), Mean(corr_can[ind_can_rangelnd])
;PRINT, 'Best lag'
;PRINT, '             Overall      Cropland     Rangeland'
;PRINT, ref + '    ', Mean(lag_ref[ind_ref]), Mean(lag_ref[ind_ref_croplnd]), Mean(lag_ref[ind_ref_rangelnd])
;PRINT, list_candidates[i], Mean(lag_can[ind_can]), Mean(lag_can[ind_can_croplnd]), Mean(lag_can[ind_can_rangelnd])