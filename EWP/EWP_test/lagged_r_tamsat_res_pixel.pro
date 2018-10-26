;PRO caller
;;'SPI1' ;'zEWP250''zEWP500''zEWP1000''zEWP1500''zEWP2000'
;;hl = 'N'+['zEWP250','zEWP500','zEWP1000','zEWP1500','zEWP2000','zEWP2500','zEWP3000','zEWP3500','zEWP5000']
;;hl = 'N'+['zEWP1500']
;hl = ['zCWP1500']
;FOR i = 0, N_ELEMENTS(hl)-1 DO lagged_r_TAMSAT_res, hl[i]
;END
;
;PRO callerSPI
;  ;lagged_r_TAMSAT_res, 'SPI1'
;  lagged_r_TAMSAT_res, 'SPI7'
;  lagged_r_TAMSAT_res, 'SPI8'
;  lagged_r_TAMSAT_res, 'SPI10'
;  lagged_r_TAMSAT_res, 'SPI12'
;END


PRO lagged_r_TAMSAT_res_pixel
sample = 589-1
line = 133-1
;xvar = 'zCWP2000' 
xvar = 'SPI7'

lag = INDGEN(13)
nLag = N_ELEMENTS(lag)


;PARAMETERS: files and dirs
mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
fn_eco = '5ecoregions.img' ;any value or 255 (no region)
fn_glc = 'glc_reclass.img' ;1 is grasslan, 2 is crop
fn_intersection = 'eco_crop_pasture_res_TAMSAT.img'
ns = 1867
nl = 348

first_year = 1999
first_dek = 1
last_year = 2013
last_dek = 36
deks = !NULL
FOR i = 0, (last_year-first_year) DO deks=[deks,  1 + INDGEN(36)]


;phenology
pheno_dir = 'E:\WA\EWP\Pheno_at_tamsat_res'
fn_start = pheno_dir + '\1gspy_1s_A1sos-1997_sos1_DOC_TZPavg_res_to_Tamsat.img'
fn_stop = pheno_dir + '\1gspy_1s_A1sos-1997_eos1_DOC_TZPavg_res_to_Tamsat.img
fn_TOM = pheno_dir + '\1gspy_1s_A1sos-1997_maxt1_DOC_TZPavg_res_to_Tamsat.img' 
;y variable (Z-ndvi)
;this must be a byte bil file from dek 1 of 1999, to dek 36 of 2013
y_label = 'ZFAPAR'
fn_y = 'E:\WA\EWP\FAPAR_Z-score_at_Tamsat_res\AAA_zFAPAR_at_TAMSAT_res_bil'

nb_y = 36*15
nb_x = nb_y
IF (STRMID(xvar, 0, 1)) EQ 'N' THEN BEGIN
  xvar0 = STRMID(xvar, 0, 5)
  xvar1 = STRMID(xvar, 5, STRLEN(xvar)-1)
  xvar = xvar0
ENDIF ELSE BEGIN
  IF (STRMID(xvar, 0, 3) NE 'SPI') THEN BEGIN
    xvar0 = STRMID(xvar, 0, 4)
    xvar1 = STRMID(xvar, 4, STRLEN(xvar)-1)
    xvar = xvar0
  ENDIF
ENDELSE
;x variable (SPIX or any EWP)
CASE xVar OF
  'SPI1': BEGIN
  x_label = xVar
  ;For SPI1 note that SPIRITS assing the deakd to the first dekad of the 3 deks composite
  ;so, to be aligned with zfapar data, spi1 starts at dek 35 (so dekad 35, 36, 1) of 2008
  fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\AAA_spi6_9835-1234_bil'
  OPENR, R2, fn_x, /GET_LUN
  line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI6': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI4': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI5': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI7': BEGIN
      x_label = xVar
      fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
      OPENR, R2, fn_x, /GET_LUN
      line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI8': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  
  'SPI9': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI10': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'SPI12': BEGIN
    x_label = xVar
    fn_x = 'E:\WA\EWP\SPI1 from Original Tamsat\BBB_'+xVar+'_1-1999_36-2013_bil'
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
  END
  'zEWP': BEGIN
    x_label = xVar + xvar1
    ;EWP is already aligned
    fn_x = 'E:\WA\EWP\EWP_images\standardized_ewp_hl'+ xvar1
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, FLTARR(ns,nb_x))
  END 
  'zCWP': BEGIN
    x_label = xVar + xvar1
    ;EWP is already aligned
    fn_x = 'E:\WA\EWP\EWP_images\standardized_cwp_hl'+ xvar1
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, FLTARR(ns,nb_x))
  END
  'NzEWP': BEGIN
    x_label = xVar + xvar1
    ;EWP is already aligned
    fn_x = 'E:\WA\EWP\EWP_images\Nstandardized_ewp_hl'+ xvar1
    OPENR, R2, fn_x, /GET_LUN
    line_ass_x = ASSOC(R2, FLTARR(ns,nb_x))
  END
  ELSE: STOP
ENDCASE

;open the y file
OPENR, R1, fn_y, /GET_LUN
line_ass_y = ASSOC(R1, BYTARR(ns,nb_y))
;****************************


mask = BYTARR(ns,nl)*0B

res = FILE_SEARCH(mask_dir+'\'+fn_intersection)
IF (res EQ '') THEN BEGIN
ENDIF ELSE BEGIN
  ;load the intersection
  OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
  READU, lun, mask
  FREE_LUN, lun
ENDELSE

;as correlation must be serached on ZFAPAR only during the growing season, open and read SOS and EOS (or maxt?)
;sos
OPENR, R3, fn_start, /GET_LUN
start_doy_image = FLTARR(ns, nl)
READU, R3, start_doy_image
CLOSE, R3
ind = WHERE(start_doy_image EQ -9999)
start_doy_image[ind] = !VALUES.F_NAN

;eos
OPENR, R3, fn_stop, /GET_LUN
stop_doy_image = FLTARR(ns, nl)
READU, R3, stop_doy_image
CLOSE, R3
ind = WHERE(stop_doy_image EQ -9999)
stop_doy_image[ind] = !VALUES.F_NAN

;time of max
OPENR, R3, fn_TOM, /GET_LUN
tom_doy_image = FLTARR(ns, nl)
READU, R3, tom_doy_image
CLOSE, R3
ind = WHERE(tom_doy_image EQ -9999)
tom_doy_image[ind] = !VALUES.F_NAN

;now transform it into dekadal
start_dek_image = doy2dek(start_doy_image)
stop_dek_image = doy2dek(stop_doy_image)
tom_dek_image = doy2dek(tom_doy_image)


best_lag  = FLTARR(ns, nl, 2) * !VALUES.F_NAN
;best_lagn = best_lag
best_crossCorr = FLTARR(ns, nl, 2) * !VALUES.F_NAN
crossCorr = FLTARR(ns, nl, nLag, 2) * !VALUES.F_NAN
;best_crossCorrn = best_crossCorr
bestPval = FLTARR(ns, nl, 2) * !VALUES.F_NAN
Pval = FLTARR(ns, nl, nLag, 2) * !VALUES.F_NAN
;bestPvaln = best_crossCorr
;loop on all lines
FOR l = 0, nl-1 DO BEGIN
  ;IF ((l MOD 100) EQ 0) THEN PRINT, 'Line: ' + STRTRIM(l,2)
  IF (l EQ line) THEN BEGIN 
    y = FLOAT(line_ass_y[l]) ;fapar
    ;scale and treat NaN
    indNan = WHERE((y GT 250), count_NaN)
    IF (count_NaN GT 0) THEN y[indNan] = !VALUES.F_NAN
    y = y * 0.02 - 2.5
    count_NaN  = 0
    
    x = FLOAT(line_ass_x[l]) ;spi1 or ewp
    indFin = WHERE(FINITE(x))
    indNan = WHERE((x[indFin] GT 10000) OR (x[indFin] LT -10000), count_NaN)
    IF (count_NaN GT 0) THEN x[indFin[indNan]] = !VALUES.F_NAN
    ;scale if it's SPI, EWP is scaled already
    IF (STRMID(xVar,0,3) EQ 'SPI') THEN BEGIN
      x = x * 0.001
    ENDIF
    count_NaN  = 0
   
    tomEQsos = 0L
    ;loop on samples
    FOR s = 0, ns-1 DO BEGIN
      IF (s EQ sample) THEN BEGIN
        IF (mask[s,l] GT 0) THEN BEGIN
    ;      IF ((s EQ 1517) AND (l EQ 858)) OR ((s EQ 2597) AND (l EQ 811)) THEN BEGIN
    ;        PRINT, s, l
    ;      ENDIF  
          sos = start_dek_image[s,l]
          eos = stop_dek_image[s,l]
          tom = tom_dek_image[s,l]
          stopTime = [eos, tom]
          
          FOR k = 0, 1 DO BEGIN ;k is the index for lag
            ;processing between sos and eos
            yy = y[s,*]
            xx = x[s,*]
            IF ((k EQ 1) AND (stopTime[k] EQ sos)) THEN BEGIN
              ;case where TOM eq SOS
              ;stopTime[k] = stopTime[k]
              tomEQsos = tomEQsos + 1 
            ENDIF
            ;associated I have deks [1,2,3..36,1,2,3..]
            ;check if is not NaN
            IF (((FINITE(sos))) AND (((FINITE(stopTime[k])))) AND $
               (TOTAL(FINITE(yy)) GT nb_y/2) AND (TOTAL(FINITE(xx)) GT nb_x/2)) THEN BEGIN
              ;set to NaN those fapar values outside growing season
              IF (stopTime[k] LT sos) THEN BEGIN ;it's a season crossing the dek 1
                ;ind_outside = WHERE(((deks LT sos) AND (deks GT stopTime[k])) OR ((deks GT stopTime[k]) AND (deks LT sos)) , count_outside)
                ind_outside = WHERE((deks LT sos) AND (deks GT stopTime[k]), count_outside)
                yy[ind_outside] = !VALUES.F_NAN
              ENDIF
              IF (stopTime[k] GT sos) THEN BEGIN ;it's a season not crossing the dek 1
                ind_outside = WHERE((deks LT sos) OR (deks GT stopTime[k]), count_outside)
                yy[ind_outside] = !VALUES.F_NAN
              ENDIF
              
              ;now I've excluded the yy outside growing season period
              ;and I can proceed with the lagged analysis
              
              ;note on crosscorr: a positve lag means that x is antipated compared to y, so
              ;for a lag of 5, y(t) goes with x(t-5)
              res = lagged_corr_mic(xx, yy, Lag, DOUBLE=1, NAN=1)
              h0 = PLOT(xx, TITLE=xvar, DIMENSIONS=[1200,200]) ;variable
              h1 = PLOT(yy, COLOR='red', OVERPLOT=1) ;z fapar
              cCorr = res[*,0] 
              pval0 = res[*,1]
              IF (cCorr[0] EQ -10000) THEN STOP
              best_crossCorr[s,l,k] = MAX(cCorr, Max_sub, /NAN)
              crossCorr[s,l,*,k] = cCorr
              best_lag[s,l,k] = lag[Max_sub]
              bestPval[s,l,k] = pval0[Max_sub]
              Pval[s,l,*,k] = pVal0
            ENDIF
          ENDFOR ; k (sos or tom)
        ENDIF ;mask
      ENDIF
    ENDFOR ;
  ENDIF
ENDFOR  ;l
PRINT,'Number of pix where tom eq sos: ' + STRTRIM(tomEQsos,2)
best_crossCorr = FLOAT(best_crossCorr)
;write the best corr at pixel level
;open ouput images
;output images
stopTimeLbl = ['_up2eos', '_up2TOM']
fn_out1 = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_bestLag' + stopTimeLbl
fn_out2 = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_bestCrossCorr' + stopTimeLbl
fn_out3 = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_bestPval' + stopTimeLbl
W1 =LONARR(2)
W2 = W1 & W1n = W1 & W2n = W1 & W3 = W1 & W3n = W1
FOR i = 0,1 DO BEGIN
  OPENW, lun, fn_out1[i], /GET_LUN
  W1[i] = lun
  OPENW, lun, fn_out2[i], /GET_LUN
  W2[i] = lun
  OPENW, lun, fn_out3[i], /GET_LUN
  W3[i] = lun
ENDFOR
FOR i = 0,1 DO BEGIN
  WRITEU, W1[i], REFORM(best_lag[*,*,i])
  WRITEU, W2[i], REFORM(best_crossCorr[*,*,i])
  WRITEU, W3[i], REFORM(bestPval[*,*,i])
;  WRITEU, W1n[i], REFORM(best_lagn[*,*,i])
;  WRITEU, W2n[i], REFORM(best_crossCorrn[*,*,i])
;  WRITEU, W3n[i], REFORM(bestPvaln[*,*,i])
  ;write hdrs
  OPENW, lun, fn_out1[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
  OPENW, lun, fn_out2[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
  OPENW, lun, fn_out3[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
;  OPENW, lun, fn_out1n[i]+'.hdr', /GET_LUN
;  PRINTF, lun, 'ENVI'
;  PRINTF, lun, 'file type = ENVI standard'
;  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
;  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
;  PRINTF, lun, 'bands = 1'
;  PRINTF, lun, 'interleave = bsq'
;  PRINTF, lun, 'data type = 4'
;  FREE_LUN, lun
;  OPENW, lun, fn_out2n[i]+'.hdr', /GET_LUN
;  PRINTF, lun, 'ENVI'
;  PRINTF, lun, 'file type = ENVI standard'
;  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
;  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
;  PRINTF, lun, 'bands = 1'
;  PRINTF, lun, 'interleave = bsq'
;  PRINTF, lun, 'data type = 4'
;  OPENW, lun, fn_out3n[i]+'.hdr', /GET_LUN
;  PRINTF, lun, 'ENVI'
;  PRINTF, lun, 'file type = ENVI standard'
;  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
;  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
;  PRINTF, lun, 'bands = 1'
;  PRINTF, lun, 'interleave = bsq'
;  PRINTF, lun, 'data type = 4'
;  FREE_LUN, lun
ENDFOR  ;i

;write them
stopTimeLbl = ['_up2eos', '_up2TOM']
fn_out1 = 'E:\WA\EWP\CROSSCORR\' + 'Fixed_lag_' + x_label + '_' + y_label + '_bestLag' + stopTimeLbl
fn_out2 = 'E:\WA\EWP\CROSSCORR\' + 'Fixed_lag_' + x_label + '_' + y_label + '_bestCrossCorr' + stopTimeLbl
fn_out3 = 'E:\WA\EWP\CROSSCORR\' + 'Fixed_lag_' + x_label + '_' + y_label + '_bestPval' + stopTimeLbl
W1 =LONARR(2)
W2 = W1 & W1n = W1 & W2n = W1 & W3 = W1 & W3n = W1
FOR i = 0,1 DO BEGIN
  OPENW, lun, fn_out1[i], /GET_LUN
  W1[i] = lun
  OPENW, lun, fn_out2[i], /GET_LUN
  W2[i] = lun
  OPENW, lun, fn_out3[i], /GET_LUN
  W3[i] = lun
ENDFOR
FOR i = 0,1 DO BEGIN
  ;compute and write the best corr at image level, select on the base of the maximum number of
  ;pixels with pVal < 0.05
  n_sig_by_lag = FLTARR(nLag)
  FOR l =  0, nLag-1 DO BEGIN
    ind = WHERE(Pval[*,*,l,i] LT 0.05, count)
    n_sig_by_lag[l] = count
    PRINT, '  Lag    n_significant'
    PRINT, l, n_sig_by_lag[l]
  ENDFOR
  max_n = MAX(n_sig_by_lag, max_sub)
  PRINT, 'Best lag = ' + STRTRIM(lag[max_sub],2)
  WRITEU, W1[i], REFORM(REFORM(crossCorr[*,*,max_sub,i])*0.0+FLOAT(lag[max_sub]))
  WRITEU, W2[i], REFORM(crossCorr[*,*,max_sub,i])
  WRITEU, W3[i], REFORM(Pval[*,*,max_sub,i])
  ;write hdrs
  OPENW, lun, fn_out1[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
  OPENW, lun, fn_out2[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
  OPENW, lun, fn_out3[i]+'.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = 1'
  PRINTF, lun, 'interleave = bsq'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
ENDFOR  ;i
CLOSE, /ALL
PRINT, 'Finished'
END