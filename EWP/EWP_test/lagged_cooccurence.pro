;PRO lagged_cooccurence
;;object:
;;Test to what extent negative RFE derived metrics (SPI1, SPI3, EWP_various_Phalf) can predict negative Z-NDVI anomaly.
;;The focus is to describe if 
;;1 a negative anomaly of a RFE metric results, with a lag, in a negative NDVI anomaly, 
;;2 FALSE POSITIVE: a negative SPI does not produce a negative NDVI
;;3 FALSE NEGATIVE: a negative NDVI is not anticipate by a neg SPI 
;;
;;Space: we work at pixel level 
;;Time: only within SOS-EOS
;
;lag = INDGEN(15)
;
;;PARAMETERS: files and dirs
;mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
;fn_eco = '5ecoregions.img' ;any value or 255 (no region)
;fn_glc = 'glc_reclass.img' ;1 is grasslan, 2 is crop
;fn_intersection = 'eco_crop_pasture'
;ns = 7841
;nl = 1458
;
;first_year = 1999
;first_dek = 1
;last_year = 2013
;last_dek = 36
;deks = !NULL
;FOR i = 0, (last_year-first_year) DO deks=[deks,  1 + INDGEN(36)]
;
;
;;phenology
;fn_start = 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\sos\1gspy_1s_A1sos-1997_sos1_DOC_TZPavg.img'
;fn_stop = 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\eos\1gspy_1s_A1sos-1997_eos1_DOC_TZPavg.img'
;fn_TOM = 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\maxt\1gspy_1s_A1sos-1997_maxt1_DOC_TZPavg.img'
;;y variable (Z-ndvi)
;;this must be a byte bil file from dek 1 of 1999, to dek 36 of 2013
;y_label = 'ZFAPAR'
;fn_y = 'E:\WA\EWP\FAPAR_Z-score\AAA_zFAPAR_99-13_bil'
;
;nb_y = 36*15
;;x variable (SPIX or any EWP)
;x_label = 'SPI1'
;;For SPI1 note that SPIRITS assing the deakd to the first dekad of the 3 month composite
;;so, to be aligned with zfapar data, spi1 starts at dek 35 (so dekad 35, 36, 1) of 2008
;fn_x = 'E:\WA\EWP\SPI1_resampled2VGT\AAA_SPI1_from_dek_VGTres99-13_bil'
;nb_x = 36*15
;;output images
;stopTimeLbl = ['_up2eos', '_up2TOM']
;fn_out1 = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_bestLag' + stopTimeLbl
;fn_out2 = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_bestCrossCorr' + stopTimeLbl
;fn_out1n = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_only_neg_bestLag'+ stopTimeLbl
;fn_out2n = 'E:\WA\EWP\CROSSCORR\' + x_label + '_' + y_label + '_only_neg_bestCrossCorr'+ stopTimeLbl
;;****************************
;
;
;mask = BYTARR(ns,nl)*0B
;
;res = FILE_SEARCH(mask_dir+'\'+fn_intersection)
;IF (res EQ '') THEN BEGIN
;  mask_eco = mask
;  mask_glc = mask
;  OPENU, lun, mask_dir+'\'+fn_eco, /GET_LUN
;  READU, lun, mask_eco
;  FREE_LUN, lun
;  OPENU, lun, mask_dir+'\'+fn_glc, /GET_LUN
;  READU, lun, mask_glc
;  FREE_LUN, lun
;  ind_eco_grass = WHERE((mask_eco NE 255) AND (mask_glc EQ 1))
;  ind_eco_crop = WHERE((mask_eco NE 255) AND (mask_glc EQ 2))
;  mask[ind_eco_grass]=1
;  mask[ind_eco_crop]=2
;  OPENW, lun, mask_dir+'\'+fn_intersection, /GET_LUN
;  WRITEU, lun, mask
;  FREE_LUN, lun
;  ;load the masks and make an intersection
;  ;save the intersection
;ENDIF ELSE BEGIN
;  ;load the intersection
;  OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
;  READU, lun, mask
;  FREE_LUN, lun
;ENDELSE
;
;;open the x and y files
;OPENR, R1, fn_y, /GET_LUN
;line_ass_y = ASSOC(R1, BYTARR(ns,nb_y))
;OPENR, R2, fn_x, /GET_LUN
;line_ass_x = ASSOC(R2, INTARR(ns,nb_x))
;
;;as correlation must be serached on ZFAPAR only during the growing season, open and read SOS and EOS (or maxt?)
;;sos
;OPENR, R3, fn_start, /GET_LUN
;start_doy_image = FLTARR(ns, nl)
;READU, R3, start_doy_image
;CLOSE, R3
;;eos
;OPENR, R3, fn_stop, /GET_LUN
;stop_doy_image = FLTARR(ns, nl)
;READU, R3, stop_doy_image
;CLOSE, R3
;;time of max
;OPENR, R3, fn_TOM, /GET_LUN
;tom_doy_image = FLTARR(ns, nl)
;READU, R3, tom_doy_image
;CLOSE, R3
;;now transform it into dekadal
;start_dek_image = doy2dek(start_doy_image)
;stop_dek_image = doy2dek(stop_doy_image)
;tom_dek_image = doy2dek(tom_doy_image)
;
;;open ouput images
;W1 =LONARR(2)
;W2 = W1 & W1n = W1 & W2n = W1
;FOR i = 0,1 DO BEGIN
;  OPENW, lun, fn_out1[i], /GET_LUN
;  W1[i] = lun
;  OPENW, lun, fn_out2[i], /GET_LUN
;  W2[i] = lun
;  OPENW, lun, fn_out1n[i], /GET_LUN
;  W1n[i] = lun
;  OPENW, lun, fn_out2n[i], /GET_LUN
;  W2n[i] = lun
;ENDFOR
;
;best_lag  = FLTARR(ns, nl, 2) * !VALUES.F_NAN
;best_lagn = best_lag
;best_crossCorr = FLTARR(ns, nl, 2) * !VALUES.F_NAN
;best_crossCorrn = best_crossCorr
;
;;loop on all lines
;FOR l = 0, nl-1 DO BEGIN
;  IF ((l MOD 100) EQ 0) THEN PRINT, 'Line: ' + STRTRIM(l,2)
;  
;  y = FLOAT(line_ass_y[l]) ;fapar
;  ;scale and treat NaN
;  indNan = WHERE((y GT 250), count_NaN)
;  IF (count_NaN GT 0) THEN y[indNan] = !VALUES.F_NAN
;  y = y * 0.02 - 2.5
;  count_NaN  = 0
;  
;  x = FLOAT(line_ass_x[l]) ;spi1 or ewp
;  indNan = WHERE((x GT 10000) OR (x LT -10000), count_NaN)
;  IF (count_NaN GT 0) THEN x[indNan] = !VALUES.F_NAN
;  x = x * 0.001
;  count_NaN  = 0
; 
;  tomEQsos = 0L
;  ;loop on samples
;  FOR s = 0, ns-1 DO BEGIN
;    IF (mask[s,l] GT 0) THEN BEGIN
;;      IF ((s EQ 1517) AND (l EQ 858)) OR ((s EQ 2597) AND (l EQ 811)) THEN BEGIN
;;        PRINT, s, l
;;      ENDIF  
;      sos = start_dek_image[s,l]
;      eos = stop_dek_image[s,l]
;      tom = tom_dek_image[s,l]
;      stopTime = [eos, tom]
;      
;      FOR k = 0, 1 DO BEGIN
;        ;processing between sos and eos
;        yy = y[s,*]
;        xx = x[s,*]
;        IF ((k EQ 1) AND (stopTime[k] EQ sos)) THEN BEGIN
;          stopTime[k] = stopTime[k]
;          tomEQsos = tomEQsos + 1 
;        ENDIF
;        ;associated I have deks [1,2,3..36,1,2,3..]
;        ;check if is not NaN
;        IF (((FINITE(sos))) AND (((FINITE(stopTime[k])))) AND $
;           (TOTAL(FINITE(yy)) GT nb_y/2) AND (TOTAL(FINITE(xx)) GT nb_x/2)) THEN BEGIN
;          ;set to NaN those fapar values outside growing season
;          IF (stopTime[k] LT sos) THEN BEGIN ;it's a season crossing the dek 1
;            ind_outside = WHERE(((deks LT sos) AND (deks GT stopTime[k])) OR ((deks GT stopTime[k]) AND (deks LT sos)) , count_outside)
;            yy[ind_outside] = !VALUES.F_NAN
;          ENDIF
;          IF (stopTime[k] GT sos) THEN BEGIN ;it's a season crossing the dek 1
;            ind_outside = WHERE((deks LT sos) OR (deks GT stopTime[k]), count_outside)
;            yy[ind_outside] = !VALUES.F_NAN
;          ENDIF
;          
;          ;now I've excluded the yy outside growing season period
;          ;and I can proceed with the lagged analysis
;          
;          ;note on crosscorr: a positve lag means that x is antipated compared to y, so
;          ;for a lag of 5, y(t) goes with x(t-5)
;          cCorr = crosscorr(xx, yy, Lag, DOUBLE=1, NAN=1)
;          best_crossCorr[s,l,k] = MAX(cCorr)
;          best_lag[s,l,k] = lag[WHERE(cCorr EQ MAX(cCorr))]
;          ;cross corr limited to where y is negative
;          indpos = WHERE(yy GT 0.0)
;          xxx = xx
;          xxx[indpos] = !VALUES.F_NAN
;          yyy = yy
;          yyy[indpos] = !VALUES.F_NAN
;          cCorr = crosscorr(xxx, yyy, Lag, DOUBLE=1, NAN=1)
;          best_crossCorrn[s,l,k] = MAX(cCorr)
;          best_lagn[s,l,k] = lag[WHERE(cCorr EQ MAX(cCorr))]
;        ENDIF
;      ENDFOR ; k (sos or tom)
;    ENDIF ;mask
;  ENDFOR ;s
;ENDFOR  ;l
;PRINT,'Number of pix where tom eq sos: ' + STRTRIM(tomEQsos,2)
;best_crossCorr = FLOAT(best_crossCorr)
;FOR i = 0,1 DO BEGIN
;  WRITEU, W1[i], REFORM(best_lag[*,*,i])
;  WRITEU, W2[i], REFORM(best_crossCorr[*,*,i])
;  WRITEU, W1n[i], REFORM(best_lagn[*,*,i])
;  WRITEU, W2n[i], REFORM(best_crossCorrn[*,*,i])
;  ;write hdrs
;  OPENW, lun, fn_out1[i]+'.hdr', /GET_LUN
;  PRINTF, lun, 'ENVI'
;  PRINTF, lun, 'file type = ENVI standard'
;  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
;  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
;  PRINTF, lun, 'bands = 1'
;  PRINTF, lun, 'interleave = bsq'
;  PRINTF, lun, 'data type = 4'
;  FREE_LUN, lun
;  OPENW, lun, fn_out2[i]+'.hdr', /GET_LUN
;  PRINTF, lun, 'ENVI'
;  PRINTF, lun, 'file type = ENVI standard'
;  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
;  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
;  PRINTF, lun, 'bands = 1'
;  PRINTF, lun, 'interleave = bsq'
;  PRINTF, lun, 'data type = 4'
;  FREE_LUN, lun
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
;  FREE_LUN, lun
;ENDFOR  ;i
;CLOSE, /ALL
;PRINT, 'Finished'
;END