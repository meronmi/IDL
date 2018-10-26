PRO extract4ValentinaConti
fn_EA = 'F:\CumulatedNDVIfromX\Malawi_EA_codes_raster.img'
;this is yera by year from 2003
fn_cumNDVI ='F:\CumulatedNDVIfromX\AAAA_cNDVI_by_year_2003-2016.img'

fn_avgSOS = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_s_adj_avgx100_1.bil'
fn_dSOS = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_s_adj_anomX100_1_bandAligned2years.bil

fn_avgGSL = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_l_adj_avgx100_1.bil'
fn_dGSL = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_l_adj_anomX100_1_bandAligned2years.bil

saveFile = 'F:\CumulatedNDVIfromX\malawi.sav'
fn_out =  'F:\CumulatedNDVIfromX\Malawi_cndvi_sos_ea2.csv'
workOnSaved = 1

;window cut out to make the code faster   
xwin = [23800, 24250]
ywin = [9400,10350]

IF (workOnSaved EQ 0) THEN BEGIN
  ;open files
  ea = ReadEnviWithHdr(fn_EA)
  ea = ea[xwin[0]:xwin[1],ywin[0]:ywin[1]]
  avgSOS = ReadEnviWithHdr(fn_avgSOS)
  avgSOS = avgSOS[xwin[0]:xwin[1],ywin[0]:ywin[1]]
  dSOS = ReadBilWithHdr(fn_dSOS)
  dSOS = dSOS[xwin[0]:xwin[1],ywin[0]:ywin[1],*]
  avgGSL = ReadEnviWithHdr(fn_avgGSL)
  avgGSL = avgGSL[xwin[0]:xwin[1],ywin[0]:ywin[1]]
  dGSL = ReadBilWithHdr(fn_dGSL)
  dGSL = dGSL[xwin[0]:xwin[1],ywin[0]:ywin[1],*]
  cNDVI = ReadEnviWithHdr(fn_cumNDVI)
  cNDVI = cNDVI[xwin[0]:xwin[1],ywin[0]:ywin[1],*]
  SAVE, /ALL, FILENAME = saveFile
ENDIF ELSE BEGIN
  RESTORE, saveFile
ENDELSE
;loop on EAs
;get unique values 
ea_list = WHERE(ea NE 0)
ea_list = ea[ea_list]
ea_list = ea_list[SORT(ea_list)]
ea_list = ea_list[UNIQ(ea_list)]
n = N_ELEMENTS(ea_list)
PRINT, 'n EA = ', n
PRINT, 'min EA = ', MIN(ea_list), '   max EA = ', MAX(ea_list)
OPENW, lun, fn_out, /GET_LUN
PRINTF, lun, 'EA_id, nEA, year, spatial_avg_SOS, temporal_SD_SOS, devSOS, nSOS, spatial_avg_GSL, temporal_SD_GSL, devGSL, avgCNDVI ([-]*days), nCNDVI'
FOR i = 0, N_ELEMENTS(ea_list)-1 DO BEGIN
  indEA = WHERE(ea EQ ea_list[i], nEA)
  EASOS = avgSOS[indEA]
  EAGSL = avgGSL[indEA]
  indFinAvgSOS = WHERE(EASOS LE 3600)
;  IF (ea_list[i] EQ 10536008) THEN BEGIN
;    PRINT, 'debug'
;  ENDIF
  ;check for NAN vals
  allSOSforEA = [!NULL]
  allGSLforEA = [!NULL]
  FOR j = 0, 13 DO BEGIN
    tmp = dSOS[*,*,j]
    EAdSOS = tmp[indEA]
    tmp = dGSL[*,*,j]
    EAdGSL = tmp[indEA]
    indFindSOS = WHERE(EAdSOS LE 3600, nSOS)
    IF (nSOS GT 0) THEN BEGIN
      ;check they are the same elements of avgSOS (some might be missing in dsos, important is that I have not god dsos and no avg
      tmpCheck = EASOS[indFindSOS] ;these must be finite
      IF (MAX(EASOS[indFindSOS]) GT 3600) THEN STOP 
      ;IF (TOTAL(indFinAvgSOS-indFindSOS) NE 0) THEN STOP
      ;they are, putthem toghether, express 0-360, make the circular avg (mean_vec0_36)
      sosYear = mean_vec0_36(ROUND((EAdSOS[indFinAvgSOS] + EASOS[indFinAvgSOS])/100.0))
      gslYear = MEAN((EAdGSL[indFinAvgSOS] + EAGSL[indFinAvgSOS])/100.0)
      allSOSforEA = [allSOSforEA, sosYear]
      allGSLforEA = [allGSLforEA, gslYear]
      dSosYear = MEAN(EAdSOS[indFinAvgSOS]/100.0)
      dGslYear = MEAN(EAdGSL[indFinAvgSOS]/100.0)
    ENDIF ELSE BEGIN
      sosYear = 'NAN'
      allSOSforEA = [allSOSforEA, !VALUES.F_NAN]
      allGSLforEA = [allGSLforEA, !VALUES.F_NAN]
    ENDELSE
    tmp = cNDVI[*,*,j]
    EAcNDVI = tmp[indEA]
    indFinCNDVI = WHERE(EAcNDVI NE -32000, nCNDVI)
    IF (nCNDVI GT 0) THEN $
      cNDVIYear = MEAN(EAcNDVI[indFinCNDVI]) $
      ELSE cNDVIYear = 'NAN'
    ;PRINT, STRJOIN([STRING(ea_list[i]), STRING(nEA), STRING(2003+j), STRING(sosYear), STRING(dSosYear), STRING(nSOS), STRING(cNDVIYear), STRING(nCNDVI)], ',')
    IF (j EQ 13) THEN BEGIN
       SD = sd_vec0_36(allSOSforEA)
       SDgsl = STDDEV(allGSLforEA, /NAN)
    ENDIF ELSE BEGIN
       SD = ''
       SDgsl = ''
    ENDELSE
   
    PRINTF, lun, STRJOIN([STRING(ea_list[i]), STRING(nEA), STRING(2003+j), $
                          STRING(sosYear), STRING(SD), STRING(dSosYear), STRING(nSOS), $
                          STRING(gslYear), STRING(SDgsl), STRING(dGslYear), $
                          STRING(cNDVIYear), STRING(nCNDVI)], ',')
  ENDFOR
ENDFOR

FREE_LUN, lun
END