PRO readmoGEE_v3
  path = 'E:\SimMod_data\MODIS_data\JOSH_DATA\250m';'X:\Active Projects\GEE\pixel extraction'
  fnameMOD = 'modisVIProfiles250_MOD_v2.csv'
  fnameMYD = 'modisVIProfiles250_MYD_v2.csv'
  res = readMODIS_MOD_MYD_VIs_profileGEE_v3(path, fnameMOD, fnameMYD)
END
;
;PRO readmoGEE2
;  path = 'E:\SimMod_data\MODIS_data\Sudan';'X:\Active Projects\GEE\pixel extraction'
;  fname = 'sudan_profile_MOD09A1.csv'
;  res = readMODISprofileGEE(path, fname)
;END

FUNCTION readMODIS_MOD_MYD_VIs_profileGEE_v3, path, fnameMOD, fnameMYD
  ;read and process a time series of product MOD09A1
  ;extracted with GEE
  ;*****************************
  ;PARAMETERS
  outNoData = -99999
  bnames = ["DayOfYear", "QA", "RelativeAzimuth", "StateQA", $
    "SolarZenith", "ViewZenith", "sur_refl_b01", "sur_refl_b02", "sur_refl_b03", $
    "sur_refl_b04", "sur_refl_b05", "sur_refl_b06", "sur_refl_b07"]
  fillval = [65535, 4294967295, 0, 65535, $
    0, 0, 0, -28672, -28672, -28672, $
    -28672, -28672, -28672, -28672]
  scalefact = [1, 1, 0.01, 1, $
    0.01, 0.01, 0.0001, 0.0001, 0.0001, $
    0.0001, 0.0001, 0.0001, 0.0001]
  ;bands to be saved in output

  bnames2output = ['RelativeAzimuth', 'SolarZenith', 'ViewZenith', $
    'sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','sur_refl_b05','sur_refl_b06','sur_refl_b07']

  quality_threshold = 1 ;(0 = corrected product produced at ideal quality all bands)
  ;*****************************
  fullPathFnameMOD = path + '\' + fnameMOD
  res = QUERY_CSV(fullPathFnameMOD, Info)
  IF (res NE 1) THEN STOP
  nlinesMOD = info.lines
  nfields = info.nfields

  fullPathFnameMYD = path + '\' + fnameMYD
  res = QUERY_CSV(fullPathFnameMYD, Info)
  IF (res NE 1) THEN STOP
  nlinesMYD = info.lines
  IF (nfields NE info.nfields) THEN STOP
  dataMOD = READ_CSV(fullPathFnameMOD, COUNT = nMOD, HEADER=hdrRow, MISSING_VALUE=outNoData)
  dataMYD = READ_CSV(fullPathFnameMYD, COUNT = nMYD, HEADER=hdrRow, MISSING_VALUE=outNoData)
  nobs = nMOD + nMYD
  IF (nfields NE 19) THEN BEGIN
    PRINT, 'The number and type of fields is hardcoded (see code)'
    STOP
  ENDIF
  data =  CREATE_STRUCT($
    'FIELD01', STRARR(nMod+NMYD), 'FIELD02', STRARR(nMod+NMYD), 'FIELD03', LONARR(nMod+NMYD), $
    'FIELD04', LONARR(nMod+NMYD), 'FIELD05', LONARR(nMod+NMYD), 'FIELD06', STRARR(nMod+NMYD), $
    'FIELD07', STRARR(nMod+NMYD), 'FIELD08', DBLARR(nMod+NMYD), 'FIELD09', DBLARR(nMod+NMYD), $
    'FIELD10', LONARR(nMod+NMYD), 'FIELD11', LONARR(nMod+NMYD), 'FIELD12', LONARR(nMod+NMYD), $
    'FIELD13', LONARR(nMod+NMYD), 'FIELD14', LONARR(nMod+NMYD), 'FIELD15', LONARR(nMod+NMYD), $
    'FIELD16', LONARR(nMod+NMYD), 'FIELD17', LONARR(nMod+NMYD), 'FIELD18', LONARR(nMod+NMYD), $
    'FIELD19', STRARR(nMod+NMYD))
  FOR i = 0, 18 DO BEGIN
    data.(i)[0:nMOD-1] = dataMOD.(i)
    data.(i)[nMOD:nMOD+nMYD-1] = dataMYD.(i)
  ENDFOR

  tableHdr = ['Code', 'qc', 'Year', 'DOY_comp', 'Year.dayfract', 'Year_acq', 'DOY_acq', 'RAA', 'SZA', 'VZA', $
    'R1_645', 'R2_858.5', 'R3_469', 'R4_555', 'R5_1240', 'R6_1640', 'R7_2130']
  table = FLTARR(15, nobs)*!VALUES.F_NAN

  ;fill the table

  ;get the site code
  ind = WHERE(hdrRow EQ 'Code')
  code = data.(ind)

  ;Year, DOY_comp, Year.dayfract, Year_acq
  ind = WHERE(hdrRow EQ 'id' OR hdrRow EQ 'system:index')
  tmp = STRSPLIT(data.(ind), '_', /EXTRACT)
  tmp2 = tmp.ToArray()
  table[0,*] = tmp2[*,0] ;year for VIs
  table[1,*] = ddmmyyyy2doy(tmp2[*,2], tmp2[*,1], tmp2[*,0])
  ;FOR i = 0, N_ELEMENTS(tmp)-1 DO BEGIN
  ;  ;Year
  ;  table[0,i] = tmp[i,2]
  ;  ;DOY_comp
  ;  table[1,i] = ddmmyyyy2doy(tmp[i,4], tmp[i,3], tmp[i,2])
  ;
  ;ENDFOR

  ;To compute 'Year_acq', 'DOY_acq' take care of the fact that after 358, the composting
  ;date ma be in year Y, but the real acq in Y+1
  ind = WHERE(hdrRow EQ 'DayOfYear')
  actDOY = data.(ind)
  ind = WHERE (actDOY NE outNoData)
  ;set DOY_acq
  table[4,ind] = actDOY[ind]
  ;preliminarly set the year to the nominal one
  table[3,ind] = table[0,ind]

  ;now copy/scale the various bands
  ;first
  ;build the qc array
  ;build the qc array
  ind = WHERE(hdrRow EQ 'SummaryQA') ;VIs
  qc = data.(ind)
  ;ind = WHERE (qc NE outNoData)
  ;FOR i = 0, N_ELEMENTS(qc[ind])-1 DO BEGIN
  ;  tmp = BINARY(FIX(qc[ind[i]]))
  ;  qc[ind[i]] = 2^0*FIX(tmp[15])+2^1*FIX(tmp[14])
  ;ENDFOR

  FOR b=0, N_ELEMENTS(bnames2output)-1 DO BEGIN
    ;retrieve (fill value not becessary in GEE)scale for that band
    pos = WHERE(bnames EQ bnames2output[b])

    scale = scalefact[pos[0]]
    ind = WHERE(hdrRow EQ bnames2output[b])
    IF (ind GE 0) THEN BEGIN
      tmp = data.(ind)
      ind = WHERE (tmp NE outNoData, count)
      PRINT, 'Band ' + bnames2output[b] + ', % of missing: ' +STRTRIM((nobs-count)/FLOAT(nobs)*100.0,2)
      indBadQ = WHERE(qc[ind] GT quality_threshold, countBadQ)
      indGoodQ = WHERE((qc[ind] LE quality_threshold) AND (qc[ind] GE 0)) ;for VI SummaryQA
      PRINT, '% of bad quality obs (with respect to available): ' + STRTRIM(countBadQ/FLOAT(count),2)
      table[5+b, ind[indGoodQ]] = tmp[ind[indGoodQ]] * scale
    ENDIF
  ENDFOR


  ; set the true year of acq is made at the end
  ; where (DOY_comp GE 358) AND (DOY_acq LT DOY_comp) it means that the true acquisition was in the following year
  ;ind = WHERE (actDOY NE outNoData)
  ;ind2corr = WHERE((table[1,ind] GE 358) AND (actDOY[ind] LT table[1,ind]), count2corr)

  ;new procedure for VIS
  ;we have the problem if DOY_comp is in the last mont and DOY_acq is in the first
  ind = WHERE(actDOY NE outNoData)
  ind2corr = WHERE((table[1,ind] GE 335) AND (actDOY[ind] LT 31) AND (actDOY[ind] LT table[1,ind]), count2corr)
  IF (count2corr GT 0) THEN BEGIN
    ;set the correct year for those matching the criterium
    table[3,ind[ind2corr]] = table[0,ind[ind2corr]] + 1
    ;now check that the following obs is not repeated (it happens)
    indToRemove = WHERE(table[4,ind[ind2corr]] EQ table[4,ind[ind2corr]+1], countToRemove)
    IF (countToRemove GT 0) THEN table[5:*,ind[ind2corr]] = !VALUES.F_NAN
    ;(the first of Y+1 is not the same, in the case remove this one)
  ENDIF



  ;Year.dayfract
  table[2,ind] = table[3,ind] + table[4,ind]/367.0
  ;remove records that are all not available (year.dayfract is not available)
  ind = WHERE(FINITE(table[2,*]), count)
  table = table[*,ind]
  code = code[ind]
  qc = qc[ind]


  ;remove record that do not have valid angles or reflectances in red and nir (it has happened that red was missing)
  ind = WHERE(FINITE(table[5,*]) AND FINITE(table[6,*]) AND FINITE(table[7,*]) AND FINITE(table[8,*]) AND FINITE(table[9,*]), count)
  table = table[*,ind]
  code = code[ind]
  qc = qc[ind]


  ;set remaining NaN to nodata value
  ind = WHERE(FINITE(table) EQ 0)
  ;set -9999 for NaN
  table[ind]  = FLOAT(outNoData)

  fname_out = 'MOD_MYD_VI_combo'
  ;move it to a strcuture for use of write_csv
  stru = CREATE_STRUCT('char0', code, 'qc', STRING(qc), $
    'f0', STRING(table[0,*]), 'f1', STRING(table[1,*]), 'f2', STRING(table[2,*]), 'f3', STRING(table[3,*]), 'f4', STRING(table[4,*]), $
    'f5', STRING(table[5,*]), 'f6', STRING(table[6,*]), 'f7', STRING(table[7,*]), 'f8', STRING(table[8,*]), 'f9', STRING(table[9,*]), $
    'f10', STRING(table[10,*]), 'f11', STRING(table[11,*]), 'f12', STRING(table[12,*]), 'f13', STRING(table[13,*]), 'f14', STRING(table[14,*]))
  WRITE_CSV, path + '\' + fname_out[0] + '_cleaned.csv', stru, HEADER = tableHdr
  RETURN, 0
END

;FUNCTION get_modis_VIprofile_v3, modis_fn, site_code
;  ;to generate this csv, use R script in d:\Users\meronmi\Documents\R_scripts\MODIS-FORGE to download the data
;  ;and then run readMODISprofile.pro IDL function to geneate the csv
;
;  ;or use GEE
;
;  ;MODIS
;  ;bands R1_645 R2_858.5  R3_469  R4_555  R5_1240 R6_1640 R7_2130
;
;  ;read the csv only once, then use the sav
;  dir = FILE_DIRNAME(modis_fn)
;  base = FILE_BASENAME(modis_fn, '.csv')
;  IF (FILE_TEST(dir + '\' + base + '.sav')) THEN BEGIN
;    RESTORE, dir + '\' + base + '.sav'
;  ENDIF ELSE BEGIN
;    tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;    SAVE, tmp, hdr, FILENAME = dir + '\' + base + '.sav'
;  ENDELSE
;
;  ;tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;  ;replace -9999 and -99999 with NaN
;  ;FOR i=1, N_TAGS(tmp)-1 DO BEGIN
;  ;  ind = WHERE((tmp.(i) EQ -9999) OR (tmp.(i) EQ -99999), count)
;  ;  IF (count GT 0) THEN tmp.(i)[ind] = !VALUES.D_NAN
;  ;ENDFOR
;  modis_data = rename_tags(tmp, $
;    ['FIELD01','FIELD02','FIELD03','FIELD04','FIELD05','FIELD06','FIELD07',$
;    'FIELD08','FIELD09','FIELD10','FIELD11','FIELD12','FIELD13','FIELD14','FIELD15','FIELD16','FIELD17'], $
;    ['Site_code','qc','Year','DOY_comp','YearDayfract','Year_acq','DOY_acq','RAA','SZA','VZA','R1','R2','R3','R4','R5','R6','R7'])
;  ind = WHERE(modis_data.Site_code EQ site_code, count)
;  IF (count EQ 0) THEN STOP
;  modis_data2 = CREATE_STRUCT('Site_code', STRARR(count),'qc', INTARR(count), 'Year', INTARR(count), $
;    'DOY_comp', INTARR(count),'YearDayfract', FLTARR(count),'Year_acq',INTARR(count),$
;    'DOY_acq',DBLARR(count), 'RAA', DBLARR(count),'SZA', DBLARR(count),'VZA', DBLARR(count), $
;    'R1', DBLARR(count),'R2', DBLARR(count),'R3', DBLARR(count),'R4', DBLARR(count),'R5', DBLARR(count),$
;    'R6', DBLARR(count),'R7', DBLARR(count), 'JD', DBLARR(count))
;  FOR i = 0,16 DO BEGIN
;    ;PRINT, i
;    IF (i EQ 0) THEN BEGIN
;      modis_data2.(i) = modis_data.(i)[ind]
;    ENDIF ELSE BEGIN
;      tmp = DOUBLE(modis_data.(i)[ind])
;      ind2 = WHERE((tmp EQ -9999) OR (tmp EQ -99999), count)
;      IF (count GT 0) THEN tmp[ind2] =  !VALUES.D_NAN
;      modis_data2.(i) = tmp
;    ENDELSE
;  ENDFOR
;
;  ; here fix the fact that a date may have doy of acq but no data, if a doy is found, ref must be present
;  ind = WHERE(FINITE(modis_data2.RAA) EQ 0, count)
;  IF (count NE 0) THEN modis_data2.DOY_acq[ind] = !VALUES.F_NAN
;  modis_data2.JD = DOY_YEAR2JD(modis_data2.DOY_acq, modis_data2.Year_acq)
;  ;sort it
;  ind = SORT(modis_data2.JD)
;  FOR i = 0,17 DO BEGIN
;    modis_data2.(i) = modis_data2.(i)[ind]
;  ENDFOR
;  RETURN, modis_data2
;END