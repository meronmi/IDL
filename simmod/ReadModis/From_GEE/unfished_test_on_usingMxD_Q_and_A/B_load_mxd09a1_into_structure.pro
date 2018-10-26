PRO test_it
path = 'E:\SimMod_data\MODIS_data\JOSH_DATA\250m\8day ref products\';'X:\Active Projects\GEE\pixel extraction'
fname = 'aabMOD09A1_c5_profiles.csv'
res = B_load_MxD09A1_into_structure(path, fname)
help, res
END

FUNCTION B_load_MxD09A1_into_structure, path, fname

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
;bands to be directly saved in output without further processing
bnames2output = ['RelativeAzimuth', 'SolarZenith', 'ViewZenith', $
  'sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','sur_refl_b05','sur_refl_b06','sur_refl_b07']

quality_threshold = 0 ;(0 = corrected product produced at ideal quality all bands)

;*****************************
fullPathFname = path + '\' + fname
res = QUERY_CSV(fullPathFname, Info)
IF (res NE 1) THEN STOP
nlines = info.lines
nfields = info.nfields

data = READ_CSV(fullPathFname, COUNT = nObs, HEADER=hdrRow, MISSING_VALUE=outNoData)
IF (nfields NE 20) THEN BEGIN
  PRINT, 'The number and type of fields is hardcoded (see code)'
  STOP
ENDIF

data = rename_tags(data, TAG_NAMES(data), ['SysInd','Code','DOY','Igbp','Extended','Lat','Lon','QA','RA','SZA','StateQA','VZA', $
  'sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','sur_refl_b05','sur_refl_b06','sur_refl_b07','Useless']) 

tableHdr = ['GeeSysInd','Code', 'qc', 'Year', 'DOY_comp', 'Year.dayfract', 'Year_acq', 'DOY_acq', 'RAA', 'SZA', 'VZA', $
  'R1_645', 'R2_858.5', 'R3_469', 'R4_555', 'R5_1240', 'R6_1640', 'R7_2130']
table = FLTARR(15, nobs)*!VALUES.F_NAN
;table
;0 year comp
;1 doy comp
;2 year.dayfract of acq
;3 year of acq (preliminary, than adjusted)
;4 doy of acq
;5 'RelativeAzimuth',
;6 'SolarZenith'
;7 'ViewZenith'
;8 'sur_refl_b01'
;9 'sur_refl_b02'
;10 'sur_refl_b03'
;11 'sur_refl_b04'
;12 'sur_refl_b05'
;13 'sur_refl_b06'
;14 'sur_refl_b07

;fill the table

;get the site code
ind = WHERE(hdrRow EQ 'Code')
code = data.(ind)

;Year, DOY_comp, Year.dayfract, Year_acq
ind = WHERE(hdrRow EQ 'id' OR hdrRow EQ 'system:index')
sysInd = data.(ind)
tmp = STRSPLIT(data.(ind), '_', /EXTRACT)
tmp2 = tmp.ToArray()
table[0,*] = FIX(tmp2[*,2]) ;be care data type dependent
table[1,*] = ddmmyyyy2doy(tmp2[*,4], tmp2[*,3], tmp2[*,2])



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
ind = WHERE(hdrRow EQ 'QA')
qc = data.(ind)
ind = WHERE (qc NE outNoData)
FOR i = 0, N_ELEMENTS(qc[ind])-1 DO BEGIN
  tmp = BINARY(qc[ind[i]])
  qc[ind[i]] = 2^0*FIX(tmp[31])+2^1*FIX(tmp[30])
ENDFOR


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
ind = WHERE (actDOY NE outNoData)
ind2corr = WHERE((table[1,ind] GE 358) AND (actDOY[ind] LT table[1,ind]), count2corr)
IF (count2corr GT 0) THEN BEGIN
  ;set the correct year for those matching the criterium
  table[3,ind[ind2corr]] = table[0,ind[ind2corr]] + 1
  ;now check that the following obs is not repeated (it happens)
  indToRemove = WHERE(table[4,ind[ind2corr]] EQ table[4,ind[ind2corr]+1], countToRemove)
  IF (countToRemove GT 0) THEN table[5:*,ind[ind2corr]] = outNoData
  ;(the first of Y+1 is not the same, in the case remove this one)
ENDIF




;Year.dayfract
table[2,ind] = table[3,ind] + table[4,ind]/367.0
;remove records that are all not available (year.dayfract is not available)
ind = WHERE(FINITE(table[2,*]), count)
table = table[*,ind]
code = code[ind]
qc = qc[ind]
sysInd = sysInd[ind]

;remove record that do not have valid angles or reflectances in red and nir (it has happened that red was missing)
ind = WHERE(FINITE(table[5,*]) AND FINITE(table[6,*]) AND FINITE(table[7,*]) AND FINITE(table[8,*]) AND FINITE(table[9,*]), count)
table = table[*,ind]
code = code[ind]
qc = qc[ind]
sysInd = sysInd[ind]


;set remaining NaN to nodata value
ind = WHERE(FINITE(table) EQ 0)
;set -9999 for NaN
table[ind]  = FLOAT(outNoData)


;move it to a strcuture
stru = CREATE_STRUCT('GeeSysInd', sysInd, 'code', code, 'qc', qc, $
  'Year_comp', table[0,*], 'Doy_comp', table[1,*], 'YearFract', table[2,*], 'Year_acq', table[3,*], 'Doy_acq', table[4,*], $
  'RA', table[5,*], 'SZA', table[6,*], 'VZA', table[7,*], 'R1', table[8,*], 'R2', table[9,*], $
  'R3', table[10,*], 'R4', table[11,*], 'R5', table[12,*], 'R6', table[13,*], 'R7', table[14,*])

RETURN, stru
END