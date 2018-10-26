PRO test_it2
path = 'E:\SimMod_data\MODIS_data\JOSH_DATA\250m\8day ref products\';'X:\Active Projects\GEE\pixel extraction'
fname = 'aaaMOD09Q1_c5_profiles.csv'
res = B_load_MxD09q1_into_structure(path, fname)
help, res
END

FUNCTION B_load_MxD09Q1_into_structure, path, fname

;read and process a time series of product MOD09A1
;extracted with GEE
;*****************************
;PARAMETERS
outNoData = -99999
bnames = ["QA", "sur_refl_b01", "sur_refl_b02"]
fillval = [65535, -28672, -28672]
scalefact = [1, 0.0001, 0.0001]
;bands to be directly saved in output without further processing
bnames2output = ['sur_refl_b01','sur_refl_b02']

quality_threshold = 0 ;(0 = corrected product produced at ideal quality all bands)

;*****************************
fullPathFname = path + '\' + fname
res = QUERY_CSV(fullPathFname, Info)
IF (res NE 1) THEN STOP
nlines = info.lines
nfields = info.nfields

data = READ_CSV(fullPathFname, COUNT = nObs, HEADER=hdrRow, MISSING_VALUE=outNoData)
IF (nfields NE 10) THEN BEGIN
  PRINT, 'The number and type of fields is hardcoded (see code)'
  STOP
ENDIF

data = rename_tags(data, TAG_NAMES(data), ['SysInd','Code','Extended','Igbp','Lat','Lon','QA','sur_refl_b01','sur_refl_b02','Useless']) 

tableHdr = ['GeeSysInd','Code', 'qc', 'R1_645', 'R2_858.5'];, 'R3_469', 'R4_555', 'R5_1240', 'R6_1640', 'R7_2130']
table = FLTARR(4, nobs)*!VALUES.F_NAN
;table
;0 year comp
;1 doy comp
;2 'sur_refl_b01'
;3 'sur_refl_b02'


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



;

;now copy/scale the various bands
;first
;build the qc array
ind = WHERE(hdrRow EQ 'QA')
qc = data.(ind)
ind = WHERE (qc NE outNoData)
FOR i = 0, N_ELEMENTS(qc[ind])-1 DO BEGIN
  tmp = BINARY(qc[ind[i]])
  qc[ind[i]] = 2^0*FIX(tmp[15])+2^1*FIX(tmp[14])  ;ok for 250 m that is 16 bit
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
    table[2+b, ind[indGoodQ]] = tmp[ind[indGoodQ]] * scale
  ENDIF
ENDFOR








;remove record that do not have valid reflectances 
ind = WHERE(FINITE(table[2,*]) AND FINITE(table[3,*]), count)
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
  'Year_comp', table[0,*], 'Doy_comp', table[1,*], 'R1', table[2,*], 'R2', table[3,*])

RETURN, stru
END