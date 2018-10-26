PRO compute_z_score
storage_dir = 'E:\WA\all sahel\data\as images\'
base_pattern = 'vt*as.img'
posYY = 2  ;position of year in filename
posTT = 4  ;position of dekad in filenam
ns = 7841
nl = 1458
ll = 0 ;line to read
input_datatype = 1
minVal = 0        ;minimum valid value
maxVal = 250      ;maximum valid value
first_YY = 1999
first_TT = 1
last_YY = 2013 ; 2013
last_TT = 36
dekarray = INDGEN(36)+1
FOR i = 0, ((last_YY-first_YY+1)-2) DO dekarray = [dekarray, INDGEN(36)+1]
;get the number of available bands
files = retrive_files(storage_dir, base_pattern, posYY, posTT, first_YY, first_TT, last_YY, last_TT)
;!!!! DEBUG !!!!!

res = READ_TIME_SERIES_BY_LINE (storage_dir, files, input_datatype, ns, ll)
nb = N_ELEMENTS(res[0,*])
;IF ((nb MOD 36) NE 0) THEN STOP
fname = 'E:\WA\EWP\FAPAR_Z-score\IDL_ZFAPAR'
OPENW, lun, 'E:\WA\EWP\FAPAR_Z-score\IDL_ZFAPAR', /GET_LUN
;line_ass_data = ASSOC(lun, FLTARR(ns,nb))
FOR i = 0, nl-1 DO BEGIN
  IF ((i MOD 50) EQ 0) THEN PRINT, '%: ' + STRTRIM(i/FLOAT(nl)*100,2)
  inData = FLOAT(READ_TIME_SERIES_BY_LINE (storage_dir, files, input_datatype, ns, i))
  ind = WHERE ((inData LT minVal) OR (inData GT maxVal), count)
  inData[ind]=!VALUES.F_NAN
  outData = inData *  !VALUES.F_NAN
  FOR tt = 1, 36 DO BEGIN
    indTT = WHERE(dekarray EQ tt)
    line_avg_lta = MEAN(inData[*,indTT], DIMENSION = 2, /NAN)
    line_sd_lta = STDDEV(inData[*,indTT], DIMENSION = 2, /NAN)
    outData[*,indTT] = (inData[*,indTT]- REBIN(line_avg_lta,N_ELEMENTS(line_avg_lta),N_ELEMENTS(indTT))) / $
                       REBIN(line_sd_lta,N_ELEMENTS(line_sd_lta),N_ELEMENTS(indTT))
  ENDFOR
  
  WRITEU, lun, outData
ENDFOR
FREE_LUN, lun

;write minimum hdr
datatype = 4
OPENW, lun, fname+'.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'file type = ENVI standard'
PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
PRINTF, lun, 'bands = ' + STRTRIM(nb, 2)
PRINTF, lun, 'interleave = bil'
PRINTF, lun, 'data type = ' + STRTRIM(datatype, 2)
PRINTF, lun, 'band names = {'
FOR i = 0, nb-2 DO BEGIN
  PRINTF, lun, files[i] + ','
ENDFOR
PRINTF, lun, files[nb-1] + '}'
FREE_LUN, lun
END