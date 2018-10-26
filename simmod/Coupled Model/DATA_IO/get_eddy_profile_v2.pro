FUNCTION get_eddy_profile_v2, eddy_dir, site_code
; -9999, the standard Fluxnet no data, is assumed to be NaN, with Josh is NaN

nanVal = 'NA'
;first locate the correct file
;remove - from dite_code
site_code = STRJOIN(STRSPLIT(site_code,'-', /EXTRACT))
res = FILE_SEARCH(eddy_dir, '*'+site_code+'*') 
;now select the "best file" for this site, order 1-COMBINED, 2-EFDC, 3-FLX
IF (TOTAL(STRMATCH(res, '*COMBINED*.csv')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*COMBINED*.csv'))]  $
  ELSE IF (TOTAL(STRMATCH(res, '*EFDC*.csv')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*EFDC*.csv'))]  $
  ELSE IF (TOTAL(STRMATCH(res, '*FLX*.csv')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*FLX*.csv'))]  $
  ELSE STOP
;EDDY

;read the csv only once, then use the sav
dir = FILE_DIRNAME(res)
base = FILE_BASENAME(res, '.csv')
IF (FILE_TEST(dir + '\' + base + '.sav')) THEN BEGIN
  RESTORE, dir + '\' + base + '.sav'
ENDIF ELSE BEGIN
  tmp =  READ_CSV2(res, HEADER=hdr)
  SAVE, tmp, hdr, FILENAME = dir + '\' + base + '.sav'
ENDELSE

;BE CARE with NAN!!!!!
;tmp =  READ_CSV2(res, HEADER=hdr);, MISSING_VALUE='NA')


n = N_ELEMENTS(tmp.(0))
eddy_data = { year: INTARR(n), $
  month: INTARR(n), $
  day:  INTARR(n), $
  JD: FLTARR(n), $
  radg: FLTARR(n) * !VALUES.F_NAN, $
  ta: FLTARR(n) * !VALUES.F_NAN, $
  precip: FLTARR(n) * !VALUES.F_NAN, $
  nee_mds: FLTARR(n) * !VALUES.F_NAN, $
  gpp_mds: FLTARR(n) * !VALUES.F_NAN, $
  fapar: FLTARR(n) * !VALUES.F_NAN}


ind = WHERE(hdr EQ 'yyyy')        & eddy_data.year = FIX(tmp.(ind))
ind = WHERE(hdr EQ 'mm')       & eddy_data.month = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'dd')         & eddy_data.day = FLOAT(tmp.(ind))
  
ind = WHERE(hdr EQ 'T')
indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
IF (countFin GT 0) THEN eddy_data.ta[indFin] = tmp.(ind)[indFin]

ind = WHERE(hdr EQ 'Pr')
indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
IF (countFin GT 0) THEN eddy_data.precip[indFin] = tmp.(ind)[indFin]

ind = WHERE(hdr EQ 'Rad')
indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
IF (countFin GT 0) THEN eddy_data.radg[indFin] = tmp.(ind)[indFin]

ind = WHERE(hdr EQ 'GPP')
indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
IF (countFin GT 0) THEN eddy_data.gpp_mds[indFin] = tmp.(ind)[indFin]

ind = WHERE(hdr EQ 'NEE')
indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
IF (countFin GT 0) THEN eddy_data.nee_mds[indFin] = tmp.(ind)[indFin]

ind = WHERE(hdr EQ 'fAPAR', count)
;fapar may noy be present
IF (count GT 0) THEN BEGIN
  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
  IF (countFin GT 0) THEN eddy_data.fapar[indFin] = tmp.(ind)[indFin]
ENDIF
eddy_data.JD = JULDAY(eddy_data.month, eddy_data.day, eddy_data.year)

FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
  ind = WHERE(eddy_data.(i) EQ -9999, count)
  IF (count GT 0) THEN eddy_data.(i)[ind] = !VALUES.D_NAN
ENDFOR


;be care it may not be ordered.. 
;first order it 
indOrdered = SORT(eddy_data.JD)
FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
  eddy_data.(i) = eddy_data.(i)[indOrdered] 
ENDFOR


;may contain missing days ..
;check and fix
frstJD = MIN(eddy_data.JD, /NAN)
lstJD = MAX(eddy_data.JD, /NAN)
nn = (lstJD-frstJD+1)
IF (nn NE n) THEN BEGIN
  ;it contains missing data to be filled
  ;make the larger array
  filled = { year: INTARR(nn) - 9999, $
    month: INTARR(nn) - 9999, $
    day:  INTARR(nn) - 9999, $
    JD: FLTARR(nn) * !VALUES.F_NAN , $
    radg: FLTARR(nn) * !VALUES.F_NAN, $
    ta: FLTARR(nn) * !VALUES.F_NAN, $
    precip: FLTARR(nn) * !VALUES.F_NAN, $
    nee_mds: FLTARR(nn) * !VALUES.F_NAN, $
    gpp_mds: FLTARR(nn) * !VALUES.F_NAN, $
    fapar: FLTARR(nn) * !VALUES.F_NAN}
  ;fill the JD field
  filled.JD = frstJD + INDGEN(nn, /FLOAT)
  ;match, a, b, suba, subb
  match, eddy_data.JD, filled.JD, sub_eddy_data, sub_filled
  FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
    filled.(i)[sub_filled] = eddy_data.(i)[sub_eddy_data]
  ENDFOR 
  ;insert year month day for those added but not filled
  indNotFilled = WHERE(filled.year EQ - 9999, cNF)
  ret =  JD2DDMMYYYY(filled.JD[indNotFilled])  ;[DDvec, MMvec, YYvec]
  filled.year[indNotFilled] = ret[cNF+cNF:-1]
  filled.month[indNotFilled] = ret[cNF:cNF+cNF-1]
  filled.day[indNotFilled] = ret[0:cNF-1]
ENDIF ELSE filled = eddy_data

RETURN, filled
END



;FUNCTION get_eddy_profile_v2, eddy_dir, site_code
;  ; -9999, the standard Fluxnet no data, is assumed to be NaN, with Josh is NaN
;
;  nanVal = 'NA'
;  ;first locate the correct file
;  ;remove - from dite_code
;  site_code = STRJOIN(STRSPLIT(site_code,'-', /EXTRACT))
;  res = FILE_SEARCH(eddy_dir, '*'+site_code+'*')
;  ;now select the "best file" for this site, order 1-COMBINED, 2-EFDC, 3-FLX
;  IF (TOTAL(STRMATCH(res, '*COMBINED*')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*COMBINED*'))]  $
;  ELSE IF (TOTAL(STRMATCH(res, '*EFDC*')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*EFDC*'))]  $
;  ELSE IF (TOTAL(STRMATCH(res, '*FLX*')) GT 0) THEN res = res[WHERE(STRMATCH(res, '*FLX*'))]  $
;  ELSE STOP
;  ;EDDY
;
;  ;BE CARE NAN!!!!!
;  tmp =  READ_CSV2(res, HEADER=hdr);, MISSING_VALUE='NA')
;
;  n = N_ELEMENTS(tmp.(0))
;  eddy_data = { year: INTARR(n), $
;    month: INTARR(n), $
;    day:  INTARR(n), $
;    JD: FLTARR(n), $
;    radg: FLTARR(n) * !VALUES.F_NAN, $
;    ta: FLTARR(n) * !VALUES.F_NAN, $
;    ;vpd: FLTARR(n) * !VALUES.F_NAN, $
;    precip: FLTARR(n) * !VALUES.F_NAN, $
;    ;swc: FLTARR(n) * !VALUES.F_NAN, $
;    nee_mds: FLTARR(n) * !VALUES.F_NAN, $
;    ;nee_ann: FLTARR(n) * !VALUES.F_NAN, $
;    gpp_mds: FLTARR(n) * !VALUES.F_NAN, $
;    fapar: FLTARR(n) * !VALUES.F_NAN}
;  ;gpp_ann: FLTARR(n) * !VALUES.F_NAN}
;
;  ind = WHERE(hdr EQ 'yyyy')        & eddy_data.year = FIX(tmp.(ind))
;  ind = WHERE(hdr EQ 'mm')       & eddy_data.month = FLOAT(tmp.(ind))
;  ind = WHERE(hdr EQ 'dd')         & eddy_data.day = FLOAT(tmp.(ind))
;
;  ind = WHERE(hdr EQ 'T')
;  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;  IF (countFin GT 0) THEN eddy_data.ta[indFin] = tmp.(ind)[indFin]
;
;  ind = WHERE(hdr EQ 'Pr')
;  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;  IF (countFin GT 0) THEN eddy_data.precip[indFin] = tmp.(ind)[indFin]
;
;  ind = WHERE(hdr EQ 'Rad')
;  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;  IF (countFin GT 0) THEN eddy_data.radg[indFin] = tmp.(ind)[indFin]
;
;  ind = WHERE(hdr EQ 'GPP')
;  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;  IF (countFin GT 0) THEN eddy_data.gpp_mds[indFin] = tmp.(ind)[indFin]
;
;  ind = WHERE(hdr EQ 'NEE')
;  indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;  IF (countFin GT 0) THEN eddy_data.nee_mds[indFin] = tmp.(ind)[indFin]
;
;  ind = WHERE(hdr EQ 'fAPAR', count)
;  ;fapar may noy be present
;  IF (count GT 0) THEN BEGIN
;    indFin = WHERE(STRING(tmp.(ind)) NE nanVal, countFin)
;    IF (countFin GT 0) THEN eddy_data.fapar[indFin] = tmp.(ind)[indFin]
;  ENDIF
;  eddy_data.JD = JULDAY(eddy_data.month, eddy_data.day, eddy_data.year)
;  ;replace -9999 with NaN
;  FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
;    ind = WHERE(eddy_data.(i) EQ -9999, count)
;    IF (count GT 0) THEN eddy_data.(i)[ind] = !VALUES.D_NAN
;  ENDFOR
;  RETURN, eddy_data
;END