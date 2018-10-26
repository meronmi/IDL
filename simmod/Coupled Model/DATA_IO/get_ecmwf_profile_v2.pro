FUNCTION get_ecmwf_profile_v2, ecmwf_fn, site_code
; new version on a4c AGRIOPER Oracle db
; see E:\SimMod_data\ECMWF\new query for ecmwf.txt for generating ecmwf csv files from db)
;ECMWF
;
;read the csv only once, then use the sav
dir = FILE_DIRNAME(ecmwf_fn)
base = FILE_BASENAME(ecmwf_fn, '.csv')
IF (FILE_TEST(dir + '\' + base + '.sav')) THEN BEGIN
  RESTORE, dir + '\' + base + '.sav'
ENDIF ELSE BEGIN
  tmp =  READ_CSV2(ecmwf_fn, HEADER=hdr, MISSING_VALUE=-9999)
  SAVE, tmp, hdr, FILENAME = dir + '\' + base + '.sav'
ENDELSE
 
;here we only use few variables ('acqdate', 'tav', 'rrr','rad')
indSite = WHERE(tmp.FIELD01 EQ site_code, n)
;n = N_ELEMENTS(tmp.FIELD01)
ecmwf_data = {year: INTARR(n), $
  site_code: site_code, $
  month: INTARR(n), $
  day:  INTARR(n), $
  JD: FLTARR(n), $
  tav: FLTARR(n), $
  rad: FLTARR(n), $
  rain: FLTARR(n), $
  et0: FLTARR(n)}
ind = WHERE(hdr EQ 'DAY')

dates = STRSPLIT(tmp.(ind)[indSite], ['- '], /EXTRACT)
dates = dates.ToArray(/TRANSPOSE)
ecmwf_data.year = dates[0,*]
ecmwf_data.month = dates[1,*]
ecmwf_data.day = dates[2,*]
;ind = WHERE(hdr EQ 'year') & ecmwf_data.year = tmp.(ind)
ecmwf_data.JD = JULDAY(ecmwf_data.month, ecmwf_data.day, ecmwf_data.year)
ind = WHERE(hdr EQ 'RADIATION') & ecmwf_data.rad = tmp.(ind)[indSite]/1000.0 ;ecmwf rad is KJ/m2 and it's needed in MJ/m2
ind = WHERE(hdr EQ 'TEMPERATURE_AVG') & ecmwf_data.tav= tmp.(ind)[indSite]
ind = WHERE(hdr EQ 'PRECIPITATION') & ecmwf_data.rain = tmp.(ind)[indSite]
ind = WHERE(hdr EQ 'ET0') & ecmwf_data.et0 = tmp.(ind)[indSite]

;ECMWF should be ordered and without missing, nevertheless implement these tests

;be care it may not be ordered..
;first order it
indOrdered = SORT(ecmwf_data.JD)
FOR i=0,N_TAGS(ecmwf_data)-1 DO BEGIN
  IF (i EQ 1) THEN ecmwf_data.(i) = ecmwf_data.(i) ELSE $
  ecmwf_data.(i) = ecmwf_data.(i)[indOrdered]
ENDFOR


;may contain missing days ..
;check and fix
frstJD = MIN(ecmwf_data.JD, /NAN)
lstJD = MAX(ecmwf_data.JD, /NAN)
nn = (lstJD-frstJD+1)
IF (nn NE n) THEN BEGIN
  ;it contains missing data to be filled
  ;make the larger array
  filled = {year: INTARR(nn) - 9999, $
    site_code: site_code, $
    month: INTARR(nn) - 9999, $
    day:  INTARR(nn) - 9999, $
    JD: FLTARR(nn) - 9999, $
    tav: FLTARR(nn)* !VALUES.F_NAN, $
    rad: FLTARR(nn)* !VALUES.F_NAN, $
    rain: FLTARR(nn* !VALUES.F_NAN), $
    et0: FLTARR(nn)* !VALUES.F_NAN}
  ;fill the JD field
  ecmwf_data.JD = frstJD + INDGEN(nn, /FLOAT)
  ;match, a, b, suba, subb
  match, ecmwf_data.JD, filled.JD, sub_ecmwf_data, sub_filled
  FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
    IF (i EQ 1) THEN filled.(i) = ecmwf_data.(i) ELSE $
    filled.(i)[sub_filled] = ecmwf_data.(i)[sub_ecmwf_data]
  ENDFOR
ENDIF ELSE filled = ecmwf_data



RETURN, filled
END


;OLD VERSION on FERDINANDO's DB
;FUNCTION get_ecmwf_profile, ecmwf_fn
;  ;(see d:\Users\meronmi\Documents\DB connection\query for ecmwf.txt for generating ecmwf csv files from db)
;  ;ECMWF
;  tmp =  READ_CSV2(ecmwf_fn, HEADER=hdr, MISSING_VALUE=-9999)
;  ;here we only use few variables ('acqdate', 'tav', 'rrr','rad')
;  n = N_ELEMENTS(tmp.FIELD01)
;  ecmwf_data = {year: INTARR(n), $
;    month: INTARR(n), $
;    day:  INTARR(n), $
;    JD: FLTARR(n), $
;    tav: FLTARR(n), $
;    rad: FLTARR(n), $
;    rain: FLTARR(n), $
;    et0: FLTARR(n)}
;  ind = WHERE(hdr EQ 'acqdate')
;  dates = STRSPLIT(tmp.(ind), '-', /EXTRACT)
;  dates = dates.ToArray(/TRANSPOSE)
;  ecmwf_data.month = dates[1,*]
;  ecmwf_data.day = dates[0,*]
;  ind = WHERE(hdr EQ 'year') & ecmwf_data.year = tmp.(ind)
;  ecmwf_data.JD = JULDAY(ecmwf_data.month, ecmwf_data.day, ecmwf_data.year)
;  ind = WHERE(hdr EQ 'rad') & ecmwf_data.rad = tmp.(ind)/1000.0 ;ecmwf rad is KJ/m2 and it's needed in MJ/m2
;  ind = WHERE(hdr EQ 'tav') & ecmwf_data.tav= tmp.(ind)
;  ind = WHERE(hdr EQ 'rrr') & ecmwf_data.rain = tmp.(ind)
;  ind = WHERE(hdr EQ 'et0') & ecmwf_data.et0 = tmp.(ind)
;  RETURN, ecmwf_data
;END