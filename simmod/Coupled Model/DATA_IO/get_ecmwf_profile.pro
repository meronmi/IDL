FUNCTION get_ecmwf_profile, ecmwf_fn
; new version on a4c AGRIOPER Oracle db
; see E:\SimMod_data\ECMWF\new query for ecmwf.txt for generating ecmwf csv files from db)
;ECMWF
tmp =  READ_CSV2(ecmwf_fn, HEADER=hdr, MISSING_VALUE=-9999)
;here we only use few variables ('acqdate', 'tav', 'rrr','rad')
n = N_ELEMENTS(tmp.FIELD01)
ecmwf_data = {year: INTARR(n), $
  month: INTARR(n), $
  day:  INTARR(n), $
  JD: FLTARR(n), $
  tav: FLTARR(n), $
  rad: FLTARR(n), $
  rain: FLTARR(n), $
  et0: FLTARR(n)}
ind = WHERE(hdr EQ 'DAY')

dates = STRSPLIT(tmp.(ind), ['- '], /EXTRACT)
dates = dates.ToArray(/TRANSPOSE)
ecmwf_data.year = dates[0,*]
ecmwf_data.month = dates[1,*]
ecmwf_data.day = dates[2,*]
;ind = WHERE(hdr EQ 'year') & ecmwf_data.year = tmp.(ind)
ecmwf_data.JD = JULDAY(ecmwf_data.month, ecmwf_data.day, ecmwf_data.year)
ind = WHERE(hdr EQ 'RADIATION') & ecmwf_data.rad = tmp.(ind)/1000.0 ;ecmwf rad is KJ/m2 and it's needed in MJ/m2
ind = WHERE(hdr EQ 'TEMPERATURE_AVG') & ecmwf_data.tav= tmp.(ind)
ind = WHERE(hdr EQ 'PRECIPITATION') & ecmwf_data.rain = tmp.(ind)
ind = WHERE(hdr EQ 'ET0') & ecmwf_data.et0 = tmp.(ind)
RETURN, ecmwf_data
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