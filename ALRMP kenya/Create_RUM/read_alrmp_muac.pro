FUNCTION read_alrmp_MUAC, monthly_alrmp_fn, nunits, first_yy, last_yy, first_mm, last_mm, ALRMP_IBLI_ids
tmp_csv = READ_CSV(monthly_alrmp_fn, HEADER = hdr, COUNT = nrecs, MISSING_VALUE=-9999)
;PRINT, TAG_NAMES(tmp_csv)
;PRINT, hdr
data_csv = rename_tags(tmp_csv, TAG_NAMES(tmp_csv), hdr)
months =['January','February','March','April', 'May','June','July','August','September','October','November','December']
;total number of elements
n = nunits * ((last_yy-first_yy+1)*12 - (first_mm-1) - (12-last_mm))
data = {alrmp_ID: INTARR(n), $
  ibli_ID: INTARR(n), $
  alrmp_divname: STRARR(n), $
  year: INTARR(n), $
  month_str: STRARR(n), $
  month_num: INTARR(n), $
  avg_Z_muac: FLTARR(n)*!VALUES.F_NAN, $
  avg_muac: FLTARR(n)*!VALUES.F_NAN}
;now I have to fill all ibli units and all years and months
c=0
FOR u = 0, nunits - 1 DO BEGIN
  FOR yy = first_yy, last_yy DO BEGIN
    FOR mm = 1, 12 DO BEGIN
        IF ~(((yy EQ first_yy) AND (mm LT first_mm)) OR ((yy EQ last_yy) AND (mm GT last_mm))) THEN BEGIN
          ;IF c EQ 9240 THEN STOP
          ;IF c EQ 9405 THEN STOP
;          IF ((yy eq 2013) and (mm EQ 12)) THEN BEGIN
;            PRINT, 'DEBUG'
;          ENDIF
          data.ibli_ID[c] = ALRMP_IBLI_ids[1,u]
          data.alrmp_ID[c] = ALRMP_IBLI_ids[0,u]
          ind = WHERE((FIX(data_csv.division) EQ data.alrmp_ID[c]) AND $
                      (FIX(data_csv.year) EQ yy) AND $
                      (data_csv.month EQ mm), count)
          data.month_num[c] = mm
          data.year[c] = yy
          IF (count GT 0) THEN BEGIN
            data.month_str[c] = months[mm-1]
            data.avg_Z_muac[c] = data_csv.MEAN_Z_MUAC[ind]
            data.avg_muac[c] = data_csv.MEAN_MUAC[ind]
          ENDIF
          IF (count GT 1) THEN STOP
          c = c + 1  
        ENDIF
    ENDFOR
  ENDFOR
ENDFOR
RETURN, data
END