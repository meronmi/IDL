FUNCTION read_alrmp, monthly_mort_alrmp_fn, nunits, first_yy, last_yy, first_mm, last_mm, ALRMP_IBLI_ids
tmp_csv = READ_CSV(monthly_mort_alrmp_fn, HEADER = hdr, COUNT = nrecs, MISSING_VALUE=-9999)
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
  avg_mortrate_m_tlu: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_dis: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_dro: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_con: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_pre: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_oth: FLTARR(n)*!VALUES.F_NAN, $
  avg_mortrate_m_tlu_mis: FLTARR(n)*!VALUES.F_NAN}
;now I have to fill all ibli units and all years and months
c=0
FOR u = 0, nunits - 1 DO BEGIN
  FOR yy = first_yy, last_yy DO BEGIN
    FOR mm = 1, 12 DO BEGIN
        IF ~(((yy EQ first_yy) AND (mm LT first_mm)) OR ((yy EQ last_yy) AND (mm GT last_mm))) THEN BEGIN
          ;IF c EQ 9240 THEN STOP
          ;IF c EQ 9405 THEN STOP
          data.ibli_ID[c] = ALRMP_IBLI_ids[1,u]
          data.alrmp_ID[c] = ALRMP_IBLI_ids[0,u]
          ind = WHERE((FIX(data_csv.divisioncode) EQ data.alrmp_ID[c]) AND $
                      (FIX(data_csv.year) EQ yy) AND $
                      (data_csv.month EQ months[mm-1]), count)
          data.month_num[c] = mm
          data.year[c] = yy
          IF (count GT 0) THEN BEGIN
            data.month_str[c] = data_csv.month[ind]
            data.avg_mortrate_m_tlu[c] = data_csv.avg_mortrate_m_tlu[ind]
            ;these are all rescaled mortalities:
            IF (data_csv.RECS_avg_mortrate_m_tlu_1[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_dis[c] = data_csv.RECS_avg_mortrate_m_tlu_1[ind]
            IF (data_csv.RECS_avg_mortrate_m_tlu_2[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_dro[c] = data_csv.RECS_avg_mortrate_m_tlu_2[ind]
            IF (data_csv.RECS_avg_mortrate_m_tlu_3[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_con[c] = data_csv.RECS_avg_mortrate_m_tlu_3[ind]
            IF (data_csv.RECS_avg_mortrate_m_tlu_4[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_pre[c] = data_csv.RECS_avg_mortrate_m_tlu_4[ind]
            IF (data_csv.RECS_avg_mortrate_m_tlu_5[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_oth[c] = data_csv.RECS_avg_mortrate_m_tlu_5[ind]
            ;this, for missing, is obviously not rescaled
            IF (data_csv.avg_mortrate_m_tlu_6[ind] NE '') THEN $ 
              data.avg_mortrate_m_tlu_mis[c] = data_csv.avg_mortrate_m_tlu_6[ind]
            ;pergorm the consistency check only if they are not all missing
            IF (data.avg_mortrate_m_tlu[c] NE data.avg_mortrate_m_tlu_mis[c]) THEN $
              IF (ROUND(10000.0*data.avg_mortrate_m_tlu[c]) - $
                  ROUND(10000.0*(data.avg_mortrate_m_tlu_dis[c] + data.avg_mortrate_m_tlu_dro[c] + $
                  data.avg_mortrate_m_tlu_con[c] + data.avg_mortrate_m_tlu_pre[c] + $
                  data.avg_mortrate_m_tlu_oth[c])) GT 1.0) THEN STOP
          ENDIF
          IF (count GT 1) THEN STOP
          c = c + 1  
        ENDIF
    ENDFOR
  ENDFOR
ENDFOR
RETURN, data
END