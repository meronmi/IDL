PRO make_RUM
  ;two column file linking the ALRMP divisioncode (first column) to IBLI_units id as of July 2014 (second column
  id_link_fname = 'E:\ILRI\SP_PROJECTS\INPUT_DATA\Admin_Units\oct2014\ID MATCH FILE.csv'
  monthly_mort_alrmp_fn = 'X:\Active Projects\Index Insurance Kenya\ILRI\Oct 1014\ALRMP with outlier-missing\AC extraction\AMLRP2_3_tlu.csv'
  save_fn = 'X:\Active Projects\Index Insurance Kenya\ILRI\Oct 1014\ALRMP with outlier-missing\AC extraction\data.sav'
  ;dir where to stor ethe fake RUM
  dir_rum = 'E:\ILRI\SP_PROJECTS\SP_Shape_Oct2014\RUM\MORTRATE'
  rum_base_names = ['total_month_mortrate', 'diseas_month_mortrate', 'drought_month_mortrate', 'conflict_month_mortrate', 'predation_month_mortrate', 'other_month_mortrate', 'missing_month_mortrate']
  sensor_ID = 3
  var_ID = [3,4,5,6,7,8,9]
  first_yy = 2001
  first_mm = 1
  last_yy = 2014
  last_mm = 9
  
  
  
  res = READ_CSV(id_link_fname, HEADER = hdr, COUNT = nunits)
  ALRMP_IBLI_ids = INTARR(2, nunits)
  ALRMP_IBLI_ids[0, *] = res.field1 ;ALRMP divisioncode
  ALRMP_IBLI_ids[1, *] = res.field2 ;IBLI UNIT
  ;open ALRMP and extract relevant data
  IF FILE_TEST(save_fn) THEN BEGIN
    PRINT, 'WARNING: data file is RESTORED from previous run!'
    RESTORE, save_fn
  ENDIF ELSE BEGIN
    data = read_alrmp(monthly_mort_alrmp_fn, nunits, first_yy, last_yy, first_mm, last_mm, ALRMP_IBLI_ids)
    SAVE, data, FILENAME=save_fn
  ENDELSE
  
  
  FOR var = 0, N_ELEMENTS(rum_base_names)-1 DO BEGIN  
    FOR yy = first_yy, last_yy DO BEGIN
      FOR mm = 1, 12 DO BEGIN
        ;skip at the beginning and end if first/last month iss not 1/12
        IF ~(((yy EQ first_yy) AND (mm LT first_mm)) OR ((yy EQ last_yy) AND (mm GT last_mm))) THEN BEGIN
          strYY = STRMID(STRTRIM(yy,2), 2, 2)
          IF (STRLEN(STRTRIM(mm,2)) EQ 2) THEN strMM = STRTRIM(mm,2) ELSE strMM = '0'+STRTRIM(mm,2)
          OPENW, lun, dir_rum + '\' +rum_base_names[var] + strYY + strMM + '.RUM' , /GET_LUN
          Print_rum_hdr, lun, sensor_ID, var_ID[var]
          FOR i = 0, nunits - 1 DO BEGIN
            ;for each units three lines must be printed, missing data should be filled as required
            u = ALRMP_IBLI_ids[1,i]
            ind = WHERE((data.ibli_ID EQ u) AND (data.year EQ yy) AND (data.month_num EQ mm), count)
            ;check for double entries
            IF N_ELEMENTS(ind) GT 1 THEN STOP; ind=ind[0]
            ;it just take the first
            IF count EQ 0 THEN STOP
            ;rum_base_names = ['total_month_mortrate', 'diseas_month_mortrate', 'drought_month_mortrate', 'conflict_month_mortrate', 'predation_month_mortrate', 'other_month_mortrate', 'missing_month_mortrate']
            CASE var OF
                0: mortrate = data.avg_mortrate_m_tlu[ind]
                1: mortrate = data.avg_mortrate_m_tlu_dis[ind]
                2: mortrate = data.avg_mortrate_m_tlu_dro[ind]
                3: mortrate = data.avg_mortrate_m_tlu_con[ind]
                4: mortrate = data.avg_mortrate_m_tlu_pre[ind]
                5: mortrate = data.avg_mortrate_m_tlu_oth[ind]
                6: mortrate = data.avg_mortrate_m_tlu_mis[ind]
            ENDCASE
            RA1 = '100.000'
            RA2 = '100.000'
            IF ~FINITE(mortrate) THEN BEGIN
              mortrate = -99999.999d
              RA1 = '0'
              RA2 = '0'
            ENDIF
            mortrateSD = '-99999.999'
            PRINTF, lun, STRTRIM(u,2) + ',0,0,0,' + STRTRIM(sensor_ID,2) + ',' + STRTRIM(var_ID[var],2) + ',30,' + $
                         STRTRIM(yy,2) + strMM + '01,'+RA1+','+RA2+',' + STRTRIM(mortrate,2) + ',' + STRTRIM(mortrateSD,2)
            PRINTF, lun, STRTRIM(u,2) + ',1,1,100,' + STRTRIM(sensor_ID,2) + ',' + STRTRIM(var_ID[var],2) + ',30,' + $
                         STRTRIM(yy,2) + strMM + '01,'+RA1+','+RA2+',' + STRTRIM(mortrate,2) + ',' + STRTRIM(mortrateSD,2)
            PRINTF, lun, STRTRIM(u,2) + ',2,1,100,' + STRTRIM(sensor_ID,2) + ',' + STRTRIM(var_ID[var],2) + ',30,' + $
                         STRTRIM(yy,2) + strMM + '01,'+RA1+','+RA2+',' + STRTRIM(mortrate,2) + ',' + STRTRIM(mortrateSD,2)
          ENDFOR
          FREE_LUN, lun
        ENDIF
      ENDFOR  ;mm
    ENDFOR  ;yy
  ENDFOR ;var
  PRINT, 'Finished'
END