PRO SummarizeBACIresults_with_power
fns = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.txt', GET_PATH = dir)
n = N_ELEMENTS(fns)

project_hdr = 'Project_id'
data_hdr = ['BACI p means', $                   ;0
            'BACI contrast means', $            ;1
            'contrast/mean_val*100 means', $    ;2
            'mean_var_val means', $             ;3
            'n_impact', $                       ;4
            'power2tailed raw', $               ;5
            'power1tail raw', $                 ;6
            'power1tail raw', $                   ;7
            'BACI p raw', $                     ;5 8      
            'BACI contrast raw', $              ;6 9
            'contrast/mean_val*100 raw', $      ;7 10
            'mean_var_val raw', $               ;8 11
            'n_impact', $                       ;9 12
            'power2tailed raw', $               ;10 13
            'power1tail raw', $                 ;11 14
            'power1tail raw']                   ;12 15                            
            
data = FLTARR(N_ELEMENTS(data_hdr),n)-99990
ids = STRARR(n)
FOR i = 0, n-1 DO BEGIN
  OPENR, lun, fns[i], /GET_LUN
  indiv = 0
  WHILE (~ EOF(lun)) DO BEGIN
    line = ''
    READF, lun, line
    IF (line NE '') THEN BEGIN
      IF (line EQ '########################## Analysis of the individual values ##########################') THEN indiv = 1
      ;start gathering results for the test on the means
      IF (indiv EQ 0) THEN BEGIN
        CASE STRTRIM(line,2) OF
          '***Listing of part of the raw data': BEGIN ;project id
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE '1') THEN STOP
            ids[i] = STRTRIM(tmp[2],2)
          END
          '***Summary report': BEGIN ;n_impact (4 and 9)
            READF, lun, line
            found = 0
            WHILE (found EQ 0) DO BEGIN
              READF, lun, line
              tmp = STRSPLIT(line, ' ', /EXTRACT)
              IF (STRTRIM(tmp[4],2) EQ ids[i]) THEN BEGIN
                data[4,i] = STRTRIM(tmp[5],2)
                data[12,i] = STRTRIM(tmp[5],2)
                found = 1
              ENDIF
            ENDWHILE
          END
          '*Estimated marginal means for Period': BEGIN ;means (3)
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 'after') THEN STOP
            aft = FLOAT(tmp[1])
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 'before') THEN STOP
            bef = FLOAT(tmp[1])
            data[3,i] = (aft + bef) / 2.0
          END
          '*Estimated BACI contrast along with a se': BEGIN ;0,1,2
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 'baci') THEN STOP
            data[1,i] = FLOAT(tmp[1]) ;contrast
            data[0,i] = FLOAT(tmp[5]) ;p
            data[2,i] = FLOAT(tmp[1]) / data[3,i] * 100.0
          END 
          '## Power on means ': BEGIN ;power on meand 5,6,7
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 1) THEN STOP
            data[5,i] = FLOAT(tmp[7]) ;power
            data[6,i] = FLOAT(tmp[9]) ;power
            READF, lun, line
            data[7,i] = FLOAT(tmp[0]) ;power
          END
          ELSE: a=!NULL 
        ENDCASE
      ENDIF
      ;here I am beyond the line for individual values
      IF (indiv EQ 1) THEN BEGIN
        CASE STRTRIM(line,2) OF
          '*Estimated marginal means for Period': BEGIN ;means (8)
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[1],2) NE 'after') THEN STOP
            aft = FLOAT(tmp[4])
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[1],2) NE 'before') THEN STOP
            bef = FLOAT(tmp[4])
            data[11,i] = (aft + bef) / 2.0
          END
          '*Estimated BACI contrast along with seBACI contrast:       [,1]': BEGIN ;5,6,7
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE '[1,]') THEN STOP
            data[9,i] = FLOAT(tmp[1]) ;contrast
            data[8,i] = FLOAT(tmp[5]) ;p
            data[10,i] = FLOAT(tmp[1]) / data[11,i] * 100.0
          END
          '########################## Power': BEGIN ;10
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 1) THEN STOP
            data[13,i] = FLOAT(tmp[8]) ;power
            data[14,i] = FLOAT(tmp[10]) ;power
            data[15,i] = FLOAT(tmp[11]) ;power
          END
          ELSE: a=!NULL
        ENDCASE
      ENDIF
    ENDIF
  ENDWHILE
  FREE_LUN, lun
ENDFOR
outStr = STRING([TRANSPOSE(ids),STRING(data)])
IF (N_ELEMENTS(size(outStr, /DIMENSIONS)) EQ 1) THEN BEGIN
  outStr=[[outStr],[STRARR(N_ELEMENTS(data_hdr)+1)]]
ENDIF
WRITE_CSV, dir + '\' + 'AAA_BACI_summary.csv', outStr, HEADER = [project_hdr, data_hdr]
END