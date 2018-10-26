PRO SummarizeBACIresults
fns = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.txt', GET_PATH = dir)
n = N_ELEMENTS(fns)

data_hdr = ['Project_id', 'BACI p', 'BACI contrast', 'contrast/mean_val*100', 'mean_var_val','n_impact']
data = FLTARR(5,n)
ids = STRARR(n)
FOR i = 0, n-1 DO BEGIN
  OPENR, lun, fns[i], /GET_LUN
  terminate = 0
  WHILE (terminate LT 4) DO BEGIN
    line = ''
    READF, lun, line
    IF (line NE '') THEN BEGIN
      CASE STRTRIM(line,2) OF
        '***Listing of part of the raw data': BEGIN
          READF, lun, line
          READF, lun, line
          READF, lun, line
          tmp = STRSPLIT(line, ' ', /EXTRACT)
          IF (STRTRIM(tmp[0],2) NE '1') THEN STOP
          ids[i] = STRTRIM(tmp[2],2)
          terminate = terminate + 1
        END
        '***Summary report': BEGIN
          READF, lun, line
          found = 0
          WHILE (found EQ 0) DO BEGIN
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[4],2) EQ ids[i]) THEN BEGIN
              data[4,i] = STRTRIM(tmp[5],2)
              found = 1
            ENDIF
          ENDWHILE
          terminate = terminate + 1
        END
        '*Estimated marginal means for Period': BEGIN
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
          terminate = terminate + 1
        END
        '*Estimated BACI contrast along with a se': BEGIN
          READF, lun, line
          READF, lun, line
          READF, lun, line
          tmp = STRSPLIT(line, ' ', /EXTRACT)
          IF (STRTRIM(tmp[0],2) NE 'baci') THEN STOP
          data[1,i] = FLOAT(tmp[1]) ;contrast
          data[0,i] = FLOAT(tmp[5]) ;p
          data[2,i] = FLOAT(tmp[1]) / data[3,i] * 100.0
          terminate = terminate + 1
        END 
        ELSE: a=!NULL 
      ENDCASE
    ENDIF
  ENDWHILE
  FREE_LUN, lun
ENDFOR
outStr = STRING([TRANSPOSE(ids),STRING(data)])
IF (N_ELEMENTS(size(outStr, /DIMENSIONS)) EQ 1) THEN BEGIN
  outStr=[[outStr],[STRARR(N_ELEMENTS(data_hdr))]]
ENDIF
WRITE_CSV, dir + '\' + 'AAA_BACI_summary.csv', outStr, HEADER = data_hdr
END