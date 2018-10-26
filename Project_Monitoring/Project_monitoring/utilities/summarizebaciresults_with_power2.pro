PRO SummarizeBACIresults_with_power2
fns = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.txt', GET_PATH = dir)
n = N_ELEMENTS(fns)

means =REPLICATE(CREATE_STRUCT('id','','BACI_p2tail',-999.0,'BACI_p1tail',-999.0,$
       'BACI_contrast',-999.0,'rel_contrast',-999.0,$
       'mean_var_val',-999.0,'n_impact',-999.0,'power2tail',-999.0,$
       'power1tailLT0',-999.0,'power1tailGT0',-999.0),n)
indiv = means
       
                          
            
;data = FLTARR(N_ELEMENTS(data_hdr),n)-99990
;ids = STRARR(n)
FOR i = 0, n-1 DO BEGIN
  OPENR, lun, fns[i], /GET_LUN
  PRINT, fns[i]
  individ = 0
  WHILE (~ EOF(lun)) DO BEGIN
    line = ''
    READF, lun, line
    IF (line NE '') THEN BEGIN
      IF (line EQ '########################## Analysis of the individual values ##########################') THEN individ = 1
      ;start gathering results for the test on the means
      IF (individ EQ 0) THEN BEGIN
        CASE STRTRIM(line,2) OF
          '***Listing of part of the raw data': BEGIN ;project id
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE '1') THEN STOP
            means[i].id = STRTRIM(tmp[2],2)
            indiv[i].id = means[i].id
          END
          '***Summary report': BEGIN ;n_impact (4 and 9)
            READF, lun, line
            found = 0
            WHILE (found EQ 0) DO BEGIN
              READF, lun, line
              tmp = STRSPLIT(line, ' ', /EXTRACT)
              IF (STRTRIM(tmp[4],2) EQ means[i].id) THEN BEGIN
                means[i].n_impact = STRTRIM(tmp[5],2)
                indiv[i].n_impact = means[i].n_impact
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
            means[i].mean_var_val = (aft + bef) / 2.0
          END
          '*Estimated BACI contrast along with a se': BEGIN ;0,1,2
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 'baci') THEN STOP
            means[i].BACI_contrast = FLOAT(tmp[1]) ;contrast
            means[i].BACI_p2tail = FLOAT(tmp[5]) ;p
            means[i].BACI_p1tail = means[i].BACI_p2tail /2.0
            means[i].rel_contrast =  means[i].BACI_contrast / means[i].mean_var_val * 100.0
          END 
          '## Power on means': BEGIN ;power on meand 5,6,7
            READF, lun, line
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            onsameline = 0
            IF (STRTRIM(tmp[-1],2) EQ 'os.power2') THEN onsameline = 1
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (onsameline EQ 1) THEN BEGIN
              means[i].power2tail = FLOAT(tmp[7]) ;power
              means[i].power1tailLT0 = FLOAT(tmp[9]) ;power
              means[i].power1tailGT0 = FLOAT(tmp[10]) 
            ENDIF ELSE BEGIN;power
              IF (STRTRIM(tmp[0],2) NE 1) THEN STOP
              means[i].power2tail = FLOAT(tmp[8]) ;power
              means[i].power1tailLT0 = FLOAT(tmp[10]) ;power
              READF, lun, line
              READF, lun, line
              tmp = STRSPLIT(line, ' ', /EXTRACT)
              means[i].power1tailGT0 = FLOAT(tmp[1]) ;power
            ENDELSE
          END
          ELSE: a=!NULL 
        ENDCASE
      ENDIF
      ;here I am beyond the line for individual values
      IF (individ EQ 1) THEN BEGIN
        CASE STRTRIM(line,2) OF
          '*Estimated marginal means for Period': BEGIN ;means (8)
            READF, lun, line
            READF, lun, line
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) EQ '1') THEN tmp = tmp[1:*]
            IF (STRTRIM(tmp[0],2) NE 'after') THEN STOP
            aft = FLOAT(tmp[1])
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
             IF (STRTRIM(tmp[0],2) EQ '2') THEN tmp = tmp[1:*]
            IF (STRTRIM(tmp[0],2) NE 'before') THEN STOP
            bef = FLOAT(tmp[1])
            indiv[i].mean_var_val = (aft + bef) / 2.0
          END
          '*Estimated BACI contrast along with seBACI contrast:       [,1]': BEGIN ;5,6,7
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE '[1,]') THEN STOP
            indiv[i].BACI_contrast = FLOAT(tmp[1]) ;contrast
            indiv[i].BACI_p2tail = FLOAT(tmp[5]) ;p
            indiv[i].rel_contrast = indiv[i].BACI_contrast / indiv[i].mean_var_val * 100.0
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
            READF, lun, line
            ;here check if there are all powers or is next line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            ind = WHERE(tmp EQ 'os.power2', count)
            
            READF, lun, line
            tmp = STRSPLIT(line, ' ', /EXTRACT)
            IF (STRTRIM(tmp[0],2) NE 1) THEN STOP
            indiv[i].power2tail = FLOAT(tmp[8]) ;power 2 tail
            indiv[i].power1tailLT0 = FLOAT(tmp[10]) ;power 1 tail
            IF (count EQ 0) THEN BEGIN
              READF, lun, line
              READF, lun, line
              tmp = STRSPLIT(line, ' ', /EXTRACT)
              indiv[i].power1tailGT0 = FLOAT(tmp[1]) ;power 1 tail
            ENDIF ELSE BEGIN
              indiv[i].power1tailGT0 = FLOAT(tmp[-1]) ;power 1 tail
            ENDELSE
          END
          ELSE: a=!NULL
        ENDCASE
      ENDIF
    ENDIF
  ENDWHILE
  FREE_LUN, lun
ENDFOR
;outStr = STRING([TRANSPOSE(ids),STRING(data)])
;IF (N_ELEMENTS(size(outStr, /DIMENSIONS)) EQ 1) THEN BEGIN
;  outStr=[[outStr],[STRARR(N_ELEMENTS(data_hdr)+1)]]
;ENDIF
dlmtr = ','
OPENW, lun, dir + '\' + 'AAA_BACI_summary.csv', /GET_LUN

data_hdr = ['Project_id', $
  'BACI 2tail p means', $                   ;0
  'BACI 1tail p means', $
  'BACI contrast means', $            ;1
  'contrast/mean_val*100 means', $    ;2
  'mean_var_val means', $             ;3
  'n_impact', $                       ;4
  'power2tailed means', $               ;5
  'power1tailLT0 means', $                 ;6
  'power1tailGT0 means', $                   ;7
  'BACI 2tail p indiv', $                   ;0
  'BACI 1tail p indiv', $
  'BACI contrast indiv', $              ;6 9
  'contrast/mean_val*100 indiv', $      ;7 10
  'mean_var_val indiv', $               ;8 11
  'n_impact', $                       ;9 12
  'power2tailed indiv', $               ;10 13
  'power1tailLT0 indiv', $                 ;11 14
  'power1tailGT0 indiv']                   ;12 15
PRINTF, lun, STRJOIN(data_hdr+dlmtr)
FOR i = 0, n-1 DO BEGIN
  tmp = [means[i].BACI_p2tail,means[i].BACI_p1tail,means[i].BACI_contrast, $
         means[i].rel_contrast,means[i].mean_var_val,means[i].n_impact,$
         means[i].power2tail,means[i].power1tailLT0,means[i].power1tailGT0,$
         indiv[i].BACI_p2tail,indiv[i].BACI_p1tail,indiv[i].BACI_contrast, $
         indiv[i].rel_contrast,indiv[i].mean_var_val,indiv[i].n_impact,$
         indiv[i].power2tail,indiv[i].power1tailLT0,indiv[i].power1tailGT0]
  PRINTF, lun, STRJOIN([means[i].id,STRTRIM(tmp,2)]+dlmtr)
ENDFOR

FREE_LUN, lun
;WRITE_CSV, dir + '\' + 'AAA_BACI_summary.csv', outStr, HEADER = [project_hdr, data_hdr]
END