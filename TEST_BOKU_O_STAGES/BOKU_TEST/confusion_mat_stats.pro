FUNCTION confusion_mat_stats, matrix, varType, var, strClasses, strNumClasses, fn_out
  dlmtr = ','
  OPENW, lun,fn_out, /GET_LUN
  PRINTF, lun, STRJOIN(['','Reference (NfLf)'], dlmtr)
  PRINTF, lun, STRJOIN(['', '="'+strNumClasses+'"'], dlmtr)
  PRINTF, lun, STRJOIN(['Esitimate (' + var + ')',strClasses,'Row total','User acc. %'], dlmtr)
  FOR i = 0, N_ELEMENTS(strClasses)-1 DO BEGIN
    ;PRINTF, lun, STRJOIN(, dlmtr)
    PRINTF, lun, STRJOIN([strClasses[i], STRING(matrix[*,i], FORMAT='(F15.2)'), STRING(TOTAL(matrix[*,i]), FORMAT='(F15.2)'), STRING(matrix[i,i]/FLOAT(TOTAL(matrix[*,i]))*100, FORMAT='(F15.2)')], dlmtr)
  ENDFOR
  PRINTF, lun, STRJOIN(['Col total',STRING(TOTAL(matrix, 2), FORMAT='(F15.2)'), STRING(TOTAL(matrix), FORMAT='(F15.2)')], dlmtr)
  ;to get the diag elements
  sz = SIZE(matrix)
  IF (sz[1] NE sz[2]) THEN STOP
  ind = INDGEN(sz[1]) + sz[1] * INDGEN(sz[1])
  PRINTF, lun, STRJOIN(['Producer acc.', STRING(matrix[ind]/FLOAT(TOTAL(matrix, 2))*100)], dlmtr)

  PRINTF, lun, 'OVERALL STATISTICS'
  n = FLOAT(TOTAL(matrix))
  oa = TRACE(matrix)/n*100.0
  PRINTF, lun, 'Overall accuracy = ,' + STRING(oa)
  PRINTF, lun, 'What was the accuracy of the forecast in predicting the correct category relative to that of random chance?'
  rowtot = FLOAT(TOTAL(matrix, 1))
  coltot = FLOAT(TOTAL(matrix, 2))

  num = TRACE(matrix)/n - (1.0/n^2) * TOTAL(rowtot*coltot)
  den = 1.0 -  (1.0/n^2) * TOTAL(rowtot*coltot)
  hss = num/den
  PRINTF, lun, 'Heidke skill score (-1:1; 0 no skill; 1 perfect) = ,' + STRING(hss) + dlmtr + '(What was the accuracy of the forecast in predicting the correct category relative to that of random chance?)'
  PRINTF, lun, '% of:'
  PRINTF, lun, 'Agreement,' + STRTRIM(oa,2)

  IF (varType EQ 'z') OR (varType EQ 's') THEN BEGIN
    subOfNorm = MEDIAN(INDGEN(sz[1])) ;subscript of the contingency table row (and column) for the normal conditions
    RCnormSum = TOTAL(matrix[subOfNorm,*]) + TOTAL(matrix[*, subOfNorm]) - matrix[subOfNorm,subOfNorm]
    upRightBloc = TOTAL(matrix[subOfNorm+1:-1,0:subOfNorm-1])
    lowLeftBlock = TOTAL(matrix[0:subOfNorm-1,subOfNorm+1:-1])
    ;Agreement
    rank_a = oa
    ;Minor mismatch
    rank_mm = (n - TRACE(matrix) - RCnormSum + matrix[subOfNorm,subOfNorm] -upRightBloc - lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Minor mismatch,' + STRTRIM(rank_mm,2)
    ;Mismatch
    rank_m = TOTAL([matrix[subOfNorm-1, subOfNorm], matrix[subOfNorm, subOfNorm-1], matrix[subOfNorm+1, subOfNorm], matrix[subOfNorm, subOfNorm+1]]/FLOAT(n)*100)
    PRINTF, lun, 'Mismatch,' + STRTRIM(rank_m,2)
    ;Serious Mismatch
    rank_sm = TOTAL([REFORM(matrix[subOfNorm-3:subOfNorm-2, subOfNorm]), REFORM(matrix[subOfNorm, subOfNorm-3:subOfNorm-2]), REFORM(matrix[subOfNorm+2:subOfNorm+3, subOfNorm]), REFORM(matrix[subOfNorm, subOfNorm+2:subOfNorm+3])])/FLOAT(n)*100
    PRINTF, lun, 'Serious mismatch,' + STRTRIM(rank_sm,2)
    ;Unacceptable Mismatch
    rank_um = (upRightBloc+lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Unacceptable mismatch,' + STRTRIM(rank_um,2)
  ENDIF
  IF (varType EQ 'n') THEN BEGIN
    subOfNorm = MEDIAN(INDGEN(sz[1])) ;subscript of the contingency table row (and column) for the normal conditions
    RCnormSum = TOTAL(matrix[subOfNorm,*]) + TOTAL(matrix[*, subOfNorm]) - matrix[subOfNorm,subOfNorm]
    upRightBloc = TOTAL(matrix[subOfNorm+1:-1,0:subOfNorm-1])
    lowLeftBlock = TOTAL(matrix[0:subOfNorm-1,subOfNorm+1:-1])
    ;Agreement
    rank_a = oa
    ;Minor mismatch
    rank_mm = (n - TRACE(matrix) - RCnormSum + matrix[subOfNorm,subOfNorm] -upRightBloc - lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Minor mismatch,' + STRTRIM(rank_mm,2)
    ;Mismatch
    rank_m = TOTAL([matrix[subOfNorm-1, subOfNorm], matrix[subOfNorm, subOfNorm-1], matrix[subOfNorm+1, subOfNorm], matrix[subOfNorm, subOfNorm+1]]/FLOAT(n)*100)
    PRINTF, lun, 'Mismatch,' + STRTRIM(rank_m,2)
    ;Serious Mismatch
    rank_sm = TOTAL([REFORM(matrix[subOfNorm-2, subOfNorm]), REFORM(matrix[subOfNorm, subOfNorm-2]), REFORM(matrix[subOfNorm+2, subOfNorm]), REFORM(matrix[subOfNorm, subOfNorm+2])])/FLOAT(n)*100
    PRINTF, lun, 'Serious mismatch,' + STRTRIM(rank_sm,2)
    ;Unacceptable Mismatch
    rank_um = (upRightBloc+lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Unacceptable mismatch,' + STRTRIM(rank_um,2)
  ENDIF


  IF (varType EQ 'v') THEN subOfNorm = 3
  IF (varType EQ 'v') THEN BEGIN
    RCnormSum = TOTAL(matrix[subOfNorm,*]) + TOTAL(matrix[*, subOfNorm]) - matrix[subOfNorm,subOfNorm]
    upRightBloc = TOTAL(matrix[subOfNorm+1:-1,0:subOfNorm-1])
    lowLeftBlock = TOTAL(matrix[0:subOfNorm-1,subOfNorm+1:-1])
    ;Agreement, same as above
    rank_a = oa
    ;Minor mismatch, same as above
    rank_mm = (n - TRACE(matrix) - RCnormSum + matrix[subOfNorm,subOfNorm] - upRightBloc - lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Minor mismatch,' + STRTRIM(rank_mm,2)
    ;Mismatch, same as above
    rank_m = TOTAL([matrix[subOfNorm-1, subOfNorm], matrix[subOfNorm, subOfNorm-1], matrix[subOfNorm+1, subOfNorm], matrix[subOfNorm, subOfNorm+1]]/FLOAT(n)*100)
    PRINTF, lun, 'Mismatch,' + STRTRIM(rank_m,2)
    ;Serious Mismatch, different from above
    rank_sm = TOTAL([REFORM(matrix[subOfNorm-3:subOfNorm-2, subOfNorm]), REFORM(matrix[subOfNorm, subOfNorm-3:subOfNorm-2])])/FLOAT(n)*100
    PRINTF, lun, 'Serious mismatch,' + STRTRIM(rank_sm,2)
    ;Unacceptable Mismatch, same as above
    rank_um = (upRightBloc+lowLeftBlock)/FLOAT(n)*100
    PRINTF, lun, 'Unacceptable mismatch,' + STRTRIM(rank_um,2)
  ENDIF





  PRINTF, lun, 'OVERALL STATISTICS for dichotomous cathegories (bad vs. not bad) from http://www.cawcr.gov.au/projects/verification/#Standard_verification_methods'
  PRINTF, lun, 'based on a 2x2 contingency table of the event BAD
  PRINTF, lun, 'BAD is "moderately bad or worse". That is, Yes is moderately bad or worse, no is near normal or better.'
  hits = TOTAL(matrix[0:subOfNorm-1, 0:subOfNorm-1])
  falseAlarms = TOTAL(matrix[subOfNorm:-1, 0:subOfNorm-1])
  misses = TOTAL(matrix[0:subOfNorm-1, subOfNorm:-1])
  correctNeg = TOTAL(matrix[subOfNorm:-1, subOfNorm:-1])
  estYes = hits  + falseAlarms
  estNo = misses + correctNeg
  refYes = hits + misses
  refNo = falseAlarms + correctNeg
  PRINTF, lun, STRJOIN(['','Reference (NfLf)'], dlmtr)                            + dlmtr + STRJOIN(['','Reference (NfLf)'], dlmtr)
  PRINTF, lun, STRJOIN(['Esitimate (' + var + ')', 'Yes', 'No', 'Totals'], dlmtr) + dlmtr + STRJOIN(['Esitimate (' + var + ')', 'Yes', 'No', 'Totals'], dlmtr)
  PRINTF, lun, STRJOIN(['Yes', 'hits', 'false alarms', 'Est. Yes'], dlmtr)        + dlmtr + STRJOIN(['Yes', STRING(hits, FORMAT='(F15.2)'), STRING(falseAlarms, FORMAT='(F15.2)'), STRING(estYes, FORMAT='(F15.2)')], dlmtr)
  PRINTF, lun, STRJOIN(['No', 'misses', 'correct negatives', 'Est. No'], dlmtr)   + dlmtr + STRJOIN(['No', STRING(misses, FORMAT='(F15.2)'), STRING(correctNeg, FORMAT='(F15.2)'), STRING(estNo, FORMAT='(F15.2)')], dlmtr)
  PRINTF, lun, STRJOIN(['Totals', 'Ref. Yes', 'Ref. No', 'Overall Tot.'], dlmtr)  + dlmtr + STRJOIN(['Totals', STRING(refYes, FORMAT='(F15.2)'), STRING(refNo, FORMAT='(F15.2)'), STRING(n, FORMAT='(F15.2)')], dlmtr)
  ; d:dichotomous
  d_a = (hits+correctNeg)/n*100
  PRINTF, lun, 'Accuracy' + dlmtr + STRTRIM(d_a,2)
  d_bias = (hits+falseAlarms)/(hits+misses)*100
  PRINTF, lun, 'Bias' + dlmtr + STRTRIM(d_bias,2) + dlmtr + '[0-Inf], 100 is perfect (hits+falseAlarms)/(hits+misses)*100. How did the forecast frequency of "yes" events compare to the observed frequency of "yes" events?'
  d_dr = hits/(hits+misses)*100
  PRINTF, lun, 'Detection ratio' + dlmtr + STRTRIM(d_dr,2) + dlmtr + '[0-100], hits/(hits+misses). What fraction of the observed "yes" events were correctly forecast?
  d_far = falseAlarms/(hits+falseAlarms)*100
  PRINTF, lun, 'False alarm ratio' + dlmtr + STRTRIM(d_far,2) + dlmtr + '[0-100], falseAlarms/(hits+falseAlarms. What fraction of the predicted "yes" events actually did not occur (i.e. were false alarms)?
  ecr = 1/n * ((hits+misses)*(hits+falseAlarms) + (correctNeg+misses)*(correctNeg+falseAlarms))
  d_hss = ((hits+correctNeg)-ecr)/(n-ecr)
  PRINTF, lun, 'Heidke skill score' + dlmtr + STRTRIM(d_hss,2) + dlmtr + '[-1:-1], 0 is no skill, 1 is perfect. What was the accuracy of the forecast relative to that of random chance?
  PRINTF, lun, ''
  PRINTF, lun, 'Meaning of stats'
  PRINTF, lun, 'Producer accuracy for a class = correctly classified / total reference'
  PRINTF, lun, 'User accuracy for a class =     correctly classified / (correctly classified + wrongly classified)'
  PRINTF, lun, 'Agreement,same class of anomaly'
  PRINTF, lun, 'Minor mismatch,same sign of the anomaly ("good" or "bad") but different magnitude'
  PRINTF, lun, 'Mismatch,one is "normal" and the other "moderately bad" or "moderately good"'
  PRINTF, lun, 'Serious mismatch,one is "normal" and the other "very" or "extremely" bad or good'
  PRINTF, lun, 'Unacceptable mismatch,anomalies with opposite sign ("good" vs. "bad", any magnitude)'
  ;mc: multi-cathegory, d:dichotomous
  statsOut = CREATE_STRUCT('mc_oa',oa,'mc_hss',hss,'mc_rank_a',rank_a,'mc_rank_mm',rank_mm,'mc_rank_m',rank_m,'mc_rank_sm',rank_sm,'mc_rank_um',rank_um,$
    'd_a',d_a,'d_bias',d_bias,'d_dr',d_dr,'d_far',d_far,'d_hss',d_hss)
  FREE_LUN, lun
  RETURN, statsOut
END