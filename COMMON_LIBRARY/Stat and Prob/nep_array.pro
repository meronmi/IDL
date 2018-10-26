FUNCTION NEP_array, val, VALHIS = valHis
  ; Y[s,l,ind] = NEP_array(X[s,l,ind], VALHIS = X4STATS[s,l,ind])

  nepOfVal = FLTARR(N_ELEMENTS(val)) * !VALUES.F_NAN
  ;check if a his archive was passes or not
  IF (N_ELEMENTS(valHis) EQ 0) THEN valHis = val

  ;remove NAN
  indFinHis = WHERE(FINITE(valHis), countFinHis)
  indFin = WHERE(FINITE(val), countFin)
  ;at least 2 obs to compute inv percentile
  IF (countFinHis GE 2) THEN BEGIN
    valHis = valHis[indFinHis]
    ;order it
    valHis = valHis[SORT(valHis)]
    ;compute corresponding p
    p = (FINDGEN(N_ELEMENTS(valHis))+1)/(1+N_ELEMENTS(valHis))*100.0
    ;find the index of non duplicated ones (in case of duplication always take the last element as unique is doing)
    ind0 = UNIQ(valHis)
    ;Y[s,l,ind] = INTERPOL(p[ind0],valHis[ind0],X[s,l,ind])
    nepOfVal[indFin] = INTERPOL(p[ind0],valHis[ind0],val[indFin])
    ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
    ;indFinHis = WHERE(FINITE(Y[s,l,ind]), countFin)
    indFinNep = WHERE(FINITE(nepOfVal), countFinNep)
    ;IF (countFin GT 0) THEN Y[s,l,ind[indFinHis]] = FLOAT(ROUND(Y[s,l,ind[indFinHis]]*100)/100.)
    IF (countFinNep GT 0) THEN nepOfVal[indFinNep] = FLOAT(ROUND(nepOfVal[indFinNep]*100)/100.)
    ;check if some of the observation are outside range and assign 0 and 100
    ;look fo observations below the min
    ;ind0 = WHERE(X[s,l,ind] LT valHis[0], count0)
    ind0 = WHERE(val[indFin] LT valHis[0], count0)
    ;IF (count0 GT 0) THEN  Y[s,l,ind[ind0]] = 0
    IF (count0 GT 0) THEN  nepOfVal[indFin[ind0]] = 0
    ;look for above the max
    ;ind0 = WHERE(X[s,l,ind] GT valHis[-1], count0)
    ;IF (count0 GT 0) THEN  Y[s,l,ind[ind0]] = 100
    ind0 = WHERE(val[indFin] GT valHis[-1], count0)
    IF (count0 GT 0) THEN  nepOfVal[indFin[ind0]] = 100
  ENDIF ELSE RETURN, val*!VALUES.F_NAN
  RETURN, nepOfVal
END
