FUNCTION SPIRITS_from_108_to_36_ARRAY, data
  ;array version (does not open a file, it works on the data passed)
  ;set NAN where data is flagged 
  matOut = FLOAT(data) * !VALUES.F_NAN
  ind1 = WHERE((data GE 1.0) AND (data LE 36.0), count1)
  IF (count1 GT 0) THEN matOut[ind1] = data[ind1]

  ind2 = WHERE((data GT 36.0) AND (data LE 72.0), count2)
  IF (count2 GT 0) THEN matOut[ind2] = data[ind2] - 36.0

  ind3 = WHERE((data GT 72.0) AND (data LE 108.0), count3)
  IF (count3 GT 0) THEN matOut[ind3] = data[ind3] - 72.0

  RETURN, matOut


END