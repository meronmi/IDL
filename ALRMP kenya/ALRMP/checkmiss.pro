FUNCTION checkMiss, x, missVal
  indMiss = WHERE(x EQ missVal,countMiss) 
  IF (countMiss GT 0) THEN x[indMiss] = !VALUES.F_NAN
  RETURN, x
END