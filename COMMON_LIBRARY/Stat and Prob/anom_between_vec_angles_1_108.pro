Function anom_between_vec_angles_1_108, array, ref 
  ;used to compute the difference betweeen an array of angles expressed as SPIRITS, 1-36, 37-72, 73-108
  ;and a reference angle (again 1-36, 37-72, 73-108)
  
  ;Treat nan
  ind = WHERE(FINITE(array), count)
  IF (count GT 0) THEN x = array[ind] ELSE STOP
  IF (~FINITE(ref)) THEN STOP
  ;check min max
  ma = MAX([x], MIN=mi)
  IF ((ma GT 108) OR (mi LT 1)) THEN STOP
  IF ((ref GT 36) OR (mi LT 0)) THEN STOP
  
  ;express everything in 1 to 36
  ind = WHERE((x GE 37) AND (x LE 72), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 36
  ind = WHERE((x GE 73) AND (x LE 108), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 72
  
  IF ((ref GE 37) AND (ref LE 72)) THEN ref = ref - 36
  IF ((ref GE 73) AND (ref LE 108)) THEN ref = ref - 72
  
  delta = x - ref
  ind = WHERE(delta GT 18, count)
  IF (count GT 0) THEN delta[ind] = delta[ind] - 36 
  ind = WHERE(delta LT -18, count)
  IF (count GT 0) THEN delta[ind] = delta[ind] + 36
  
  return, delta;ROUND(mv*10)/100.0
End