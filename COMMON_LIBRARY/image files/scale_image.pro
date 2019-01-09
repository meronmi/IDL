FUNCTION scale_image, XX, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset

  ;Michele Meroni 7/11/2018

  ; Scale X to floating with NaN
  ; X: 3d array 
  
  ; PASSED AS OPTIONAL PARAMETERS BUT ALL REQUIRED
    
  ; MINVAL: X (and Xhis) are in DN, minval is the minimum valid DN
  ; MAXVAL = X (and Xhis) are in DN, minval is the maximum valid DN
  
  ; GAIN = gain of physical value = gain * DN + offset
  ; OFFSET = offset of physical value = gain * DN + offset
  
  ; OPTIONAL PARAMETER
  ; NODATAVAL = DN reserved for nodata
  
  ; Avoid making changes to input arrays, make it float
  ; can be removed if variables are passed by reference (REFORM(X))
  X = FLOAT(TEMPORARY(XX))
  szX = SIZE(X)

  ; Determine if keywords were passed and act consequently
  IF N_ELEMENTS(minval) THEN BEGIN
    ind = WHERE(X LT minval, count)
    IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  ENDIF ELSE STOP
  
  IF N_ELEMENTS(maxval) THEN BEGIN
    ind = WHERE(X GT maxval, count)
    IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  ENDIF ELSE STOP
  
  IF N_ELEMENTS(nodataval) THEN BEGIN
    ind = WHERE(X EQ nodataval, count)
    IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  ENDIF 
  
  IF N_ELEMENTS(gain) THEN BEGIN
    X = TEMPORARY(X) * gain
  ENDIF ELSE STOP
  
  IF N_ELEMENTS(offset) THEN  BEGIN
    X = TEMPORARY(X) + offset
  ENDIF ELSE STOP
  
  RETURN, X
END


