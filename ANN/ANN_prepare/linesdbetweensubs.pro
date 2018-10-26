FUNCTION lineSDBetweenSubs, DataMatrix2D, subStart2D, subStop2D
;compute the SD over a 2D array, columns are pixels, rows is time
sz = SIZE(subStart2D)
IF (sz[0] NE 2) THEN STOP
sd = FLTARR(sz[1])
FOR i = 0, sz[1]-1 DO BEGIN
  ;find the subscripts of all seasons
  subOfSeas = !NULL
  FOR j = 0,  sz[2]-1 DO BEGIN
    ;check that is positive (it exists)
    IF ((subStart2D[i,j] GE 0) AND (subStop2D[i,j] GE 0)) THEN BEGIN
      subOfSeas = [subOfSeas, INDGEN(subStop2D[i,j]-subStart2D[i,j]+1)+subStart2D[i,j]]
      ;IF CHECK_MATH() NE 0 THEN STOP
    ENDIF
  ENDFOR
  IF (N_ELEMENTS(subOfSeas) GT 0) THEN  BEGIN
    ind = WHERE(FINITE(DataMatrix2D[i,subOfSeas]),count)
    IF (count GT 0) THEN sd[i] = STDDEV(REFORM(DataMatrix2D[i,subOfSeas]), /NAN) ELSE sd[i]  = !VALUES.F_NAN
  ENDIF ELSE sd[i]  = !VALUES.F_NAN
  IF CHECK_MATH() NE 0 THEN STOP
ENDFOR
RETURN, sd
END