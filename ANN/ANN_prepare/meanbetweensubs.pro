
FUNCTION MeanBetweenSubs, DataMatrix2D, subStart2D, subStop2D
;compute the overall median of all data within the season (as given by subStart2D, subStop2D)
sz = SIZE(subStart2D)
IF (sz[0] NE 2) THEN STOP
seasMean = FLTARR(sz[1])
FOR i = 0, sz[1]-1 DO BEGIN
  ;find the subscripts of all seasons
  subOfSeas = !NULL
  FOR j = 0,  sz[2]-1 DO BEGIN
    ;check that is positive (it exists)
    IF ((subStart2D[i,j] GE 0) AND (subStop2D[i,j] GE 0)) THEN BEGIN
      subOfSeas = [subOfSeas, INDGEN(subStop2D[i,j]-subStart2D[i,j]+1)+subStart2D[i,j]]
    ENDIF
  ENDFOR
  IF (N_ELEMENTS(subOfSeas) GT 0) THEN  BEGIN
    ind = WHERE(FINITE(DataMatrix2D[i,subOfSeas]),count)
    IF (count GT 0) THEN seasMean[i] = MEAN(DataMatrix2D[i,subOfSeas], /NAN) ELSE seasMean[i] = !VALUES.F_NAN
  ENDIF ELSE seasMean[i] = !VALUES.F_NAN
ENDFOR


RETURN, seasMean
END