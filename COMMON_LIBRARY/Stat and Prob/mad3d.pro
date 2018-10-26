FUNCTION MADarray, X
  ;compute the Median Ansolute Deviation over the z (3rd dimension)
  ;Median absolute deviation MAD = MEDIAN(|x-MEDIAN(x)|)

  ;return strcuture res
  ;res.med is the 2D array with the median
  ;res.mad is the 2D array with the MAD
  ;res.meanAD is the 2D array with the mean AD

  sz = SIZE(X)
  IF (sz[0] NE 1) THEN BEGIN
    PRINT, 'MADarray.pro: Error, MADarray requested on something that is not 1d'
    STOP
  ENDIF

  res = CREATE_STRUCT('med', 0.0, 'mad', 0.0, 'meanAD', 0.0)

  res.med = MEDIAN(X)
  res.mad = MEDIAN(ABS(X-res.med))
  res.meanAD = MEAN(ABS(X-res.med), /NAN)

  RETURN, res
END

FUNCTION MAD3D, X
;compute the Median Ansolute Deviation over the z (3rd dimension)
;Median absolute deviation MAD = MEDIAN(|x-MEDIAN(x)|)

;return strcuture res
;res.med is the 2D array with the median
;res.mad is the 2D array with the MAD
;res.meanAD is the 2D array with the mean AD

sz = SIZE(X)
IF (sz[0] NE 3) THEN BEGIN
  PRINT, 'MAD3D.pro: Error, MAD3D requested on something that is not 3d'
  STOP
ENDIF

res = CREATE_STRUCT('med', FLTARR(sz[1],sz[2]), 'mad', FLTARR(sz[1],sz[2]), 'meanAD', FLTARR(sz[1],sz[2]))

res.med = MEDIAN(X, DIMENSION = sz[0])
res.mad = MEDIAN(ABS(X-REBIN(res.med, sz[1], sz[2], sz[3])), DIMENSION = sz[0])
res.meanAD = MEAN(ABS(X-REBIN(res.med, sz[1], sz[2], sz[3])), DIMENSION = sz[0],/NAN)

RETURN, res 
END