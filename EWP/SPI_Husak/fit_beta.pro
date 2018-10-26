FUNCTION fit_beta, x
  ;using moments method, https://en.wikipedia.org/wiki/Beta_distribution#Two_unknown_parameters
  m = MEAN(x, /NAN)
  v = VARIANCE(x, /NAN)
  IF (v LT m*(1-m)) THEN BEGIN
    a = m *   (((m*(1-m))/v)-1)
    b = (1-m)*(((m*(1-m))/v)-1)
  ENDIF ELSE BEGIN
    STOP
  ENDELSE
  RETURN, [a,b]
END