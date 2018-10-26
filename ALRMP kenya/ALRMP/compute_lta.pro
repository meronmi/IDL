FUNCTION compute_lta, y, x
 lta = FLTARR(36)
 FOR i = 1, 36 DO BEGIN
  ind_dek = WHERE(x EQ i)
  lta[i-1] = MEAN(y[ind_dek], /NAN)
 ENDFOR
RETURN, lta
END