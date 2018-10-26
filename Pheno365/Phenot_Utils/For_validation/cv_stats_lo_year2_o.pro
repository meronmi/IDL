FUNCTION cv_stats_lo_year2_o, x, y, years

  ;Purpose:
  ;     To compute the cross validated statitistics using
  ;     leave one out of a full year out out
  ;  Return values:
  ;     -20: Arrays contain missing data
  ;     -10: Arrays have different dimensions
  ;     >0:  res structure
  ;     res.R2cv
  ;     res.RMSEcv
  ;     res.mod_eff
  ;
  ;     Mod_eff is the modelling efficiency of Jansen and Heuberger (1995), that is,
  ;     the relative improvement of the modelling solution (i.e., the linear regression) with
  ;     respect to the 'null' or 'bench-mark' model: the simple average of the retained y elements
  ;
  
  res = {R2cv: 0.0d, $
         RMSEcv: 0.0d, $
         mod_eff: 0.0d, $
         yobs: FLTARR(N_ELEMENTS(y)), $
         ypre: FLTARR(N_ELEMENTS(y))}
  n = N_ELEMENTS(x)
  ;check that arrays have the same dimension
  IF (n NE N_ELEMENTS(y)) THEN RETURN, -10
  ;check that they do not contain NaN
  ind = WHERE(FINITE(x) EQ 0, count) & IF (count NE 0) THEN RETURN, -20
  ind = WHERE(FINITE(y) EQ 0, count) & IF (count NE 0) THEN RETURN, -20
  
  yr_list = uniqlist(years)
  n_yr_2_exclude = N_ELEMENTS(yr_list)
  ;tmp = WHERE(years EQ years[0], n_depts)
  yp = DBLARR(n)  ;y predicted
  ym = yp         ;y mean (it's the mean of y excluding the left out year)
  ;loo loop
  FOR i = 0, n_yr_2_exclude-1 DO BEGIN
    ;year to exclude
    yr_2_exclude = yr_list[i]
    ;remove those left out
    ind = WHERE(years NE yr_2_exclude)    ;indexes of the kept
    indoo = WHERE(years EQ yr_2_exclude)  ;indexes of the one year out
    b1 = REGRESS(x[ind], y[ind], CONST=b0, CORRELATION = corr, /DOUBLE)
    yp[indoo] = b0 + b1[0] * x[indoo]
    ym[indoo] = MEAN(y[ind],/DOUBLE)
  ENDFOR
  
  ;compute r2 cv
  SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE)
  SSerrloo = TOTAL((y - yp)^2 , /DOUBLE)
  res.obs = y
  res.pre = yp
  res.R2cv = 1.0 - (SSerrloo/SStot)
  ;compute RMSE
  res.RMSEcv = SQRT((1.0 / DOUBLE(N_ELEMENTS(y))) * SSerrloo)
  num = TOTAL((y - ym)^2,/DOUBLE) $
    - TOTAL((yp-y)^2,/DOUBLE)
  den = TOTAL((y - ym)^2,/DOUBLE)
  res.mod_eff = num / den
  RETURN, res
END