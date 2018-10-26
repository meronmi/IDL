FUNCTION crossValRes, x, y, years
;handler for the cross validated stats
;reshape the matrix for the use of cv_stats_lo_year_o
s = SIZE(x)
FOR i = 0, s[1]-1 DO BEGIN
  IF (i EQ 0) THEN BEGIN
    xx = REFORM(x[i,*])
    yy = REFORM(y[i,*])
    yyears = INTARR(s[2]) * 0 + years[i]
  ENDIF ELSE BEGIN
    xx = [xx, REFORM(x[i,*])]
    yy = [yy, REFORM(y[i,*])]
    yyears = [yyears, INTARR(s[2]) * 0 + years[i]]
  ENDELSE
ENDFOR
RETURN, cv_stats_lo_year_o(xx, yy, yyears)
END

FUNCTION uniqlist, array
;Purpose:
;     To form a list of unique array values from an array
;     print, uniqlist([2,3,2,2,4,1])
;     >1 2 3 4

mlist = array(SORT(array)) 
mlist = mlist[UNIQ(mlist)] 
mlist = mlist(SORT(mlist))

RETURN, mlist
END

FUNCTION cv_stats_lo_year_o, x, y, years
;
;Cross Validated Statistics using Leave One YEAR out
;
;Purpose:
;     To compute the cross validated statitistics using
;     leave one out of a full year out out 
;  Return values:
;     -20: Arrays contain missing data
;     -10: Arrays have different dimensions
;     >0:  [R2cv,RMSEcv,mod_eff]
;     
;     Mod_eff is the modelling efficiency of Jansen and Heuberger (1995), that is, 
;     the relative improvement of the modelling solution (i.e., the linear regression) with
;     respect to the 'null' or 'bench-mark' model: the simple average of the retained y elements   
;

n = N_ELEMENTS(x)
;check that arrays have the same dimension
IF (n NE N_ELEMENTS(y)) THEN RETURN, -10
;check that they do not contain NaN
ind = WHERE(FINITE(x) EQ 0, count) & IF (count NE 0) THEN RETURN, -20
ind = WHERE(FINITE(y) EQ 0, count) & IF (count NE 0) THEN RETURN, -20

res = {$
  r2cv: 0.0, $
  rmse: 0.0, $
  mod_eff: 0.0, $
  residuals: FLTARR(n)}
  
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
res.residuals = (y - yp)

res.r2cv = 1.0 - (SSerrloo/SStot)
;compute RMSE
res.rmse = SQRT((1.0 / DOUBLE(N_ELEMENTS(y))) * SSerrloo)
;compute modelling efficiency (regression model against null model (the mean of the kept)
num = TOTAL((y - ym)^2,/DOUBLE) $
      - TOTAL((yp-y)^2,/DOUBLE)

den = TOTAL((y - ym)^2,/DOUBLE)
res.mod_eff = num / den
RETURN, res
END