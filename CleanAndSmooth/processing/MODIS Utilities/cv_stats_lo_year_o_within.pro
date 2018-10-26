FUNCTION crossValWithinRes, x, y, years
;handler for the cross validated stats
;reshape the matrix for the use of cv_stats_lo_year_o
s = SIZE(x)
FOR i = 0, s[1]-1 DO BEGIN
  IF (i EQ 0) THEN BEGIN
    xx = REFORM(x[i,*])
    yy = REFORM(y[i,*])
    div_id = INDGEN(s[2])+1
    yyears = INTARR(s[2]) * 0 + years[i]
  ENDIF ELSE BEGIN
    xx = [xx, REFORM(x[i,*])]
    yy = [yy, REFORM(y[i,*])]
    div_id = [div_id, INDGEN(s[2])+1]
    yyears = [yyears, INTARR(s[2]) * 0 + years[i]]
  ENDELSE
ENDFOR
RETURN, cv_stats_lo_year_o_within(xx, yy, yyears, div_id)
END

FUNCTION crossValWithinDivAndSeasonRes, x, y, years, season
;handler for the cross validated stats
;reshape the matrix for the use of cv_stats_lo_year_o
s = SIZE(x)
FOR i = 0, s[1]-1 DO BEGIN
  IF (i EQ 0) THEN BEGIN
    xx = REFORM(x[i,*])
    yy = REFORM(y[i,*])
    div_id = INDGEN(s[2])+1
    yyears = INTARR(s[2]) * 0 + years[i]
    season_id =  INTARR(s[2]) * 0 + season[i]
  ENDIF ELSE BEGIN
    xx = [xx, REFORM(x[i,*])]
    yy = [yy, REFORM(y[i,*])]
    div_id = [div_id, INDGEN(s[2])+1]
    yyears = [yyears, INTARR(s[2]) * 0 + years[i]]
    season_id = [season_id, INTARR(s[2]) * 0 + season[i]]
  ENDELSE
ENDFOR
RETURN, cv_stats_lo_year_o_within_div_within_season(xx, yy, yyears, div_id, season_id)
END

FUNCTION cv_stats_lo_year_o_within, x, y, years, within_factor
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

yr_list = uniqlist(years)
within_factor_list = uniqlist(within_factor)
n_yr_2_exclude = N_ELEMENTS(yr_list)



yp = DBLARR(n)  ;y predicted
ym = yp         ;y mean (it's the mean of y excluding the left out year)
ydivm = yp      ;division level mean
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
  FOR d=0,  N_ELEMENTS(within_factor_list)-1 DO BEGIN
    ;find within the kept those belonging to div d
    ind4m = WHERE(within_factor[ind] EQ within_factor_list[d])
    ;find within the left out those belonging to div d
    indd  = WHERE(within_factor[indoo] EQ within_factor_list[d])
    ;assign to the lest out beonging to div d, the mean of the kept for division d
    ydivm[indoo[indd]] = MEAN(y[ind[ind4m]], /DOUBLE)  
  ENDFOR
ENDFOR

tmp =WHERE(within_factor EQ within_factor_list[0], nResid)
res = {$
  cvr2_within: 0.0, $
  cvr2pooled_model_by_group: FLTARR(N_ELEMENTS(within_factor_list)), $
  residuals: FLTARR(N_ELEMENTS(within_factor_list),nResid)*!VALUES.F_NAN}
;;compute r2 cv
;SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE)
;SSerrloo = TOTAL((y - yp)^2 , /DOUBLE)
;cvr2 = 1.0 - (SSerrloo/SStot)

;within: /the mean is the department mean
;SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE)
SStot = TOTAL((y - ydivm)^2 , /DOUBLE, /NAN)
SSerrloo = TOTAL((y - yp)^2 , /DOUBLE, /NAN)
res.cvr2_within = 1.0 - (SSerrloo/SStot)

;here compute the crossval results applying the pooled model to all subgroups.
;the result is a vector containing a crossval R2 for each subgroup (e.g. 2 in the case of season L and S)


FOR d=0,  N_ELEMENTS(within_factor_list)-1 DO BEGIN
    ind = WHERE(within_factor EQ within_factor_list[d], n)
    SSerrloo = TOTAL((y[ind] - yp[ind])^2 , /DOUBLE, /NAN)
    SStot =    TOTAL((y[ind] - MEAN(y[ind]))^2, /DOUBLE, /NAN)
    res.residuals[d,0:n-1] = y[ind] - yp[ind]
    res.cvr2pooled_model_by_group[d] = 1.0 - (SSerrloo/SStot)
ENDFOR


RETURN, res
END

FUNCTION cv_stats_lo_year_o_within_div_within_season, x, y, years, div_id, season_id
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

yr_list = uniqlist(years)
div_list = uniqlist(div_id)
ssn_list = uniqlist(season_id)
n_yr_2_exclude = N_ELEMENTS(yr_list)
;tmp = WHERE(years EQ years[0], n_depts) 
yp = DBLARR(n)  ;y predicted
ym = yp         ;y mean (it's the mean of y excluding the left out year)
ydivm = yp      ;division level mean
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
  FOR d=0,  N_ELEMENTS(div_list)-1 DO BEGIN
    FOR s=0,  N_ELEMENTS(ssn_list)-1 DO BEGIN
      ;find within the kept those belonging to div d and season s
      ind4m = WHERE((div_id[ind] EQ div_list[d]) AND (season_id[ind] EQ ssn_list[s]))
      ;find within the left out those belonging to div d and season s
      indd  = WHERE((div_id[indoo] EQ div_list[d]) AND (season_id[indoo] EQ ssn_list[s]))
      ;assign to the lest out beonging to div d, the mean of the kept for division d
      ydivm[indoo[indd]] = MEAN(y[ind[ind4m]], /DOUBLE)
    ENDFOR  
  ENDFOR
  
ENDFOR

;;compute r2 cv
;SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE)
;SSerrloo = TOTAL((y - yp)^2 , /DOUBLE)
;cvr2 = 1.0 - (SSerrloo/SStot)

;within: /the mean is the department mean

;SStot = TOTAL((y - MEAN(y, /DOUBLE))^2 , /DOUBLE, /NAN)
SStot = TOTAL((y - ydivm)^2 , /DOUBLE, /NAN)
SSerrloo = TOTAL((y - yp)^2 , /DOUBLE, /NAN)
cvr2_within = 1.0 - (SSerrloo/SStot)

;here compute the crossval results applying the pooled model to all subgroups.
;the result is a vector containing a crossval R2 for each subgroup (e.g. 2 in the case of season L and S)
cvr2pooled_model_by_group = FLTARR(N_ELEMENTS(div_list))
FOR d=0,  N_ELEMENTS(div_list)-1 DO BEGIN
    ind = WHERE(div_id EQ div_list[d])
    ;SStot = TOTAL((y[ind] - ydivm[ind])^2 , /DOUBLE, /NAN)
    SStot = TOTAL((y[ind] - MEAN(y[ind], /DOUBLE))^2 , /DOUBLE, /NAN)
    SSerrloo = TOTAL((y[ind] - yp[ind])^2 , /DOUBLE, /NAN)
    cvr2pooled_model_by_group[d] = 1.0 - (SSerrloo/SStot)
ENDFOR

RETURN, [cvr2_within, cvr2pooled_model_by_group]
END

