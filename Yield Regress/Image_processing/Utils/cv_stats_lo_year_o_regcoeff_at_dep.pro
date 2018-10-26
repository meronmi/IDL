 FUNCTION cv_stats_lo_year_o_regCoeff_at_dep, db, dlist, ylist, pp, pheno_products, verbose
 ;EX  FUNCTION r2cv_dep_tuned, db, dlist, ylist, pp, pheno_products, verbose
 ;
 ; dlist and ylist are already without excluded dep and years
 ; compute the overall r2 cv with same RS indicator but regression
 ; coefficients tuned at dep level 
 
 ; compute the r2 cv tuning the regression at dep level
 ; 1. remove one year, 2 tune regression and estimate that year for all depts
 n_yr_2_exclude = N_ELEMENTS(ylist)
 initvar = 1
 
 FOR i=0, n_yr_2_exclude-1 DO BEGIN
   yr_2_exclude = ylist[i]
   
   FOR d=0,  N_ELEMENTS(dlist)-1 DO BEGIN
    
    ;C1) collect unexcluded data 
    ;index to keep
    ind=WHERE((db.years.excluded EQ 0) $
               AND (db.depts.excluded EQ 0 $
               AND db.depts.data EQ dlist[d] $
               AND (db.years.data NE yr_2_exclude)), count)
    ;index to leave out
    indoo=WHERE((db.years.excluded EQ 0) $
               AND (db.depts.excluded EQ 0 $
               AND db.depts.data EQ dlist[d] $
               AND (db.years.data EQ yr_2_exclude)), count)
    x=REFORM(db.pheno_wavg.data[pp,ind])  ;pheno/RS data
    y=REFORM(db.yield.data[ind])          ;yield data
    xoo=REFORM(db.pheno_wavg.data[pp,indoo])  ;pheno/RS data
    yoo=REFORM(db.yield.data[indoo])          ;yield data
    
    ; now x and y do not contain the year loo (to exclude)
    ;Check for missing data
    ind_finite = ind_of_XYfinite(x, y, db.depts.data[d], pheno_products[pp], verbose)
    x=x[ind_finite] & y=y[ind_finite] & n=N_ELEMENTS(y)
    ;IF (N_ELEMENTS(ind_finite) GT N_ELEMENTS(used_years)) THEN STOP
    ;C2) regress
    coeff = REGRESS(x, y, CORRELATION=corr, CONST=b0, /DOUBLE)
    ;compute the y mean of the retained
    ym = MEAN(y,/DOUBLE)
    ;predict the left out
    yoop =  b0 + coeff[0] * xoo 
    ;store the xoo, yoo and yoop
    IF (initvar EQ 1) THEN BEGIN
      ;check that none of them is NaN, in the case do not store it
      IF ((FINITE(xoo)) AND (FINITE(yoo)) AND (FINITE(yoop))) THEN BEGIN
        initvar = 0
        all_xoo = xoo
        all_yoo = yoo
        all_ym = ym
        all_yoop = yoop
     ENDIF 
    ENDIF ELSE BEGIN
      IF ((FINITE(xoo)) AND (FINITE(yoo)) AND (FINITE(yoop))) THEN BEGIN
        all_xoo = [all_xoo, xoo]
        all_yoo = [all_yoo, yoo]
        all_ym = [all_ym, ym]
        all_yoop = [all_yoop, yoop]
      ENDIF 
    ENDELSE              
  ENDFOR ;depart analyisis
ENDFOR; i
;compute r2 cv
;overall: /the mean is the overall mean
SStot = TOTAL((all_yoo - MEAN(all_yoo, /DOUBLE))^2 , /DOUBLE)
SSerrloo = TOTAL((all_yoo - all_yoop)^2 , /DOUBLE)
cvr2_overall = 1.0 - (SSerrloo/SStot)
;within: /the mean is the department mean
SStot = TOTAL((all_yoo - all_ym)^2 , /DOUBLE)
SSerrloo = TOTAL((all_yoo - all_yoop)^2 , /DOUBLE)
cvr2_within = 1.0 - (SSerrloo/SStot)
;compute RMSE
rmse = SQRT((1.0 / DOUBLE(N_ELEMENTS(all_yoo))) * SSerrloo)
;compute rel_eff
rel_eff = (1.0/DOUBLE(N_ELEMENTS(all_yoo))) * TOTAL((all_yoo - all_ym)^2,/DOUBLE) / rmse^2
num = TOTAL((all_yoo - all_ym)^2,/DOUBLE) $
      - TOTAL((all_yoop-all_yoo)^2,/DOUBLE)
den = TOTAL((all_yoo - all_ym)^2,/DOUBLE)
mod_eff = num / den
RETURN, [cvr2_overall,cvr2_within,rmse,mod_eff]
END