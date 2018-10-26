FUNCTION cv_stats_lo_year_o_regCoeff_for_global_model, db, dlist, ylist, pp, pheno_products, verbose
 ;EX FUNCTION r2cvLoyo, x, y, years
 ;to be used for computing within statistics
 ;
 ; dlist and ylist are already without excluded dep and years
 ; compute the overall r2 cv with same RS indicator but regression
 ; coefficients tuned at dep level 
 
 ; compute the r2 cv tuning the regression at dep level
 ; 1. remove one year, 2 tune regression and estimate that year for all depts
 n_yr_2_exclude = N_ELEMENTS(ylist)
 initvar = 1
 yp = DBLARR(n_yr_2_exclude)
 FOR i=0, n_yr_2_exclude-1 DO BEGIN
   yr_2_exclude = ylist[i]
   ;index to keep to build the global model
   indg=WHERE((db.years.excluded EQ 0) $
              AND (db.depts.excluded EQ 0 $
              AND (db.years.data NE yr_2_exclude)), count)
       ;index to leave out
   indgoo=WHERE((db.years.excluded EQ 0) $
              AND (db.depts.excluded EQ 0 $
              AND (db.years.data EQ yr_2_exclude)), count)
   xg=REFORM(db.pheno_wavg.data[pp,indg])  ;pheno/RS data
   yg=REFORM(db.yield.data[indg])          ;yield data
   ;checck for nan
   ind_x_finite=WHERE(FINITE(xg) EQ 1, count_x_finite) 
   ind_y_finite=WHERE(FINITE(yg) EQ 1, count_y_finite)
   ind_finite=SetIntersection(ind_x_finite, ind_y_finite)
   coeff = REGRESS(xg[ind_finite], yg[ind_finite], CONST=b0, CORRELATION = corr, /DOUBLE)
   
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
    yoo=REFORM(db.yield.data[indoo])  
    xoo=REFORM(db.pheno_wavg.data[pp,indoo])  ;pheno/RS data
    yoop =  b0 + coeff[0] * xoo   
    ;compute the y mean of the retained
    ym = MEAN(y,/DOUBLE, /NAN)
    
    IF (initvar EQ 1) THEN BEGIN
      ;check that none of them is NaN, in the case do not store it
      IF FINITE(ym)THEN BEGIN
        initvar = 0
        all_ym = ym
        all_yoo = yoo
        all_yoop = yoop
     ENDIF 
    ENDIF ELSE BEGIN
      IF FINITE(ym)THEN BEGIN
        all_ym = [all_ym, ym]
        all_yoo = [all_yoo, yoo]
        all_yoop = [all_yoop, yoop]
      ENDIF 
    ENDELSE              
  ENDFOR ;depart analyisis
  
ENDFOR; i
;compute r2 cv
;overall: /the mean is the overall mean
SStot = TOTAL((all_yoo - MEAN(all_yoo, /DOUBLE,/NAN))^2 , /DOUBLE, /NAN)
SSerrloo = TOTAL((all_yoo - all_yoop)^2 , /DOUBLE, /NAN)
cvr2_overall = 1.0 - (SSerrloo/SStot)
;within: /the mean is the department mean
SStot = TOTAL((all_yoo - all_ym)^2 , /DOUBLE, /NAN)
SSerrloo = TOTAL((all_yoo - all_yoop)^2 , /DOUBLE, /NAN)
cvr2_within = 1.0 - (SSerrloo/SStot)
;compute RMSE
rmse = SQRT((1.0 / DOUBLE(N_ELEMENTS(all_yoo))) * SSerrloo)
;compute rel_eff
rel_eff = (1.0/DOUBLE(N_ELEMENTS(all_yoo))) * TOTAL((all_yoo - all_ym)^2,/DOUBLE, /NAN) / rmse^2
num = TOTAL((all_yoo - all_ym)^2,/DOUBLE, /NAN) $
      - TOTAL((all_yoop-all_yoo)^2,/DOUBLE, /NAN)
den = TOTAL((all_yoo - all_ym)^2,/DOUBLE, /NAN)
mod_eff = num / den
RETURN, [cvr2_overall,cvr2_within,rmse,mod_eff]
END