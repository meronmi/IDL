Function opt_model2, fctname, x, y, yerror, fg, yweights
;This function handle the model optimization using MPFIT   
;The part for fitting the upper envelope is based on the followings:
;   Written by:  Jin Chen, November 2003
;   Modified by: Allard de Wit, December 2004
;     *Replaced many FORTRANish pieces of code with native IDL (i.e. interpol)
;     *Removed call to SAVGOL() from iterative WHILE loop
;   Modified by: Anton Vrieling, February 2008
;     *Changed programme for integer output
;     *Introduced maximum value, to avoid values being higher than maxNDVI
;   Modified by: Michele Meroni, October 2011
;     *Adaptation to model fit. Modification of the weights only

                  
@cb_optimization.comm

iMax = 10        ;Maximum of iteration when fitting the upper envelope 




IF (upenv EQ 0) THEN BEGIN
  p = MPFITFUN(fctname, double(x), double(y), double(yerror), $
                        double(fg), weights=yweights, $
                        STATUS = stat, YFIT=yfit, BESTNORM=BESTNORM, FTOL=1.D-3, $
                        PERROR=PERROR, ERRMSG=error, ITMAX=500, /AUTODERIVATIVE, /QUIET) 
ENDIF ELSE BEGIN ;upenv EQ 1
  ; Fit the upper envelop
  ; Start with the first fitting
  p = MPFITFUN(fctname, double(x), double(y), double(yerror), $
               double(fg), weights=yweights, $
               STATUS = stat, YFIT=yfit, BESTNORM=BESTNORM, FTOL=1.D-3, $
               PERROR=PERROR, ERRMSG=error, ITMAX=500, /AUTODERIVATIVE, /QUIET)
  ; If unable to fit, return code 10
  IF (stat EQ 5) THEN BEGIN
    RETURN, 10
  ENDIF ELSE BEGIN
    ;store the output in last parameters
    lp = p & lyfit = yfit & lBESTNORM = BESTNORM & lPERROR=PERROR
  ENDELSE
  ; weight calculation
  dif  = ABS(y - yfit)
  mm   = MAX(dif)
  resu = y - yfit
  
  weights = FLTARR(N_ELEMENTS(x)) + 1.
  index = WHERE(resu LE 0)
  IF index[0] NE -1 THEN weights[index] = 1 - (dif[index]/mm)
  weights0=weights
  gdis = TOTAL(ABS(dif * weights), /DOUBLE)
  
  ra4 = FLTARR(N_ELEMENTS(x))
  ormax = gdis
  it = 1
  ;window, 0 & plot, x, y, psym=1 & oplot, x, yfit

  WHILE  (gdis LE ormax) AND (it LT imax) DO BEGIN ;(gdis LE ormax) AND
    ;Recompute the weights loop until
    ;reaching the point where the sum of weigthed differences starts
    ;to increase again.

    ; New fitting
    p = MPFITFUN(fctname, double(x), double(y), double(yerror), $
                 double(fg), weights=weights, $
                 STATUS = stat, YFIT=yfit, BESTNORM=BESTNORM, FTOL=1.D-3, $
                 PERROR=PERROR, ERRMSG=error, ITMAX=500, /AUTODERIVATIVE, /QUIET)
    

    ;oplot, x, yfit, color=5000*it^2
    IF (stat EQ 5) THEN BEGIN
      ;take the last good one 
      p = lp & yfit = lyfit & BESTNORM = lBESTNORM & PERROR=lPERROR
      chi= TOTAL( (y-yfit)^2 * ABS(yweights) )
      DOF     = N_ELEMENTS(x) - N_ELEMENTS(p) ; deg of freedom
      PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties                
      RETURN, 0
    ENDIF ELSE BEGIN
      ;store the output in last parameters
      lp = p & lyfit = yfit & lBESTNORM = BESTNORM & lPERROR=PERROR
    ENDELSE
    resu  = y - yfit
    dif  = ABS(y - yfit)
    mm   = MAX(dif)
    index = WHERE(resu LE 0)
    IF index[0] NE -1 THEN weights[index] = 1 - (dif[index]/mm)

    ;Calculate the weighted difference
    ormax = gdis
    gdis = TOTAL(ABS(resu*weights0), /DOUBLE)
    it++
  ENDWHILE   
ENDELSE ;fit the upenv
            
chi= TOTAL( (y-yfit)^2 * ABS(yweights) )
DOF     = N_ELEMENTS(x) - N_ELEMENTS(p) ; deg of freedom
PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties
                    
RETURN, 0
END