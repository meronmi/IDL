FUNCTION InvWrapper_water_lim, parinfo, fg, tJD, globrad, globrad2par, tair, wlim, proSailVar, $
                     satObsJD, satObsRAA, satObsSZA, satObsVZA, $
                     r1, r2, r3, r4, r5, r6, r7, uppEnvFit, weightingScheme, inversion_algrtm



;Inversion manager
;questa funzione riorganizza i parametri e li passa a funct2min che chima simFORWDmodel, calcola gli scarti delle riflettanze e
;ritorna un solo valore f

;MPFIT PARAMETERS
iMax = 10  ;maximum number of iteration for upper envelope adaptation
it  = 1    ;iteration counter for upper envelop fit
TIC



fcnargs = {PROSAILVAR:[proSailVar.Cab,proSailVar.Car,proSailVar.Cbrown,proSailVar.Cw,proSailVar.Cm, $
           proSailVar.Ns,proSailVar.lidf_a,proSailVar.lidf_b,proSailVar.lai,proSailVar.hspot, $
           proSailVar.tts,proSailVar.tto,proSailVar.psi],$
           RSOIL: proSailVar.rsoil, $
           TJD:tJD, GLOBRADVAR:globrad, GLOBRAD2PAR:globrad2par, TAIR:tair, WLIM: wlim, $
           SATOBSJD:satObsJD, SATOBSRAA:satObsRAA, SATOBSSZA:satObsSZA, SATOBSVZA:satObsVZA}
robs = [TRANSPOSE(r1), TRANSPOSE(r2), TRANSPOSE(r3), TRANSPOSE(r4), TRANSPOSE(r5), $
       TRANSPOSE(r6), TRANSPOSE(r7)]


;Compute observed NDVI
indobs = WHERE(FINITE(satObsJD), count)
obsJD = satObsJD[indobs]
Red_obs = REFORM(robs[0,indobs])
Nir_obs = REFORM(robs[1,indobs])
NDVI_obs = (Nir_obs-Red_obs)/(Nir_obs+Red_obs)
err = NDVI_obs * 0.0 + 1.0

CASE inversion_algrtm OF
  'METROPOLIS': BEGIN
    COMMON METRO, fcnargs2
    fcnargs2 = fcnargs
    functName = 'METROPOLIS_NDVI_from_model_water_lim'
    find_betas, functName, "log_like_chisq", obsJD, NDVI_obs, err, fg, betamu, parinfo=parinfo
    SAVE, betamu, FILENAME='d:\Users\meronmi\Documents\IDL\simmod\Metropolis\betamu.sav'
    PRINT, 'END TEST METROPOLIS'
  END ;'METROPOLIS'
  'MPFIT': BEGIN
    functName = 'MPFIT_NDVI_from_model_water_lim'
    CASE weightingScheme OF
      'none': weights = NDVI_obs * 0.0 + 1.0
      'minmax': weights = (NDVI_obs - MIN(NDVI_obs, /NAN)) / (MAX(NDVI_obs, /NAN) - MIN(NDVI_obs, /NAN))
      ELSE: STOP
ENDCASE

    ;Perform normal or upper envelop fit
    IF (uppEnvFit NE 1) THEN BEGIN
      parms = MPFITFUN(functName, obsJD, NDVI_obs, err, fg, WEIGHTS = weights, $
        FUNCTARGS=fcnargs, MAXITER=500, ERRMSG=errmsg, $; NPRINT=nprint, QUIET=quiet, FTOL=1.D-3, XTOL=xtol, GTOL=gtol, NITER=niter,
        STATUS=status, $;ITERPROC=iterproc, ITERARGS=iterargs,
        COVAR=covar, PERROR=perror, BESTNORM=bestnorm, NPEGGED = npegged, $
        PARINFO=parinfo, PFREE_INDEX = pfree_index, YFIT=yfit, /AUTODERIVATIVE, /QUIET)
    ENDIF ELSE BEGIN
      ; Upper Fnvelope Fit
      ; Start with the first fitting
      parms = MPFITFUN(functName, obsJD, NDVI_obs, err, fg, WEIGHTS = weights, $
        FUNCTARGS=fcnargs, MAXITER=500, ERRMSG=errmsg, $; NPRINT=nprint, QUIET=quiet, FTOL=1.D-3, XTOL=xtol, GTOL=gtol, NITER=niter,
        STATUS=status, $;ITERPROC=iterproc, ITERARGS=iterargs,
        COVAR=covar, PERROR=perror, BESTNORM=bestnorm, NPEGGED = npegged, $
        PARINFO=parinfo, PFREE_INDEX = pfree_index, YFIT = NDVI_fit, /AUTODERIVATIVE, /QUIET)
      ;return code 10 if unable to fit, store results otherwise
      IF (status LE 0) THEN BEGIN
        PRINT, errmsg
        PRINT, 'Try MPFIT_RESET_RECURSION if the execution was stopped because of recursion'
        RETURN, 10
      ENDIF
      ;store the output
      last_parms = parms & last_npegged = npegged & last_pfree_index = pfree_index
      last_bestnorm = bestnorm & last_status = status
      ;fit effect calculation
      UEF_weights_and_fit_effect, NDVI_obs, NDVI_fit, fit_effect
      fit_effect0 = fit_effect

      WHILE  (fit_effect LE fit_effect0) AND (it LT imax) DO BEGIN
        ;Recompute the weights loop until the point where the fitting effect index is minimum (stop at iteration before it starts to increase)
        ;Update the time series
        updNDVI_obs = NDVI_obs
        index = WHERE(NDVI_obs LT NDVI_fit)
        IF index[0] NE -1 THEN updNDVI_obs[index] = NDVI_fit[index]
        ;Fit the updated time series using the latest parameter estimation as first guess
        fg2 = parms
        parms = MPFITFUN(functName, obsJD, updNDVI_obs, err, fg2, WEIGHTS = weights, $
          FUNCTARGS=fcnargs, MAXITER=500, ERRMSG=errmsg, $; NPRINT=nprint, QUIET=quiet, FTOL=1.D-3, XTOL=xtol, GTOL=gtol, NITER=niter,
          STATUS=status, $;ITERPROC=iterproc, ITERARGS=iterargs,
          COVAR=covar, PERROR=perror, BESTNORM=bestnorm, NPEGGED = npegged, $
          PARINFO=parinfo, PFREE_INDEX = pfree_index, YFIT = NDVI_fit, /AUTODERIVATIVE, /QUIET)
        IF (status LE 0) THEN BEGIN
          ;optimization failed, take last succesfull attempt
          parms = last_parms & npegged = npegged & pfree_index = last_pfree_index
          bestnorm = last_bestnorm & status = last_status
          ;info_mpfit = info_mpfit_str(status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, it, weightingScheme)
          ;info_mpfit = info_mpfit_str_water_lim(status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, it, weightingScheme, wlim.eps_onoff)
          info_mpfit = info_mpfit_str_water_lim2(status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, it, weightingScheme, wlim.eps_onoff)
          ret = {parms:parms, info_mpfit:info_mpfit}
          RETURN, ret
        ENDIF
        ;store the output
        last_parms = parms & last_npegged = npegged & last_pfree_index = pfree_index
        last_bestnorm = bestnorm & last_status = status

        ;computation for weights
        fit_effect0 = fit_effect
        UEF_weights_and_fit_effect, NDVI_obs, NDVI_fit, fit_effect
        it ++
      ENDWHILE
    ENDELSE

    IF (status LE 0) THEN BEGIN
      PRINT, errmsg
      ;when it sees a problem of recursion run the command
      ;MPFIT_RESET_RECURSION
      ;at command line
      PRINT, 'Try MPFIT_RESET_RECURSION id the execution was stopped because of recursion'
      RETURN, 10
    ENDIF


    ;FOR i=0, N_ELEMENTS(info_mpfit)-1 DO PRINT, info_mpfit[i]
    TOC   
    ;info_mpfit = info_mpfit_str_water_lim(status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, it, weightingScheme, wlim.eps_onoff)
    info_mpfit = info_mpfit_str_water_lim2(status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, it, weightingScheme, wlim.eps_onoff)
    ret = {parms:parms, info_mpfit:info_mpfit}
    RETURN, ret
  END ;'MPFIT'
  ELSE:stop
ENDCASE

END