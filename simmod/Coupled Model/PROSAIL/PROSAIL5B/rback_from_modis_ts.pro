; PROCEDURE COMMENTS +
; NAME: rback_from_MODIS_TS
; AUTHOR: Michele Meroni
; CONTACT INFO: michele.meroni@gmail.com
; DESCRIPTION:
; CALLING SEQUENCE:
;       rback = rback_from_MODIS_TS(R1, R2, R3, R4, R5, R6, R7)
; INPUTS: time series of MODIS reflectances
;         prct: the desired percentile used to retrieve background reflectance
; OUTPUTS: reflectance of the background to be used in PROSAIL (400-2500 nm, 1 nm step)
;          Not that it is assumed that the bare soil is visible during the dry season
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
;  - compute the NDVI time series
;    smooth it
;    take the all obs below the prct percentile (usually 5%)
;    of this 5 % check which are closer to the smoothed value (to avoid outliers)
;    take the mean of the 5 obs that are most similar to the smoothed value
;  - extract the mean 7 reflectances
;  Then:
;  - fit Price function
;  OR
;  - fit a 3rd degree polynomium and estimate the reflectance over th 400-2500 nm range
;  Note that the values are used by PROSAIL only within MODIS bands
; EXAMPLE:
; MODIFICATION HISTORY:
;       Coded on 21 July 2014
; CODING NOTES:



FUNCTION rback_from_MODIS_TS, R1, R2, R3, R4, R5, R6, R7, prct, doPlot
  fitPrice = 1 ;fit Price or use cubic function (set it to 0)
  doPlot = 0
  IF (prct LT 0.0) OR (prct GT 10.0) THEN STOP      ;ASKING FOR PRCT GT THAN 10 WOULD be strange..
  ; missing data must have NaN value
  maxVal = MAX([R1, R2, R3, R4, R5, R6, R7], /NAN)
  minval = MIN([R1, R2, R3, R4, R5, R6, R7], /NAN)
  ;IF (minVal LT 0.0) OR (maxVal GT 1.0) THEN STOP
  ;compute NDVI
  ndvi =  (R2 - R1) / (R2 + R1)
  ;smooth it to avoid to pick up ouliers
  SAGO_INTERPOL3, ndvi, ndvi*0, smNDVI=smNDVI, iMAX=10
  ;indFin = WHERE(FINITE(ndvi))
  ;indSortFin = SORT(ndvi[indFin])
  indFin = WHERE(FINITE(smNDVI) AND FINITE(ndvi))
  indSortFin = SORT(smNDVI[indFin])
  ;Take form 0 percentile to the observation corresponding to the prct percentile
  n = N_ELEMENTS(indSortFin)
  scaledPerc = linspace(0, 100, n)
  ind = VALUE_LOCATE(scaledPerc, prct)
  data_ind = indFin[indSortFin[0:ind]]
  ;of this smoothed ndvi some will have an actual NDVI much lower than the smoothed ones (these are outliers)
  ;i want to get rid of them
  delta = ABS(smNdvi[data_ind]-ndvi[data_ind])
  ind = SORT(delta)
  ;  x = FINDGEN(N_ELEMENTS(ndvi))
  ;  hw = WINDOW(DIMENSIONS=[1000,200])
  ;  h0 = PLOT(ndvi, /CURRENT)
  ;  h1 = PLOT (x,smNdvi, color ='r', overplot = 1)
  ;  h2 = PLOT (x[data_ind], ndvi[data_ind], COLOR='green', SYMBOL='+', LINESTYLE='none', overplot = 1)
  ;  h2 = PLOT (x[data_ind[ind[0:4]]], ndvi[data_ind[ind[0:4]]], COLOR='red', SYMBOL='o', LINESTYLE='none', overplot = 1)
  wl = datacentwl_MODIST_7b()
  ;take the mean of the five obs with NDVI below prct and with lowest diff with smoothed ndvi
  R = [MEAN(R1[data_ind[ind[0:4]]]), MEAN(R2[data_ind[ind[0:4]]]), MEAN(R3[data_ind[ind[0:4]]]), $
    MEAN(R4[data_ind[ind[0:4]]]), MEAN(R5[data_ind[ind[0:4]]]), MEAN(R6[data_ind[ind[0:4]]]), $
    MEAN(R7[data_ind[ind[0:4]]])]
  indwl = SORT(wl)
  indwlAvailable = WHERE(FINITE(R[indwl]), count)
  IF (count EQ 0) THEN STOP
  indwl = indwl[indwlAvailable]
  IF (fitPrice EQ 1) THEN BEGIN
    wl_modis = wl[indwl]
    r_modis = R[indwl]
    weights =wl_modis * 0.0 + 1.0
    ;    ;here remove nan wl if we are using VIs
    ;    indR = WHERE(FINITE(r_modis))
    ;    wl_modis = wl[indR]
    ;    r_modis = r_modis[indR]
    ;    weights =wl_modis[indR]
    ;    indwl2 = SORT(wl_modis) ;indwl should not be used as it is the used in resampling in MPIFIT
    ;    wl_modis = wl_modis[indwl2]
    ;    r_modis = r_modis[indwl2]
    err = weights ;ignored by MPFIT
    fcnargs = {wl_modis:wl_modis, r_modis:r_modis, indwl:indwl}
    parinfo = REPLICATE({FIXED:1, LIMITED:[0,0], LIMITS:[-999.d,-999.d], STEP:0.0d}, 4)
    parinfo[*].FIXED = 0
    parinfo[*].LIMITED = [1, 1]
    parinfo[*].STEP = 0.01
    parinfo[0].LIMITS  = [0.05, 0.5]
    parinfo[1].LIMITS  = [-0.1, 0.1]
    parinfo[2].LIMITS  = [-0.05, 0.05]
    parinfo[3].LIMITS  = [ -0.04, 0.04]
    fg = [MEAN(parinfo[0].LIMITS),MEAN(parinfo[1].LIMITS),MEAN(parinfo[2].LIMITS),MEAN(parinfo[3].LIMITS)]
    parms = MPFITFUN('MPFIT_SOIL_SPEC', wl_modis, r_modis, err, fg, WEIGHTS = weights, $
      FUNCTARGS=fcnargs, MAXITER=500, ERRMSG=errmsg, $; NPRINT=nprint, QUIET=quiet, FTOL=1.D-3, XTOL=xtol, GTOL=gtol, NITER=niter,
      STATUS=status, $;ITERPROC=iterproc, ITERARGS=iterargs,
      COVAR=covar, PERROR=perror, BESTNORM=bestnorm, NPEGGED = npegged, $
      PARINFO=parinfo, PFREE_INDEX = pfree_index, YFIT=yfit, /AUTODERIVATIVE, /QUIET)
    IF (status LE 0) THEN BEGIN
      PRINT, errmsg
      PRINT, 'Try MPFIT_RESET_RECURSION if the execution was stopped because of recursion'
      RETURN, 10
    ENDIF
    wlspec = FLOAT(INDGEN(2101) + 400)
    Rfit2 = soilspec(wlspec,parms[0],parms[1],parms[2],parms[3])
    IF (doPlot) THEN BEGIN
      PRINT, fg
      PRINT, parms
      h0 = PLOT(wl_modis, r_modis, LINESTYLE='', SYMBOL='o', TITLE='rsoil')
      h1 = PLOT(wl_modis, yfit, COLOR='red', OVERPLOT=1)
      h2 = PLOT(wlspec, Rfit2, COLOR='green', OVERPLOT=1)
    ENDIF
  ENDIF ELSE BEGIN
    c = REFORM(POLY_FIT(wl, R, 3, YFIT = Rfit, STATUS = status))
    IF (status NE 0) THEN STOP
    wlspec = FLOAT(INDGEN(2101) + 400)
    Rfit2 = c[0] + c[1] * wlspec + c[2] * wlspec^2 + c[3] * wlspec^3
  ENDELSE
  ;  RSD = [STDDEV(R1[data_ind[ind[0:4]]]), STDDEV(R2[data_ind[ind[0:4]]]), STDDEV(R3[data_ind[ind[0:4]]]), $
  ;         STDDEV(R4[data_ind[ind[0:4]]]), STDDEV(R5[data_ind[ind[0:4]]]), STDDEV(R6[data_ind[ind[0:4]]]), $
  ;         STDDEV(R7[data_ind[ind[0:4]]])]
  ;  h = PLOT(wl, R, LINESTYLE = "none", SYMBOL="o")
  ;  h = PLOT(wl, R+RSD, LINESTYLE = "none", SYMBOL="+", OVERPLOT=1)
  ;  h = PLOT(wl, R-RSD, LINESTYLE = "none", SYMBOL="+", OVERPLOT=1)
  ;  h = PLOT(wlspec, Rfit2, COLOR="green", OVERPLOT=1)


  RETURN, Rfit2
END