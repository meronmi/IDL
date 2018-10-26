FUNCTION compute_LTABewm, tJD, rain, tair, sDOY, eDOY, DOYoffset, half_life, cap
;example:
; res = compute_LTABewm(ecmwf_data.JD, ecmwf_data.rain, ecmwf_data.tav, sDOY, eDOY, DOYoffset, fg[11], fg[10])

;#################################################
;PRINT, JD2DDMMYYYY(tJD[0])
;PRINT, JD2DDMMYYYY(tJD[-1])
;look for all occurrences of start
yyyy=JD2YEAR(tJD)
doy=JD2DOY(tJD)

;find the leap years
indLeap =  WHERE((yyyy MOD 4) EQ 0, countLeap)

indStart = WHERE(doy EQ sDOY)
indStop = WHERE(doy EQ eDOY)
;treat incomplete seasons
IF (indStop[0] LT indStart[0]) THEN indStop = indStop[1:*]
IF (indStart[-1] GT indStop[-1]) THEN indStart = indStart[0:-2]
indStart = indStart  + DOYoffset
;the offset may make the first start to happen before the time series, drop one year if this is the cas
IF (indStart[0] LT 0) THEN BEGIN
  indStart = indStart[1:*]
  indStop = indStop[1:*]
ENDIF
;if the season starts in a leap and crossed the end of year (366), I have to start from sDOY+1
IF (countLeap GT 0) THEN BEGIN
  ind = ind_int_HISTOGRAM(indStart, indLeap)
  indStart[ind] = indStart[ind]+1
ENDIF
lengthOfPeriod = indStop[0] - indStart[0] + 1
lta_prec = FLTARR(lengthOfPeriod)
lta_tair = FLTARR(lengthOfPeriod)
;now element i of indStart and indStop indicates first and last JD of the season
;compute LTA
prec = FLTARR(lengthOfPeriod, N_ELEMENTS(indStart))
FOR i = 0,  lengthOfPeriod - 1 DO BEGIN
  ;lta_prec[i] = MEAN(rain[indStart+i], /NAN)
  lta_prec[i] = MEDIAN(rain[indStart+i])
  prec[i,*]= rain[indStart+i]
  ;lta_tair[i] = MEAN(tair[indStart+i], /NAN)
  lta_tair[i] = MEDIAN(tair[indStart+i])
ENDFOR
;h = PLOT(prec[*,0])
;FOR i = 1, N_ELEMENTS(indStart)-1 DO h = PLOT(prec[*,i], OVERPLOT =1)
;h = PLOT(lta_prec, color='green', OVERPLOT=1)


RETURN, BackwardExpWeightedMean_FAST(INDGEN(N_ELEMENTS(lta_tair)), lta_tair, lta_prec, Tb, half_life, cap)

END
