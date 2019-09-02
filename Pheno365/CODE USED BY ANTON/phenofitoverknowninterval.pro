; NAME:
;   phenoFitOverKnownInterval
;
; PURPOSE:
;   FUNCTION THAT RETRIEVES FOR A SINGLE TIME SERIES RELEVANT PHENOLOGICAL PARAMETERS
;   BASED ON A USER-DEFINED FUNCTION AND ITERATIVE FITTING WITH MPFIT

;
; INPUTS:
;   timeSeries of NDVI values (double) and corresponding Julian Dates
;     
;
; USAGE:
;   result =
;
; OUTPUT:
;   An structure of phenological attributes
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, March 2016, based on input from Michele Meroni
;   Adapted by:  Anton Vrieling, December 2016, for full season including decay
;   ReAdpated by: Michele Meroni, April 2019


;********************************************************************************************************


FUNCTION phenoFitOverKnownInterval, t, NDVIseries, dateRange, dateMax, smoothNDVIleft, smoothNDVIright, smoothNDVImax, smoothNDVIobs, $
                                    xjdLimP2, xjdLimP5, xjdFgP2, xjdFgP5, baseDate, indexName, crop2process, polyID       
;t:                 time in JD
;NDVIseries:        NDVI
;dateRange:         time interval over which to fit
;baseDate:          JD date that will be consider day 0 when returing results
;dateMax:           approximate of the time separating the ascending and descending phase, as estimated by locate_largest_season_in_jd_range()
;smoothNDVIleft, smoothNDVIRight, smoothNDVImax:   smoothed values on the left, right and max, as estimated by locate_largest_season_in_jd_range() 

; Include the necessary common block(s):
@cb_optimization.comm

fctName = 'tanh_Double'
nParms  = 7               ; number of parameters in the function to be fitted
noDataValue  = !VALUES.F_NAN       ; noDataValue for output
minDataPoint = 8          ; minimum number of data points needed to start phenological retrieval
sos_amplitude = [0.20,0.50] ; the value of the SOS_amplitude (i.e. at 0.20 or 20% of the amplitude between 1 march and 31 August the SOS is found
max_amplitude = 0.90        ; the value of the amplitude where we find the approximate moment of maximum (we do not take full maximum due to sometimes long high plateaus causing instable/strange retrievals

;Old version CR in Db
;IF (indexName EQ 'NDVI_mean') THEN BEGIN 
;  min95_5perc = 0.10          ; minimum difference between 95th and 5th percentile of valid NDVI data to start processing  (0.20 for Schier)
;  minMedian = 0.05            ; minimum median NDVI value to start processing (avoid processing in sea)  (0.20 for Schiermonnikoog, 0.15 trial for Kenya)
;  minNDVI = -0.10             ; values below this minimum will be removed from time series
;  upenv = 1                 ; fitting on upper envelope? 1=yes, 0=no
;ENDIF ELSE BEGIN
;  ;sentinel 1
;  min95_5perc = 2             ; minimum difference between 95th and 5th percentile of VH VV ratio
;  minMedian = -20             ; minimum median ratio
;  minNDVI = -10000            ; values below this minimum will be removed from time series
;  upenv = 0                 ; fitting on upper envelope? 1=yes, 0=no
;ENDELSE

CASE indexName OF
  'NDVIm':BEGIN
    min95_5perc = 0.10          ; minimum difference between 95th and 5th percentile of valid NDVI data to start processing  (0.20 for Schier)
    minMedian = 0.05            ; minimum median NDVI value to start processing (avoid processing in sea)  (0.20 for Schiermonnikoog, 0.15 trial for Kenya)
    minNDVI = -0.10             ; values below this minimum will be removed from time series
    upenv = 1                 ; fitting on upper envelope? 1=yes, 0=no
  END
  'CR':BEGIN
     ;sentinel 1
     min95_5perc = 0.1             ; minimum difference between 95th and 5th percentile of VH VV ratio
     minMedian = 0.001             ; minimum median ratio
     minNDVI = 0.0000000001           ; values below this minimum will be removed from time series
     upenv = 0                 ; fitting on upper envelope? 1=yes, 0=no
  END
  'RVI':BEGIN
     min95_5perc = 0.1             ; minimum difference between 95th and 5th percentile of VH VV ratio
     minMedian = 0.001             ; minimum median ratio
     minNDVI = 0.0000000001           ; values below this minimum will be removed from time series
     upenv = 0                 ; fitting on upper envelope? 1=yes, 0=no
  END
  ELSE: res = DIALOG_MESSAGE('Stopped at phenoFitOverKnownInterval, line 81')
ENDCASE

;dateRangeTmp = dateRange  ; temporary version of the dateRange (because it gets adapted in programme)


  
; define output structure
pheno= {$
  retr:    0b, $                                      ; whether an appropriate retrieval took place (0 is no, 1 is yes)
  maxNDVI: FLOAT(noDataValue), $                      ; the maximum value for the model
  AMP1:    FLOAT(noDataValue), $                      ; the amplitude (between maximum and fitted value for preceding 1 Feb) -- so growth phase)
  AMP2:    FLOAT(noDataValue), $                      ; the amplitude (between maximum and fitted value for subsequent 1 Feb -- so decay phase)
  SOS20:   FLOAT(noDataValue), $                      ; the moment when modelled NDVI reaches 20% of amplitude1 (above 1Feb value)
  SOSunrel:   FLOAT(noDataValue), $                   ; index of unreliability
  SOSfractUnrel:   FLOAT(noDataValue), $                   ; index of unreliability (0-1)
  SOS50:   FLOAT(noDataValue), $                      ; the moment when modelled NDVI reaches 50% of amplitude1 (above 1Feb value)
  EOS20:   FLOAT(noDataValue), $                      ; the first moment when modelled NDVI reaches 20% of amplitude2 (above min)
  EOSunrel:   FLOAT(noDataValue), $                   ; index of unreliability
  EOSfractUnrel:   FLOAT(noDataValue), $                   ; index of unreliability (0-1)
  EOS50:   FLOAT(noDataValue), $                      ; the first moment when modelled NDVI reaches 50% of amplitude2 (above min)
  PS90:    FLOAT(noDataValue), $                      ; the "peak season": the first moment when modelled NDVI reaches 90% of its maximum (maxNDVI) --> note, we specifically avoid the full maximum because of long plateaus at times of high values... 
  LGS20:   FLOAT(noDataValue), $                      ; length of growing season = EOS20-SOS20  
  LGS50:   FLOAT(noDataValue), $                      ; length of growing season = EOS50-SOS50
  cumNDVI20: FLOAT(noDataValue), $                    ; integral of the fitted model between SOS20 and EOS20
  cumNDVI50: FLOAT(noDataValue), $                    ; integral of the fitted model between SOS50 and EOS50 
  PearsonCC: FLOAT(noDataValue), $                    ; Pearson correlation coefficient between fitted and original NDVI (for all observations)
  RMSD:    FLOAT(noDataValue), $                      ; RMSD (root mean square deviation) between fitted and original NDVI (for all observations)
  MSD:     FLOAT(noDataValue), $                      ; MSD (mean signed deviation) between fitted and original NDVI (for all observations)
  Pval:    FLOAT(noDataValue), $                      ; P value of the test (model fir significantly better than mean of obs), the smaller, the better, in principle should be <0.05
  PCERROR: FLTARR(nParms)+noDataValue, $
  nImages: 0B, $                                      ; number of NDVI data points used for fitting (for all observations)
  maxGap:  FLOAT(noDataValue), $                        ; maximum gap between two valid observations within season
  PARAM_0:   FLTARR(nParms)+noDataValue, $            ; parameter values for the model with estimated parameters
  PARAM_1:   FLTARR(nParms)+noDataValue, $            ; parameter values for the first fitted model
  PARAM_x:   FLTARR(nParms)+noDataValue, $            ; parameter values for the iteratively fitted model
  jdInterval: FLTARR(2), $
  message: ''}
tempPheno = pheno               ; create a temporary copy of the base structure to assign all noData in the end if needed 
  
; First test if NDVI values allow in principle for phenology-retrieval (5-95 percentile at least 0.2 NDVI unit, at least 5 valid points)
; NOTE: additional test to check if first and/or last observation are not very strange values, i.e. >0.1 below 5th percentile

;extract the requested data range
index = WHERE(FINITE(NDVIseries) AND (NDVIseries GE minNDVI) AND (t GE dateRange[0]) AND (t LE dateRange[1]))
IF N_ELEMENTS(index) LT minDataPoint THEN BEGIN
   pheno.message = 'Insufficients data points' 
   RETURN, pheno
ENDIF

;go on, sufficient data points
NDVIseriesGood = NDVIseries[index]  &  tGood = t[index]
smoothNDVIobsGood = smoothNDVIobs[index]
; sort observations based on tGood
ind = SORT(tGood)
tGood = tGood[ind]  &  NDVIseriesGood=NDVIseriesGood[ind] & smoothNDVIobsGood = smoothNDVIobsGood[ind]
; check if 1) min95_5perc is okay, 2) median is below minMedian, 3) at least 4 observations exist before and after midpoint season (1 August)
percNDVI = percentiles(NDVIseriesGood,value=[0.05,0.50,0.95])
; check if first or last index value is strange and remove (strange = smaller than 5th percentile minus 0.1 NDVI) --> this can result in very weird fits
;M+ This was done by Anton, omit it for now
;  IF NDVIseriesGood[0] LE percNDVI[0]-0.1 THEN BEGIN
;    NDVIseriesGood = NDVIseriesGood[1:*] & tGood = tGood[1:*]
;  ENDIF
;  IF NDVIseriesGood[N_ELEMENTS(tGood)-1] LE percNDVI[0]-0.1 THEN BEGIN
;    NDVIseriesGood = NDVIseriesGood[0:N_ELEMENTS(tGood)-2] & tGood = tGood[0:N_ELEMENTS(tGood)-2]
;  ENDIF
;  percNDVI = percentiles(NDVIseriesGood,value=[0.05,0.50,0.95])            ; recalculate after removing weird values
  ;M+ do not use midtime as the curve might be asymmetric, use dateMax instead
  ;midTime = dateRangeTmp[0] + (dateRangeTmp[1]-dateRangeTmp[0])/2
  
  
; we become a bit more lenient as before: now we want at least 3 values before and 3 values after the midTime
IF ~((percNDVI[2] - percNDVI[0] GE min95_5perc) AND $
   (percNDVI[1] GE minMedian) AND $
   (tGood[2] LT dateMax) AND $              ;3 obs before max
   (tGood[N_ELEMENTS(tGood)-3] GT dateMax)) THEN BEGIN ;3 obs after max
   pheno.message = 'Variability too low or insufficient data points on one side or 5% percetile too low'
   RETURN, pheno
ENDIF 

;go on
startParam = FLTARR(7)
parinfo = REPLICATE({limited:[0,0], limits:[0.d,0.d], fixed:0}, nparms)
;##########################################
;p [0] + p [1] * (TANH((x - p [2]) * p [3]) + 1) / 2.0 + p [4] * (TANH((x - p [5]) * p [6]) + 1) / 2.0 - p [4]
;##########################################
indT1 = WHERE(tGood LE dateMax)
indT2 = WHERE(tGood GT dateMax)
startParam[0] =  MIN(NDVIseriesGood[indT1], indMin1) 
parinfo[0].limited = [1,1]
CASE indexName OF
  'NDVIm': BEGIN
    parinfo[0].limits = [startParam[0]-ABS(startParam[0])/1.0,0.8d]     ;manage the case where startParam[0] is negative with ABS
  END
  'CR': BEGIN
    parinfo[0].limits = [startParam[0]-ABS(startParam[0])/1.0,1.0d]     ;manage the case where startParam[0] is negative with ABS
  END
  'RVI': BEGIN
    parinfo[0].limits = [MAX([startParam[0]-ABS(startParam[0])/1.0,0.0]), MAX(NDVIseriesGood[indT1], indMin1) ]
  END
  ELSE: res = DIALOG_MESSAGE('Stopped at phenoFitOverKnownInterval, line 176.' + crop2process + ', ID ' + STRTRIM(polyID))
ENDCASE
;IF (indexName EQ 'NDVI_mean') THEN $
;  parinfo[0].limits = [startParam[0]-ABS(startParam[0])/2.0d,0.8d] $    ;manage the case where startParam[0] is negative with ABS
;ELSE parinfo[0].limits = [startParam[0]-ABS(startParam[0])/2.0d,0.0d]   

startParam[1] = smoothNDVImax - smoothNDVIleft + 0.1
parinfo[1].limited = [1,1]
parinfo[1].limits = [0.01d,startParam[1]*2d]           ; we constrain the amplitude1 to be maximum (25%) 100 more than the difference of observed max and min NDVI

;startParam[2] = dateRange[0]+(dateMax-dateRange[0])/2        ; midpoint between start of optimization window and timing observed maximum  on smoothed curve
startParam[2] = xjdFgP2        ; time at which it grows (decay) 50%
parinfo[2].limited = [1,1]
parinfo[2].limits = [dateRange[0],xjdLimP2]         ;Mic: constrain the inflection point in between

startParam[4] = smoothNDVImax - smoothNDVIright + 0.1
parinfo[4].limited = [1,1]
parinfo[4].limits = [0.01,startParam[4]*2d]           ; we constrain the amplitude2 to be maximum (25 was from Anton) 100% more than the difference of observed max and min NDVI

;startParam[5] =  dateMax+(dateRange[1]-dateMax)/2           ; midpoint between timing observed maximum and end of optimization window 
startParam[5] =  xjdFgP5           ; time at which it grows (decay) 50%
parinfo[5].limited = [1,1]
parinfo[5].limits = [xjdLimP5,dateRange[1]]

CASE indexName OF
  'NDVIm':BEGIN
    startParam[3] = 0.02d
    parinfo[3].limited = [1,1]
    parinfo[3].limits = [0.0d,0.275d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
    startParam[6] =  -0.02d
    parinfo[6].limited = [1,1]
    parinfo[6].limits = [-0.275d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
  END
  'CR':BEGIN
    startParam[3] = 0.02d
    parinfo[3].limited = [1,1]
    parinfo[3].limits = [0.0d,0.275d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
    startParam[6] =  -0.02d
    parinfo[6].limited = [1,1]
    parinfo[6].limits = [-0.275d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
  END
  'RVI':BEGIN
    startParam[3] = 0.02d
    parinfo[3].limited = [1,1]
    parinfo[3].limits = [0.0d,0.275d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
    startParam[6] =  -0.02d
    parinfo[6].limited = [1,1]
    parinfo[6].limits = [-0.275d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
  END
  ELSE: res = DIALOG_MESSAGE('Stopped at phenoFitOverKnownInterval, line 225 '+ crop2process + ', ID ' + STRTRIM(polyID))
ENDCASE

;;slope is sensor dependent (this was for dB)
;IF (indexName EQ 'NDVI_mean') THEN BEGIN
;  startParam[3] = 0.02d
;  parinfo[3].limited = [1,1]
;  parinfo[3].limits = [0.0d,0.275d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
;  startParam[6] =  -0.02d
;  parinfo[6].limited = [1,1]
;  parinfo[6].limits = [-0.275d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
;ENDIF ELSE BEGIN
;  ;s1
;  startParam[3] = 0.2d
;  parinfo[3].limited = [1,1]
;  parinfo[3].limits = [0.0d, 2.75d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
;  startParam[6] =  -0.2d
;  parinfo[6].limited = [1,1]
;  parinfo[6].limits = [-2.75d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
;ENDELSE
;firstFitParam = MPFITFUN(fctName, tGood, NDVIseriesGood, 1, startParam, parinfo=parinfo, STATUS = stat, /QUIET)
; that is a single fit, let's try now to fit iteratively...

weights = FLTARR(N_ELEMENTS(NDVIseriesGood))*0.0+1.0         ; initialize weights
;with S2 it may happen that there are few obs, some negatively biased, and before entering the upper envelop loop (at first otimization), the optimization get stuck
;in a shape that satisfies the negative spikes. The upper envelope then does not work. so we weight the observations giving less credit to those
;that are below the smoothed curve in the first optimization, after that we remov the weights.
;Weights are compute similarly as done in the upper envelope loop
; weight calculation
resu = NDVIseriesGood - smoothNDVIobsGood
dif  = ABS(resu)
mm   = MAX(dif)
weights4firsOpt = weights
IF (indexName EQ 'NDVIm') THEN BEGIN
  index = WHERE(resu LE 0)
  IF index[0] NE -1 THEN weights4firsOpt[index] = 1 - (dif[index]/mm)
ENDIF
;IF (indexName EQ 'NDVI_mean') THEN BEGIN 
;  index = WHERE(resu LE 0)
;  IF index[0] NE -1 THEN weights4firsOpt[index] = 1 - (dif[index]/mm)
;ENDIF ELSE BEGIN
;  weights4firsOpt = 1 - (dif/mm)
;ENDELSE
;check conistency in start and parinfo, print if there is a problem
FOR i = 0, N_ELEMENTS(startParam)-1 DO BEGIN
  IF ((startParam[i] LE parinfo[i].limits[0]) OR (startParam[i] GE parinfo[i].limits[1])) THEN BEGIN
    str = 'startParam ' + STRTRIM(i,2) + ' outside limits'
    PRINT, str 
  ENDIF
ENDFOR
ret = opt_model2parinfo(fctname, double(tGood), double(NDVIseriesGood), double(weights)*0.0, FLOAT(startParam), double(weights4firsOpt), parinfo)
IF (stat EQ 0) THEN BEGIN
  PRINT, 'ERROR IN INPUT PARAMETER MUST BE FIXED'
  res = DIALOG_MESSAGE('ERROR IN INPUT PARAMETER MUST BE FIXED, Stopped at phenoFitOverKnownInterval, line 272 ' + crop2process + ', ID ' + STRTRIM(polyID)) + '. ' + str
  STOP
ENDIF
IF ~((stat gt 0 and stat lt 5) OR (stat gt 5)) THEN BEGIN
  tempPheno.message = 'Fitting failed: ' + error
  RETURN, tempPheno
ENDIF
pheno.retr = 1           ; meaning a good retrieval because of positive slope (if this point not reached, then value=0)
pheno.PCERROR = PCERROR
;M remove this of Anton as it should be taken into account by     locate_largest_season_in_jd_range
;; check for strange cases where the season starts with decay and ends with growth
;; this is probably due to ploughing or something, and we attempt still in making a reasonable prediction, taking off the first part of the year
;; to avoid working on weird fittings, request a minimal separation of a month
;IF p[5] LT p[2]-30 THEN BEGIN
;  index = WHERE(tGood GE p[5] + 0.25*(p[2]-p[5]))    ; retain only points from first midpoint onwards: add 25% to avoid taking values in the decay phase
;  IF N_ELEMENTS(index) GE minDataPoint THEN BEGIN
;    dateRangeTmp[0]=p[5]                              ; adapt dateRangeTmp to estimate minimum not anymore for 1 February, but much later (at time of previous inflection point)
;    tGood = tGood[index] & NDVIseriesGood=NDVIseriesGood[index]
;    ; and repeat again the fitting
;    firstFitParam = MPFITFUN(fctName, tGood, NDVIseriesGood, 1, startParam, parinfo=parinfo, /QUIET)
;    weights = FLTARR(N_ELEMENTS(NDVIseriesGood))*0.0+1.0         ; initialize weights
;    ret = opt_model2parinfo(fctname, double(tGood), double(NDVIseriesGood), double(weights)*0.0, FLOAT(startParam), double(weights), parinfo)
;  ENDIF ELSE p[*]=noDataValue                     ; if no sufficient points remain
;ENDIF
    
; calculate maximum gap in dataset (in days)
maxGap = max(tGood-SHIFT(tGood,1))  
; assign values to output parameters
pheno.Param_0 = startParam
;pheno.Param_1 = firstFitParam
pheno.Param_x = p
;IF fit eq 1 THEN param = pheno.Param_1
;IF fit eq 2 THEN param = pheno.Param_x
param = pheno.Param_x

;  ; note here we have an additional condition to avoid weird profiles were the first inflection point is later in time than the second
;  ; we tested this before for initial parameter estimate, but afterwards could still occur.
;IF param[5] LT param[2]+15 THEN BEGIN  
;  stat = 0
;  pheno = tempPheno
;ENDIF
; 
tDayGrid = INDGEN(dateRange[1]-dateRange[0]+1)+dateRange[0]     
pheno.jdInterval = [dateRange[0], dateRange[1]]
fitNDVIdayGrid = TANH_DOUBLE(tDayGrid,param)                   ; corresponding fitted NDVI values (daily)
pheno.maxNDVI = max(fitNDVIdayGrid, indMax)                   ; return maximum NDVI value
indMax = indMax[0]                                           ; probably it should not happen, but just in case multiple maxima are found
;be care, a season may non have an scending phase and indMax is 0, so the following line (commented) would point to the last element
;pheno.AMP1 = pheno.maxNDVI - MIN(fitNDVIdayGrid[0:indMax-1])   ; in principle minimum should be fitted value for 1 February, but sometimes strange retrievals (inverted) and give still result...
IF (indMax EQ 0) THEN  pheno.AMP1 = !VALUES.F_NAN ELSE pheno.AMP1 = pheno.maxNDVI - MIN(fitNDVIdayGrid[0:indMax-1])   
; SOS/EOS/PS are found as first moments when threshold is reached: we also ascertain that SOS/PS is along increasing NDVI, and EOS along decreasing NDVI
indSOS= -1
IF  (FINITE(pheno.AMP1) EQ 1) THEN BEGIN
  IF  (pheno.AMP1 GT 0.001) THEN BEGIN 
    ; 20% threshold
    ;be care SHIFT is circular, replace the first element mantaining the actual slope
    shifted = SHIFT(fitNDVIdayGrid,1)
    shifted[0] = fitNDVIdayGrid[0] - (fitNDVIdayGrid[1]-fitNDVIdayGrid[0])
    indSOS = WHERE(fitNDVIdayGrid GT (sos_amplitude[0]*pheno.AMP1 + MIN(fitNDVIdayGrid[0:indMax-1])) AND fitNDVIdayGrid GT shifted)
    IF indSOS[0] ne -1 THEN pheno.SOS20 = tDayGrid[indSOS[0]] - baseDate
    ;%50
    indSOS = WHERE(fitNDVIdayGrid GT (sos_amplitude[1]*pheno.AMP1 + MIN(fitNDVIdayGrid[0:indMax-1])) AND fitNDVIdayGrid GT shifted)
    IF indSOS[0] ne -1 THEN pheno.SOS50 = tDayGrid[indSOS[0]] - baseDate
  ENDIF
ENDIF
; 50% threshold

IF (indMax EQ N_ELEMENTS(fitNDVIdayGrid)-1) THEN  pheno.AMP2 = !VALUES.F_NAN  ELSE pheno.AMP2 = pheno.maxNDVI - MIN(fitNDVIdayGrid[indMax+1:*])
indEOS= -1 

;It may happen that the "descending phase" is indeed flkat, no need to determine EOS (that for som math may be available for 50 and not for 20)
IF  (FINITE(pheno.AMP2) EQ 1) THEN BEGIN
  IF  (pheno.AMP2 GT 0.001) THEN BEGIN
    IF (indMax LT N_ELEMENTS(fitNDVIdayGrid)-1) THEN BEGIN
      ; 20% threshold
      ;be care SHIFT is circular, , replace the last element mantaining the actual slope
      shifted = SHIFT(fitNDVIdayGrid,-1)
      shifted[-1] = fitNDVIdayGrid[-1] - (fitNDVIdayGrid[-2]-fitNDVIdayGrid[-1])
      ;because of the rquested time range, the function may not have a descending phase, meaning that indMax is already at the right edge
      IF (FINITE(pheno.SOS20) EQ 1) THEN SOS20 = pheno.SOS20 ELSE SOS20 = -1000
      indEOS =  WHERE(fitNDVIdayGrid LT (sos_amplitude[0]*pheno.AMP2 + MIN(fitNDVIdayGrid[indMax+1:*])) AND $
                      fitNDVIdayGrid GT shifted AND $
                      tDayGrid GT (SOS20 + baseDate)); AND $
                      ;INDGEN(N_ELEMENTS(fitNDVIdayGrid)) GT indMax; check also if EOS is after SOS! and MAX as WEL
      IF indEOS[0] ne -1 THEN pheno.EOS20 = tDayGrid[indEOS[0]] - baseDate
      IF indEOS[0] ne -1 AND indSOS[0] ne -1 THEN BEGIN
        pheno.LGS20 = pheno.EOS20 - pheno.SOS20
        pheno.cumNDVI20 = TOTAL(fitNDVIdayGrid[indSOS[0]:indEOS[0]]);/30.               ; note: we divide by 30 (~month) to get lower values to be compared with common cumNDVI measures from low-res satellites
      ENDIF
      ; 50% threshold
      IF (FINITE(pheno.SOS50) EQ 1) THEN SOS50 = pheno.SOS50 ELSE SOS50 = -1000
      indEOS =  WHERE(fitNDVIdayGrid LT (sos_amplitude[1]*pheno.AMP2 + MIN(fitNDVIdayGrid[indMax+1:*])) AND fitNDVIdayGrid GT shifted AND tDayGrid GT (SOS50 + baseDate))    ; check also if EOS is after SOS!
      IF indEOS[0] ne -1 THEN pheno.EOS50 = tDayGrid[indEOS[0]] - baseDate
      IF indEOS[0] ne -1 AND indSOS[0] ne -1 THEN BEGIN
        pheno.LGS50 = pheno.EOS50 - pheno.SOS50
        pheno.cumNDVI50 = TOTAL(fitNDVIdayGrid[indSOS[0]:indEOS[0]]);/30.               ; note: we divide by 30 (~month) to get lower values to be compared with common cumNDVI measures from low-res satellites
      ENDIF
    ENDIF
  ENDIF  
ENDIF


; peak season
IF  (FINITE(pheno.AMP1) EQ 1) THEN BEGIN
  indPS = WHERE(fitNDVIdayGrid GT (max_amplitude*pheno.AMP1 + MIN(fitNDVIdayGrid[0:indMax-1])) AND fitNDVIdayGrid gt SHIFT(fitNDVIdayGrid,1))
  IF indPS[0] ne -1 THEN pheno.PS90 = tDayGrid[indPS[0]] - baseDate
ENDIF
; Stats
fittedNDVIobsGrid = TANH_DOUBLE(tGood,param)                   ; corresponding fitted NDVI values (daily)
pheno.PearsonCC = CORRELATE(fittedNDVIobsGrid, NDVIseriesGood)
pheno.RMSD = SQRT(TOTAL((fittedNDVIobsGrid-NDVIseriesGood)^2)/N_ELEMENTS(fittedNDVIobsGrid))
pheno.MSD = TOTAL(fittedNDVIobsGrid-NDVIseriesGood)/N_ELEMENTS(fittedNDVIobsGrid)
pheno.Pval = ModelGoodnessOfFit(NDVIseriesGood, fittedNDVIobsGrid, nParms)
pheno.nImages = N_ELEMENTS(fittedNDVIobsGrid)
;reliability indicator:
unrel = reliabilitySOSEOS(tGood, tDayGrid, fitNDVIdayGrid)
pheno.SOSunrel = unrel[0]
pheno.EOSunrel = unrel[1]
pheno.SOSfractUnrel = unrel[2]
pheno.EOSfractUnrel = unrel[3]
; FOR VALUES OF EOS INTO JANUARY OF THE NEXT YEAR: QUICK SOLUTION
;IF pheno.EOS20 gt 366 THEN pheno.EOS20 = pheno.EOS20-366             ; NOTE: this is for LEAP years, and to see if maintain like this...
;IF pheno.EOS50 gt 366 THEN pheno.EOS50 = pheno.EOS50-366
;ENDIF
 
;; apparently some results tend to be infinite or have negative SOS (check reason). To avoid that, set those back to noDataValue
;IF (~FINITE(pheno.SOS20) OR ~FINITE(pheno.PS90) OR ~FINITE(pheno.EOS20) OR ~FINITE(pheno.LGS20) OR ~FINITE(pheno.AMP1) OR ~FINITE(pheno.AMP2) OR ~FINITE(pheno.maxNDVI) OR pheno.SOS20 lt 0 $
;    OR ~FINITE(pheno.SOS50) OR ~FINITE(pheno.EOS50) OR ~FINITE(pheno.LGS50) OR pheno.SOS50 lt 0) OR $
;    ABS(pheno.PS90-pheno.SOS20) LT 5 THEN $
;    pheno = tempPheno
;;ENDIF ; pheno retrieved?
pheno.maxGap = maxGap              ; retain this value even for pixels where no retrievals are obtained ELSE pheno.message = 'Variability too low or insufficient data points on one side'     
  
;now, as it may happen that there is only one HT, keep it and give a warning
IF (FINITE(pheno.SOS50) EQ 0) OR (FINITE(pheno.EOS50) EQ 0) THEN BEGIN
  ;tempPheno.message = 'Fitting did not fail BUT THE FITTED FUNCTION IS NOT COMPLETE (only green up or decay)
  Pheno.message = 'Fitting did not fail BUT THE FITTED FUNCTION IS NOT COMPLETE (only green up or decay)
  ;RETURN, tempPheno
ENDIF
  
RETURN, pheno

END ; 