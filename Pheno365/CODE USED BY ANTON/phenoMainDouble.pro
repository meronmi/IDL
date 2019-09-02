; NAME:
;   PhenoMainDouble
;
; PURPOSE:
;   FUNCTION THAT RETRIEVES FOR A SINGLE TIME SERIES RELEVANT PHENOLOGICAL PARAMETERS
;   BASED ON A USER-DEFINED FUNCTION AND ITERATIVE FITTING WITH MPFIT
;    --> contrary to phenoMain here we aim at getting the full season including decay
;
; INPUTS:
;   timeSeries of NDVI values (double) and corresponding Julian Dates
;     we expect only positive NDVI values
;     fit (1 if first fitting of function, 2 if iterative fitting): 1 only for pheno attributes...
;
; USAGE:
;   result = PhenoMainDouble(t,NDVIseries,fit)
;
; OUTPUT:
;   An array of phenological attributes
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, March 2016, based on input from Michele Meroni
;   Adapted by:  Anton Vrieling, December 2016, for full season including decay


;********************************************************************************************************
; Section of user-defined functions used within program

FUNCTION phenoMainDouble, t, NDVIseries, fit, dateRange, baseDate       ; t = in Julian Dates (ENVI), NDVI should be double and have set values < 0 as noData
; Include the necessary common block(s):
  @cb_optimization.comm

  fctName = 'tanh_Double'
  nParms  = 7               ; number of parameters in the function to be fitted
  noDataValue  = -99d       ; noDataValue for output
  minDataPoint = 8          ; minimum number of data points needed to start phenological retrieval
  min95_5perc = 0.10        ; minimum difference between 95th and 5th percentile of valid NDVI data to start processing  (0.20 for Schier)
  
;  minMedian = 0.15          ; minimum median NDVI value to start processing (avoid processing in sea)  (0.20 for Schiermonnikoog, 0.15 trial for Kenya)
;  minNDVI = 0.10            ; values below this minimum will be removed from time series
;  sos_amplitude = [0.20,0.50]      ; the value of the SOS_amplitude (i.e. at 0.20 or 20% of the amplitude between 1 march and 31 August the SOS is found 
;  max_amplitude = 0.90      ; the value of the amplitude where we find the approximate moment of maximum (we do not take full maximum due to sometimes long high plateaus causing instable/strange retrievals
  
  minMedian = 0.05           ; minimum median NDVI value to start processing (avoid processing in sea)  (0.20 for Schiermonnikoog, 0.15 trial for Kenya)
  minNDVI = -0.10            ; values below this minimum will be removed from time series
  sos_amplitude = [0.20,0.50]      ; the value of the SOS_amplitude (i.e. at 0.20 or 20% of the amplitude between 1 march and 31 August the SOS is found
  max_amplitude = 0.90      ; the value of the amplitude where we find the approximate moment of maximum (we do not take full maximum due to sometimes long high plateaus causing instable/strange retrievals
  
  upenv = 1                 ; fitting on upper envelope? 1=yes, 0=no
  verbose = 1               ; if 0, no messages provided, if 1, messages provided for failure
  dateRangeTmp = dateRange  ; temporary version of the dateRange (because it gets adapted in programme)
  ;dateRange = [JULDAY(2,11,2016),JULDAY(2,15,2017)]  ; window range for finding phenology
  ;baseDate = JULDAY(12,31,2015)  ; base date for writing info (so 1 = 1 January 2016)
  
  ; define output structure
  pheno= {$
    retr:    0b, $                                      ; whether an appropriate retrieval took place (0 is no, 1 is yes)
    maxNDVI: FLOAT(noDataValue), $                      ; the maximum value for the model
    AMP1:    FLOAT(noDataValue), $                      ; the amplitude (between maximum and fitted value for preceding 1 Feb) -- so growth phase)
    AMP2:    FLOAT(noDataValue), $                      ; the amplitude (between maximum and fitted value for subsequent 1 Feb -- so decay phase)
    SOS20:   FLOAT(noDataValue), $                      ; the moment when modelled NDVI reaches 20% of amplitude1 (above 1Feb value)
    SOS50:   FLOAT(noDataValue), $                      ; the moment when modelled NDVI reaches 50% of amplitude1 (above 1Feb value)
    EOS20:   FLOAT(noDataValue), $                      ; the first moment when modelled NDVI reaches 20% of amplitude2 (above min)
    EOS50:   FLOAT(noDataValue), $                      ; the first moment when modelled NDVI reaches 50% of amplitude2 (above min)
    PS90:    FLOAT(noDataValue), $                      ; the "peak season": the first moment when modelled NDVI reaches 90% of its maximum (maxNDVI) --> note, we specifically avoid the full maximum because of long plateaus at times of high values... 
    LGS20:   FLOAT(noDataValue), $                      ; length of growing season = EOS20-SOS20  
    LGS50:   FLOAT(noDataValue), $                      ; length of growing season = EOS50-SOS50
    cumNDVI20: FLOAT(noDataValue), $                    ; integral of the fitted model between SOS20 and EOS20
    cumNDVI50: FLOAT(noDataValue), $                    ; integral of the fitted model between SOS50 and EOS50 
    PearsonCC: FLOAT(noDataValue), $                    ; Pearson correlation coefficient between fitted and original NDVI (for all observations)
    RMSD:    FLOAT(noDataValue), $                      ; RMSD (root mean square deviation) between fitted and original NDVI (for all observations)
    MSD:     FLOAT(noDataValue), $                      ; MSD (mean signed deviation) between fitted and original NDVI (for all observations)
    nImages: 0B, $                                      ; number of NDVI data points used for fitting (for all observations)
    maxGap:  FIX(noDataValue), $                        ; maximum gap between two valid observations within season
    PARAM_0:   FLTARR(nParms)+noDataValue, $            ; parameter values for the model with estimated parameters
    PARAM_1:   FLTARR(nParms)+noDataValue, $            ; parameter values for the first fitted model
    PARAM_x:   FLTARR(nParms)+noDataValue, $            ; parameter values for the iteratively fitted model
    jdInterval: FLTARR(2), $
    message: ''}
  tempPheno = pheno               ; create a temporary copy of the base structure to assign all noData in the end if needed 
  
  ; First test if NDVI values allow in principle for phenology-retrieval (5-95 percentile at least 0.2 NDVI unit, at least 5 valid points)
  ; NOTE: additional test to check if first and/or last observation are not very strange values, i.e. >0.1 below 5th percentile
  index = WHERE(FINITE(NDVIseries) AND NDVIseries ge minNDVI AND t GE dateRangeTmp[0] AND t LE dateRangeTmp[1])
  ; Strange NDVI values for 16-1-2017 image: remove for now (this is probably due to snow-cover in big part of imagery: particularly problem on low marshes)
  ;index = WHERE(FINITE(NDVIseries) AND t GE dateRangeTmp[0] AND t LE dateRangeTmp[1]) AND t NE JULDAY(1,16,2017))
  IF N_ELEMENTS(index) GE minDataPoint THEN BEGIN
    NDVIseriesGood = NDVIseries[index]  &  tGood = t[index]
    ; sort observations based on tGood
    ind = SORT(tGood)
    tGood = tGood[ind]  &  NDVIseriesGood=NDVIseriesGood[ind]
    ; check if 1) min95_5perc is okay, 2) median is below minMedian, 3) at least 4 observations exist before and after midpoint season (1 August)
    percNDVI = percentiles(NDVIseriesGood,value=[0.05,0.50,0.95])
    ; check if first or last index value is strange and remove (strange = smaller than 5th percentile minus 0.1 NDVI) --> this can result in very weird fits
    IF NDVIseriesGood[0] LE percNDVI[0]-0.1 THEN BEGIN
      NDVIseriesGood = NDVIseriesGood[1:*] & tGood = tGood[1:*]
    ENDIF
    IF NDVIseriesGood[N_ELEMENTS(tGood)-1] LE percNDVI[0]-0.1 THEN BEGIN
      NDVIseriesGood = NDVIseriesGood[0:N_ELEMENTS(tGood)-2] & tGood = tGood[0:N_ELEMENTS(tGood)-2]
    ENDIF
    percNDVI = percentiles(NDVIseriesGood,value=[0.05,0.50,0.95])            ; recalculate after removing weird values
    midTime = dateRangeTmp[0] + (dateRangeTmp[1]-dateRangeTmp[0])/2
    ;print, percNDVI[2] - percNDVI[0], tGood[1], midTime, tGood[N_ELEMENTS(tGood)-4]
    ; we become a bit more lenient as before: now we want at least 3 values before and 3 values after the midTime
    IF percNDVI[2] - percNDVI[0] GE min95_5perc AND percNDVI[1] GE minMedian AND tGood[2] LT midTime AND tGood[N_ELEMENTS(tGood)-3] GT midTime THEN BEGIN
      ; set Start parameters for model
      startParam = tanhDouble_findStart(tGood,NDVIseriesGood,dateRangeTmp)
      
      parinfo = REPLICATE({limited:[0,0], limits:[0.d,0.d], fixed:0}, nparms)
      parinfo[0].limited = [1,1]
      ;Michele, manage the case where min is negative
      parinfo[0].limits = [startParam[0]-ABS(startParam[0])/2d,0.8d]
      ;parinfo[0].limits = [startParam[0]/2d,0.8d]              ; as the lower limit for baseline we take half of the minimum NDVI value
      parinfo[1].limited = [1,1]
      parinfo[1].limits = [0.0d,startParam[1]*1.25d]           ; we constrain the amplitude1 to be maximum 25% more than the difference of observed max and min NDVI
      parinfo[3].limited = [1,1]
      parinfo[3].limits = [0.0d,0.3d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??)
      parinfo[4].limited = [1,1]
      parinfo[4].limits = [0.0d,startParam[4]*1.25d]           ; we constrain the amplitude2 to be maximum 25% more than the difference of observed max and min NDVI
      parinfo[6].limited = [1,1]
      ;parinfo[6].limits = [-0.1d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
      parinfo[6].limits = [-0.3d,-0.0000001d]           ; we constrain the slope here as well (maximum NDVI-unit change per day??): very small value as upper limit to avoid zero-slope
      
      firstFitParam = MPFITFUN(fctName, tGood, NDVIseriesGood, 1, startParam, parinfo=parinfo, STATUS = stat, /QUIET)
      ; that is a single fit, let's try now to fit iteratively...
      weights = FLTARR(N_ELEMENTS(NDVIseriesGood))*0.0+1.0         ; initialize weights
      ret = opt_model2parinfo(fctname, double(tGood), double(NDVIseriesGood), double(weights)*0.0, FLOAT(startParam), double(weights), parinfo)
      
      ; check for strange cases where the season starts with decay and ends with growth
      ; this is probably due to ploughing or something, and we attempt still in making a reasonable prediction, taking off the first part of the year
      ; to avoid working on weird fittings, request a minimal separation of a month
      IF p[5] LT p[2]-30 THEN BEGIN
        index = WHERE(tGood GE p[5] + 0.25*(p[2]-p[5]))    ; retain only points from first midpoint onwards: add 25% to avoid taking values in the decay phase
        IF N_ELEMENTS(index) GE minDataPoint THEN BEGIN
          dateRangeTmp[0]=p[5]                              ; adapt dateRangeTmp to estimate minimum not anymore for 1 February, but much later (at time of previous inflection point)
          tGood = tGood[index] & NDVIseriesGood=NDVIseriesGood[index]
          ; and repeat again the fitting
          firstFitParam = MPFITFUN(fctName, tGood, NDVIseriesGood, 1, startParam, parinfo=parinfo, /QUIET)
          weights = FLTARR(N_ELEMENTS(NDVIseriesGood))*0.0+1.0         ; initialize weights
          ret = opt_model2parinfo(fctname, double(tGood), double(NDVIseriesGood), double(weights)*0.0, FLOAT(startParam), double(weights), parinfo)
        ENDIF ELSE p[*]=noDataValue                     ; if no sufficient points remain
      ENDIF
      ; calculate maximum gap in dataset (in days)
      maxGap = max(tGood-SHIFT(tGood,1))
      
      ; assign values to output parameters
      pheno.Param_0 = startParam
      pheno.Param_1 = firstFitParam
      pheno.Param_x = p
      IF fit eq 1 THEN param = pheno.Param_1
      IF fit eq 2 THEN param = pheno.Param_x
      ; The following lines were made unnecessary following the update of opt_model2parinfo.pro in October 2018
      ; evaluate if something goes rather wrong in the iteration resulting in a much poorer correlation: in case, reset to single fit
      ;fittedNDVI1 = TANH_DOUBLE(tGood,firstFitParam)  &  fittedNDVI2 = TANH_DOUBLE(tGood,pheno.Param_x)  ; corresponding fitted NDVI values (daily)
      ;PearsonCC1 = CORRELATE(fittedNDVI1, NDVIseriesGood) & PearsonCC2 = CORRELATE(fittedNDVI2, NDVIseriesGood)
      ;IF PearsonCC2 LT PearsonCC1-0.10 THEN BEGIN
      ;  param = pheno.Param_1
      ;  pheno.Param_x = param
      ;ENDIF
      ; note here we have an additional condition to avoid weird profiles were the first inflection point is later in time than the second
      ; we tested this before for initial parameter estimate, but afterwards could still occur.
      IF param[5] LT param[2]+15 THEN BEGIN  
        stat = 0
        pheno = tempPheno
      ENDIF
      IF ((stat gt 0 and stat lt 5) OR (stat gt 5)) THEN pheno.retr = 1           ; meaning a good retrieval because of positive slope (if this point not reached, then value=0)
      IF pheno.retr eq 1 THEN BEGIN          
        dateTmp = INDGEN(dateRangeTmp[1]-dateRangeTmp[0])+dateRangeTmp[0]     ; array with all days (JD) in the modelled year (automatically adapts for shorter years in case year started with decay)
        pheno.jdInterval = [dateRangeTmp[0], dateRangeTmp[1]]
        fitNDVIseries = TANH_DOUBLE(dateTmp,param)                   ; corresponding fitted NDVI values (daily)
        pheno.maxNDVI = max(fitNDVIseries, indMax)                   ; return maximum NDVI value
        indMax = indMax[0]                                           ; probably it should not happen, but just in case multiple maxima are found
        IF indMax LE N_ELEMENTS(dateTmp)-2 THEN BEGIN
          pheno.AMP1 = pheno.maxNDVI - MIN(fitNDVIseries[0:indMax-1])   ; in principle minimum should be fitted value for 1 February, but sometimes strange retrievals (inverted) and give still result...
          pheno.AMP2 = pheno.maxNDVI - MIN(fitNDVIseries[indMax+1:*])
          ; SOS/EOS/PS are found as first moments when threshold is reached: we also ascertain that SOS/PS is along increasing NDVI, and EOS along decreasing NDVI         
          ; 20% threshold
          indSOS = WHERE(fitNDVIseries GT (sos_amplitude[0]*pheno.AMP1 + MIN(fitNDVIseries[0:indMax-1])) AND fitNDVIseries gt SHIFT(fitNDVIseries,1))
          IF indSOS[0] ne -1 THEN pheno.SOS20 = dateTmp[indSOS[0]] - baseDate
          indEOS =  WHERE(fitNDVIseries LT (sos_amplitude[0]*pheno.AMP2 + MIN(fitNDVIseries[indMax+1:*])) AND fitNDVIseries gt SHIFT(fitNDVIseries,-1) AND dateTmp GT (pheno.SOS20 + baseDate))    ; check also if EOS is after SOS!
          IF indEOS[0] ne -1 THEN pheno.EOS20 = dateTmp[indEOS[0]] - baseDate
          IF indEOS[0] ne -1 AND indSOS[0] ne -1 THEN BEGIN
            pheno.LGS20 = pheno.EOS20 - pheno.SOS20
            pheno.cumNDVI20 = TOTAL(fitNDVIseries[indSOS[0]:indEOS[0]])/30.               ; note: we divide by 30 (~month) to get lower values to be compared with common cumNDVI measures from low-res satellites
          ENDIF
          ; 50% threshold
          indSOS = WHERE(fitNDVIseries GT (sos_amplitude[1]*pheno.AMP1 + MIN(fitNDVIseries[0:indMax-1])) AND fitNDVIseries gt SHIFT(fitNDVIseries,1))
          IF indSOS[0] ne -1 THEN pheno.SOS50 = dateTmp[indSOS[0]] - baseDate
          indEOS =  WHERE(fitNDVIseries LT (sos_amplitude[1]*pheno.AMP2 + MIN(fitNDVIseries[indMax+1:*])) AND fitNDVIseries gt SHIFT(fitNDVIseries,-1) AND dateTmp GT (pheno.SOS50 + baseDate))    ; check also if EOS is after SOS!
          IF indEOS[0] ne -1 THEN pheno.EOS50 = dateTmp[indEOS[0]] - baseDate
          IF indEOS[0] ne -1 AND indSOS[0] ne -1 THEN BEGIN
            pheno.LGS50 = pheno.EOS50 - pheno.SOS50
            pheno.cumNDVI50 = TOTAL(fitNDVIseries[indSOS[0]:indEOS[0]])/30.               ; note: we divide by 30 (~month) to get lower values to be compared with common cumNDVI measures from low-res satellites
          ENDIF
          ; peak season
          indPS = WHERE(fitNDVIseries GT (max_amplitude*pheno.AMP1 + MIN(fitNDVIseries[0:indMax-1])) AND fitNDVIseries gt SHIFT(fitNDVIseries,1))
          IF indPS[0] ne -1 THEN pheno.PS90 = dateTmp[indPS[0]] - baseDate
          ; Stats
          fittedNDVI = TANH_DOUBLE(tGood,param)                   ; corresponding fitted NDVI values (daily)
          pheno.PearsonCC = CORRELATE(fittedNDVI, NDVIseriesGood)
          pheno.RMSD = SQRT(TOTAL((fittedNDVI-NDVIseriesGood)^2)/N_ELEMENTS(fittedNDVI))
          pheno.MSD = TOTAL(fittedNDVI-NDVIseriesGood)/N_ELEMENTS(fittedNDVI)
          pheno.nImages = N_ELEMENTS(fittedNDVI)
          
          ; FOR VALUES OF EOS INTO JANUARY OF THE NEXT YEAR: QUICK SOLUTION
          IF pheno.EOS20 gt 366 THEN pheno.EOS20 = pheno.EOS20-366             ; NOTE: this is for LEAP years, and to see if maintain like this...
          IF pheno.EOS50 gt 366 THEN pheno.EOS50 = pheno.EOS50-366
        ENDIF
   
        ; apparently some results tend to be infinite or have negative SOS (check reason). To avoid that, set those back to noDataValue
        IF (~FINITE(pheno.SOS20) OR ~FINITE(pheno.PS90) OR ~FINITE(pheno.EOS20) OR ~FINITE(pheno.LGS20) OR ~FINITE(pheno.AMP1) OR ~FINITE(pheno.AMP2) OR ~FINITE(pheno.maxNDVI) OR pheno.SOS20 lt 0 $
            OR ~FINITE(pheno.SOS50) OR ~FINITE(pheno.EOS50) OR ~FINITE(pheno.LGS50) OR pheno.SOS50 lt 0) OR $
            ABS(pheno.PS90-pheno.SOS20) LT 5 THEN $
          pheno = tempPheno
      ENDIF ; pheno retrieved?
      pheno.maxGap = maxGap              ; retain this value even for pixels where no retrievals are obtained     
    ENDIF ELSE pheno.message = 'Variability too low or insufficient data points on one side'; 95th-5th percentile greater than min95_5perc?
  ENDIF ELSE pheno.message = 'Insufficients data points'; sufficient data points?
  
  RETURN, pheno

END ; 



;********************************************************************************************************