; tanh_findStart
; based on the tanh_Single function, find relevant start parameters to start fitting
;   P[0] is the overall minimum value
;   P[1] is the amplitude of the growth phase
;   P[2] shifts the inflexion point of the growth phase along the x-axis (approximate by mid-point in time between local maximum and preceding minimum)
;   P[3] is the slope of the growth phase
;   P[4] is the amplitude of the decay phase
;   P[5] shifts the inflexion point of the decay phase along the x-axis (approximate by mid-point in time between local minimum and preceding maximum)
;   P[6] is the slope of the decay phase
;
; INPUTS:
;   - timeSeries of NDVI values (double)
;   - corresponding Julian Dates
;   - midPoint of period of interest  (for now midway between 1 March and 31 August 2015)
;           we expect only positive NDVI values
;     
; USAGE:
;   result = tanh_Findstart(t,NDVIseries,midTime)

FUNCTION tanhDouble_FindStart, t, NDVIseries, dateRange
  P = DBLARR(7)-99d           ; parameter array; initialize at -99d
  midTime = dateRange[0] + (dateRange[1]-dateRange[0])/2
  indT1 = WHERE(t LE midTime)
  indT2 = WHERE(t GT midTime)
  IF indT1[0] NE -1 AND indT2[0] NE -1 THEN BEGIN
    ; split time series into two halfs
    NDVIseries1st = NDVIseries[indT1]   &     NDVIseries2nd = NDVIseries[indT2]   & t1st = t[indT1]  & t2nd = t[indT2]
    minNDVI1st = min(NDVIseries1st, indMin1)
    maxNDVI = max(NDVIseries,indMax)
    minNDVI2nd = min(NDVIseries2nd, indMin2)
   ; test1 = linfit(t1st,NDVIseries1st)           ; additional test to see if overall the slope of a linear fit is positive (so we have increasing NDVI values overall)
   ; test2 = linfit(t2nd,NDVIseries2nd)           ; additional test to see if overall the slope of a linear fit is negative (so we have decreasing NDVI values overall)
   ; IF test1[1] GT 0 AND test2[1] LE 0 THEN BEGIN           ;  
      P[0] = minNDVI1st                                     ; minimum value in first half series, change to: min(NDVIseries) to overall minimum?
      P[1] = maxNDVI - minNDVI1st                           ; difference between observed maximum and minimum value in first half series
      P[2] = dateRange[0]+(t[indMax]-dateRange[0])/2        ; midpoint between start of optimization window and timing observed maximum  
      P[3] = 0.02                                           ; for now fixed value: not sure how to get this better
      P[4] = maxNDVI - minNDVI2nd                           ; difference between observed maximum and minimum value in second half series
      P[5] = t[indMax]+(dateRange[1]-t[indMax])/2           ; midpoint between timing observed maximum and end of optimization window 
      P[6] = -0.02                                          ; for now fixed value: not sure how to get this better
   ; ENDIF
  ENDIF
  
  RETURN, P 
END