; NAME:
;   SAGO_INTERPOL3
;
; PURPOSE:
;   Savitsky Golay filtering of NDVI time series
;
; CALLING SEQUENCE:
;   SAGO_INTERPOL, Data, Cloud, smNDVI=smNDVI, iNDVI=iNDVI, IMAX=IMAX
;
; INPUTS:
;     Data:   Array of length n with input vegetation index values
;     Cloud:  Binary array of length n cloud mask values (1 = cloudy; 0 = cloudfree)
;
; KEYWORD PARAMETERS:
;     smNDVI:   Output Savitsky Golay smoothed NDVI profile
;      iNDVI:   Output interpolated NDVI profile, with clouded points replaced by
;               Savitsky Golay filter estimate
;       iMAX:   Maximum nr of iterations in processing loop
;
; EXAMPLE:
;   Data=[0.1,0.12,0.2,0.3,0.5, 0.1, 0.6, 0.55, 0.3, 0.2, 0.15, 0.1]
;   Cloud=[0, 1,   0,  0,  0,   0,   0,   0,    0,   1,   0,     0]
;
;  SAGO_INTERPOL, Data, Cloud, smNDVI=smNDVI, iNDVI=iNDVI, iMAX=10
;
;  window, 10
;  plot, Data, xtitle='Time [dekad]', ytitle='NDVI [-]'
;  oplot, smndvi, linestyle=1
;
; MODIFICATION HISTORY:
;   Written by:  Jin Chen, November 2003
;   Modified by: Allard de Wit, December 2004
;     *Replaced many FORTRANish pieces of code with native IDL (i.e. interpol)
;     *Removed call to SAVGOL() from iterative WHILE loop
;   Modified by: Anton Vrieling, February 2008
;     *Changed programme for integer output
;     *Introduced maximum value, to avoid values being higher than maxNDVI
;   Modified by: Anton Vrieling, August 2010
;     *Changed programme for SPOT VGT for binary (0-255) output
;   Modified by: Michele Meroni, April 2013
;     *Changed programme for for integer output
;   Modified by: Michele Meroni, Sep 2014
;     *Changed programme for for float output and checks for NaN in input
;
; CODE IS BASED ON:
;   Jin Chen, Per Jonsson, Masayuki Tamura, Zhihui Gu, Bunkei Matsushita, Lars Eklundh. 2004.
;   A simple method for reconstructing a high-quality NDVI time-series data set based on
;   the Savitzky-Golay filter. Remote Sensing of Environment 91: 332-344
;

PRO SAGO_INTERPOL3, data, cloud, smNDVI=smNDVI, iNDVI=iNDVI, iMax=iMax
  ; added line to avoid values going over maxNDVI or under minNDVI
  maxNDVI = 1.0
  minNDVI = 0
  
  ;find size of arrays and reform array variables
  data  = REFORM(data)
  cloud = REFORM(cloud)
  s = N_ELEMENTS(data)
  i_data = FLOAT(data)
  ;check for Nan
  indNaN = WHERE(FINITE(i_data) NE 1, countNan) 
  IF (countNan GT 0) THEN BEGIN
    i_data[indNaN] = 0.0
    cloud[indNaN] = 1
  ENDIF
  IF NOT KEYWORD_SET(iMax) THEN imax = 10
  
  ;Perform first reconstruction of VI by linear interpolation
  ;Interpolating the cloudy data according to cloud flag
  xrange = INDGEN(s)
  index0 = WHERE((cloud EQ 0) OR  (xrange EQ 0) OR  (xrange EQ s-1))
  index1 = WHERE((cloud EQ 1) AND (xrange NE 0) AND (xrange NE s-1))
  
  IF index1[0] NE -1 THEN BEGIN
    r = INTERPOL(i_data[index0],index0,index1)
    i_data[index1] = r
  ENDIF
  
  ; The first Savitzky-Golay filtering for long term change trend fitting
  savgolFilter = SAVGOL(6,6,0,4)
  sgfit = CONVOL(i_data,savgolFilter,/EDGE_TRUNCATE)
  
  ; weight calculation
  dif  = ABS(i_data - sgfit)
  mm   = MAX(dif)
  resu = i_data - sgfit
  
  weights = FLTARR(s) + 1.
  index = WHERE(resu LE 0)
  IF index[0] NE -1 THEN weights[index] = 1 - (dif[index]/mm)
  gdis = TOTAL(ABS(dif * weights))
  
  ra4 = FLTARR(s)
  ormax = gdis
  it = 1
  savgolFilter = SAVGOL(4, 4, 0, 3)
  WHILE (gdis LE ormax) AND (it LT imax) DO BEGIN
    ;Substitute underestimated VI values by SG values and loop until
    ;reaching the point where the sum of weigthed differences starts
    ;to increase again.
    ra4 = i_data
    index = WHERE(i_data LT sgfit)
    IF index[0] NE -1 THEN ra4[index] = sgfit[index]
    
    ; The Savitzky-Golay fitting
    sgfit = CONVOL(ra4, savgolFilter, /EDGE_TRUNCATE)
    resu  = i_data - sgfit
    
    ;Calculate the weighted difference
    ormax = gdis
    gdis = TOTAL(ABS(resu*weights))
    
    it++
  ENDWHILE
  
  ;Assign return values
  index1 = WHERE(sgfit gt maxNDVI)
  index2 = WHERE(sgfit lt minNDVI)
  smNDVI=sgfit
  IF index1[0] NE -1 THEN smNDVI[index1]=maxNDVI
  IF index2[0] NE -1 THEN smNDVI[index2]=minNDVI
  
  ; the following 3 lines only needed if also iNDVI output desired
  ;iNDVI=data
  ;index = WHERE(cloud EQ 1)
  ;IF index[0] NE -1 THEN iNDVI[index]=sgfit[index]
END
