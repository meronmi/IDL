FUNCTION accWithGlobradFromParam, pVec, tVec, threshSOStype, threshSOS, threshEOStype, threshEOS, year, gry, grx

;Builds all the require pheno indicators form the 7+2 DHTM parameters:
;pdhtf_base
;pdhtf_amp1
;pdhtf_sft1
;pdhtf_slo1
;pdhtf_amp2
;pdhtf_sft2
;pdhtf_slo2
;gsinistart
;gsiniend
;which are all stored as multi-band (multi-year) bil images

;INPUT:
;- pVec: 7 parameters of the DHT mode
;- tVec: time axis limits (gsinistart, gsiniend) where the DHT model was fitted
;  time is JD! not dekads
;- threshSOS(EOS)type: defines if the threshold used to compute SOS (EOS) is in % of amplitude (decay)
;  (threshSOS(EOS)type='%') or as DOY (threshSOS(EOS)type='DOY')
;- threshSOS(EOS): the threshold, either in % or in DOY according to threshSOS(EOS)typ



;Computation

timeHR = tVec[0] + INDGEN(tVec[1]-tVec[0]+1)
;make HR vector from Globrad
;first divive gry by 10 to get daily value from 10-day cumulative
indNaN = WHERE(FINITE(gry) EQ 0, countNaN)
IF countNaN GT 0 THEN STOP
gry=gry/10.0
grHR = INTERPOL(gry, grx, timeHR, /NAN)



;grHR = timeHR*0.0
;FOR i=0, N_ELEMENTS(gr)-2 DO BEGIN
;  grHR[i*resampling:i*resampling+resampling-1]=gr[i]/FLOAT(resampling) ;resample radiation
;END
;grHR[(N_ELEMENTS(gr)-2)*resampling+resampling]=gr[N_ELEMENTS(gr)-1]/FLOAT(resampling)
;compute the fitted DHT curve
pdhtf, timeHR, pVec, fittedHR
;extract the relevant indicators according the thresholds
;value and position of the maximum value on the fitted curve
fitmax = MAX(fittedHR, fitmax_idx)

;from phenot_p3_pdhtf_mm
xHR = timeHR
fitmaxHR = MAX(fittedHR, idMax_xHR)
;here we have the problem that the function may be inistially descending, reaching a min and then making the bell
;so the minimum must be computed between the first element and the max to ensure that it's a real min
fitmin1HR = MIN(fittedHR[0:idMax_xHR], idInitMin_xHR)
;fitmin1HR = fittedHR [0]
;same for the end
fitmin2HR = MIN(fittedHR[idMax_xHR : N_ELEMENTS(fittedHR)-1], idEndMin_xHR)
idEndMin_xHR = idMax_xHR + idEndMin_xHR
;fitmin1 = fittedHR [0]
;fitmin2 = fittedHR [N_ELEMENTS(fittedHR)-1]

;lower integration limits
CASE threshSOStype OF
  '%':BEGIN
      ;from phenot_p3_pdhtf_mm
      fract_thresh = threshSOS/100.0
      relstart = WHERE(fittedHR [idInitMin_xHR:idMax_xHR] GE $
        fitmin1HR + (fitmaxHR - fitmin1HR) * fract_thresh, cnt)
      startgsHR_ind =  idInitMin_xHR + relstart[0]
      startgsHR_value = xHR[startgsHR_ind]
;      relstart = WHERE(fittedHR [0:fitmax_idx] GE fitmin1 + (fitmax - fitmin1) * threshSOS/100.0, cnt)
;      startgs = relstart [0]
    END
  'DOY':BEGIN
      threshSOSJD = DOY_YEAR2JD(threshSOS, year)
      startgsHR_ind = WHERE(timeHR GE threshSOSJD, cnt)
      IF (cnt EQ 0) THEN STOP ELSE startgsHR_ind = startgsHR_ind [0]
    END
  ELSE: STOP
ENDCASE

CASE threshEOStype OF
  '%':BEGIN
      ;from phenot_p3_pdhtf_mm
      fract_thresh = threshEOS/100.0
      relstop = WHERE(fittedHR[idMax_xHR : idEndMin_xHR] GE $
        fitmin2HR + (fitmaxHR - fitmin2HR) * fract_thresh, cnt)
      stopgsHR_ind = idMax_xHR + relstop [N_ELEMENTS(relstop) - 1]
      stopgsHR_value = xHR[stopgsHR_ind]
;      relstop = WHERE(fittedHR [fitmax_idx:(N_ELEMENTS(fittedHR)-1)] GE fitmin2 + (fitmax - fitmin2) * threshEOS/100.0, cnt)
;      stopgs = fitmax_idx + relstop [(N_ELEMENTS(relstop) - 1)]
   END
  'DOY':BEGIN
        threshEOSJD = DOY_YEAR2JD(threshEOS, year)
        stopgsHR_ind = WHERE(timeHR GE threshEOSJD, cnt)
        ;if cnt is 0 it means that the fixed DOY is beyond the end of season, so take it all
        IF (cnt EQ 0) THEN stopgsHR_ind = N_ELEMENTS(timeHR)-1 ELSE stopgsHR_ind = stopgsHR_ind [0]
    END
  ELSE: STOP
ENDCASE

IF (startgsHR_ind LT stopgsHR_ind) THEN BEGIN
  acc = TOTAL(fittedHR[startgsHR_ind: stopgsHR_ind], /DOUBLE)
  accb = TOTAL((fittedHR[startgsHR_ind: stopgsHR_ind]-fitmin1HR), /DOUBLE)
  accgr = TOTAL(fittedHR[startgsHR_ind: stopgsHR_ind]*grHR[startgsHR_ind: stopgsHR_ind], /DOUBLE)
  accbgr = TOTAL((fittedHR[startgsHR_ind: stopgsHR_ind]-fitmin1HR)*grHR[startgsHR_ind: stopgsHR_ind], /DOUBLE)
ENDIF ELSE BEGIN
  ;this is the case where I set only eos as doy and in some area it's before the start
  acc = !VALUES.F_NAN
  accb = !VALUES.F_NAN
  accgr = !VALUES.F_NAN
  accbgr = !VALUES.F_NAN
ENDELSE
RETURN, [acc, accb, accgr, accbgr]
END