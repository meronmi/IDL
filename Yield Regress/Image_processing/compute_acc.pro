FUNCTION compute_acc_grow_only, pdhtf_pars, t0, t1, resampling, ftgg, ftdd, b
dt=1.0/DOUBLE(resampling) 
dekads = t0 + INDGEN(t1-t0+1)
dekadsHR=(FINDGEN((N_ELEMENTS(dekads)-1)*resampling+1)+dekads[0]*resampling)/DOUBLE(resampling)
pdhtf, dekadsHR, pdhtf_pars, fittedHR
;extract the relevant indicators according ftg and ftd
cftg=ftgg/100.0
cftd=ftdd/100.0
fitmax = MAX(fittedHR, fitmax_idx)
fitmin1 = fittedHR [0]
fitmin2 = fittedHR [N_ELEMENTS(fittedHR)-1]
relstart = WHERE(fittedHR [0:fitmax_idx] GE $
                 fitmin1 + (fitmax - fitmin1) * cftg, cnt)
startgs = relstart [0]

relstop = WHERE(fittedHR [0:fitmax_idx] GE $
                 fitmin1 + (fitmax - fitmin1) * cftd, cnt)
stopgs = relstop [0]

IF (B EQ 0) THEN acc=TOTAL(fittedHR[startgs:stopgs]*dt, /DOUBLE)
IF (B EQ 1) THEN acc=TOTAL((fittedHR[startgs:stopgs]-fitmin1)*dt, /DOUBLE)

RETURN, acc
END

FUNCTION compute_acc_dec_only, pdhtf_pars, t0, t1, resampling, ftgg, ftdd, b
dt=1.0/DOUBLE(resampling) 
dekads = t0 + INDGEN(t1-t0+1)
dekadsHR=(FINDGEN((N_ELEMENTS(dekads)-1)*resampling+1)+dekads[0]*resampling)/DOUBLE(resampling)
pdhtf, dekadsHR, pdhtf_pars, fittedHR
;extract the relevant indicators according ftg and ftd
cftg=ftgg/100.0
cftd=ftdd/100.0
fitmax = MAX(fittedHR, fitmax_idx)
fitmin1 = fittedHR [0]
fitmin2 = fittedHR [N_ELEMENTS(fittedHR)-1]

relstart = WHERE(fittedHR [fitmax_idx:(N_ELEMENTS(fittedHR)-1)] GE $
                 fitmin2 + (fitmax - fitmin2) * cftg, cnt)
startgs = fitmax_idx + relstart [(N_ELEMENTS(relstart) - 1)]

relstop = WHERE(fittedHR [fitmax_idx:(N_ELEMENTS(fittedHR)-1)] GE $
                fitmin2 + (fitmax - fitmin2) * cftd, cnt)
stopgs = fitmax_idx + relstop [(N_ELEMENTS(relstop) - 1)]

IF (B EQ 0) THEN acc=TOTAL(fittedHR[stopgs:startgs]*dt, /DOUBLE)
IF (B EQ 1) THEN acc=TOTAL((fittedHR[stopgs:startgs]-fitmin1)*dt, /DOUBLE)

RETURN, acc
END

FUNCTION compute_acc, pdhtf_pars, t0, t1, resampling, ftgg, ftdd, b
dt=1.0/DOUBLE(resampling) 
dekads = t0 + INDGEN(t1-t0+1)
dekadsHR=(FINDGEN((N_ELEMENTS(dekads)-1)*resampling+1)+dekads[0]*resampling)/DOUBLE(resampling)
pdhtf, dekadsHR, pdhtf_pars, fittedHR
;extract the relevant indicators according ftg and ftd
cftg=ftgg/100.0
cftd=ftdd/100.0
fitmax = MAX(fittedHR, fitmax_idx)
fitmin1 = fittedHR [0]
fitmin2 = fittedHR [N_ELEMENTS(fittedHR)-1]
relstart = WHERE(fittedHR [0:fitmax_idx] GE $
                 fitmin1 + (fitmax - fitmin1) * cftg, cnt)
startgs = relstart [0]

relstop = WHERE(fittedHR [fitmax_idx:(N_ELEMENTS(fittedHR)-1)] GE $
                fitmin2 + (fitmax - fitmin2) * cftd, cnt)
stopgs = fitmax_idx + relstop [(N_ELEMENTS(relstop) - 1)]

IF (B EQ 0) THEN acc=TOTAL(fittedHR[startgs:stopgs]*dt, /DOUBLE)
IF (B EQ 1) THEN acc=TOTAL((fittedHR[startgs:stopgs]-fitmin1)*dt, /DOUBLE)

RETURN, acc
END