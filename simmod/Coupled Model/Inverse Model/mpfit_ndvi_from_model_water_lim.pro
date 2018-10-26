FUNCTION MPFIT_NDVI_from_model_water_lim, obsJD, p, PROSAILVAR=prosailVar, RSOIL=rsoil,  $
  TJD=tJD, GLOBRADVAR=globrad, GLOBRAD2PAR=globrad2par, TAIR=tair, WLIM=wlim, $
  SATOBSJD=satObsJD, SATOBSRAA=satObsRAA, SATOBSSZA=satObsSZA, SATOBSVZA=satObsVZA
  ;exluded from functargs: ROBS=robs,

  proSailVar = {Cab    : prosailVar[0], Car    : prosailVar[1],$
    Cbrown : prosailVar[2], Cw     : prosailVar[3],$
    Cm     : prosailVar[4], Ns     : prosailVar[5],$
    lidf_a : prosailVar[6], lidf_b : prosailVar[7],$
    lai    : prosailVar[8], hspot  : prosailVar[9],$
    tts    : prosailVar[10], tto    : prosailVar[11],$
    psi    : prosailVar[12], rsoil  : rsoil}


  JD0a=FLOOR(p[0])
  IF ((p[0] MOD 1 NE 0.0)) THEN JD0b=CEIL(p[0])
  laiDOY0=p[1] & SLA=p[2] & eps_max=p[3] & gamma=p[4]
  c=p[5]  & d=p[6]       & a=p[7]   & b=p[8]       & tb=p[9]
  cap=p[10] & half_life=p[11] & bewm_opt=p[12]
  IF (JD0a GT  MAX(tJD) OR JD0a LT  MIN(tJD))THEN BEGIN
    ;This should never happen as parametrs are bounded, stop if it is the case
    STOP
  ENDIF
  IF (laiDOY0 LT 0.0) THEN STOP
  sima = simForwMod_water_lim2(JD0a, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)
  ;Print,'x'
  ;return sim has the dimension of tJD.
  ;Only those corresponding to must be compared to MODIS observation to build the cost function

  ;find the index on obs and sim to be compared
  ;match, tJD, obsJD, indsim
  ;merging MOD and MYD VIs it may happen that the same day is selcted by MOD and MYD,
  indsim = bSub_with_value_matching_a_values(tJD, obsJD)
  ;so the obsJD is repeated twice
  Red_sim = REFORM(sima.ModRef[0,indsim])
  Nir_sim = REFORM(sima.ModRef[1,indsim])
  NDVI_sima = (Nir_sim-Red_sim)/(Nir_sim+Red_sim)
;  ;test
;  ;see where it grows
;  indRestPeriod = WHERE(sima.lai EQ 0.0)
;  indRestPeriod = SetIntersection(indsim, indRestPeriod)
;  NDVI_sima[indRestPeriod]=!VALUES.F_NAN
;  ;end test
 
  
  IF ((p[0] MOD 1 NE 0.0)) THEN BEGIN
    simb = simForwMod_water_lim2(JD0b, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)
    Red_sim = REFORM(simb.ModRef[0,indsim])
    Nir_sim = REFORM(simb.ModRef[1,indsim])
    NDVI_simb = (Nir_sim-Red_sim)/(Nir_sim+Red_sim)
;    ;test
;    NDVI_simb[indRestPeriod]=!VALUES.F_NAN
;    ;end test
    ;lineraly interpolate
    IF (N_ELEMENTS(NDVI_sima) NE N_ELEMENTS(NDVI_simb)) THEN STOP
    RETURN, NDVI_sima * (JD0b-p[0]) + NDVI_simb * (p[0]-JD0a)
    ;RETURN, NDVI_sima + ((NDVI_simb-NDVI_sima)/ (JD0b-JD0a)) * (p[0]-JD0a)
    ; NDVI_sima * (JD0b-p[0]) + NDVI_simb * (p[0]-JD0a) 
  ENDIF ELSE BEGIN
    RETURN, NDVI_sima
  ENDELSE
;Test con Stefano
; laiDOY0=p[1] & SLA=p[2] & eps_max=p[3] & gamma=p[4]
;  c=p[5]  & d=p[6]       & a=p[7]   & b=p[8]       & tb=p[9]
;  cap=p[10] & half_life=p[11] & bewm_opt=p[12]
;  NDVI_sima = NDVI_sima *  (SLA-0.01) / (0.1) *  (c-250) / (2500-250) + RANDOMU(seed, 1)/20.0
END