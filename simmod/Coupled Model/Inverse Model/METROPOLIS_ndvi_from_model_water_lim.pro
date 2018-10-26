FUNCTION METROPOLIS_NDVI_from_model_water_lim, obsJD, p

COMMON METRO, fcnargs2

;fcnargs = {PROSAILVAR:[proSailVar.Cab,proSailVar.Car,proSailVar.Cbrown,proSailVar.Cw,proSailVar.Cm, $
;  proSailVar.Ns,proSailVar.lidf_a,proSailVar.lidf_b,proSailVar.lai,proSailVar.hspot, $
;  proSailVar.tts,proSailVar.tto,proSailVar.psi],$
;  RSOIL: proSailVar.rsoil, $
;  TJD:tJD, GLOBRADVAR:globrad, GLOBRAD2PAR:globrad2par, TAIR:tair, WLIM: wlim, $
;  SATOBSJD:satObsJD, SATOBSRAA:satObsRAA, SATOBSSZA:satObsSZA, SATOBSVZA:satObsVZA}

  
  
  proSailVar = {Cab    : fcnargs2.prosailVar[0], Car    : fcnargs2.prosailVar[1],$
    Cbrown : fcnargs2.prosailVar[2], Cw     : fcnargs2.prosailVar[3],$
    Cm     : fcnargs2.prosailVar[4], Ns     : fcnargs2.prosailVar[5],$
    lidf_a : fcnargs2.prosailVar[6], lidf_b : fcnargs2.prosailVar[7],$
    lai    : fcnargs2.prosailVar[8], hspot  : fcnargs2.prosailVar[9],$
    tts    : fcnargs2.prosailVar[10], tto    : fcnargs2.prosailVar[11],$
    psi    : fcnargs2.prosailVar[12], rsoil  : fcnargs2.rsoil}
  tJD = fcnargs2.tJD
  globrad  = fcnargs2.GLOBRADVAR
  globrad2par = fcnargs2.GLOBRAD2PAR
  tair  = fcnargs2.TAIR
  wlim = fcnargs2.WLIM
  satObsJD = fcnargs2.SATOBSJD
  satObsRAA = fcnargs2.SATOBSRAA
  satObsSZA = fcnargs2.SATOBSSZA
  satObsVZA = fcnargs2.SATOBSVZA


  JD0a=FLOOR(p[0])
  IF ((p[0] MOD 1 NE 0.0)) THEN JD0b=CEIL(p[0])
  laiDOY0=p[1] & SLA=p[2] & eps_max=p[3] & gamma=p[4]
  c=p[5]  & d=p[6]       & a=p[7]   & b=p[8]       & tb=p[9]
  cap=p[10] & half_life=p[11] & bewm_opt=p[12]
  IF (JD0a GT  MAX(tJD) OR JD0a LT  MIN(tJD))THEN BEGIN
    ;This should neer happen as parametrs are bounded, return a huge value
    RETURN, tJD*0.0+!VALUES.F_INFINITY
  ENDIF
  sima = simForwMod_water_lim2(JD0a, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)

  ;return sim has the dimension of tJD.
  ;Only those corresponding to must be compared to MODIS observation to build the cost function

  ;find the index on obs and sim to be compared
  ; indobs = WHERE(FINITE(satObsJD), count)
  ; obsJD = satObsJD[indobs]
  indsim = !NULL
  FOR i = 0, N_ELEMENTS(obsJD)-1 DO indsim = [indsim, WHERE(tJD EQ obsJD[i])]
  ;obsJD = satObsJD[indobsa]
  Red_sim = REFORM(sima.ModRef[0,indsim])
  Nir_sim = REFORM(sima.ModRef[1,indsim])
  NDVI_sima = (Nir_sim-Red_sim)/(Nir_sim+Red_sim)
  IF ((p[0] MOD 1 NE 0.0)) THEN BEGIN
    simb = simForwMod_water_lim2(JD0b, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)
    Red_sim = REFORM(simb.ModRef[0,indsim])
    Nir_sim = REFORM(simb.ModRef[1,indsim])
    NDVI_simb = (Nir_sim-Red_sim)/(Nir_sim+Red_sim)
    ;lineraly interpolate
    IF (N_ELEMENTS(NDVI_sima) NE N_ELEMENTS(NDVI_simb)) THEN STOP
    RETURN, NDVI_sima + ((NDVI_simb-NDVI_sima)/ (JD0b-JD0a)) * (p[0]-JD0a)
  ENDIF ELSE BEGIN
    RETURN, NDVI_sima
  ENDELSE

END