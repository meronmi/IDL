; PROCEDURE COMMENTS +
; NAME: simForwMod_water_lim
; AUTHOR: Michele Meroni
; CONTACT INFO: michele.meroni@gmail.com

; DESCRIPTION:
; Simple vegetation growth model (based on the GRAMI model) coupled with radiative transfer model PROSAIL5b
; to compute FAPAR from LAI (used internally) and simulate MODIS 7 bands reflectances.

; CALLING SEQUENCE:
;       output = simForwMod(JD0, laiDOY0, SLA, eps_max, wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)

; INPUTS:
; PARAMETERS are denoted with:
; (k) = fixed in inversion
; (kf) = fixed or free in the time series run (and therefore constant at the seasonal level)
; (f) = free in the seasonal run
; INPUTS are denoted with: (i)

; JD0:          (f) {scalar} JUlian Day of trhe DOY at which the LAI is laiDOY0, it is used to approximate emergence of the crop
; laiDOY0:      (kf) {scalar} LAI value at DOY0, emergence
; SLA:          (kf) {scalar} Specific Leaf Area(m2 g-1)
; eps_max:      (kf) {scalar} maximum Light Use Efficiency (gC m-2 d-1 MJ-1)
; wlim:         STRUCTURE for water limitation computation
;               (k) wlim.onoff: 0/1 for deactivating/activating water limitation
;               (i) wlim.tJD: time axis of precipitation variable (it must start before the other variables, tJD[0]-365) (Julian days)
;               (i) wlim.tair: air temperature (°C)
;               (i) wlim.rain: precipitation (mm)
; cap:          (k)  {scalar} cap value for precipitation (rain>cap is set to rain=cap) (mm)
; half_life:    (kf) {scalar}half_life control the stepnees of the decay of weights, it's the time (expressed in GDD units) in which
;                    the weights fall to one half (degree days)
; bewm_opt:     (kf) {scalar}optimal value for the Backward Exponentially Weighted Mean (mm)
; gamma:        (k) {scalar} respiration coefficient (set to 0 if eps_max is optimized), re=gamma*GPP
;               ** Note: gamma has the only effect of making a distinction between GPP and NPP but it is useless
;               ** if eddy data are not used in the inversion
; c:            parameter controlling the leaf lifespan (J = c + d * GDD, as in Maas, 1993)
;               (f) {scalar} c (DegDays)cts as a base life span
; d:            parameter controlling the leaf lifespan (J = c + d * GDD, as in Maas, 1993)
;               (f) {scalar} d (-) controls the lifespan (J) a function of the degree days at emergence,
;                   e.g., if d if GT 0 it makes the leaves emerging later to live longer
;Partitioning, GRAMI function (LeafPartitioning_on_GDD)
; a:            paremeter controlling partitioning (sim.P1 = Max(1-a*EXP(b*GDD), 0) as in Maas, 1993)
;               (f) {scalar} a (-) controls the magnitude of partitioning
; b:            paremeter controlling partitioning to new leaves (sim.P1 = Max(1-a*EXP(b*GDD), 0) as in Maas, 1993)
;Partitioning, Michele's function (LeafPartitioning_on_GDD2)
; a:            paremeter controlling the length of the period of leaf partitioning (sim.P1 = MAX([1.0 - (GDD/(GDD+EXP(b)*(a-GDD))),0.0]))
;               (f) {scalar} a (-) controls the length
; b:            paremeter controlling the shape of the decay of partitioning, pecial cases:
;               b<0: convex function(U shape)
;               b=0: linear decreasong function
;               b>0: concave function (standard behaviour, in this case it resample the exponential of GRAMI)
;               (f) {scalar} b (-) controls how quickly partitioning is reduced with GDD
; tJD:          (i) {array} time axis of meteo variables (in Julian Day)
; globrad:      (i) {array} Global radiation (GJ ha-1 d-1)
; globrad2par:  (k) {scalar} conversion factor from Global radiation to PAR *
; tair:         (i) {array} Air temperature (°C)
; tb:           (k) {scalar} base temperature to comput degree days (dDD = MAX(Tair-Tb, 0)
; proSailVar:   PROSAIL PARAMETERS {structure}
;               (k) proSailVar.Cab    : chlorophyll content (µg.cm-2) (0-100 ug cm-2)
;               (k) proSailVar.Car    : carotenoid content (µg.cm-2)  (0-25 ug cm-2)
;               (k) proSailVar.Cbrown : brown pigment content (0-1 arbitrary units)
;               (k) proSailVar.Cw     : EWT (cm) - Equivalent water thickness (0-0.05 cm)
;               (k) proSailVar.Cm     : LMA (g.cm-2) - leaf mass per unit leaf area (0-0.02 g cm-2)
;               (k) proSailVar.Ns     : N structure coefficient (1-3 dimensionless)
;               (k) proSailVar.lidf_a : LIDFa LIDF parameter a, which controls the average leaf slope
;               (k) proSailVar.lidf_b : LIDFb LIDF parameter b, which controls the distribution's bimodality
;               (f) proSailVar.lai    : leaf area index
;               (k) proSailVar.hspot  : hot spot parameter
;               (i) proSailVar.tts    : solar zenith angle (°)
;               (i) proSailVar.tto    : observer zenith angle (°)
;               (i) proSailVar.psi    : azimuth (°)
;               (i) proSailVar.rsoil  : {array} spectral soil reflectance (0-1), extracted from the 1st NDVI percentile oh hystorical distribution
; satObsJD:     (i) {array} Julian Day for which a satellite observation is available
; satObsRAA:    (i) {array} Relative Azimuth Angle (between sun and obs) (-180, +180°)
; satObsSZA:    (i) {array} Solar Zenith Angle (0, 90°)
; satObsVZA:    (i) {array} View Zenith Angle (0, 90°)
;
; OUTPUTS:
; Structure res
; res.gpp
; res.lai
; res.ModRef (array 7 columns, n raws)
;
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
;* on global radiation:
;   ecmwf glob rad is KJ m-2
;   eddy glob rad  is MJ m-2 (even if in the xls file is W m-2)
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
; 14 July 2014: Cleaning
; 28 Aug 2014: Sumlation is started at the the first day of the time array (tJD[0]) instead
; of JD0. Therefore, LAI = 0 until JD0 and the spectral reflecantances are those of the background.
; (previous version save as simforwmodV1.pro)
; 29 Sep 2014: water limitation (eps_s) added
; CODING NOTES:
;


FUNCTION simForwMod_water_lim2, JD0, laiDOY0, SLA, eps_max, wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, tJD, globrad, globrad2par, tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA
  @cb_various.comm
  ;COMMON wlim, wLimParFixed, wlim_type, kc, bewm, et0, bewm_et0, wpar, rsrLR, cwl, wl, P1_sen_model

  ;!EXCEPT=2
  ; Similar to GRAMI.
  ;
  ; Input variable are provided at daily time step, the simulation is conducted from DOY0 to DOY0+N_ELEMENTS(globrad)

  ;DA FARE completamante:

  ;- partitioning HI
  ;- leaf life span, it should take into account temperature and water stress

  ;DIFFERENCES WITH GRAMI:
  ; - GDD is compute from emergence
  ; - GDD are used only in the leaf lifespan, not in phenology determination
  ; - DOY0 is determined in the inversion of the model against RS observation
  ; - water limitation
  ;- PROSAIL is used to compute FAPAR (see details in d:\Users\meronmi\Documents\IDL\simmod\Coupled Model\PROSAIL\DOCS\ MM modifications to prosail.docx



  ;TIC
  ;TIC, /PROFILER
  ;TEST with simple ratio
  C2DM = 1.0/0.45           ;conversion carbon to dry matter (https://doi.org/10.1046/j.1365-2486.2002.00503.x)
  ratio_method = 'casa'     ;method use to compute eps_s (see compute_eps_s_ratio.pro). Ratio is the simple ratio (>0.5 and <1), casa is (0.5 + ratio)<1.
  fapar_type = 'fapara'     ;can be 'fapara' for instantaneous fapar (for give sun angle) or 'faparh' for -bi-hemispherical fapar
  ; Initialize varaiables
  ; number of days to be simulated
  Tb = FLOAT(tb)
  nDays = N_ELEMENTS(tJD)
  ; - returned variables
  sim = { gdd:            FLTARR(nDays)*0.0, $              ;cumulated growing dergree days
    fa:             FLTARR(nDays), $              ;fapar
    lai:            FLTARR(nDays), $              ;lai
    dlai:           FLTARR(nDays) , $             ;daily increment in LAI
    dlai_senescent: FLTARR(nDays) , $             ;daily reduction in LAI
    gpp:            FLTARR(nDays), $              ;daily GPP value
    npp:            FLTARR(nDays), $              ;cumulative NPP value
    ;cgpp:           FLTARR(nDays), $              ;cumulative GPP value
    ;cnpp:           FLTARR(nDays), $              ;cumulative NPP value
    P1:             FLTARR(nDays), $              ;patitioning into leaves
    ;sen:            DBLARR(nDays), $              ;patitioning into senescence (only used with abondoned logistic)
    eps_s:          FLTARR(nDays), $              ;eps stress
    ModRef:         FLTARR(7, nDays)*!VALUES.F_NAN, $
    NRTtJDOfMax:    0.0}   ;simulated MODIS reflectances

  ; - working variables
  J = FLTARR(nDays)                                       ;leaf lifespan for leaf emerged each day in degree days
  ddLeafAge = FLTARR(nDays)                              ;cumulative degree days from each day up to t, use for senescence
  aliveLeaf = BYTARR(nDays) + 1                          ;aliveLeaf[t] = 1 if the leaves emerged at t are still alive, 0 if they were removed already

  ; Convert Global radiation to PAR
  par = globrad2par * globrad                   ;(MJ m-2 d-1) Note ECMWF is KJ m-2d-1 and it's divided by 1000 to get MJ m-2 d-1

  ; Time domain
  indTJD0 = WHERE(tJD EQ JD0)                   ;index of the day at which LAI is set to laiDOY0, it is LAI = 0 before
  indTJD0 = indTJD0[0]    ;cast to a scalar
  ; Time index for wlim (that starts befote tJD)
  indWlimTJD0 = WHERE(wlim.tJD EQ JD0)
  indWlimTJD0 = indWlimTJD0[0]    ;cast to a scalar
  ;Make sure that SLA and prosail Cm are consistent
  ;TEST +
  proSailVar.cm = 1.d/(SLA*100.d*100.d)
  ;proSailVar.cm = 0.008
  ;TEST -
  laiDOY0removed = 0                            ;flag used to avoid checking if laiDOY0 is removed once it is removed
  tOfMaxLai = 0
  maxLaiReached = 0                             ;boolean, becomes 1 after max LAI is reached, used with casa_kc
  tOflaiDOY0removed = -1
  satObsPresentTplus1 = 0
  ;water limitation
  IF (wlim.eps_onoff EQ 1)THEN BEGIN
    ;***************************
    ;DEFINITION OF eps stress
    CASE wlim_type OF
      'casa_kc': BEGIN
        ;in this case wLimParFixed is always 0
        ;bewm is precomputed, but bewm_et0 cannot be, because of the Kc
        ;extract relevant data (for the period of analysis only)
        bewmTmp = extract_timerange(wlim.tJD, tJD[0], tJD[nDays-1], bewm)
        ;at this point here we don't know when the maximum will be, we can only compute the exp waighted variable until indTJD0
        ;make it until the end,  when the max is recahed it will be modified
        bewm_et0 = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, [kc.kini*et0[0:indWlimTJD0], kc.kmid*et0[indWlimTJD0+1:-1]], Tb, half_life, 1000.0) ;fg[11], fg[10])
        bewm_et0_tmp = extract_timerange(wlim.tJD, tJD[0], tJD[-1], bewm_et0)
        ;CASA Approach for water scaler (Maselli et al, 2009;Field et al., 1995; Potter et al., 1993)
        ;sim.eps_s = (0.5 + bewmTmp / DOUBLE(bewm_et0_tmp)) < 1.0 ;limite to a max of 1
        sim.eps_s = compute_eps_s_ratio(bewmTmp, bewm_et0_tmp, ratio_method);compute_eps_s_ratio, nom, denom, method
      END
      'casa': BEGIN
        ;if bewm parameters are not free in the inversion do not recomputed at ecah iteration,
        ;use the one stored in the common block (and compute ind A_simInvMod_handler_water_lim.pro)
        ; now extract relevant data (for the period of analysis only)
        bewmTmp = extract_timerange(wlim.tJD, tJD[0], tJD[nDays-1], bewm)
        ;TEST with ratio bewm_rain/bewn_et0 +
        bewm_et0_tmp = extract_timerange(wlim.tJD, tJD[0], tJD[nDays-1], bewm_et0)
        ;CASA Approach for water scaler (Maselli et al, 2009;Field et al., 1995; Potter et al., 1993)
        sim.eps_s = compute_eps_s_ratio(bewmTmp, bewm_et0_tmp, ratio_method);compute_eps_s_ratio, nom, denom, method
      END
    ENDCASE
  ENDIF ELSE BEGIN
    ;- eps stress deactivated
    sim.eps_s = FLTARR(nDays) + 1.0
  ENDELSE

  ;compute once and for all the GDD from JD0, GDD(JD0)=0
  ;sim.gdd = [FLTARR(indTJD0+1), computedGDD(Tair[indTJD0:nDays-2], Tb, /CUMULATIVE)]
  ;TEST using abs to sum also negative temperatures
  sim.gdd = [FLTARR(indTJD0+1), computedGDD_abs(Tair[indTJD0:nDays-2], Tb, /CUMULATIVE)]
  IF (P1_sen_model EQ 'MaasLikeSen_P1logistic') THEN BEGIN
    sim.P1 = logistic_coeff(b, 'decay', 0.0, a, sim.gdd, 0.01)
  ENDIF
  ;compute once and for all J and the leaf ages in GDD
  IF (P1_sen_model EQ 'MaasLike') OR (P1_sen_model EQ 'MaasLikeSen_P1logistic') THEN BEGIN
    J[indTJD0:-2] = c + d * sim.gdd[indTJD0:-2]
    ;test abs
    ;ddLeafAge[indTJD0:-1] = [REVERSE(TOTAL(REVERSE((Tair[indTJD0:-2]-Tb > 0.0)), /CUMULATIVE, /DOUBLE, /NAN)),0]
    ddLeafAge[indTJD0:-1] = [REVERSE(TOTAL(REVERSE((ABS(Tair[indTJD0:-2]-Tb))), /CUMULATIVE, /DOUBLE, /NAN)),0]
    ;analogous to code commented in the loop
  ENDIF

  ;####################################### LOOP OVER THE DAYS ############################################
  FOR t=0, nDays-2 DO BEGIN
    ; note that tJD[t] = tJD[0] + t
    ; Until JD0 (subscript indTJD0) only perform spectral simulation with lai = 0, from JD0 on, start the growth development simulations

    ;see if there is a sat observation for t (t + 1 after the stast of the growth)
    IF (t LE indTJD0) THEN ind = WHERE((satObsJD EQ tJD[t]), satObsPresent)
    IF (t GE indTJD0) THEN indTplus1 = WHERE((satObsJD EQ tJD[t+1]), satObsPresentTplus1); satObsPresent is zero if there are no obs
    ;    IF (t LT indTJD0) THEN ind = WHERE((satObsJD EQ tJD[t]), satObsPresent)
    ;    IF (t EQ indTJD0) THEN BEGIN
    ;      ;see if the obs is at t or t +1
    ;      ind = WHERE((satObsJD EQ tJD[t]), satObsPresent)
    ;      indTplus1 = WHERE((satObsJD EQ tJD[t+1]), satObsPresentTplus1)  ;if it is here it cannot be a t
    ;      ;IF (satObsPresentTplus1 GT 0) THEN satObsPresent = satObsPresentTplus1
    ;    ENDIF
    ;    IF (t GT indTJD0) THEN ind = WHERE((satObsJD EQ tJD[t+1]), satObsPresentTplus1); satObsPresent is zero if there are no obs
    IF (t GE indTJD0) THEN BEGIN
      ; the model normally compute GPP and new LAI for the current day t and update fapar and reflectances for the next day (t+1). However,
      ; if current t is the day of leaves emergence, fapar and reflectances must be computed from scratch for day t
      IF (t EQ indTJD0) THEN BEGIN
        sim.lai[t] = laiDOY0
        ;use actual illumination and view geometry if there is a MODIS observation, otherwise closest geometry here (it is indeed irrelevant what to use)
        !NULL = MIN(ABS(satObsJD-tJD[t]), ind, /NAN)
PRO4SAIL5B,proSailVar.Cab,proSailVar.Car,proSailVar.Cbrown,proSailVar.Cw,proSailVar.Cm, $
  proSailVar.Ns,proSailVar.lidf_a,proSailVar.lidf_b,sim.lai[t]*DOUBLE(clump),proSailVar.hspot, $
  satObsSZA[ind],satObsVZA[ind],satObsRAA[ind],proSailVar.rsoil,resh,resv,absh,absa
  CASE fapar_type OF
    'fapara': sim.fa[t] = TOTAL(wpar*absa[0:300])
    'faparh': sim.fa[t] = TOTAL(wpar*absh[0:300])
    ELSE: STOP
  ENDCASE
  IF (satObsPresent GT 0) THEN sim.ModRef[*,t] = resample2MODIS7b(wl, resv, rsrLR) ;resample it
ENDIF
;Note the if casa_kc is used for water_lim sim.eps_es is computed up to indTJD0 (include) only
IF ((wlim.eps_onoff EQ 1) AND (wlim_type EQ 'casa_kc') AND ((t GT indTJD0)))THEN BEGIN
  ;here I have to update sim.eps_s[t] for t. And I have to use kc.kimd if the max has not be reached yet, kc.kend afterwards
  ;determine if the max LAI was reached (small oscillations are possible (dlai small for clouds, senescence of initial , look for a decrease for few days)
  IF ((sim.lai[t] LT MAX(sim.lai[0:t])*0.95) AND (maxLaiReached EQ 0) AND (tOflaiDOY0removed NE (t-1))) THEN BEGIN ; condition on tOflaiDOY0removed is because ;the drop is due  lai0 removal, ignore it
    maxLaiReached = 1
    tOfMaxLai = t
    ;we are after max, use kini, kmid and kend
    bewm_et0 = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, $
      [kc.kini*et0[0:indWlimTJD0], kc.kmid*et0[indWlimTJD0+1 : tOfMaxLai - wlim.DOYoffset], $
      kc.kend*et0[tOfMaxLai - wlim.DOYoffset + 1:-1]], Tb, half_life, 1000.0)
    ;knowing that nDays = N_ELEMENTS(tJD)
    bewm_et0_tmp = extract_timerange(wlim.tJD, tJD[0], tJD[-1], bewm_et0)
    ;CASA Approach for water scaler (Maselli et al, 2009;Field et al., 1995; Potter et al., 1993)
    sim.eps_s = compute_eps_s_ratio(bewmTmp, bewm_et0_tmp, ratio_method);compute_eps_s_ratio, nom, denom, method
    ;ind = WHERE(sim.eps_s GT 1, count)
    ;IF (count GT 0) THEN sim.eps_s[ind] = 1.0
  ENDIF
ENDIF
;compute GPP and NPP for today ;;dgpp[t] = eps_max * sim.eps_s[t] * (sim.fa[t]*par[t])
;note: no photosynthesis if T < Tb, in this case there is no senescence lai as well
sim.gpp[t] = eps_max * sim.eps_s[t] * (sim.fa[t]*par[t]) * (tair[t] GT Tb)         ; gC m-2 d-1
sim.npp[t] =sim.gpp[t] * (1.0-gamma)                         ; same as above

;partitioning (if logistic is used this is already computed and not recomputed here)
IF (P1_sen_model EQ 'MaasLike') THEN BEGIN
  ;hyperbolic function
  sim.P1[t] = LeafPartitioning_on_GDD2(sim.gdd[t], a, b)
ENDIF

; LAI and fAPAR evolution
; New LAI
;sim.dlai[t] = sim.P1[t] * (sim.npp[t]*C2DM) * (0.1 * SLA);(m2 m-2)     new lai, 1/(100.0*100.0) * SLA * 1000.0 = 0.1 * SLA
sim.dlai[t] = sim.P1[t] * (sim.npp[t]*C2DM) * (SLA);(m2 m-2)     new lai, gDM m-2 d-1 * m2 g-1 
IF (t EQ indTJD0) then sim.dlai[t] = sim.dlai[t] + laiDOY0  ;if it is the first day of sim, add lai at DOY0
; Senescent LAI (Maas like(
; find out the past days at which the leaves has reached the lifespan and remove them. Ther are two conditions for removing LAI:
; - leaves for those days were not removed already,
; - the age of those days has exceeded its lifespan J
indSen = indTJD0 + WHERE(((ddLeafAge[indTJD0:t] - ddLeafAge[t+1]) GT J[indTJD0:t]) AND (aliveLeaf[indTJD0:t] EQ 1), countSen)
IF (countSen GT 0) THEN BEGIN
  ;remove the LAI accumulated during those days
  sim.dlai_senescent[t] = TOTAL(sim.dlai[indSen])
  aliveLeaf[indSen] = 0
ENDIF

;Update LAI and FAPAR with net LAI increase
sim.lai[t+1]=sim.lai[t] + sim.dlai[t] - sim.dlai_senescent[t]
IF (sim.lai[t+1] GT 20) THEN STOP
IF (sim.lai[t+1] LT 0) THEN BEGIN
  ;a small negative value is acceptable as the LAI may be close to zero because of machine precision, e.g.
  IF (sim.lai[t+1] LT -0.0001) THEN STOP ELSE sim.lai[t+1] = 0.0
ENDIF
; conversion of lai into fapar
;use actual illumination and view geometry if there is a MODIS observation
;if not use closest illum last geometry here (and the last later)
!NULL = MIN(ABS(satObsJD-tJD[t+1]), ind, /NAN)
IF (FINITE(satObsSZA[ind]) NE 1) THEN STOP
PRO4SAIL5B,proSailVar.Cab,proSailVar.Car,proSailVar.Cbrown,proSailVar.Cw,proSailVar.Cm, $
  proSailVar.Ns,proSailVar.lidf_a,proSailVar.lidf_b,sim.lai[t]*DOUBLE(clump),proSailVar.hspot, $
  satObsSZA[ind],satObsVZA[ind],satObsRAA[ind],proSailVar.rsoil,resh,resv,absh,absa
  CASE fapar_type OF
    'fapara': sim.fa[t+1] = TOTAL(wpar*absa[0:300])
    'faparh': sim.fa[t+1] = TOTAL(wpar*absh[0:300])
    ELSE: STOP
  ENDCASE
  IF (satObsPresentTplus1 GT 0) THEN sim.ModRef[*,t+1] = resample2MODIS7b(wl, resv, rsrLR)
  ;yield
  ;yield[t]=alpha_g[t]*sim.cnpp[t]    ;(kgDM ha-1 d-1)
ENDIF ELSE BEGIN
  ; The section before is execute only from JD0. Note that all variables are set to 0 (LAI, GPP, etc.) at their initialization.
  ; The only variable that must be computed is reflectance, that, in the absence of vegetation, is the reflectance of the soil.
  ; In addition this reflectance, being considered isotropic, does not change with observation geoemtry. Therefore is compute once and for all at the first request.
  IF (t EQ 0) THEN modRef0LAI = resample2MODIS7b(wl, proSailVar.rsoil, rsrLR);modRef0LAI = rsrArray ## proSailVar.rsoil;
  IF (satObsPresent GT 0) THEN sim.ModRef[*,t] = modRef0LAI
ENDELSE
ENDFOR
;TOC
;TOC, REPORT=rep_profiler
;PROFILER, /REPORT

IF (tOfMaxLai EQ 0) THEN sim.NRTtJDOfMax = tJD[-1] ELSE sim.NRTtJDOfMax = tJD[tOfMaxLai]
;  match, tJD, SatobsJD, indsim
;  If (MIN(FINITE(sim.ModRef[0,indsim])) EQ 0) THEN STOP
RETURN, sim
END


;'bewm_opt': BEGIN
;  ;if bewm parameters are not free in the inversion do not recomputed at ecah iteration,
;  ;use the one stored in the common block (and compute ind A_simInvMod_handler_water_lim.pro)
;  IF (wLimParFixed EQ 0) THEN BEGIN
;    bewm = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, wlim.rain, Tb, half_life, cap)
;    ;bewm et0 should be computed here if adopted (pass the data, remove stop)
;    et0 = extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.et0)
;    bewm_et0 = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, et0, Tb, fg[11], fg[10])
;    STOP ;a stop is here as this is not anymore used in 2017..
;  ENDIF
;  ; now extract relevant data (for the period of analysis only)
;  bewmTmp = extract_timerange(wlim.tJD, tJD[0], tJD[nDays-1], bewm)
;  ;exp decay weighted average of p
;  sim.eps_s = (bewmTmp / DOUBLE(bewm_opt)) < 1.0
;END
;

;  ;test rel lai - fapar
;  LUTl = [FINDGEN(101, INCREMENT=0.01), FINDGEN(80, INCREMENT=0.05, START=1.05),  FINDGEN(50, INCREMENT=0.1, START=5.1)]
;  LUTfa = FLTARR(N_ELEMENTS(LUTl))
;  FOR i=0, N_ELEMENTS(LUTl)-1 DO BEGIN
;    PRO4SAIL5B,proSailVar.Cab,proSailVar.Car,proSailVar.Cbrown,proSailVar.Cw,proSailVar.Cm, $
;    proSailVar.Ns,proSailVar.lidf_a,proSailVar.lidf_b,LUTl[i],proSailVar.hspot, $
;    0,0,0,proSailVar.rsoil,resh,resv,absh
;    LUTfa[i] = TOTAL(wpar*absh[0:300])
;  ENDFOR
;; usage fapar di lai x is INTERPOL(LUTfa, LUTl, x, /SPLINE)
;; res = INTERPOL(ffa, l, ll, /SPLINE)
;  ;end_test

;IF (wLimParFixed EQ 0) THEN BEGIN
;  ;          bewm = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, wlim.rain, half_life, cap)
;  ;          ;bewm et0 should be computed here if adopted (pass the data, remove stop)
;  ;          et0 = extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.et0)
;  ;          bewm_et0 = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, et0, fg[11], fg[10])
;  ;          STOP ;a stop is here as this is not anymore used in 2017..
;ENDIF

;;make a check that one of the two is requested
;IF ((P1_sen_model NE 'Logistic') AND (P1_sen_model NE 'MaasLike') AND (P1_sen_model NE 'MaasLikeSen_P1logistic')) THEN STOP
;IF (P1_sen_model EQ 'Logistic') THEN BEGIN
;  ;The logistic model for se is not used anymore
;  sim.P1 = logistic_coeff(c, 'decay', 0.0, a, sim.gdd, 0.01)
;  sim.sen = logistic_coeff(1.0, 'grow', d, b, sim.gdd, 0.01)
;  ;The senescense may be estimated to end after the current period of simulation
;  IF (max(sim.sen) NE 1) THEN sim.sen[-1] = 1.0
;ENDIF

;CASE P1_sen_model OF
;  'MaasLike': opt= 'MaasSen'
;  'Logistic': opt= 'LogisticSen'
;  'MaasLikeSen_P1logistic': opt= 'MaasSen'
;ENDCASE
;CASE opt OF
;  'MaasSen': BEGIN
;    ;find out the past days at which the leaves has reached the lifespan and remove them,
;    ;2 conditions for removing LAI:
;    ; - leaves for those days were not removed already,
;    ; - the age of those days has exceeded its lifespan J
;    ;indSen2 = indTJD0 + WHERE(((ddLeafAge2[indTJD0:t] - ddLeafAge2[t+1]) GT J2[indTJD0:t])AND (aliveLeaf[indTJD0:t] EQ 1), countSen2)
;    indSen = indTJD0 + WHERE(((ddLeafAge[indTJD0:t] - ddLeafAge[t+1]) GT J[indTJD0:t]) AND (aliveLeaf[indTJD0:t] EQ 1), countSen)
;    ;indSen = WHERE((aliveLeaf EQ 1) AND (ddLeafAge GT J), countSen)
;    IF (countSen GT 0) THEN BEGIN
;      ;remove the LAI accumulated during those days
;      sim.dlai_senescent[t] = TOTAL(sim.dlai[indSen])
;      ;if also the first day of growth LAI has to be removed, remove laiDOY0 as well
;      IF (laiDOY0removed EQ 0) THEN BEGIN
;        ind = WHERE(indSen EQ indTJD0, count)
;        IF (count NE 0) THEN BEGIN
;          sim.dlai_senescent[t] = sim.dlai_senescent[t] +  laiDOY0
;          laiDOY0removed = 1
;          tOflaiDOY0removed = t
;        ENDIF
;      ENDIF
;      ;set the leaves to dead
;      aliveLeaf[indSen] = 0
;    ENDIF
;  END
;  'LogisticSen': BEGIN
;    ;from gdd = a I have to start removing LAI from the maximum achieved so far
;    IF (sim.gdd[t] GE d) THEN BEGIN
;      sim.dlai_senescent[t] = MAX(sim.lai)*(sim.sen[t]-sim.sen[t-1])
;    ENDIF ELSE BEGIN
;      sim.dlai_senescent[t] = 0.0
;    ENDELSE
;  END
;  ELSE: STOP
;ENDCASE

;if also the first day of growth LAI has to be removed, remove laiDOY0 as well. 07/03/2017: not anymore, laiDOY0 was added to as dlai for the dau before (indTJD0)
;        IF (laiDOY0removed EQ 0) THEN BEGIN
;          ind = WHERE(indSen EQ indTJD0, count)
;          IF (count NE 0) THEN BEGIN
;            sim.dlai_senescent[t] = sim.dlai_senescent[t] +  laiDOY0
;            laiDOY0removed = 1
;            tOflaiDOY0removed = t
;          ENDIF
;        ENDIF
;set the leaves to dead

