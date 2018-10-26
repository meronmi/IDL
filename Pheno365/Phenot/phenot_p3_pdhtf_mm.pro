FUNCTION phenot_p3_pdhtf_mm

;  Purpose:
;     To fit a Parametric Double Hyperbolic Tangent Function (PDHTF)
;     7-parameter model to the appropriate periods of the FAPAR time
;     series.
;     MM+: the time period is first fractioned in yearly intervals
;     according to the median year
;  Outcome:
;     The PDHTF model parameters are available for each growing
;     season.

;  Usage:
;     rc = phenot_p3_pdhtf()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.
;     1: Season probably incomplete (min after max is identical to max
;        or min is larger than median of FAPAR distribution).
;     2: Processing ended before identifying maxngrowseas growing
;        seasons due to lack of valid data.

;  Remarks:
;     The variables gsinistartDek[c,y] and gsiniendDek[c,y], which define the period
;     within which a growing season is to be searched for, must be
;     declared and initialized before calling this routine.

;  History:
;     This is an updated version of Version: 6.0, Release: 1.0, of the routine
;     gs6t_p3_pdhtf.pro developed on 2007-09-20.
;     Afterwards it has been modiefied by Miche Meroni in March April 2011
;     Version 1.1: MM, compute phenology using real acquisition date (vtYYTTg.img files treated with
;     generate_acqJULDAY_image)

;  Include the necessary common block(s):
@cb_job.comm
@cb_in.comm
@cb_stat.comm
@cb_frst.comm
@cb_pdhtf.comm
;MM+
@cb_options.comm
@cb_optimization.comm
;OPTIONS 
print_par=1                   ;0 if to suppress model paramerer outputs
use_curve_fit=0               ;set to 0 use MPFIT, set to 1 use CURVEFIT
resampling = 10               ;factor to resample dekads to get more precise results
dt=1.d/DOUBLE(resampling)       ;for integral computation
dtJD = 1.d
;MM-

;  Initialize the return code:
retcode = 0
;  Initialize the number of growing seasons found so far:
numgs = 0
;  Estimate the maximum number of expected growing seasons in the FAPAR record:
maxngrowseas = nyears * ngspy
;  MM: note that in my records I have nyears (including the first and the last that
;  may not be complete). In such time period I have n_full_solar_years, where
;  the solar year is the 36 decaded period containing the repeatitive cycle (1 or 2)
;  of fAPAR 


;  Define the arrays required to store the 7 PDHTF model parameters
;  (base value, amplitude, time shift and slope of the growth phase,
;  and amplitude, time shift and slope of the decay phase), as well
;  as the status of the CURVEFIT outcome, for each of the expected
;  growing seasons:
; MM: the arrays have been divided in first and second gs 
pdhtf_base = FLTARR(2, nyears) & pdhtf_amp1 = FLTARR(2, nyears)
pdhtf_sft1 = FLTARR(2, nyears) & pdhtf_slo1 = FLTARR(2, nyears)
pdhtf_amp2 = FLTARR(2, nyears) & pdhtf_sft2 = FLTARR(2, nyears)
pdhtf_slo2 = FLTARR(2, nyears)
;Define the arrays required to store the search periods
gsinistartDek=FLTARR(2, nyears) & gsiniendDek=FLTARR(2, nyears)
gsinistartDekJD=FLTARR(2, nyears) & gsiniendDekJD=FLTARR(2, nyears) ;expressed in Julian day
;  Define the arrays required to store the growing season characteristics:
gsstrt = FLTARR(2, nyears) & gsstop = FLTARR(2, nyears)   ;definition based on (base value + fract_thresh * amplitude)
                             gsstop2 = FLTARR(2, nyears)  ;definition based on a different fract_thresh2 for the decay phase
Tc=0.05     ;threshold used for comp (completness)                             
gsstrtTc = FLTARR(2, nyears) & gsstopTc = FLTARR(2, nyears)   ;for gscomp based on 0.05 thresh (5%)
;gsslenTc = FLTARR(2, nyears)
gsleng = FLTARR(2, nyears) & gsmaxv = FLTARR(2, nyears)
gsleng2 = FLTARR(2, nyears)                               ;based om gsstop2
gsmaxt = FLTARR(2, nyears) & gsaccu = FLTARR(2, nyears)
                             gsaccue = FLTARR(2, nyears)  ;based om gsstop and min baseline
                             gsaccub = FLTARR(2, nyears)  ;based om gsstop2
                             gsaccueb = FLTARR(2, nyears)  ;based om gsstop2 and min baseline 
;MM+
gsstrtdelta=FLTARR(2, nyears)  ;this variable stores the delta fapar corresponding to SOS
;  Define the arrays required to store the diagnostics of the inversion procedures:
gsstat = FLTARR(2, nyears) & gschis = FLTARR(2, nyears)
gsstat[*,*]=!VALUES.F_NAN   ;set to unkown
;  Define the quality flag stating the reliability of the growing season information.
;  gsflag is only for the optimization procedure:
;  -2: the opt was not performed (because there is not enough variability)
;  -1: the optimization was not performed because of too few observations (less than model parameters)
;   0: the optimization failed to converge
;   10: succesful optimization
gsflag = FLTARR(2, nyears)
gsflag[*,*]=!VALUES.F_NAN

; gsrel informs about the reliability of the growing season data
; gsrel + 1000: the expected end of the Opt Window is beyond the last record (the last solar year is incomplete)
; gsrel + 500:  no max above fap50pctl in the season
; gsrel + 100:  the growing season is not well formed (no rise [or decay] from [or to] below fap50pctl
; gsrel + 10:   2/3 of missing values in the rise
; gsrel + 20:   2/3 of missing values in the decay
gsrel = FLTARR(2, nyears)
gsrel[*,*]=0  ;set to unknown 

;Initailize returned parameters to NAN
gschis [*,*]=!VALUES.F_NAN
gsstrtTc [*,*]=!VALUES.F_NAN  & gsstopTc [*,*]=!VALUES.F_NAN
;gsstrtTcDek [*,*]=!VALUES.F_NAN  & gsstopTcDek [*,*]=!VALUES.F_NAN
;gsslenTc [*,*]= !VALUES.F_NAN
gsstrt [*,*]=!VALUES.F_NAN  & gsstop [*,*]=!VALUES.F_NAN
gsleng [*,*]=!VALUES.F_NAN  & gsmaxv [*,*]=!VALUES.F_NAN
gsmaxt [*,*]=!VALUES.F_NAN  & gsaccu [*,*]=!VALUES.F_NAN
gsstop2[*,*]=!VALUES.F_NAN  & gsleng2 [*,*]=!VALUES.F_NAN
gsaccue[*,*]=!VALUES.F_NAN  & gsaccub [*,*]=!VALUES.F_NAN
gsaccueb[*,*]=!VALUES.F_NAN 
pdhtf_base [*,*] = !VALUES.F_NAN & pdhtf_amp1 [*,*] = !VALUES.F_NAN
pdhtf_sft1 [*,*] =!VALUES.F_NAN & pdhtf_slo1 [*,*] = !VALUES.F_NAN
pdhtf_amp2 [*,*] = !VALUES.F_NAN & pdhtf_sft2 [*,*] = !VALUES.F_NAN
pdhtf_slo2 [*,*] = !VALUES.F_NAN
gsinistartDek [*,*] = !VALUES.F_NAN & gsiniendDek [*,*] = !VALUES.F_NAN
gsinistartDekJD [*,*] = !VALUES.F_NAN & gsiniendDekJD [*,*] = !VALUES.F_NAN

;  Define the number of complete growing seasons found so far:
numgs = 0

;  Define the array that will contain the estimated best guesses for
;  the missing values in the FAPAR record, based on the model fit:
; MM+ not used
;missfapar = FLTARR(npts)
; MM-
;  Define the array that will contain the optimally estimated FAPAR
;  values during the various growing seasons:
;fitted = FLTARR(npts)

;  Define the array containing the model parameters for a particular
;  model fit:
pdhtf_pars = FLTARR(7)

i = 0 ;i counts the total number of GS being analyzed
IF (n_remaing_year EQ 0) THEN n_full_solar_years = n_full_solar_years-1
;  Loop over all years and expected growing seasons:
FOR y=0, n_full_solar_years DO BEGIN  
  ;starting from 0 it is looping also on the last, uncomplete year (if any)
  ;define the range (in decades) of the working year by using the xoffst
  ;xoffst[0] is the decade at which the first season start (and finish if
  ;there's only 1 season)
  ;xoffst[1] is the decade at which the second season start  

  decrange_solar_year=[(y*36+xoffst[0]),((y+1)*36+xoffst[0])]
  ;if y eq 13 then stop
  ;Loop over the expected numebr of GS in a year (can be 1 or 2)
  FOR c = 0, ngspy-1 DO BEGIN
    if (vrbs eq 1) then print, '>>> Search for GS #', i + 1
    ;IF (i+1 EQ 5) THEN STOP
    ;MM+
    ;### (BEGIN) CHECK THAT THERE IS ENOUGH VARIABILITY IN THE CURRENT OW (Optimization Window)
    ;if y eq 12 then stop
    ;retrieve the expected breakpoints for the current OW
    if (ngspy eq 1) then begin  ;1 GS
      ows=decrange_solar_year[0]    ;Optimization Window Start
      owe=decrange_solar_year[1]    ;Optimization Window End
      search_range=6            ;this will be used afterwards to adjust the s and e locally
    endif else begin            ;2 GS
      search_range=4            ;4 this will be used afterwards to adjust the s and e locally
      if (c eq 0) then begin  ;first GS (it comes here only if 2 GS are present)
        ows=decrange_solar_year[0]
        owe=ows+(xoffst[1]-xoffst[0])
      endif else begin        ;second GS
        ows=decrange_solar_year[0]+(xoffst[1]-xoffst[0])
        owe=decrange_solar_year[1]
      endelse
    endelse
    
    ;check that owe is not beyond the end of the record
    if (owe gt N_ELEMENTS(fapar)-1) then begin
      ;the current OW is not complete
      owe=N_ELEMENTS(fapar)-1
      gsrel[c,y]=gsrel[c,y]+1000
    endif
    ;check that ows or owe is not before the start of the record (it may happen if the first season, which is
    ;the first in the solar year starts before the RS data)
    IF (ows lt 0) THEN BEGIN
      ows=0
      if gsrel[c,y] lt 999 then gsrel[c,y]=gsrel[c,y]+1000
    ENDIF
    IF (owe lt 0) THEN BEGIN
      owe=0
      if gsrel[c,y] lt 999 then gsrel[c,y]=gsrel[c,y]+1000
    ENDIF
    ;check that ows is not beyond the end of the record (may happen if only few obs are present for the current year)
    if (ows gt N_ELEMENTS(fapar)-1) then begin
      ;the current OW is not complete
      ows=N_ELEMENTS(fapar)-1
      if gsrel[c,y] lt 999 then gsrel[c,y]=gsrel[c,y]+1000
    endif
    
    IF ((owe-ows) LE 7) THEN BEGIN
      ;the interval is to small to check if there is enough variability
      gsflag[c,y]=-1
    ENDIF ELSE BEGIN
      ;compute percentiles 
      rc05 = prctl(0.05, fapar[ows:owe], 1, ibel, iabo, 0, owfap05pctl)
      rc95 = prctl(0.95, fapar[ows:owe], 1, ibel, iabo, 0, owfap95pctl)
      ;### (END) CHECK THAT THERE IS ENOUGH VARIABILITY IN THE CURRENT OW (Optimization Window)
      IF ((owfap95pctl - owfap05pctl) LT faprangeminthresh) $ ;/2.0) $ ;locally faprangeminthresh is lowered
         OR (rc05 NE 0) $
         OR (rc95 NE 0)THEN BEGIN
        gsflag[c,y]=-2
        ;store appropriate results
        gsflag[c,y]=-2
        ;store appropriate results
        gsstrt [c,y] = -999 & gsstop [c,y] =  -999
        gsstop2 [c,y] =  -999 &  gsstopTc  [c,y] = -999
        gsleng [c,y] =  -999 & gsleng2 [c,y] =  -999 & pdhtf_base [c,y] = -999
        pdhtf_amp1 [c,y] = -999 & pdhtf_sft1 [c,y] = -999
        pdhtf_slo1 [c,y] = -999 & pdhtf_amp2 [c,y] = -999
        pdhtf_sft2 [c,y] = -999 & pdhtf_slo2 [c,y] = -999
        gsstrtdelta[c,y]= !VALUES.F_NAN & gsmaxv [c,y] = 0.0
        gsmaxt [c,y] =  -999 & gsaccu [c,y] = 0.0
        gsaccub [c,y] = 0.0 & gsaccue [c,y] = 0.0 & gsaccueb [c,y] = 0.0 
      ENDIF ELSE BEGIN  ;There is enough variability
        ;### (BEGIN) DEFINE THE LOCALLY ADJUSTED BREAKPOINTS FOR THE CURRENT OW 
        ;select start and stop as min(fapar) around the offsets
        start_search_ind=[ows-search_range, ows+search_range]
        stop_search_ind=[owe-search_range, owe+search_range]
        IF stop_search_ind[0] LT start_search_ind[0] THEN  stop_search_ind[0]=start_search_ind[0]
        ;check that the first/last point is not beyond the record
        if (start_search_ind[0] lt 0) then start_search_ind[0]=0
        if (stop_search_ind[0] lt 0) then stop_search_ind[0]=0
        if (start_search_ind[1] gt N_ELEMENTS(fapar)-1) then begin
          if gsrel[c,y] lt 999 then gsrel[c,y]=gsrel[c,y]+1000  ;if it was not already flagged, flag it
          start_search_ind[1] = N_ELEMENTS(fapar)-1
          if (vrbs eq 1) then print, 'This period most probably contains an incomplete growing season.'
        endif   
        if (stop_search_ind[1] gt N_ELEMENTS(fapar)-1) then begin
          if gsrel[c,y] lt 999 then gsrel[c,y]=gsrel[c,y]+1000  ;if it was not already flagged, flag it
          stop_search_ind[1] = N_ELEMENTS(fapar)-1
          stop_search_ind[0] = stop_search_ind[1]-search_range
          if (vrbs eq 1) then print, 'This period most probably contains an incomplete growing season.'
        endif  
        ;start and stop, look for the minimum around on the smoothed curve (to avoid being trapped in unrealistic drop)       
        rc=smoothmv(fapar, ibel, iabo, 7, sfapar) ;use MV version because savgol with NaN has problem
        ;conversion of -999 (coming from smoothmv that copy -999) to NaN
        ind_nan=where((sfapar lt ibel) or (sfapar gt iabo), count_nan)
        if (count_nan gt 0) then sfapar[ind_nan]=!VALUES.F_NAN
        minv=MIN(sfapar[start_search_ind[0]:start_search_ind[1]], indmin, /NAN)
        gsinistartDek[c,y]=indmin+start_search_ind[0]
        minv=MIN(sfapar[stop_search_ind[0]:stop_search_ind[1]], indmin, /NAN)
        gsiniendDek[c,y]=indmin+stop_search_ind[0]
        ;Here I start to keep track of the true Julian Day
        gsinistartDekJD[c,y] = acq[gsinistartDek[c,y]]
        gsiniendDekJD[c,y] = acq[gsiniendDek[c,y]]
        
        IF ((gsinistartDekJD[c,y] EQ -999) OR (gsiniendDekJD[c,y] EQ -999)) THEN STOP       ;if they are -999 there is something wrong, because the FAPAR is there, but has no JD
        
        fmax = MAX(sfapar [gsinistartDek[c,y]:gsiniendDek[c,y]], fmax_idx, /NAN)
        fmax_idx = fmax_idx+gsinistartDek[c,y]
        fmax_JDx = acq[fmax_idx]
        IF (vrbs eq 1) THEN BEGIN        
          print, 'gsinistartDek[c,y] gsiniendDek[c,y] = ', gsinistartDek[c,y], gsiniendDek[c,y]
          print, 'fmax fmaidx (based on fapar) = ', fmax, fmax_idx
          print, 'fmax Julian Day (based on fapar) = ', fmax_JDx
        ENDIF
              
        ;### (END) DEFINE THE LOCALLY ADJUSTED BREAKPOINTS FOR THE CURRENT OW
        
  
        ;### (BEGIN) FIT THE MODEL ON OW DEFINED BY [gsinistartDek[c,y], gsiniendDek[c,y]]
        ;  Collect the valid values to be used for the model fitting:
        ndat = gsiniendDek[c,y] - gsinistartDek[c,y] + 1
        ;X
        goodfi = gsinistartDek[c,y] + WHERE((fapar [gsinistartDek[c,y]:gsiniendDek[c,y]] GE ibel) $   ;ibel is ignore below 
                 AND (fapar [gsinistartDek[c,y]:gsiniendDek[c,y]] LE iabo), ngood) ;iabo is ignore above
        ;X good expressed in Julian Day
        goodfJD =  acq[goodfi]       
               
        ;Y
        goodf = fapar [goodfi]
        nbad = ndat - ngood
        
        ;  Here check again that there is enough variability in the locally adjusted interval
        ;compute percentiles 
        rc05 = prctl(0.05, goodf, 1, ibel, iabo, 0, goodf05pctl)
        rc95 = prctl(0.95, goodf, 1, ibel, iabo, 0, goodf95pctl)
        ;proceed only if there is enough variability
        ;IF (column EQ 2035) and (line eq 92) and (y eq 6) and (c eq 1) THEN STOP
        IF ((goodf95pctl - goodf05pctl) GT faprangeminthresh/2.0) AND (rc05 EQ 0) AND (rc95 EQ 0) THEN BEGIN 
        ;IF ((goodf95pctl - goodf05pctl) GT faprangeminthresh) AND (rc05 EQ 0) AND (rc95 EQ 0) THEN BEGIN
          ;  If the number of missing points equals or exceeds a quarter of the total, and especially if these are clustered near
          ;  the start or end of the period, just recorded into a flag
          ;  gsrel: +10, +20
          IF (nbad GE FIX(ndat / 4.0)) THEN BEGIN
            IF (vrbs EQ 1) THEN print, 'A quarter or more of the data points in this period are missing.'
            ;  Compute the ranks (timing) of the missing observations
            ;  corresponding to 1/3 and 2/3 of the missing values within
            ;  the current [gsinistartDek[c,y], gsiniendDek[c,y]] period:
            midper = gsinistartDek[c,y] + FIX((gsiniendDek[c,y] - gsinistartDek[c,y]) / 2.0)
            nmiss = 0 & rnk13 = 0 & rnk13found = 0 & rnk23 = 0 & rnk23found = 0
            FOR j = gsinistartDek[c,y], gsiniendDek[c,y] DO BEGIN
              IF ((fapar [j] LT ibel) OR (fapar [j] GT iabo)) THEN nmiss = nmiss + 1
              IF (nmiss GE FIX(nbad * 1.0 / 3.0) AND (rnk13found EQ 0)) THEN $
                rnk13 = j & rnk13found = 1
              IF (nmiss GE FIX(nbad * 2.0 / 3.0) AND (rnk23found EQ 0)) THEN $
                rnk23 = j & rnk23found = 1
            ENDFOR
        
            ;  If 2/3 or more of the missing values are within the
            ;  first half of the period of interest (or, equivalently
            ;  1/3 or less of the missing values are within the second
            ;  half), update flag
            IF (rnk23 LT midper) THEN gsrel[c,y]=gsrel[c,y]+10
            
            ;  Similarly, if 2/3 or more of the missing values are
            ;  within the second half of the period of interest (or,
            ;  equivalently 1/3 or less of the missing values are
            ;  within the first half), update flag:
            IF (rnk13 GT midper) THEN gsrel[c,y]=gsrel[c,y]+20
            

          ENDIF 
       
          ;  Make sure the first and last good values within that new
          ;  period are at least as low as the median (50th) percentile
          ;  of the entire distribution, and that there is a local maximum
          ;  above that value, to avoid trying to fit a growing season
          ;  through a partial record where only the initial rise (or
          ;  only the final decline) is available (e.g., near the start
          ;  or end of the record): [For now, this is only a diagnostic stored in gsrel as + 100]:
           
          IF ((goodf [0] GT fap50pctl) $
            OR (goodf [N_ELEMENTS(goodf) - 1] GT fap50pctl)) THEN BEGIN
            gsrel[c,y]=gsrel[c,y]+100
            IF (vrbs eq 1) THEN BEGIN 
              PRINT, 'This GS may not contain a well-formed growing season...'
            ENDIF
          ENDIF
          IF (MAX(goodf) LT fap50pctl) THEN BEGIN
            gsrel[c,y]=gsrel[c,y]+500
            IF (vrbs eq 1) THEN BEGIN 
              PRINT, 'This GS may not contain a well-formed growing season...'
            ENDIF
          ENDIF
          ;  Re-define the array containing the fitting weights, of the same
          ;  dimension as goodf, and assign their values. Currently, these are
          ;  all set to 1.0:
          weights = FLTARR(N_ELEMENTS(goodf))*0.0+1.0
          pdhtf_parsJD = pdhtf_pars
          ;  Estimate the best initial guess values for the 7 PDHTF model
          ;  parameters for the this growing season on the basis of actual
          ;  valid FAPAR values:
          ;  Set the initial base value to the first valid value in the period of interest or the min in the first half of the data:
          ;pdhtf_pars [0] = goodf [0]
          pdhtf_pars [0] = MIN(goodf [0:FIX(N_ELEMENTS(goodf)/2.0)])
          pdhtf_parsJD[0] = pdhtf_pars[0]
          ;  Set the range for the growth phase to the difference between the local
          ;  maximum and this initial base value (The factor 2 is included because
          ;  the model dividex this amplitude by 2; MM+ removed, the /2 in the model is for the tangent):
          ;pdhtf_pars [1] = (fmax - goodf [0]) ;* 2.0
          pdhtf_pars [1] = (fmax - pdhtf_pars[0]) ;* 2.0
          pdhtf_parsJD [1] = pdhtf_pars [1]
          ;  Set the timining of the maximum growth rate as the middle point
          ;  between the start of the period and the time of the local maximum:
          ;MM+ 
          ;  the gaussian-like obeservations may not di centred in the optimization window,
          ;  e.g. typically a long tail on the right side. In this case the second inlexion point
          ;  is set in the flat part of the curve and MPFIT fails. To avoid this I assume symmetry and 
          ;  that one of the two distances is correct (the shortest).
          dist0 = MIN([fmax_idx - gsinistartDek[c,y], gsiniendDek[c,y] - fmax_idx])
          dist0JD = MIN([fmax_JDx - gsinistartDekJD[c,y], gsiniendDekJD[c,y] - fmax_JDx])
          pdhtf_pars [2] = fmax_idx - dist0 / 2.0 
          pdhtf_parsJD [2] = fmax_JDx - dist0JD / 2.0
          ;MM16 alternative method: find the time when fapar is close to (max-min_before_max)/2 and (max-min_after_max)/2
          ;work on the smoothed curve
          ; Find min before and after max 
          fminB = MIN(sfapar [gsinistartDek[c,y]:fmax_idx], fminB_idx, /NAN)
          fminB_idx = fminB_idx + gsinistartDek[c,y]
          fminA = MIN(sfapar [fmax_idx:gsiniendDek[c,y]], fminA_idx, /NAN)
          fminA_idx = fminA_idx + fmax_idx
          tmp = MIN(ABS(sfapar[gsinistartDek[c,y]:fmax_idx] - (fmax-fminB)/2.0), hmB_idx, /NAN)
          hmB_idx = hmB_idx + gsinistartDek[c,y]
          tmp = MIN(ABS(sfapar[fmax_idx:gsiniendDek[c,y]] - (fmax-fminA)/2.0), hmA_idx, /NAN)
          hmA_idx = hmA_idx + fmax_idx
          pdhtf_parsJD [2] = acq[hmB_idx] 
          
          ;pdhtf_pars [2] = (gsinistartDek[c,y] + fmax_idx) / 2.0
          ;  Set the initial guess growth slope:
          IF (ngspy EQ 1) THEN pdhtf_pars [3] = 0.5 ELSE pdhtf_pars [3] = 1.0
          pdhtf_parsJD[3] = pdhtf_pars[3]/10.0
          ;  Set the range for the decay phase to the difference between the local
          ;  maximum and last valid value (The factor 2 is included because
          ;  the model dividex this amplitude by 2):
          pdhtf_pars [4] = (fmax - goodf [N_ELEMENTS(goodf) - 1]) ;* 2.0
          pdhtf_parsJD[4] = pdhtf_pars[4]
          ;  Set the timining of the maximum decay rate as the middle point
          ;  between the time of the local maximum and the end of the period:
          ;pdhtf_pars [5] = (fmax_idx + gsiniendDek[c,y]) / 2.0
          pdhtf_pars [5] = fmax_idx + dist0 / 2.0
          ;pdhtf_parsJD [5] = fmax_JDx + dist0JD / 2.0
          pdhtf_parsJD [5] = acq[hmA_idx]
          ;MM- see above
          ;  Set the initial guess decay slope:
          IF (ngspy EQ 1) THEN pdhtf_pars [6] = -0.5 ELSE pdhtf_pars [6] = -1.0
          pdhtf_parsJD[6] = pdhtf_pars[6]/10.0 
          IF ((vrbs eq 1) AND (print_par eq 1)) THEN PRINT, pdhtf_pars, FORMAT='("FIRST GUESS:", 100(f7.3,x))'   
         
          
           
        ;  Fit the PDHTF model to the actual FAPAR values within the
        ;  sub-period if there are enough valid data points:
          IF (use_curve_fit eq 1) THEN BEGIN
  ;          IF (N_ELEMENTS(goodf) GT 7) THEN BEGIN
  ;            res = CURVEFIT(goodfi, goodf, weights, pdhtf_pars, $
  ;              FUNCTION_NAME = 'pdhtf', /NODERIVATIVE, $
  ;              CHISQ = chi, STATUS = stat, ITMAX=200)  ;MM+ original: CHISQ = chi, STATUS = stat)
  ;            if (vrbs eq 1) then print, 'status of CURVEFIT = ', stat
  ;            IF (stat EQ 0) THEN gsflag [c,y] = 10 
  ;          ENDIF ELSE BEGIN
  ;            ;too felw obs
  ;            gsflag[c,y]=-1
  ;          ENDELSE
  ;          if stat gt 0 then begin ;CURVEFIT failure
  ;            gsflag [c,y] = 0  ;from CURVEFIT: 1 and 2 indicate failure 
  ;          endif else begin        ;CURVEFIT success
  ;            gsstat [c,y] = stat
  ;            gschis [c,y] = chi
  ;          endelse
          ENDIF ELSE BEGIN  ;USE MPFIT instead of IDL CURVEFIT
          ;**********************************************************************
            ;  Fit the PDHTF model to the actual FAPAR values within the
            ;  sub-period if there are enough valid data points:
            ;IF i EQ 14 THEN STOP 
            fg = pdhtf_pars
            fgJD = pdhtf_parsJD
            IF (N_ELEMENTS(goodf) GT mingoodf) THEN BEGIN
              nparms=N_ELEMENTS(pdhtf_pars)
              parinfo = REPLICATE({limited:[0,0], limits:[0.d,0.d], fixed:0}, nparms)
              parinfo[0].limited = 1                ;base
              parinfo[0].limits  = [0.0d, 0.8d]
              IF fgJD[0] LT parinfo[0].limits[0] THEN fgJD[0] = parinfo[0].limits[0] + 0.001  ;small offset added because if fg = parinfo, mpfit gives "improper parameters"
              IF fgJD[0] GT parinfo[0].limits[1] THEN fgJD[0] = parinfo[0].limits[1] - 0.001
              parinfo[1].limited = 1                ;range 1
              parinfo[1].limits  = [0.0d, 1.2] ; [0.0d, 0.8d]
              IF fgJD[1] LT parinfo[1].limits[0] THEN fgJD[1] = parinfo[1].limits[0] + 0.001
              IF fgJD[1] GT parinfo[1].limits[1] THEN fgJD[1] = parinfo[1].limits[1] - 0.001
              parinfo[2].limited = 0                ;shift 1
              parinfo[3].limited = 1                ;slope 1
              parinfo[3].limits  = [0.0d, 50.0d]
              IF fgJD[3] LT parinfo[3].limits[0] THEN fgJD[3] = parinfo[3].limits[0] + 0.001
              IF fgJD[3] GT parinfo[3].limits[1] THEN fgJD[3] = parinfo[3].limits[1] - 0.001
              parinfo[4].limited = 0                ;range 2 
              parinfo[4].limits  = [0.0d, 1.2] ;[0.0d, 0.8]
              IF fgJD[4] LT parinfo[4].limits[0] THEN fgJD[4] = parinfo[4].limits[0] + 0.001
              IF fgJD[4] GT parinfo[4].limits[1] THEN fgJD[4] = parinfo[4].limits[1] - 0.001
              parinfo[6].limited = 1                ;slope 2
              parinfo[6].limits  = [-50.0d, 0.0d]
              IF fgJD[6] LT parinfo[6].limits[0] THEN fgJD[6] = parinfo[4].limits[0] + 0.001
              IF fgJD[6] GT parinfo[6].limits[1] THEN fgJD[6] = parinfo[4].limits[1] - 0.001
              mch=MACHAR(/double) & xtol=mch.EPS*1.0e+6
              ;END OF PARINFO
              ;IF i EQ 5 THEN STOP    
              fctname='pdhtf4mpfit'

  ;            ;optim with dekad
  ;            ret1 = opt_model2(fctname, double(goodfi), double(goodf),double(weights)*0.0, double(fg), double(weights)) 
  ;            p1 = p & yfit1 = yfit
       
              ;optim with Julian Days 
              ;ret = opt_model2(fctname, double(goodfJD), double(goodf), double(weights)*0.0, double(fgJD), double(weights))
              
              ;IF y eq 13 Then stop
              ret = opt_model2parinfo(fctname, double(goodfJD), double(goodf), double(weights)*0.0, FLOAT(fgJD), double(weights), parinfo)
              
              ;IF y eq 11 then stop
  ;            some trials using mpfit and penalizing the cuspidis.. aborted
  ;            ret = opt_model2parinfo_mpfit(fctname, double(goodfJD), double(goodf), double(weights)*0.0, double(fgJD), double(weights), parinfo)
                           
  ;            IF i EQ 14 THEN BEGIN
  ;              x = goodfJD
  ;              xhr = FLOAT(INDGEN(1000))*(MAX(x)-MIN(x))/1000.0+MIN(x)
  ;              fhr = p [0] + p [1] * (TANH((xhr - p [2]) * p [3]) + 1) / 2.0 + p [4] * (TANH((xhr - p [5]) * p [6]) + 1) / 2.0 - p [4]
  ;              fmax = MAX(fhr, id_max)
  ;              fhr1=SMOOTH(DERIV(fhr),20)
  ;              indn = WHERE(fhr1[0:id_max] LT 0, countn)
  ;              indp = WHERE(fhr1[id_max:999] GT 0, countp)
  ;              IF (countn GT 0) OR (countp GT 0) THEN STOP  
  ;             STOP
  ;            ENDIF
              ; IF PARINFO IS SET HERE I HAVE TO CHECK THAT IT DIDN'T REACH THE BOUNDARIES
    ;          for k=0, nparms -1 do begin
    ;            IF (p[k] EQ parinfo[k].limits[0]) OR (p[k] EQ parinfo[k].limits[1]) THEN BEGIN
    ;              status = -100-k & stop      ; so status from -100 to -106 indicates that the boundary of parameter from 0 to 6 were reached
    ;            ENDIF  
    ;          endfor
   
              ; Treat stat properly
              IF ((stat gt 0 and stat lt 5) OR (stat gt 5)) THEN BEGIN
                 gsflag[c,y]=10              ;success
                 pdhtf_parsJD=p
                if (vrbs eq 1) and (print_par eq 1) then begin
                  print, pdhtf_pars, FORMAT='("RETRIEVED P:", 100(f7.3,x))'
                  print, pcerror/ABS(p)*100.0, FORMAT='("%ERROR ON P:", 100(f7.3,x))'
                  print, 'gsstat [' + STRTRIM(STRING(i), 2) + '] = ', gsstat [c,y], ' gschis [' + STRTRIM(STRING(i), 2) + '] = ', gschis [c,y]
                endif
              ENDIF ELSE BEGIN 
                 gsflag[c,y]=0               ;failure
                 gsstat[c,y]=5
              ENDELSE
              
              if (vrbs eq 1) then print, 'status of MPFIT FITTING  = ', stat        
            ENDIF ELSE BEGIN    ; IF (N_ELEMENTS(goodf) GT 7) THEN BEGIN
              ;too few obs, the optimization is not performed
              gsflag[c,y]=-1
              stat=-100
            ENDELSE
          ENDELSE ; IF (use_curve_fit eq 1) THEN BEGIN
          ;**********************************************************************
          IF (ltdr_check) THEN BEGIN
            ;check implemted to see that we are not in the ltdr big gap
            jds = acq[gsinistartDek[c,y]:gsiniendDek[c,y]]
            ind = WHERE((jds GT 2451535) AND (jds LT 2451867), count)
            IF (count GT 3) THEN gsflag[c,y] = -1
          ENDIF
        ;  If the PDHTF model fit worked, store the results
        ;  for the current growing season.
    
          IF (gsflag[c,y] EQ 10) THEN BEGIN
       
            ind = gsinistartDek[c,y] + INDGEN(gsiniendDek[c,y]-gsinistartDek[c,y]+1)
            ind = acq[ind]
            pdhtf, ind, pdhtf_parsJD, f
            fitted[gsinistartDek[c,y]: gsiniendDek[c,y]]=f
          ENDIF ELSE BEGIN
            pdhtf_base [c,y] = !VALUES.F_NAN
            pdhtf_amp1 [c,y] = !VALUES.F_NAN
            pdhtf_sft1 [c,y] = !VALUES.F_NAN
            pdhtf_slo1 [c,y] = !VALUES.F_NAN
            pdhtf_amp2 [c,y] = !VALUES.F_NAN
            pdhtf_sft2 [c,y] = !VALUES.F_NAN
            pdhtf_slo2 [c,y] = !VALUES.F_NAN
            gsinistartDek[c,y] = !VALUES.F_NAN
            gsiniendDek[c,y] = !VALUES.F_NAN
          ;  Print diagnostic messages and continue, keeping in mind that
          ;  the model fitting procedure may not have worked:
            if (vrbs eq 1) then begin
              print, 'The PDHTF model could not be fitted,'
              print, 'skip to the next possible growing season.'
            endif
          ENDELSE
        ;ltdr check
      
        
        ;  IF optimization was a success
        ;  Define the start and end of the growing season as the dates at
        ;  which the simulated signal grows beyond or decays below the first
        ;  and last simulated value by more than fract_thresh (defined in phenot.pro) of the appropriate
        ;  amplitude.
        ;  IF it failed store NaN
        ;  IF there was not enough variability (gsflag=-2) store proper  results
          IF (gsflag[c,y] EQ 10) THEN BEGIN
            ;  If the PDHTF model fit worked, control that the fitted function is well behaved and then 
            ;  store the results for the current growing season. 
            gsstat [c,y] = stat
            gschis [c,y] = chi        
            pdhtf_base [c,y] = pdhtf_parsJD [0] & pdhtf_amp1 [c,y] = pdhtf_parsJD [1]
            pdhtf_sft1 [c,y] = pdhtf_parsJD [2] & pdhtf_slo1 [c,y] = pdhtf_parsJD [3]
            pdhtf_amp2 [c,y] = pdhtf_parsJD [4] & pdhtf_sft2 [c,y] = pdhtf_parsJD [5]
            pdhtf_slo2 [c,y] = pdhtf_parsJD [6]
            ;New version with resampling to increase the accuracy
            ;x axis (dekads)
            ;x = gsinistartDek[c,y] + INDGEN(gsiniendDek[c,y]-gsinistartDek[c,y]+1)
            ;same at High Resolution
            ;xHR = (FINDGEN((N_ELEMENTS(x)-1)*resampling+1)+x[0]*resampling)/DOUBLE(resampling)                        
            ;x axis (julian days at 1 day resolution)
            xHR = gsinistartDekJD[c,y] + INDGEN(gsiniendDekJD[c,y]-gsinistartDekJD[c,y]+1)
            ;pdhtf, xHR, pdhtf_pars, fittedHR 
            pdhtf, xHR, pdhtf_parsJD, fittedHR
            ; from her on I don't copy comment the lines with dekads
            ; i just modify them to make it work with JD
            fitmaxHR = MAX(fittedHR, idMax_xHR)
            ;here we have the problem that the function may be inistially descending, reaching a min and then making the bell
            ;so the minimum must be computed between the first element and the max to ensure that it's a real min
            fitmin1HR = MIN(fittedHR[0:idMax_xHR], idInitMin_xHR)
            ;fitmin1HR = fittedHR [0]
            ;same for the end
            fitmin2HR = MIN(fittedHR[idMax_xHR : N_ELEMENTS(fittedHR)-1], idEndMin_xHR)
            idEndMin_xHR = idMax_xHR + idEndMin_xHR
            
            ;Check that it's well behaved (there is min, then a max, and min again (it may happen that is 
            ;growing or decay monotonically, in that case set it to NaN as gsflag[c,y]=0    ; optimization failure 
            IF (idInitMin_xHR LT idMax_xHR) AND (idMax_xHR LT idEndMin_xHR) THEN BEGIN
              ;fitmin2HR = fittedHR [N_ELEMENTS(fittedHR)-1]
              ;normal SOS
  ;            relstart = WHERE(fittedHR [0:idMax_xHR] GE $
  ;            fitmin1HR + (fitmaxHR - fitmin1HR) * fract_thresh, cnt)
              relstart = WHERE(fittedHR [idInitMin_xHR:idMax_xHR] GE $
                fitmin1HR + (fitmaxHR - fitmin1HR) * fract_thresh, cnt)
              ;startgsHR_ind =  relstart[0]
              startgsHR_ind =  idInitMin_xHR + relstart[0]
              startgsHR_value = xHR[startgsHR_ind]
                
              ;SOS for completness, low threshold
    ;            relstart = WHERE(fittedHR[0:idMax_xHR] GE $
    ;            fitmin1HR + (fitmaxHR - fitmin1HR) * Tc, cnt)
              relstart = WHERE(fittedHR[idInitMin_xHR:idMax_xHR] GE $
                fitmin1HR + (fitmaxHR - fitmin1HR) * Tc, cnt)
              ;startgsTcHR_ind = relstart[0]
              startgsTcHR_ind = idInitMin_xHR + relstart[0]
              startgsTcHR_value = xHR[startgsTcHR_ind]
              ;M+
              strtgsdeltaHR = (fitmaxHR - fitmin1HR) * fract_thresh ;delta required to start
              ;stop 1, computed with fract_thresh
    ;            relstop = WHERE(fittedHR[idMax_xHR:N_ELEMENTS(fittedHR)-1] GE $
    ;              fitmin2HR + (fitmaxHR - fitmin2HR) * fract_thresh, cnt)
              relstop = WHERE(fittedHR[idMax_xHR : idEndMin_xHR] GE $
                fitmin2HR + (fitmaxHR - fitmin2HR) * fract_thresh, cnt)
              stopgsHR_ind = idMax_xHR + relstop [N_ELEMENTS(relstop) - 1]
              stopgsHR_value = xHR[stopgsHR_ind]
              ;stop 2, computed with fract_thresh2
    ;            relstop = WHERE(fittedHR [idMax_xHR:N_ELEMENTS(fittedHR)-1] GE $
    ;              fitmin2HR + (fitmaxHR - fitmin2HR) * fract_thresh2, cnt)
              relstop = WHERE(fittedHR [idMax_xHR:idEndMin_xHR] GE $
                fitmin2HR + (fitmaxHR - fitmin2HR) * fract_thresh2, cnt)
              stopgs2HR_ind = idMax_xHR + relstop [N_ELEMENTS(relstop) - 1]
              stopgs2HR_value = xHR[stopgs2HR_ind]
              ; EOS for completeness
    ;            relstop = WHERE(fittedHR [idMax_xHR:N_ELEMENTS(fittedHR)-1] GE $
    ;              fitmin2HR + (fitmaxHR - fitmin2HR) * Tc, cnt)
              relstop = WHERE(fittedHR [idMax_xHR : idEndMin_xHR-1] GE $
                fitmin2HR + (fitmaxHR - fitmin2HR) * Tc, cnt)
              stopgsTcHR_ind = idMax_xHR + relstop [N_ELEMENTS(relstop) - 1]
              stopgsTcHR_value = xHR[stopgsTcHR_ind]
              ;  Compute the accumulated FAPAR value (integral) over the growing
              ;  season:
              accumHR=TOTAL(fittedHR[startgsHR_ind: stopgsHR_ind]*dtJD, /DOUBLE)
              accumbHR=TOTAL((fittedHR[startgsHR_ind: stopgsHR_ind]-fitmin1HR)*dtJD, /DOUBLE)
              accum2HR=TOTAL(fittedHR[startgsHR_ind: stopgs2HR_ind]*dtJD, /DOUBLE)
              accum2bHR=TOTAL((fittedHR[startgsHR_ind: stopgs2HR_ind]-fitmin1HR)*dtJD, /DOUBLE)
                
              fitmax = fitmaxHR
              fitmax_idx = idMax_xHR
              fitmax_t = xHR[idMax_xHR]
              startgs = startgsHR_value
              startgsTc = startgsTcHR_value 
              stopgs = stopgsHR_value
              stopgs2 = stopgs2HR_value
              stopgsTc = stopgsTcHR_value
              strtgsdelta = strtgsdeltaHR
              accum = accumHR
              accumb = accumbHR
              accum2 = accum2HR
              accum2b = accum2bHR
            ENDIF ELSE BEGIN
              gsflag[c,y] = 0
            ENDELSE 
          ENDIF ;(gsflag[c,y] EQ 10)
          IF (gsflag[c,y] NE 10) THEN BEGIN
            fitmax = !VALUES.F_NAN ;0.1
            fitmax_idx = !VALUES.F_NAN ;gsinistartDek[c,y] + 1
            fitmax_t = !VALUES.F_NAN ;gsinistartDek[c,y] + 1
            startgs = !VALUES.F_NAN ;gsinistartDek[c,y]
            startgsTc = !VALUES.F_NAN 
            stopgs = !VALUES.F_NAN ;gsiniendDek[c,y]
            stopgs2 = !VALUES.F_NAN ;gsiniendDek[c,y]
            stopgsTc = !VALUES.F_NAN
            strtgsdelta = !VALUES.F_NAN ;gsiniendDek[c,y]
            accum=!VALUES.F_NAN
            accumb=!VALUES.F_NAN
            accum2=!VALUES.F_NAN
            accum2b=!VALUES.F_NAN
            pdhtf_base [c,y] = !VALUES.F_NAN
            pdhtf_amp1 [c,y] = !VALUES.F_NAN
            pdhtf_sft1 [c,y] = !VALUES.F_NAN
            pdhtf_slo1 [c,y] = !VALUES.F_NAN
            pdhtf_amp2 [c,y] = !VALUES.F_NAN
            pdhtf_sft2 [c,y] = !VALUES.F_NAN
            pdhtf_slo2 [c,y] = !VALUES.F_NAN
            gsinistartDek[c,y] = !VALUES.F_NAN
            gsiniendDek[c,y] = !VALUES.F_NAN
            ;  Print diagnostic messages and continue, keeping in mind that
            ;  the model fitting procedure may not have worked:
            IF (vrbs eq 1) THEN BEGIN
              print, 'The PDHTF model could not be fitted,'
              print, 'skip to the next possible growing season.'
            ENDIF
          ENDIF ;(gsflag[c,y] NE 10)
    
  
          
        ;MM+: the following part has been excluded for the time being  
        ;  In the case of an abrupt end of the season (e.g., harvesting,
        ;  leaving bare ground), the end of the season as defined above
        ;  may take place on a period where the FAPAR is still quite high,
        ;  while the next valid value is very low. In this case, it may be
        ;  best to set the end of the season the the first valid low value
        ;  instead:
          
    ;      IF ((gsflag[c,y] EQ 10) AND (fapar [stopgs] GE fap75pctl) $
    ;        AND (fapar [stopgs] LE iabo)) THEN BEGIN
    ;        if (vrbs eq 1) then print, 'stopgs was initially set to ', stopgs
    ;        REPEAT BEGIN
    ;          stopgs = stopgs + 1
    ;          ;MM+ if the end of the profile is reached, stop there
    ;          if stopgs eq npts - 1 then break
    ;          ;MM+ 
    ;          ;ENDREP UNTIL ((fapar [stopgs] LT fap50pctl) AND $ ;original version
    ;        ENDREP UNTIL ((fapar [stopgs] LT fap75pctl) AND $
    ;          (fapar [stopgs] GE ibel) AND (stopgs LE npts - 1))
    ;        if (vrbs eq 1) then print, 'stopgs has been delayed to ', stopgs
    ;      ;MM+ 
    ;      ;print, 'because fapar [stopgs] was > fap50pctl.'  ;original version
    ;        if (vrbs eq 1) then print, 'because fapar [stopgs] was > fap75pctl.'
    ;      ENDIF
     
        ;  Store all results for this growing season:
          gsstrt [c,y] = startgs
          gsstrtTc [c,y] = startgsTc
          gsstop [c,y] = stopgs
          gsstop2 [c,y] = stopgs2
          gsstopTc  [c,y] = stopgsTc
          gsleng [c,y] = stopgs - startgs + 1
          gsleng2 [c,y] = stopgs2 - startgs + 1
          gsstrtdelta[c,y]=strtgsdelta
          gsmaxv [c,y] = fitmax
          gsmaxt [c,y] = fitmax_t
          gsaccu [c,y] = accum
          gsaccub [c,y] = accumb
          gsaccue [c,y] = accum2
          gsaccueb [c,y] = accum2b
          
          if (gsflag[c,y] EQ 10) AND (pltres eq 1 ) then begin
            IF (wantplotgs NE 0) THEN rc = phenot_plt_gs_JDmm(c, y)  ;MM
          ENDIF
          ;### (END) FIT THE MODEL ON OW DEFINED BY [gsinistartDek[c,y], gsiniendDek[c,y]] 
        ENDIF ELSE BEGIN;IF ((goodf95pctl - goodf05pctl) GT faprangeminthresh/2.0) OR (rc05 EQ 0) OR (rc95 EQ 0) THEN BEGIN
          ;there was no enough variability in the locally adjusted window, 
          gsflag[c,y]=-2
          ;store appropriate results
          gsstrt [c,y] = -999 & gsstop [c,y] =  -999
          gsstop2 [c,y] =  -999 &  gsstopTc  [c,y] = -999
          gsleng [c,y] =  -999 & gsleng2 [c,y] =  -999 & pdhtf_base [c,y] = -999
          pdhtf_amp1 [c,y] = -999 & pdhtf_sft1 [c,y] = -999
          pdhtf_slo1 [c,y] = -999 & pdhtf_amp2 [c,y] = -999
          pdhtf_sft2 [c,y] = -999 & pdhtf_slo2 [c,y] = -999
          gsstrtdelta[c,y]= !VALUES.F_NAN & gsmaxv [c,y] = 0.0
          gsmaxt [c,y] =  -999 & gsaccu [c,y] = 0.0
          gsaccub [c,y] = 0.0 & gsaccue [c,y] = 0.0 & gsaccueb [c,y] = 0.0 
        ENDELSE
      ENDELSE ;IF ((fap95pctl - fap05pctl) LT faprangeminthresh) THEN BEGIN
    ENDELSE ;IF (owe-ows) LT 7
;    JUMP1:
    ;  Record the fact that the next growing season has been documented:
    numgs = numgs + 1
    
    if (vrbs eq 1) then begin
      print, 'GS starts  = ', gsstrt [c,y], ' GS stops  = ', gsstop [c,y]
      ;print, 'GS length gsleng [c,y] = ', gsleng [c,y]
      ;print, 'GS reaches maximum gsmaxv [c,y] = ', gsmaxv [c,y]
      ;print, 'GS max reached in period gsmaxt [c,y] = ', gsmaxt [c,y]
      ;print, 'Accumulated FAPAR during GS gsaccu [c,y] = ', gsaccu [c,y]
    endif

  ;  Go on with the next iteration:
    i = i + 1  ;growing season counter
  ENDFOR ;FOR c = 0, ngspy-1 DO BEGIN
  
ENDFOR  ;FOR y=0, n_full_solar_years DO BEGIN 
if (vrbs eq 1) then print, 'required deltas for SOS=', gsstrtdelta
;MM15, if 2 GS were detected, look that they are both existing, if one is always missing, redo stat and
;constrain the ngspy = 1
IF (ngspy EQ 2) THEN BEGIN
  rel1 = WHERE(gsrel [0,*] LT 1000, countRel1)
  rel2 = WHERE(gsrel [1,*] LT 1000, countRel2)
  foundGS1 = WHERE(gsflag[0,rel1] GT 0, countGS1)
  foundGS2 = WHERE(gsflag[1,rel2] GT 0, countGS2)
  IF (countGS1 LT MNGS) OR (countGS2 LT MNGS) THEN BEGIN 
    empty_season = 1  ;this var is in common block
    IF (countGS1 LT MNGS) AND (countGS2 LT MNGS) THEN empty_season = 2
    retcode = 10
  ENDIF
ENDIF
RETURN, retcode

END