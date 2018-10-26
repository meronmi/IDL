FUNCTION phenot_p3_pdhtf

;  Purpose:
;     To fit a Parametric Double Hyperbolic Tangent Function (PDHTF)
;     7-parameter model to the appropriate periods of the FAPAR time
;     series.

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
;     The variables inistart and iniend, which define the period
;     within which a growing season is to be searched for, must be
;     declared and initialized before calling this routine.

;  History:
;     This is an updated version of Version: 6.0, Release: 1.0, of the routine
;     gs6t_p3_pdhtf.pro developed on 2007-09-20.

;  Include the necessary common block(s):
@cb_job.comm
@cb_in.comm
@cb_stat.comm
@cb_frst.comm
@cb_pdhtf.comm

;  Initialize the return code:
retcode = 0

;  Initialize the number of growing seasons found so far:
numgs = 0

;  Estimate the maximum number of expected growing seasons in the
;  FAPAR record:
maxngrowseas = nyears * ngspy

;IF (diag GT 2) THEN BEGIN
;   PRINTF, diag_unit, 'numgs = ', numgs
;   PRINTF, diag_unit, 'maxngrowseas = ', maxngrowseas
;ENDIF
print, 'Maximum No. of growing seasons (maxngrowseas): ', maxngrowseas

;  Define the arrays required to store the 7 PDHTF model parameters
;  (base value, amplitude, time shift and slope of the growth phase,
;  and amplitude, time shift and slope of the decay phase), as well
;  as the status of the CURVEFIT outcome, for each of the expected
;  growing seasons:
pdhtf_base = FLTARR(maxngrowseas)
pdhtf_amp1 = FLTARR(maxngrowseas)
pdhtf_sft1 = FLTARR(maxngrowseas)
pdhtf_slo1 = FLTARR(maxngrowseas)
pdhtf_amp2 = FLTARR(maxngrowseas)
pdhtf_sft2 = FLTARR(maxngrowseas)
pdhtf_slo2 = FLTARR(maxngrowseas)

;  Define the arrays required to store the growing season characteristics:
gsstrt = INTARR(maxngrowseas)
gsstop = INTARR(maxngrowseas)
gsleng = INTARR(maxngrowseas)
gsmaxv = FLTARR(maxngrowseas)
gsmaxt = INTARR(maxngrowseas)
gsaccu = FLTARR(maxngrowseas)

;  Define the arrays required to store the diagnostics of the
;  inversion procedures:
gsstat = INTARR(maxngrowseas)
gschis = FLTARR(maxngrowseas)

;  Define the quality flag stating the reliability of the growing
;  season information:
gsgood = INTARR(maxngrowseas)

;  Define the number of complete growing seasons found so far:
numgs = 0

;  Define the array that will contain the estimated best guesses for
;  the missing values in the FAPAR record, based on the model fit:
missfapar = FLTARR(npts)

;  Define the array that will contain the optimally estimated FAPAR
;  values during the various growing seasons:
fitted = FLTARR(npts)

;  Define the array containing the model parameters for a particular
;  model fit:
pdhtf_pars = FLTARR(7)

;IF (diag GT 2) THEN BEGIN
;   PRINTF, diag_unit, 'At the start of the REPEAT loop, iniend = ', $
;      iniend
;ENDIF
print, 'At the start of the REPEAT loop, iniend = ', iniend

;  Loop over all expected growing seasons:
i = 0
REPEAT BEGIN

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, '*** Search for GS #', i + 1
;   ENDIF
   print, '>>> Search for GS #', i + 1

;  Set the start and end of the next period within which to search for
;  a growing season, and compute and locate the maximum value of the
;  smoothed time series within this period:
   rc = phenot_p3_inper()

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'Upon returning from gs6t_p3_inper.pro,'
;      PRINTF, diag_unit, 'rc = ', rc
;      PRINTF, diag_unit, 'inistart = ', inistart
;      PRINTF, diag_unit, 'iniend = ', iniend
;      PRINTF, diag_unit, 'fmax (based on smoothfapar) = ', fmax
;      PRINTF, diag_unit, 'fmax_idx (based on smoothfapar) = ', fmax_idx
;   ENDIF
print, 'After phenot_p3_inper,'
print, 'inistart = ', inistart
print, 'iniend = ', iniend
print, 'fmax (based on fapar) = ', fmax
print, 'fmax_idx (based on fapar) = ', fmax_idx

;  If rc = 1, set a local flag to indicate that the FAPAR record
;  probably contains an incomplete growing season for which the
;  statistics (start and especially end) would be unreliable:
   IF (rc EQ 1) THEN BEGIN
      incomplete = 1
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, $
;            'This period most probably contains an incomplete growing season.'
;      ENDIF
print, 'This period most probably contains an incomplete growing season.'
   ENDIF ELSE BEGIN
      incomplete = 0
   ENDELSE

;  Collect the valid values to be used for the model fitting:
   ndat = iniend - inistart + 1
   goodfi = inistart + $
      WHERE((fapar [inistart:iniend] GE ibel) $
      AND (fapar [inistart:iniend] LE iabo), ngood)
   goodf = fapar [goodfi]
   nbad = ndat - ngood

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'fapar [inistart:iniend] = '
;      PRINTF, diag_unit, fapar [inistart:iniend]
;      PRINTF, diag_unit, 'goodfi = '
;      PRINTF, diag_unit, goodfi
;      PRINTF, diag_unit, 'goodf = '
;      PRINTF, diag_unit, goodf
;      PRINTF, diag_unit, 'This sequence contains ', ndat, ' data points.'
;      PRINTF, diag_unit, 'This sequence contains ', ngood, ' valid points.'
;      PRINTF, diag_unit, 'This sequence contains ', nbad, ' missing values.'
;   ENDIF

;  If the number of missing points equals or exceeds a quarter of
;  the total, and especially if these are clustered near
;  the start or end of the period, the variables inistart
;  and iniend may need to be adjusted so that the model
;  fitting is attempted only over the period which contains
;  a reasonable number of consecutive valid values:
   IF (nbad GE FIX(ndat / 4.0)) THEN BEGIN
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, $
;            'A quarter or more of the data points in this period are missing.'
;      ENDIF
print, 'A quarter or more of the data points in this period are missing.'
;  Compute the ranks (timing) of the missing observations
;  corresponding to 1/3 and 2/3 of the missing values within
;  the current [inistart, iniend] period:
      nmiss = 0
      rnk13 = 0
      rnk13found = 0
      rnk23 = 0
      rnk23found = 0
      FOR j = inistart, iniend DO BEGIN
         IF ((fapar [j] LT ibel) OR (fapar [j] GT iabo)) THEN nmiss = nmiss + 1
         IF (nmiss GE FIX(nbad * 1.0 / 3.0) AND (rnk13found EQ 0)) THEN BEGIN
            rnk13 = j
            rnk13found = 1
         ENDIF
         IF (nmiss GE FIX(nbad * 2.0 / 3.0) AND (rnk23found EQ 0)) THEN BEGIN
            rnk23 = j
            rnk23found = 1
         ENDIF
      ENDFOR
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'rnk13 = ', rnk13
;         PRINTF, diag_unit, 'rnk23 = ', rnk23
;      ENDIF
;  If 2/3 or more of the missing values are within the
;  first half of the period of interest (or, equivalently
;  1/3 or less of the missing values are within the second
;  half), adjust inistart:
      newstrt = 0
      newend = 0
      midper = inistart + FIX((iniend - inistart) / 2.0)
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'midper = ', midper
;      ENDIF
      IF (rnk23 LT midper) THEN BEGIN
         newstrt = rnk23
;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, 'Start looking for new start after rnk23.'
;         ENDIF
         REPEAT BEGIN
            newstrt = newstrt + 1
         ENDREP UNTIL ((fapar [newstrt] GE ibel) AND (fapar [newstrt] LE iabo))
;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, '2/3 of the missing values are found in the'
;            PRINTF, diag_unit, 'first half of the period; inistart delayed to ', $
;               newstrt
;         ENDIF
print, '2/3 of the missing values are found in the'
print, 'first half of the period; inistart delayed to ', newstrt
         inistart = newstrt
      ENDIF
;  Similarly, if 2/3 or more of the missing values are
;  within the second half of the period of interest (or,
;  equivalently 1/3 or less of the missing values are
;  within the first half), adjust iniend:
      IF (rnk13 GT midper) THEN BEGIN
         newend = rnk13
;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, 'Start looking for new end before rnk23.'
;         ENDIF
         REPEAT BEGIN
            newend = newend - 1
         ENDREP UNTIL ((fapar [newend] GE ibel) AND (fapar [newend] LE iabo))
;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, '2/3 of the missing values are found in the'
;            PRINTF, diag_unit, 'second half of the period; iniend advanced to ', $
;               newend
;         ENDIF
print, '2/3 of the missing values are found in the'
print, 'second half of the period; iniend advanced to ', newend
         iniend = newend
      ENDIF

;  If either inistart or iniend has been reset, re-collect the valid
;  values to be used for the model fitting in this new period:
      IF ((newstrt NE 0) OR (newend NE 0)) THEN BEGIN
         ndat = iniend - inistart + 1
         goodfi = inistart + $
            WHERE ((fapar [inistart:iniend] GE ibel) $
            AND (fapar [inistart:iniend] LE iabo), ngood)
         goodf = fapar [goodfi]
         nbad = ndat - ngood

;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, 'inistart or iniend may have been adjusted:'
;            PRINTF, diag_unit, 'fapar [inistart:iniend] = '
;            PRINTF, diag_unit, fapar [inistart:iniend]
;            PRINTF, diag_unit, 'goodfi = '
;            PRINTF, diag_unit, goodfi
;            PRINTF, diag_unit, 'goodf = '
;            PRINTF, diag_unit, goodf
;            PRINTF, diag_unit, 'This new sequence contains ', ndat, ' data points.'
;            PRINTF, diag_unit, 'This new sequence contains ', ngood, ' valid points.'
;            PRINTF, diag_unit, 'This new sequence contains ', nbad, ' missing values.'
;         ENDIF
      ENDIF ;ELSE BEGIN
;         IF (diag GT 2) THEN BEGIN
;            PRINTF, diag_unit, 'Neither inistart nor iniend have been modified.'
;         ENDIF
;      ENDELSE
   ENDIF

;  Make sure the first and last good values within that new
;  period are at least as low as the median (50th) percentile
;  of the entire distribution, and that there is a local maximum
;  above that value, to avoid trying to fit a growing season
;  through a partial record where only the initial rise (or
;  only the final decline) is available (e.g., near the start
;  or end of the record): [For now, this is only a diagnostic]:
   IF ((goodf [0] GT fap50pctl) $
      OR (goodf [N_ELEMENTS(goodf) - 1] GT fap50pctl) $
      OR (MAX(goodf) LT fap50pctl)) THEN BEGIN
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'The revised period from inistart = ', inistart
;         PRINTF, diag_unit, 'to iniend = ', iniend
;         PRINTF, diag_unit, 'may not contain a well-formed growing season...'
;      ENDIF
print, 'The revised period from inistart = ', inistart
print, 'to iniend = ', iniend
print, 'may not contain a well-formed growing season...'
   ENDIF

;  Re-define the array containing the fitting weights, of the same
;  dimension as goodf, and assign their values. Currently, these are
;  all set to 1.0:
   weights = FLTARR(N_ELEMENTS(goodf))
   weights [*] = 1.0

;  Estimate the best initial guess values for the 7 PDHTF model
;  parameters for the this growing season on the basis of actual
;  valid FAPAR values:
;  Set the initial base value to the first valid value in the period of interest:
   pdhtf_pars [0] = goodf [0]
;  Set the range for the growth phase to the difference between the local
;  maximum and this initial base value (The factor 2 is included because
;  the model dividex this amplitude by 2):
   pdhtf_pars [1] = (fmax - goodf [0]) * 2.0
;  Set the timining of the maximum growth rate as the middle point
;  between the start of the period and the time of the local maximum:
   pdhtf_pars [2] = (inistart + fmax_idx) / 2.0
;  Set the initial guess growth slope:
   IF (ngspy EQ 1) THEN pdhtf_pars [3] = 0.5 ELSE pdhtf_pars [3] = 1.0
;  Set the range for the decay phase to the difference between the local
;  maximum and last valid value (The factor 2 is included because
;  the model dividex this amplitude by 2):
   pdhtf_pars [4] = (fmax - goodf [N_ELEMENTS(goodf) - 1]) * 2.0
;  Set the timining of the maximum decay rate as the middle point
;  between the time of the local maximum and the end of the period:
   pdhtf_pars [5] = (fmax_idx + iniend) / 2.0
;  Set the initial guess decay slope:
   IF (ngspy EQ 1) THEN pdhtf_pars [6] = -0.5 ELSE pdhtf_pars [6] = -1.0

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'Initial guess values:'
;      PRINTF, diag_unit, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0], $
;         ' [0.0, 1.0].'
;      PRINTF, diag_unit, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1], $
;         ' [0.0, 2.0].'
;      PRINTF, diag_unit, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
;      PRINTF, diag_unit, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3], $
;         ' [0.5 or 1.0].'
;      PRINTF, diag_unit, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4], $
;         ' [0.0, 2.0].'
;      PRINTF, diag_unit, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
;      PRINTF, diag_unit, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6], $
;         ' [-0.5 or -1.0].'
;   ENDIF
print, 'Initial guess values:'
print, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0], ' [0.0, 1.0].'
print, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1], ' [0.0, 2.0].'
print, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
print, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3], ' [0.5 or 1.0].'
print, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4], ' [0.0, 2.0].'
print, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
print, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6], ' [-0.5 or -1.0].'

;  Fit the PDHTF model to the actual FAPAR values within the
;  sub-period if there are enough valid data points:
   IF (N_ELEMENTS(goodf) GT 7) THEN BEGIN
      res = CURVEFIT(goodfi, goodf, weights, pdhtf_pars, $
         FUNCTION_NAME = 'pdhtf', /NODERIVATIVE, $
         CHISQ = chi, STATUS = stat)
print, 'status of CURVEFIT = ', stat
      IF (stat EQ 0) THEN gsgood [i] = 1 ELSE gsgood [i] = 0
   ENDIF ELSE BEGIN
      chi = -1.0
      stat = -1
      retcode = 2
   ENDELSE
   gsstat [i] = stat
   gschis [i] = chi

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'res = ', res
;      PRINTF, diag_unit, 'Retrieved values:'
;      PRINTF, diag_unit, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0]
;      PRINTF, diag_unit, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1]
;      PRINTF, diag_unit, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
;      PRINTF, diag_unit, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3]
;      PRINTF, diag_unit, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4]
;      PRINTF, diag_unit, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
;      PRINTF, diag_unit, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6]
;      PRINTF, diag_unit, 'gsstat [' + STRTRIM (STRING (i), 2) + '] = ', $
;         gsstat [i]
;      PRINTF, diag_unit, 'gschis [' + STRTRIM (STRING (i), 2) + '] = ', $
;         gschis [i]
;   ENDIF
print, 'Retrieved values:'
print, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0]
print, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1]
print, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
print, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3]
print, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4]
print, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
print, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6]
print, 'gsstat [' + STRTRIM(STRING(i), 2) + '] = ', gsstat [i]
print, 'gschis [' + STRTRIM(STRING(i), 2) + '] = ', gschis [i]

;;  If the optimization procedure failed, re-try fitting the smoothed
;;  FAPAR record:
;   IF (stat GT 0) THEN BEGIN
;      goodfi = inistart + $
;         WHERE ((smoothfapar [inistart:iniend] GE ibel) $
;         AND (smoothfapar [inistart:iniend] LE iabo))
;      goodf = smoothfapar [goodfi]
;      pdhtf_pars [0] = goodf [0]
;      pdhtf_pars [1] = fmax - goodf [0]
;      pdhtf_pars [2] = (inistart + fmax_idx) / 2.0
;      IF (ngspy EQ 1) THEN pdhtf_pars [3] = 0.5 ELSE pdhtf_pars [3] = 1.0
;      pdhtf_pars [4] = fmax - goodf [N_ELEMENTS (goodf) - 1]
;      pdhtf_pars [5] = (fmax_idx + iniend) / 2.0
;      IF (ngspy EQ 1) THEN pdhtf_pars [6] = -0.5 ELSE pdhtf_pars [6] = -1.0
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, '   +++ WARNING: Optimization failed' + $
;            ' on first attempt.'
;         PRINTF, diag_unit, 'New initial guess values:'
;         PRINTF, diag_unit, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0]
;         PRINTF, diag_unit, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1]
;         PRINTF, diag_unit, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
;         PRINTF, diag_unit, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3]
;         PRINTF, diag_unit, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4]
;         PRINTF, diag_unit, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
;         PRINTF, diag_unit, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6]
;      ENDIF
;      IF (N_ELEMENTS (nelm) GT 7) THEN BEGIN
;         res = CURVEFIT (goodfi, goodf, weights, pdhtf_pars, $
;            FUNCTION_NAME = 'pdhtf', /NODERIVATIVE, STATUS = stat)
;      ENDIF ELSE BEGIN
;         chi = -1.0
;         stat = -1
;         retcode = 4
;      ENDELSE
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'res = ', res
;         PRINTF, diag_unit, 'New retrieved values:'
;         PRINTF, diag_unit, 'pdhtf_pars [0]   (base) = ', pdhtf_pars [0]
;         PRINTF, diag_unit, 'pdhtf_pars [1] (range1) = ', pdhtf_pars [1]
;         PRINTF, diag_unit, 'pdhtf_pars [2] (shift1) = ', pdhtf_pars [2]
;         PRINTF, diag_unit, 'pdhtf_pars [3] (slope1) = ', pdhtf_pars [3]
;         PRINTF, diag_unit, 'pdhtf_pars [4] (range2) = ', pdhtf_pars [4]
;         PRINTF, diag_unit, 'pdhtf_pars [5] (shift2) = ', pdhtf_pars [5]
;         PRINTF, diag_unit, 'pdhtf_pars [6] (slope2) = ', pdhtf_pars [6]
;         PRINTF, diag_unit, 'gsstat [' + STRTRIM (STRING (i), 2) + '] = ', $
;            gsstat [i]
;         PRINTF, diag_unit, 'gschis [' + STRTRIM (STRING (i), 2) + '] = ', $
;            gschis [i]
;      ENDIF
;   ENDIF

;  If the PDHTF model fit worked, either with the original
;  series or with the smoothed series, store the results
;  for the current growing season:
   IF (stat EQ 0) THEN BEGIN
      pdhtf_base [i] = pdhtf_pars [0]
      pdhtf_amp1 [i] = pdhtf_pars [1]
      pdhtf_sft1 [i] = pdhtf_pars [2]
      pdhtf_slo1 [i] = pdhtf_pars [3]
      pdhtf_amp2 [i] = pdhtf_pars [4]
      pdhtf_sft2 [i] = pdhtf_pars [5]
      pdhtf_slo2 [i] = pdhtf_pars [6]
   ENDIF ELSE BEGIN
;  Print diagnostic messages and continue, keeping in mind that
;  the model fitting procedure may not have worked:
;      PRINTF, diag_unit, 'The PDHTF model could not be fitted,'
;      PRINTF, diag_unit, 'skip to the next possible growing season.'
print, 'The PDHTF model could not be fitted,'
print, 'skip to the next possible growing season.'
   ENDELSE

;  If the fit worked, use this fit to optimally estimate the missing FAPAR values and
;  update the corresponding array of fitted values:
   IF (stat EQ 0) THEN BEGIN
      kk = 0
      FOR k = inistart, iniend DO BEGIN
         IF ((fapar [k] LT ibel) OR $
            (fapar [k] GT iabo)) THEN BEGIN
            pdhtf, k, pdhtf_pars, f
            missfapar [k] = f
            fitted [k] = f
         ENDIF ELSE BEGIN
            fitted [k] = res [kk]
            kk = kk + 1
         ENDELSE
      ENDFOR

;      IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'k', 'fapar [k]', 'smoothfapar [k]', $
;         'missfapar [k]', 'fitted [k]', FORMAT = '(A4, 4A16)'
;         FOR k = inistart, iniend DO BEGIN
;            PRINTF, diag_unit, k, fapar [k], smoothfapar [k], $
;               missfapar [k], fitted [k], FORMAT = '(I4, 4F16.3)'
;         ENDFOR
;      ENDIF
   ENDIF

;  Define the start and end of the growing season as the dates at
;  which the simulated signal grows beyond or decays below the first
;  and last simulated value by more than 5% of the appropriate
;  amplitude:
   IF (stat EQ 0) THEN BEGIN
      fitmax = MAX(fitted [inistart:iniend], idx)
      fitmax_idx = inistart + idx
      fitmin1 = fitted [inistart]
      fitmin2 = fitted [iniend]

      relstart = WHERE(fitted [inistart:fitmax_idx] GE $
         fitmin1 + (fitmax - fitmin1) * 0.05, cnt)
      startgs = inistart + relstart [0]

      relstop = WHERE(fitted [fitmax_idx:iniend] GE $
         fitmin2 + (fitmax - fitmin2) * 0.05, cnt)
      stopgs = fitmax_idx + relstop [N_ELEMENTS(relstop) - 1]
   ENDIF ELSE BEGIN
      fitmax = 0.1
      fitmax_idx = inistart + 1
      startgs = inistart
      stopgs = iniend
   ENDELSE

;  In the case of an abrupt end of the season (e.g., harvesting,
;  leaving bare ground), the end of the season as defined above
;  may take place on a period where the FAPAR is still quite high,
;  while the next valid value is very low. In this case, it may be
;  best to set the end of the season the the first valid low value
;  instead:
   IF ((stat EQ 0) AND (fapar [stopgs] GE fap50pctl) $
      AND (fapar [stopgs] LE iabo)) THEN BEGIN
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'stopgs was initially set to ', stopgs
;      ENDIF
print, 'stopgs was initially set to ', stopgs
      REPEAT BEGIN
         stopgs = stopgs + 1
      ENDREP UNTIL ((fapar [stopgs] LT fap50pctl) AND $
         (fapar [stopgs] GE ibel) AND (stopgs LE npts - 1))
;      IF (diag GT 2) THEN BEGIN
;         PRINTF, diag_unit, 'stopgs has been delayed to ', stopgs
;         PRINTF, diag_unit, 'because fapar [stopgs] was > fap50pctl.'
;      ENDIF
print, 'stopgs has been delayed to ', stopgs
print, 'because fapar [stopgs] was > fap50pctl.'
   ENDIF

;  Compute the accumulated FAPAR value (integral) over the growing
;  season:
   IF (stat EQ 0) THEN BEGIN
      accum = 0.0
      FOR k = startgs, stopgs DO BEGIN
         accum = accum + fitted [k]
      ENDFOR
   ENDIF ELSE BEGIN
      accum = 0.0
   ENDELSE

;  Store all results for this growing season:
   gsstrt [i] = startgs
   gsstop [i] = stopgs
   gsleng [i] = stopgs - startgs + 1
;   gsmaxv [i] = fmax
;   gsmaxt [i] = fmax_idx
   gsmaxv [i] = fitmax
   gsmaxt [i] = fitmax_idx
   gsaccu [i] = accum

;  Record the fact that the next growing season has been documented:
   numgs = numgs + 1

;   IF (diag GT 2) THEN BEGIN
;      PRINTF, diag_unit, 'Characteristics of the growing season numgs = ', numgs
;      PRINTF, diag_unit, 'gsstat [i] = ', gsstat [i]
;      PRINTF, diag_unit, 'gschis [i] = ', gschis [i]
;      PRINTF, diag_unit, 'GS starts in period gsstrt [i] = ', gsstrt [i]
;      PRINTF, diag_unit, 'GS stops in period gsstop [i] = ', gsstop [i]
;      PRINTF, diag_unit, 'GS length gsleng [i] = ', gsleng [i]
;      PRINTF, diag_unit, 'GS reaches maximum gsmaxv [i] = ', gsmaxv [i]
;      PRINTF, diag_unit, 'GS max reached in period gsmaxt [i] = ', gsmaxt [i]
;      PRINTF, diag_unit, 'Accumulated FAPAR during GS gsaccu [i] = ', gsaccu [i]
;   ENDIF
print, 'Characteristics of the growing season numgs = ', numgs
print, 'gsstat [i] = ', gsstat [i]
print, 'gschis [i] = ', gschis [i]
print, 'GS starts in period gsstrt [i] = ', gsstrt [i]
print, 'GS stops in period gsstop [i] = ', gsstop [i]
print, 'GS length gsleng [i] = ', gsleng [i]
print, 'GS reaches maximum gsmaxv [i] = ', gsmaxv [i]
print, 'GS max reached in period gsmaxt [i] = ', gsmaxt [i]
print, 'Accumulated FAPAR during GS gsaccu [i] = ', gsaccu [i]

   IF (wantplotgs NE 0) THEN BEGIN
      rc = phenot_plt_gs(i, stat)
   ENDIF

;  Go on with the next iteration:
   i = i + 1

ENDREP UNTIL (i EQ maxngrowseas)

RETURN, retcode

END