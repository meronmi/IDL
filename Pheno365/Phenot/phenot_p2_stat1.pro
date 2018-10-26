FUNCTION phenot_p2_stat1

;  Purpose:
;     To compute and save the basic statistical information on the
;     FAPAR time series.

;  Outcome:
;     Non-parametric statistics on the FAPAR time series are available
;     in a text file

;  Usage:
;     rc = phenot_p2_stat1()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.
;    10: More than 40% of the data are missing values.
;    20: The variability exhibited by the FAPAR time series appears to be too
;        small: 95th - 5th percentile is less than faprangeminthresh. Only
;        basic statistics (e.g., percentiles) have been computed.

;  History:
;     Version 1.0: This routine includes the first part of the old routine
;        'phenot_p2_stat.pro', which had become unmanageable in size. This
;        latter routine was itself an updated version of 'gs6t_p2_stat.pro',
;        ported to IDL 7.0 on 24 September 2008.
;        Current version dates 10 March 2009.
;     Version 1.1: Port under Suse Linux (starting February 2010).

;  Include the necessary common block(s):
@cb_os.comm
@cb_job.comm
@cb_in.comm
@cb_stat.comm
;MM+
@cb_options.comm
;  Initialize the return code:
retcode = 0

;  Define the array containing the index to the non-missing (valid) values:
nomiss = WHERE((fapar GE ibel) AND (fapar LE iabo), count)

;  Check that at least 60% of the reported values are valid:
minnumval = LONG(npts * 0.6)
IF (count LE minnumval) THEN BEGIN
   retcode = 10
   if (vrbs eq 1) then begin
     PRINT, '*** phenot_p2_stat1.pro [ERROR]:'
     PRINT, '    More than 40% of the values in the input record are invalid.'
     PRINT, '    No statistics computed.'
     PRINT
   endif
   RETURN, retcode
ENDIF

;  Record the periods for which the data are valid, the valid FAPAR values and
;  the number of non-missing values:
per_nomiss = FLOAT(periods [nomiss])
fap_nomiss = fapar[nomiss]
nfap_nomiss = count

;  Acquire basic (non-parametric) statistical information about the series.

;  Compute the absolute minimum and absolute maximum valid (non-missing)
;  values:
fapmin = MIN(fap_nomiss, MAX = max)
fapmax = max

;  Indicate whether the fapar series may contain missing values and whether it
;  is already sorted (0 = no, 1 = yes):
fmiss = 1
fsort = 0

;  Compute the 5th, 25th, 50th, 75th, and 95th percentiles:
; MM+ always compute 5, 50 and 95, other only if required (vrbs)
per = 0.05
rc = prctl(per, fapar, fmiss, ibel, iabo, fsort, val)
IF (rc NE 0) THEN BEGIN
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2_stat1 [ERROR]:'
    PRINT, '    Return code from prctl = ', rc, ' for per = ', per
    PRINT
   endif
   RETURN, retcode
ENDIF ELSE BEGIN
   fap05pctl = val
ENDELSE

per = 0.95
rc = prctl(per, fapar, fmiss, ibel, iabo, fsort, val)
IF (rc NE 0) THEN BEGIN
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2_stat1 [ERROR]:'
    PRINT, '    Return code from prctl = ', rc, ' for per = ', per
    PRINT
   endif
   RETURN, retcode
ENDIF ELSE BEGIN
   fap95pctl = val
ENDELSE

per = 0.50
rc = prctl(per, fapar, fmiss, ibel, iabo, fsort, val)
IF (rc NE 0) THEN BEGIN
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2_stat1 [ERROR]:'
    PRINT, '    Return code from prctl = ', rc, ' for per = ', per
    PRINT
   endif
   RETURN, retcode
ENDIF ELSE BEGIN
   fap50pctl = val
ENDELSE

if (vrbs eq 1) then begin 
  per = 0.25
  rc = prctl(per, fapar, fmiss, ibel, iabo, fsort, val)
  IF (rc NE 0) THEN BEGIN
     if (vrbs eq 1) then begin
      PRINT, '*** phenot_p2_stat1 [ERROR]:'
      PRINT, '    Return code from prctl = ', rc, ' for per = ', per
      PRINT
     endif
     RETURN, retcode
  ENDIF ELSE BEGIN
     fap25pctl = val
  ENDELSE
   
  per = 0.75
  rc = prctl(per, fapar, fmiss, ibel, iabo, fsort, val)
  IF (rc NE 0) THEN BEGIN
     if (vrbs eq 1) then begin
      PRINT, '*** phenot_p2_stat1 [ERROR]:'
      PRINT, '    Return code from prctl = ', rc, ' for per = ', per
      PRINT
     endif
     RETURN, retcode
  ENDIF ELSE BEGIN
     fap75pctl = val
  ENDELSE
endif


;  Set the expected minimum variability value to 0.15 because the typical
;  accuracy of FAPAR estimates is about 0.1 (in absolute value): The observed
;  variability must thus significantly exceed the possible level of noise.
;  WARNING: This value is valid only for FAPAR. For a more generic code, it may
;  be more appropriate to test for such a difference larger than some value
;  depending on the intrinsic declared accuracy of that variable instead.
;  Action: If the variability is too low, return to the calling routine as the
;  rest of the calculations are appropriate only for seasonal time series.
;MM+ was not running on part of the Niger profiles
;MM+faprangeminthresh = 0.15
;;MM+ moved to phenot and phenot_image
;faprangeminthresh = 0.10

;  Save the results in a text file:
vrbs2 = 0
if (vrbs2 eq 1) then begin
  ;  Set the full name of the statistical output file:
  stat1_fspec = run_dir + 'stat1.txt'

  ;  Open the output file for statistical information:
  GET_LUN, stat_unit
  OPENW, stat_unit, stat1_fspec

  PRINTF, stat_unit, 'Results from the non-parametric statistical analysis:'
  PRINTF, stat_unit

  PRINTF, stat_unit, 'I/O:'
  PRINTF, stat_unit
  PRINTF, stat_unit, 'Input directory (input_dir) ='
  PRINTF, stat_unit, '   ' + input_dir
  PRINTF, stat_unit, 'Input data file (data_fname) ='
  PRINTF, stat_unit, '   ' + data_fname
  PRINTF, stat_unit, 'Output directory (run_dir) = '
  PRINTF, stat_unit, '   ' + run_dir
  PRINTF, stat_unit
  PRINTF, stat_unit, 'Overall time coverage:'
  PRINTF, stat_unit
  PRINTF, stat_unit, 'First year processed (fst_year) =         ', FIX(fst_year)
  PRINTF, stat_unit, 'Last year processed (lst_year) =          ', FIX(lst_year)
  PRINTF, stat_unit, 'No. years processed (nyears) =            ', FIX(nyears)
  PRINTF, stat_unit, 'Sampling period [days] (period) =         ', FIX(period)
  PRINTF, stat_unit, 'No. values (npts) =                       ', FIX(npts)
  PRINTF, stat_unit, 'Required No. of valid values (minnumval)= ', FIX(minnumval)
  PRINTF, stat_unit, 'Actual No. valid values (nfap_nomiss) =   ', FIX(nfap_nomiss)
  PRINTF, stat_unit
  PRINTF, stat_unit, 'Global statistics on FAPAR data:'
  PRINTF, stat_unit
  PRINTF, stat_unit, 'Minimum valid value (fapmin) =            ', fapmin
  PRINTF, stat_unit, '0.05 percentile (fap05pctl) =             ', fap05pctl
  PRINTF, stat_unit, '0.25 percentile (fap25pctl) =             ', fap25pctl
  PRINTF, stat_unit, '0.50 percentile (fap50pctl) =             ', fap50pctl
  PRINTF, stat_unit, '0.75 percentile (fap75pctl) =             ', fap75pctl
  PRINTF, stat_unit, '0.95 percentile (fap95pctl) =             ', fap95pctl
  PRINTF, stat_unit, 'Maximum valid value (fapmax) =            ', fapmax
  PRINTF, stat_unit, 'Required min. range (faprangeminthresh) = ', faprangeminthresh
  PRINTF, stat_unit, 'Actual (95-05) range =                    ', fap95pctl - fap05pctl
  PRINTF, stat_unit
  
  CLOSE, stat_unit
  FREE_LUN, stat_unit
endif
;  Check that the overall variability of the FAPAR record, as measured by
;  the (95th - 5th) percentile difference, exceeds faprangeminthresh. This is to ensure
;  that the time series is not mostly or entirely composed of near-constant
;  values that do not exhibit any seasonality.
IF ((fap95pctl - fap05pctl) LT faprangeminthresh) THEN BEGIN
   retcode = 20
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2_stat1 [ERROR]:'
    PRINT, '    [95th - 5th] percentile range is too low, less than ', faprangeminthresh
    PRINT
   endif
ENDIF

;;  Update the array fapar_inter_lin, linearly interpolating the missing values indide
;;  the time series (i.e., from the first to the last valid points, but not outside of
;;  that period).
;;  WARNING: this initial implementation assumes equally spaced data, which is not exactly
;;  the case for decadal FAPAR. A correct implementation should be based on the Julian
;;  day dates of each data point.
;good = 1
;FOR i = nomiss[0], nomiss[N_ELEMENTS(nomiss) - 1] DO BEGIN
;   IF ((good EQ 1) AND ((fapar[i] LT ibel) OR (fapar GT iabo))) THEN BEGIN
;      good = 0
;      
;   ENDIF
;
;
;ENDFOR
;
;;;  Define the array containing the index to the missing (invalid) values:
;;miss_idx = WHERE((fapar LT ibel) OR (fapar GT iabo), miss_count)
;;needed = periods [miss_idx]
;;print, 'needed = ', needed
;;res_lin = INTERPOL(fapar, periods, needed)
;;print, 'res_lin = ', res_lin
;;; fapar_inter_lin [miss_idx] = res_lin
;;
;;;window, /free
;;;plot, periods, fapar, psym = 1, min_value = 0.0, max_value = 1.0
;;;oplot, miss_idx, fapar_inter_lin[miss_idx], psym = 5
;;
;;close, /all
;stop

RETURN, retcode

END