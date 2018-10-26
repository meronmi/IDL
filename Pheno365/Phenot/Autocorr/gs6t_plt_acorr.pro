FUNCTION gs6t_plt_acorr

;  Purpose:
;     To plot the autocorrelograms of the FAPAR time series.

;  Outcome:
;     The autocorrelogram graphs are generated on screen and two files
;     containing the same graphs are created in the output directory.

;  Usage:
;     rc = gs6t_plt_acorr ()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.

;  Remarks: None.

;  History:
;     Version: 6.0
;     Release: 1.0.
;     Date: 2007-09-20.

;  Include the cb_os common block:
@cb_os.comm

;  Include the cb_job common block:
@cb_job.comm

;  Include the cb_in common block:
@cb_in.comm

;  Include the cb_stat common block:
@cb_stat.comm

;  ========================================
;  Set the level of diagnostic information to output:
diag = 0
IF (diag GT 0) THEN BEGIN
   PRINTF, diag_unit, '>>> Entering gs6t_plt_acorr.pro.'
   PRINTF, diag_unit
ENDIF

;  ----------------------------------------
;  Initialize the return code:
retcode = 0

;  Set the name of the time units:
CASE period OF
   1: tunit = 'days'
   10: tunit = 'decades'
   30: tunit = 'months'
ENDCASE

;  Generate a set of lags:
nlags = N_ELEMENTS (cor)
lags = INDGEN (nlags)

;  Plot the FAPAR autocorrelogram on the screen:
SET_PLOT, screen
WINDOW, /FREE, TITLE = "Autocorrelation"
!P.MULTI = [0, 0, 0, 0, 0]

PLOT, $
   lags, $
   cor, $
   XSTYLE = 1, $
;   MIN_VALUE = -2.0, $
;   MAX_VALUE = 2.0, $
;   XRANGE = [0, N_ELEMENTS (cor)], $
;   YRANGE = [-1.0, 1.0], $
;   PSYM = 0, $
   XTITLE = 'Time lags [' + tunit + ']', $
   YTITLE = 'FAPAR Autocorrelogram', $
   TITLE = site_name + ' [' + $
      STRTRIM (STRING (site_lat), 2) + ', ' + $
      STRTRIM (STRING (site_lon), 2) + ']', $
      BACKGROUND = 16777215, COLOR = 0 

;  Save the same plot in an Encapsulated PostScript graphics file in
;  the output directory:
;fn = STRMID (data_fname, 0, STRLEN (data_fname) - 4)
;SET_PLOT, 'PS'
;DEVICE, /ENCAPSULATED, FILENAME = run_dir + fn + '_acorr.eps'
;!P.MULTI = [0, 0, 0, 0, 0]
;PLOT, $
;   lags, $
;   cor, $
;;   MIN_VALUE = -2.0, $
;;   MAX_VALUE = 2.0, $
;;   XRANGE = [0, N_ELEMENTS (cor)], $
;;   YRANGE = [-1.0, 1.0], $
;;   PSYM = 0, $
;   XTITLE = 'Time lags [' + tunit + ']', $
;   YTITLE = 'FAPAR Autocorrelogram', $
;   TITLE = site_name + ' [' + $
;      STRTRIM (STRING (site_lat), 2) + ', ' + $
;      STRTRIM (STRING (site_lon), 2) + ']'
;DEVICE, /CLOSE
;SET_PLOT, screen

;IF (wantsmooth EQ 1) THEN BEGIN
;;  Plot the smoothed FAPAR autocorrelogram on the screen:
;   SET_PLOT, screen
;   WINDOW, /FREE
;   !P.MULTI = [0, 0, 0, 0, 0]

;   PLOT, $
;      periods, $
;      sm_cor, $
;      XRANGE = [0, N_ELEMENTS (sm_cor)], $
;;   YRANGE = [-1.0, 1.0], $
;      PSYM = 0, $
;      XTITLE = 'Time lags (in ' + tunit + ')', $
;      YTITLE = 'Smoothed FAPAR Autocorrelogram', $
;      TITLE = site_name + ' [' + $
;         STRTRIM (STRING (site_lat), 2) + ', ' + $
;         STRTRIM (STRING (site_lon), 2) + ']'

;;  Save the same plot in an Encapsulated PostScript graphics file in
;;  the output directory:
;   SET_PLOT, 'PS'
;   DEVICE, /ENCAPSULATED, FILENAME = run_dir + fn + '_sm_acorr.eps'
;   !P.MULTI = [0, 0, 0, 0, 0]
;   PLOT, $
;      periods, $
;      sm_cor, $
;      XRANGE = [0, N_ELEMENTS (sm_cor)], $
;;   YRANGE = [-1.0, 1.0], $
;      PSYM = 0, $
;      XTITLE = 'Time lags (in ' + tunit + ')', $
;      YTITLE = 'Smoothed FAPAR Autocorrelogram', $
;      TITLE = site_name + ' [' + $
;         STRTRIM (STRING (site_lat), 2) + ', ' + $
;         STRTRIM (STRING (site_lon), 2) + ']'
;   DEVICE, /CLOSE
;   SET_PLOT, screen
;ENDIF

;  ----------------------------------------
IF (diag GT 0) THEN BEGIN
   PRINTF, diag_unit, '<<< Exiting gs6t_plt_acorr.pro.'
   PRINTF, diag_unit, ''
ENDIF

;  ========================================
RETURN, retcode

END
