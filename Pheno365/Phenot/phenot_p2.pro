FUNCTION phenot_p2

;  Purpose:
;     To pre-process a time series of FAPAR products for a given site.

;  Outcome:
;     - Time series with too many missing values are screened out.
;     - The overall statistical properties of the valid FAPAR data are
;       documented and saved in text files.
;     - A graph of the Lomb Normalized Periodogram is optionally displayed and
;       saved in an external file.
;     - The expected number of growing seasons per year is estimated.

;  Usage:
;     rc = phenot_p2()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.

;  History:
;     Version 1.0: This routine is an updated version of 'gs6t_p2.pro',
;        ported to IDL 7.0 on 24 September 2008.
;     Version 1.1: Port under Suse Linux (starting February 2010).

;  Include the necessary common block(s):
@cb_job.comm
@cb_in.comm
;MM+
@cb_options.comm
@cb_pdhtf.comm
mingoodf=7 ;minimum number of obseravtion to perform optimization

;  Initialize the return code:
retcode = 0



;  Compile basic non-parametric statistical information on the
;  FAPAR time series:
rc = phenot_p2_stat1()

IF (rc NE 0) THEN BEGIN
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2.pro [WARNING]:'
    PRINT, '    Return code from phenot_p2_stat1.pro = ', rc
   endif
   RETURN, rc
ENDIF

;  Compute the Lomb normalized periodogram for the time series and output
;  the results:
;rc = phenot_p2_stat2()
;test lombrat 
rc = phenot_p2_stat2v2()
; rc different from 0;
; 10 More than 40% of the values in the input record are invalid.'
; 20 [95th - 5th] percentile range is too low, less than faprangeminthresh
; 30 0 or more than 2 Gs has been found 
IF (rc NE 0) THEN BEGIN
   if (vrbs eq 1) then begin
    PRINT, '*** phenot_p2.pro [WARNING]:'
    PRINT, '    Return code from phenot_p2_stat2.pro = ', rc
   endif
   RETURN, rc
ENDIF

;;  Regenerate the time series without missing values (preliminary):
;rc = phenot_p2_stat3()
;
;IF (rc NE 0) THEN BEGIN
;   PRINT, '*** phenot_p2.pro [WARNING]:'
;   PRINT, '    Return code from phenot_p2_stat3.pro = ', rc
;   RETURN, rc
;ENDIF

;  Report the outcome of Phase 2:
if (vrbs eq 1) then PRINT, '*** phenot_p2.pro [INFO]: Completed Phase 2.'

RETURN, retcode

END