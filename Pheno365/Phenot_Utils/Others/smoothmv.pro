FUNCTION smoothmv, rseries, ignore_below, ignore_above, width, sseries

;  Purpose:
;     To smooth a time series possibly containing missing values using a
;     low-pass filter of given width, currently 3, 5, or 7.

;  Usage:
;     rc = smoothmv(rseries, ignore_below, ignore_above, width, sseries)

;  Input arguments:
;     - rseries {floating point array}: A time series possibly containing an
;       arbitrary number of missing values.
;     - ignore_below {floating point}: Threshold value, all values lower than
;       this value are ignored.
;     - ignore_above {floating point}: Threshold value, all values higher than
;       this value are ignored.
;     - width {odd positive integer}: Width of the low-pass window.

;  Output arguments:
;     - sseries {floating point array}: A smooth version of rseries where
;       consecutive non-missing values are replaced by a smoother sequence, and
;       missing values are copied in place.

;  Return codes:
;     0: Normal completion.
;     1: width was set to 0 or 1, sseries = rseries.
;     2: width was an even number, reset to width + 1.
;     3: width exceeded the longest low-pass filter window for which
;        coefficients are available, reset to longestwin.
;     4: rseries contained less than 3 elements, sseries = rseries.
;    10: rseries is not a 1D array: control returns to the calling program
;        without declaring or assigning values to sseries.
;    12: rseries is not a numeric array: control returns to the calling program
;        without declaring or assigning values to sseries.
;
;  Initialize the return code:
retcode = 0

;  Check that rseries is a proper 1D array:
IF (SIZE(rseries, /N_DIMENSIONS) NE 1) THEN BEGIN
   retcode = 10
   RETURN, retcode
ENDIF

;  Check that rseries is a numeric array:
IF ((SIZE(rseries, /TYPE) LT 2) OR (SIZE(rseries, /TYPE) GT 6)) THEN BEGIN
   retcode = 12
   RETURN, retcode
ENDIF

;  Ensure that the width is a positive integer:
width = ABS(FIX(width))

;  If the width is set to 0 or 1, return a smooth series identical to the rough
;  series:
IF ((width EQ 0) OR (width EQ 1)) THEN BEGIN
   sseries = rseries
   retcode = 1
   RETURN, retcode
ENDIF

;  Check that the width is an odd integer. If not, set it to the next higher
;  integer:
rc = iseven(width)
IF (rc NE 0) THEN BEGIN
   width = width + 1
   retcode = 2
ENDIF

;  If the width is larger than the longest low-pass filter window for which the
;  coefficients are provided in the CASE statement below, reset width to the
;  longest window available:
longestwin = 11
IF (width GT longestwin) THEN BEGIN
   width = longestwin
   retcode = 3
ENDIF

;  Compute the dimension of rseries:
nelm = N_ELEMENTS(rseries)

;  If rseries only contains 1 or 2 points, set sseries = rseries; otherwise
;  create sseries of the same length as rseries:
IF (nelm LT 3) THEN BEGIN
   sseries = rseries
   retcode = 4
   RETURN, retcode
ENDIF ELSE BEGIN
   sseries = FLTARR(nelm)
ENDELSE

;  Check that the width of the low-pass filter is smaller than the length of
;  the series:
IF (width GE nelm) THEN BEGIN
   retcode = 12
   RETURN, retcode
ENDIF

;  Define mwi such that width = 2 * mwi + 1:
mwi = (width - 1) / 2

;  Define the arrays of weights up to longestwin:
CASE width OF
   3: wei = [1.0, 2.0, 1.0]
   5: wei = [1.0, 4.0, 6.0, 4.0, 1.0]
   7: wei = [1.0, 6.0, 15.0, 20.0, 15.0, 6.0, 1.0]
   9: wei = [1.0, 8.0, 28.0, 56.0, 70.0, 56.0, 28.0, 8.0, 1.0]
   11: wei = [1.0, 45.0, 120.0, 210.0, 252.0, 210.0, 120.0, 45.0, 10.0, 1.0]
ENDCASE

;  Main loop over the elements of rseries:
FOR i = 0, nelm - 1 DO BEGIN

;  Copy missing values in rseries to sseries:
   IF ((rseries [i] LT ignore_below) OR $
      (rseries [i] GT ignore_above)) THEN BEGIN
      sseries [i] = rseries [i]
   ENDIF ELSE BEGIN

;  If the current element of rseries is not missing, reset the
;  accumulators:
      cumv = 0.0
      cumw = 0.0

;  Loop over the low-pass window:
      FOR j = -mwi, mwi DO BEGIN
         ij = i + j
         IF ((ij GE 0) AND (ij LT nelm)) THEN BEGIN
            IF ((rseries [ij] GE ignore_below) AND $
               (rseries [ij] LE ignore_above)) THEN BEGIN
               cumv = cumv + rseries [ij] * wei [j + mwi]
               cumw = cumw + wei [j + mwi]
            ENDIF
         ENDIF
      ENDFOR
      sseries [i] = cumv / cumw
   ENDELSE
ENDFOR

END