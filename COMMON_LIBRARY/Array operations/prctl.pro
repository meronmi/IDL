FUNCTION prctl, per, array, fmiss, ignore_below, ignore_above, fsort, val

;  Purpose:
;     To estimate the value, in the given array, corresponding to the
;     specified percentile.

;  Outcome:
;     The percentile value is returned as an argument.

;  Usage:
;     rc = prctl (per, array, fmiss, ignore_below, ignore_above, $
;        fsort, val)

;  Input parameters:
;     per {floating point}: The desired percentile, a decimal number
;        between 0.0 and 1.0.
;     array {floating point array}: The (unsorted) statistical sample
;        or population from which the percentile needs to be
;        estimated.
;     fmiss {integer}: Flag indicating whether the array contains
;        (fmiss = 1) or does not contain (fmiss = 0) missing values.
;        If missing values are present in array, they are assumed to
;        be strictly smaller than ignore_below or larger than
;        ignore_above.
;     ignore_below {floating point}: Threshold value: all array values
;        lower than it are ignored for the purpose of computing
;        percentiles.
;     ignore_above {floating point}: Threshold value: all array values
;        larger than it are ignored for the purpose of computing
;        percentiles.
;     fsort {integer}: Flag indicating whether the array is already
;        sorted in ascending order (fsort = 1) or not (fsort = 0).

;  Output parameters:
;     val {floating point}: The threshold value in the given array
;        such that per percents of the ranked array are below per.

;  Return values:
;     0: Normal completion.
;     1: The 'array' argument is a scalar.
;    10: The percentile requested is not a floating point number
;        between 0.0 and 1.0.
;    20: The input variable 'array' is undefined.
; MM:30: There are less than 2 good observations, returned value will be -999.9

;  Remark:
;     This IDL function is identical to the one in percentile.pro
;     except that it does not include any of the diagnostic PRINT
;     statements of the latter. It should execute faster but does not
;     provide warnings or information on its intermediary results.

;  References:
;     http://www.itl.nist.gov/div898/handbook/prc/section2/prc252.htm
;     http://www.data-for-all.com/documents/computing-percentiles.pdf

;  History:
;     Version: 1.0
;     Minor new release on 2008-10-14: Corrections to the documentation.
;     Version 1.1: Port under Suse Linux (starting February 2010).

;  Initialize the default return code of the function:
retcode = 0

;  Check the validity of the requested percentile value:
IF ((per LT 0.0) OR (per GT 1.0)) THEN BEGIN
   retcode = 10
   val = -999.9
   RETURN, retcode
ENDIF

;  Check that the 'array' argument is indeed an array with at least
;  two elements:
n = N_ELEMENTS(array)
IF (n EQ 0) THEN BEGIN
   retcode = 20
   val = -999.9
   RETURN, retcode
ENDIF
IF (n EQ 1) THEN BEGIN
   retcode = 1
   val = array
   RETURN, retcode
ENDIF

;  Create a new array containing no missing values, compute the
;  number of elements in that:
IF (fmiss NE 0) THEN BEGIN
   inomiss = WHERE((array GE ignore_below) AND (array LE ignore_above), countnomiss)
   ;MM
   IF countnomiss GT 0 THEN BEGIN 
      anomiss = array [inomiss]
   ENDIF ELSE BEGIN ;they are all -999, ust take one and it will be stopped afterward
      anomiss = array[0]
   ENDELSE
ENDIF ELSE BEGIN
   anomiss = array
END
nnomiss = N_ELEMENTS(anomiss)

;  Check that this new array itself contains at least two elements:
IF (N_ELEMENTS(anomiss) LT 2) THEN BEGIN
   retcode = 30
   val = -999.9
   RETURN, retcode
ENDIF

;  If the original array is unsorted or if there are missing values,
;  sort the new array in ascending order:
IF ((fsort EQ 0) OR (fmiss NE 0)) THEN BEGIN
   sorted = SORT(anomiss)
   sanomiss = anomiss [sorted]
ENDIF ELSE BEGIN
   sanomiss = anomiss
ENDELSE

;  Compute the rank of the observation in array corresponding to the
;  desired percentile:
rank = per * FLOAT(nnomiss + 1)

;  Check for extreme cases arising with small arrays:
IF (rank LT 1.0) THEN BEGIN
   val = sanomiss [0]
   RETURN, retcode
ENDIF
IF (rank GT FLOAT (nnomiss)) THEN BEGIN
   val = sanomiss [nnomiss - 1]
   RETURN, retcode
ENDIF

;  If the rank corresponds to an existing observation, return that
;  observation:
flo = FLOOR(rank)
IF (rank EQ FLOAT(flo)) THEN BEGIN
   val = sanomiss [flo - 1]
   RETURN, retcode
ENDIF

;  In all other cases, assign the percentile as a weighted average of
;  the two relevant observations in the sorted array. First identify
;  the observations that bracket the desired percentile:
a1 = sanomiss [flo - 1]
a2 = sanomiss [flo]

;  Compute the interpolated percentile:
val = a1 + (rank - FLOAT(flo)) * (a2 - a1)

RETURN, retcode

END