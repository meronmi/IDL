FUNCTION numminmax, series, ignore_below, ignore_above, nseqs, $
   nmins, nmaxs, mins, maxs, imins, imaxs

;  Purpose:
;     To compute the number of local minima and maxima in a data
;     series, ignoring values that are strictly smaller than
;     ignore_below or that are strictly larger than ignore_above,
;     and provide their values and positions in the time series.

;  Algorithm:
;     This function identifies the number of times sequences of
;     increasing values are followed by sequences of decreasing
;     values, and conversely, in the series provided. Once these
;     numbers are known, appropriately sized arrays are generated
;     and populated with the desired information.

;  Usage:
;     rc = numminmax (series, ignore_below, ignore_above, nseqs, $
;        nmins, nmaxs, mins, maxs, imins, imaxs)

;  Input arguments:
;     - series {floating point array}: A 1-dimensional time series.
;     - ignore_below {floating point}: Threshold below which series
;       values are to be ignored.
;     - ignore_above {floating point}: Threshold above which series
;       values are to be ignored.

;  Output arguments:
;     - nseqs {integer}: Number of runs of consecutive valid values.
;     - nmins {integer}: Number of minima in series.
;     - nmaxs {integer}: Number of maxima in series.
;     - mins {floating point array}: Values of the various minima
;       identified in the time series.
;     - maxs {floating point array}: Values of the various maxima
;       identified in the time series.
;     - imins {integer array}: Positions of the various minima
;       identified in the time series.
;     - imaxs {integer array}: Positions of the various maxima
;       identified in the time series.

;  Return values:
;     0: Computations completed normally.
;    10: Too few data points (valid or missing) in the series.
;    20: No valid values in the series.
;    30: At least one valid value in the series but no local minima
;        or maxima found.

;  Version:
;     Release 4.0.
;     2007-07-14, First release producing these statistics as well as
;     the positions of the various mins and maxs found.
;     Release 5.0.
;     Remove tmpxxx and foundlocxxx variables, further algorithm testing.

;  Initialize the return code:
retcode = 0

;  Evaluate the number of elements in series:
nelm = N_ELEMENTS(series)
IF (nelm LT 2) THEN BEGIN
   retcode = 10
   RETURN, retcode
ENDIF

;  Initialize the number of sequences of consecutive valid values:
nseqs = 0

;  Initialize the number of minima:
nmins = 0

;  Initialize the number of maxima:
nmaxs = 0

;  Initialize the flag indicating whether the previous value was
;  valid (1) or invalid (0):
isval = 0

;  Initialize the trend indicator:
trend = 0

;  Main loop:
FOR i = 0, (nelm - 1) DO BEGIN

;  Inspect whether the current value is valid:
   IF ((series [i] GE ignore_below) AND $
      (series [i] LE ignore_above)) THEN BEGIN

;  Current value is valid; now check whether this is the start of a
;  new sequence of valid values:
      IF (isval NE 1) THEN BEGIN
         isval = 1
         nseqs = nseqs + 1
      ENDIF ELSE BEGIN

;  Current value is valid and follows previous valid value; now check
;  whether the trend is changing. If no trend was previously set,
;  define one if appropriate and increase the counters for the number
;  of minima or maxima:
         IF (trend EQ 0) THEN BEGIN
            IF (series [i] GT series [i - 1]) THEN BEGIN
               trend = 1
               nmins = nmins + 1
               nmaxs = nmaxs + 1
            ENDIF
            IF (series [i] LT series [i - 1]) THEN BEGIN
               trend = -1
               nmins = nmins + 1
               nmaxs = nmaxs + 1
            ENDIF
         ENDIF

;  Current value is valid and follows previous valid value; if the
;  previous trend was positive, check whether it has changed:
         IF (trend EQ 1) THEN BEGIN
            IF (series [i] LT series [i - 1]) THEN BEGIN
               trend = -1
               nmins = nmins + 1
            ENDIF
         ENDIF

;  Current value is valid and follows previous valid value; if the
;  previous trend was negative, check whether it has changed:
         IF (trend EQ -1) THEN BEGIN
            IF (series [i] GT series [i - 1]) THEN BEGIN
               trend = 1
               nmaxs = nmaxs + 1
            ENDIF
         ENDIF
      ENDELSE
   ENDIF ELSE BEGIN

;  Current value is invalid, reset the trend indicator to 0:
      isval = 0
      trend = 0
   ENDELSE
ENDFOR

;  If no sequence of valid values was found, return with an error
;  code:
IF (nseqs EQ 0) THEN BEGIN
   retcode = 20
   RETURN, retcode
ENDIF

;  If no minima or maxima were found (as could happen in a series of
;  single valid values separated from each other by missing values),
;  return with an error code:
IF ((nmins EQ 0) OR (nmaxs EQ 0)) THEN BEGIN
   retcode = 30
   RETURN, retcode
ENDIF

;  Now that the number of local minima and maxima is known and
;  non-zero, define output arrays of the appropriate dimensions to
;  contain the values and time indices of these minima and maxima:
mins = FLTARR(nmins)
maxs = FLTARR(nmaxs)
imins = INTARR(nmins)
imaxs = INTARR(nmaxs)

;  Reset all the counters:
nseqs = 0
nmins = 0
nmaxs = 0
isval = 0
trend = 0

;  Re-execute the main loop and store the values and positions of the
;  minima and maxima along the way:
FOR i = 0, (nelm - 1) DO BEGIN

;  Inspect whether the current value is valid:
   IF ((series [i] GE ignore_below) AND $
      (series [i] LE ignore_above)) THEN BEGIN

;  Current value is valid; now check whether this is the start of a
;  new sequence of valid values, and if so set the flags to indicate
;  that no minima and maxima have been found yet:
      IF (isval NE 1) THEN BEGIN
         isval = 1
         nseqs = nseqs + 1
      ENDIF ELSE BEGIN

;  Current value is valid and follows at least 1 previous valid value;
;  now check whether the trend is changing. If trend was not
;  previously set, define one if appropriate and increase the counters
;  for the number of minima or maxima:
         IF (trend EQ 0) THEN BEGIN
            IF (series [i] GT series [i - 1]) THEN BEGIN
               trend = 1
               nmins = nmins + 1
               nmaxs = nmaxs + 1
               mins [nmins - 1] = series [i - 1]
               imins [nmins - 1] = i - 1
            ENDIF
            IF (series [i] LT series [i - 1]) THEN BEGIN
               trend = -1
               nmins = nmins + 1
               nmaxs = nmaxs + 1
               maxs [nmaxs - 1] = series [i - 1]
               imaxs [nmaxs - 1] = i - 1
            ENDIF

;  Note that if series [i] EQ series [i - 1], the trend remains 0 and
;  no new minima or maxima are expected.
         ENDIF

;  Current value is valid and follows at least 2 previous valid
;  values, since trend was already set.
;  If the previous trend was positive but is now going down, a local
;  maximum has been found and a new local minimum is implied:
         IF (trend EQ 1) THEN BEGIN
            IF (series [i] LT series [i - 1]) THEN BEGIN
               trend = -1
               maxs [nmaxs - 1] = series [i - 1]
               imaxs [nmaxs - 1] = i - 1
               nmins = nmins + 1
            ENDIF
         ENDIF

;  If the previous trend was negative but is now going up, a local
;  minimum has been found and a new local maximum is implied:
         IF (trend EQ -1) THEN BEGIN
            IF (series [i] GT series [i - 1]) THEN BEGIN
               trend = 1
               mins [nmins - 1] = series [i - 1]
               imins [nmins - 1] = i - 1
               nmaxs = nmaxs + 1
            ENDIF
         ENDIF

;  Treat the special case of a valid value at the end of the series:
         IF (i EQ nelm - 1) THEN BEGIN
            IF (trend EQ -1) THEN BEGIN
               mins [nmins - 1] = series [i]
               imins [nmins - 1] = i
            ENDIF
            IF (trend EQ 1) THEN BEGIN
               maxs [nmaxs - 1] = series [i]
               imaxs [nmaxs - 1] = i
            ENDIF
            
         ENDIF
         
      ENDELSE
   ENDIF ELSE BEGIN

;  Current value is invalid (missing); now check whether this is the
;  end of a sequence of valid values, and if so save the previous
;  valid value as a minimum or a maximum, as appropriate:
      IF (isval NE 0) THEN BEGIN

;  If trend was previously set and negative, the last valid value was
;  a local minimum:
         IF (trend EQ -1) THEN BEGIN
            mins [nmins - 1] = series [i - 1]
            imins [nmins - 1] = i - 1
         ENDIF

;  If trend was previously set and positive, the last valid value was
;  a local maximum:
         IF (trend EQ 1) THEN BEGIN
            maxs [nmaxs - 1] = series [i - 1]
            imaxs [nmaxs - 1] = i - 1
         ENDIF

;  Reset the valid values and trend flags:
         isval = 0
         trend = 0
      ENDIF
   ENDELSE
ENDFOR

RETURN, retcode

END