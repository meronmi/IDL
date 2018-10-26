FUNCTION autocorr, ts, lags, ignore_below, ignore_above, $
   nval, mean, var, ncv, cov, cor
   
;  Purpose:
;     This IDL function computes the mean, variance, autocovariance
;     and autocorrelation for a (time) series that may contain an
;     arbitrary number of missing values arbitrarily distributed
;     within the series.

;  Usage:
;     retcode = autocorr (ts, lags, ignore_below, ignore_above, $
;        nval, mean, var, ncv, cov, cor)

;  Input arguments:
;     1. ts {floating point array}: Arrray (time series) of values
;        to analyze.
;     2. lags {integer array}: Array of lags to consider.
;     3. ignore_below {floating point}: Threshold below which values
;        should be ignored.
;     4. ignore_above {floating point}: Threshold above which values
;        should be ignored.

;  Output arguments:
;     1. nval {integer}: Number of valid values in ts.
;     2. mean {long floating point}: Arithmetic mean of the valid
;        values in ts.
;     3. var {long floating point}: Variance of the valid values
;        in ts.
;     4. ncv {integer array}: Number of pairs of valid FAPAR values
;        contributing to the autocovariance and autocorrelation
;        functions at each of the lags.
;     5. cov {long floating point array}: Autocovariances at the
;        specified lags.
;     6. cor {long floating point array}: Autocorrelations at the
;        specified lags.

;  Return value:
;     0: Normal situation.
;     1: There may be too few valid products at one or more lags to
;        compute the autocorrelation at that lag (reported as -999.0).
;    11: Time series contains less than 2 valid (non missing) data
;        values: the mean or the variance cannot be computed.

;  References:
;  *  Box, G. and G. Jenkins (1976) 'Time Series Analysis',
;     Holden-Day.
;  *  Fuller, W. (1976) 'Introduction to Statistical Time Series',
;     Wiley.
;  *  http://en.wikipedia.org/wiki/Autocorrelation

;  Version:
;     Release 4.0.
;     2007-02-18, First release using the PDHTF model.

;  Initialize the default return code of the function:
retcode = 0

;  Evaluate the number of elements in the series:
nelm = N_ELEMENTS (ts)

;  Evaluate the number of lags required:
nlags = N_ELEMENTS (lags)

;  Define the output arguments:
nval = 0
sum = DOUBLE (0.0)
mean = DOUBLE (0.0)
var = DOUBLE (0.0)
ncv = INTARR (nlags)
cov = DBLARR (nlags)
cor = DBLARR (nlags)

;  First, estimate the mean of all valid values in the series:
FOR i = 0, nelm - 1 DO BEGIN
   IF ((ts [i] GE ignore_below) AND $
      (ts [i] LE ignore_above)) THEN BEGIN
      nval = nval + 1
      sum = sum + DOUBLE (ts [i])
   ENDIF
ENDFOR

;  Check that at least two valid values have been found to
;  dividing by zero in computing the mean or the variance:
IF (nval LT 2) THEN BEGIN
   retcode = 11
   RETURN, retcode
ENDIF

mean = sum / DOUBLE (nval)

;  Then estimate the (unbiased) variance of all valid values
;  in the series:
FOR i = 0, (nelm - 1) DO BEGIN
   IF ((ts [i] GE ignore_below) AND $
      (ts [i] LE ignore_above)) THEN BEGIN
      var = var + (DOUBLE (ts [i]) - mean) * (DOUBLE (ts [i]) - mean)
   ENDIF
ENDFOR

var = var / DOUBLE (nval - 1)

;  Now estimate the sum of the lagged products and the autocorrelation
;  function:

;  For each of the requested lags:
FOR l = 0, nlags - 1 DO BEGIN

;  Initialize the counter of the number of contributing values and
;  the covariance at this lag:
   ncv [l] = 0
   cov [l] = DOUBLE (0.0)

;  For each couple of values at that lag:
   FOR i = 0, (nelm - lags [l] - 1) DO BEGIN

;  Check that both values are valid; if so compute their product and
;  increment the applicable counter:
      IF (((ts [i] GE ignore_below) AND $
         (ts [i] LE ignore_above)) AND $
         ((ts [i + lags [l]] GE ignore_below) AND $
         (ts [i + lags [l]] LE ignore_above))) THEN BEGIN
            cov [l] = cov [l] + $
               (DOUBLE (ts [i]) - mean) * $
               (DOUBLE (ts [i + lags [l]] - mean))
            ncv [l] = ncv [l] + 1
      ENDIF
   ENDFOR

;  Calculate the series covariance as the expected value of the sum
;  of the products at the applicable lag.
;  Note that, in the statistical literature, this normalization is
;  often done by dividing the sum of the products by the number of
;  elements in the original series (thus ignoring the presence of
;  missing values) or by that number minus the lag, to account for the
;  smaller number of pairs of points contributing to the covariance at
;  larger lags. Here, the normalization is made by the number of pairs
;  actually entering the computation (e.g., taking into account the
;  presence of missing values and the smaller number of pairs at large
;  lags). Since this number is smaller than (or equal to) either of
;  the previous solutions, this estimate will always yield
;  comparatively higher autocorrelations than obtained with classical
;  routines. This bias will decrease significantly as the series grows
;  in length and/or with fewer missing values:

;  To implement the classical definition, activate the following line:
;   cov [l] = cov [l] / DOUBLE (nelm - 1)

;  To implement the limited series definition, activate this line:
;   cov [l] = cov [l] / DOUBLE (nval - 1)

;  To implement the solution accounting for missing values, activate
;  this block of lines:
   IF (ncv [l] LT 2) THEN BEGIN
      cov [l] = DOUBLE (-999.0)
      cor [l] = DOUBLE (-999.0)
      retcode = 1
   ENDIF ELSE BEGIN
      cov [l] = cov [l] / DOUBLE (ncv [l] - 1)

;  Compute the autocorrelation only if the variance of the valid
;  values is non-null:
      IF (var NE 0.0) THEN BEGIN
         cor [l] = cov [l] / var
      ENDIF ELSE BEGIN
         cor [l] = DOUBLE (-999.0)
      ENDELSE
   ENDELSE
ENDFOR

RETURN, retcode

END
