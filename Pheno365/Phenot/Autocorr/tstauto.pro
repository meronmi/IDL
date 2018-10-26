PRO compare, ts, lags, ignore_below, ignore_above, diag

;  IDL program to compare the results from autocorr with those obtained
;  with standard IDL procedures.

res = MEAN (ts)
PRINT, 'IDL mean = ', res
res = VARIANCE (ts)
PRINT, 'IDL variance = ', res
res = A_CORRELATE (ts, lags, /COVARIANCE)
PRINT, 'IDL covariance = ', res
res = A_CORRELATE (ts, lags)
PRINT, 'IDL autocorrelation = '
PRINT, res

;MMretcode = autocorr (ts, lags, ignore_below, ignore_above, diag, $
retcode = autocorr (ts, lags, ignore_below, ignore_above, $
   nval, mean, var, ncv, cov, cor)
   
PRINT, 'autocorr retcode = ', retcode
PRINT, 'autocorr nval = ', nval
PRINT, 'autocorr mean = ', mean
PRINT, 'autocorr var = ', var
PRINT, 'autocorr ncv = ', ncv
PRINT, 'autocorr cov = ', cov
PRINT, 'autocorr cor = ', cor

END

PRO tstauto

;  IDL program to test the autocorr function against the IDL
;  a_correlate function.

ignore_below = 0.0
ignore_above = 10.0
diag = 1

lags = [0, 1, 2, 3, 4]

PRINT, 'Case 1: constant time series.'
ts = [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
PRINT, 'ts = ', ts
PRINT, 'lags = ', lags
compare, ts, lags, ignore_below, ignore_above, diag

PRINT
PRINT, 'Case 2: small regular time series.'
ts = [1, 1, 1, 2, 5]
PRINT, 'ts = ', ts
PRINT, 'lags = ', lags
compare, ts, lags, ignore_below, ignore_above, diag


PRINT
PRINT, 'Case 3: regular time series.'
ts = [0.1, 0.2, 0.3, 0.2, 0.4, 0.3, 0.2, 0.1]
PRINT, 'ts = ', ts
PRINT, 'lags = ', lags
compare, ts, lags, ignore_below, ignore_above, diag

PRINT
PRINT, 'Case 4: time series with missing values.'
ts = [0.1, -0.2, 0.3, -0.2, 0.4, 0.3, -0.2, 0.1]
PRINT, 'ts = ', ts
PRINT, 'lags = ', lags
compare, ts, lags, ignore_below, ignore_above, diag



END
