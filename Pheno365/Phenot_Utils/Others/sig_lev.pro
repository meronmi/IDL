FUNCTION sig_lev, m, p

;  Purpose:
;     To compute the minimum value a spike in Lomb's normalized periodogram
;     (spectral power as a function of angular frequency) must reach to be
;     considered statistically significant at the stated level, for an
;     unequally spaced time series.

;  Usage:
;     rc = sig_lev (m, p)

;  Input arguments:
;     * m {floating point): The number of independent frequencies scanned from
;       an unequally spaced time series, a function of the number of points in
;       the original time series.
;     * p {floating point): The desired significance level (e.g., 0.005).

;  Output arguments: N/A

;  Return value:
;     The minimum power spectrum value that must be reached to achieve the
;     desired statistical significance level.

;  Reference:
;     Press et al. (1992) Numerical Recipes, Cambridge University Press, p.
;     569-577.

z = -ALOG(1.0 - EXP((ALOG(1.0 - p)) / m))

RETURN, z

END