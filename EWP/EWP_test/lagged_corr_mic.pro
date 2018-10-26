; $Id: crosscorr.pro,v 1.1 2009/08/21 16:32:03 haimov Exp $
;+
; NAME:
;       lagged_corr_mic from C_CORRELATE and CROSSCORR (a modified version of C_CORRELATE)
;       DIFFERENCE: correlation is made using cov(x,y)/SQRT(var(x) * var(y))
;       where x and y are the extracted samples where both are finite for that lag
;       
;
; PURPOSE:
;       This function computes the normalized cross-correlation of two sample
;       populations X and Y (1-D vectors) as a function of the lag.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = crosscorr(X, Y, Lag)
;
; INPUTS:
;       X:    An n-element vector of type integer, float or double.
;
;       Y:    An n-element vector of type integer, float or double.
;
;     LAG:    A scalar or n-element vector, in the interval [-(n-2), (n-2)],
;             of type integer that specifies the absolute distance(s) between
;             indexed elements of X.
;
; KEYWORD PARAMETERS:
;       COVARIANCE:    If set to a non-zero value, the sample cross covariance
;                      is computed; to calculate autocorrelation function (does
;                      not remove the mean from X and Y) set COVARIANCE to -1
;
;       DOUBLE:        If set to a non-zero value, computations are done in
;                      double precision arithmetic.
;       NAN:           If set to a non-zero value, treats NaNs in X and Y as
;                      missing values.
;                      Note: the presence of missing values changes the
;                            statistics of the estimate and the errors could be
;                            significant; the errors have not been characterized
;       UNBIASED:      If set to a non-zero value, unbiased estimate of the
;                      covariance is calculated; this keyword is ignored if
;                      COVARIANCE is 0 or not defined.
;                      Note: the unbiased estimator is subject to a larger
;                            variance in the estimate (for lags closer to the
;                            end points) than the biased estimator
;       HELP:          show this text
;
;
; EXAMPLE
;       Define two n-element sample populations.
;         x = [3.73, 3.67, 3.77, 3.83, 4.67, 5.87, 6.70, 6.97, 6.40, 5.57]
;         y = [2.31, 2.76, 3.02, 3.13, 3.72, 3.88, 3.97, 4.39, 4.34, 3.95]
;
;       Compute the cross correlation of X and Y for LAG = -5, 0, 1, 5, 6, 7
;         lag = [-5, 0, 1, 5, 6, 7]
;         result = crosscorr(x, y, lag)
;
;       The result should be:
;         [-0.428246, 0.914755, 0.674547, -0.405140, -0.403100, -0.339685]
;
; PROCEDURE:
;       See computational formula published in IDL manual.
;
; REFERENCE:
;       INTRODUCTION TO STATISTICAL TIME SERIES
;       Wayne A. Fuller
;       ISBN 0-471-28715-6
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, October 1994
;       Modified:    GGS, RSI, August 1995
;                    Corrected a condition which excluded the last term of the
;                    time-series.
;              - GGS, RSI, April 1996
;                    Simplified CROSS_COV function. Added DOUBLE keyword.
;                    Modified keyword checking and use of double precision.
;              - W. Biagiotti,  Advanced Testing Technologies
;              Inc., Hauppauge, NY, July 1997, Moved all
;              constant calculations out of main loop for
;              greatly reduced processing time.
;   CT, RSI, September 2002. Further speed improvements, per W. Biagiotti.
;                Now handles large vectors and complex inputs.
;   SH,      March 2009: Changed the name of the function from c_correlate to
;                    autocorr, changed COVARIANCE keyword and added UNBIASED,
;                    NAN and HELP keywords.
;                    The changes have not been thoroughly tested
;-
function lagged_corr_mic, X, Y, Lag, Covariance = Covariance, Double = doubleIn, $
  Unbiased = Unbiased, Nan = Nan, Help = Help

  compile_opt idl2

  ON_ERROR, 2

  if keyword_set(help) then begin
    doc_library,'crosscorr'
    return,''
  endif

  nX = N_ELEMENTS(x)

  if (nX ne N_ELEMENTS(y)) then $
    MESSAGE, "X and Y arrays must have the same number of elements."

  ; Check length.

  if (nX lt 2) then $
    MESSAGE, "X and Y arrays must contain 2 or more elements."

  ; Check lags

  if n_elements(lag) eq 0 then lag=lindgen(nX+(nX mod 2 eq 0))-nX/2L

  if max(abs(Lag)) gt nX-2 then $
    message,"Maximum abs(Lag) must not exceed n-2"

  ; Check which cross-corr function is requested

  if n_elements(COVARIANCE) eq 0 then Covariance=0

  ; If the DOUBLE keyword is not set then the internal precision and
  ; result are identical to the type of input.

  typeX = SIZE(X, /TYPE)
  typeY = SIZE(Y, /TYPE)
  isComplex = (typeX eq 6) or (typeX eq 9) or (typeY eq 6) or (typeY eq 9)
  useDouble = (N_ELEMENTS(doubleIn) eq 1) ? KEYWORD_SET(doubleIn) : $
    (typeX eq 5 or typeY eq 5) or (typeX eq 9 or typeY eq 9)

;  ; This will now be in double precision if Double is set.
;  Xd = x ;- TOTAL(X, Double = useDouble) / nX ;Deviations
;  Yd = y ;- TOTAL(Y, Double = useDouble) / nX

  nLag = N_ELEMENTS(Lag)

  RP = FLTARR(nLag,2) 
;  Cross = useDouble ? $
;    (isComplex ? DCOMPLEXARR(nLag) : DBLARR(nLag)) : $
;    (isComplex ? COMPLEXARR(nLag) : FLTARR(nLag))
;  Pval = FLTARR(nLag)
  if Covariance eq 0 then begin ; Calculate normalized cross-correlation

    ;Mic + remove mean computed on the overall and compute it on sample   
    ;Xd = X - mean(X,Double=useDouble,Nan=Nan);,/silent)
    ;Yd = Y - mean(Y,Double=useDouble,Nan=Nan);,/silent)

    for k = 0L, nLag-1 do begin
      ; form the vectors
      IF ((Lag[k] ge 0)) THEN BEGIN
        ;postive lag, move y forward
        xlagged = x[0:nX - Lag[k] - 1L]
        ylagged = y[Lag[k]:*]
      ENDIF ELSE BEGIN
        ;negative lag, move x forward
        xlagged = x[-Lag[k]:*]
        ylagged = y[0:nX + Lag[k] - 1L]
      ENDELSE
      ;now extract the subsample where both are finite
      indfin = WHERE(((FINITE(xlagged)) AND (FINITE(ylagged))), countfin)
      indfiny = WHERE(FINITE(ylagged), countfiny)
      ;IF (countfin GT 10) THEN BEGIN   ;10 as the minimum number of data to compute correlation
      ;changed, allow for a max of 50 % missing (of those required)
      IF (countfin/FLOAT(countfiny) GT 0.5) THEN BEGIN   
        xlagged = xlagged[indfin]
        ylagged = ylagged[indfin]
        ;compute the deviations
        Xd = xlagged - mean(xlagged,Double=useDouble);,Nan=Nan);,/silent)
        Yd = ylagged - mean(ylagged,Double=useDouble);,Nan=Nan);,/silent)
        ;Cross[k] = TOTAL(Xd * Yd) / (SQRT(TOTAL(Xd^2)*TOTAL(Yd^2)))
        res = LINCORR(Xd, Yd)
        RP[k,0] = res[0]
        RP[k,1] = res[1]
      ENDIF ELSE BEGIN
        RP[k,0] = !VALUES.F_NAN
        RP[k,1] = !VALUES.F_NAN
      ENDELSE
   
;      ; Note the reversal of the variables for negative lags.
;      Cross[k] = (Lag[k] ge 0) ? $
;        TOTAL(Xd[0:nX - Lag[k] - 1L] * Yd[Lag[k]:*], Nan=Nan) : $
;        TOTAL(Yd[0:nX + Lag[k] - 1L] * Xd[-Lag[k]:*], Nan=Nan)
    endfor

    ;Cross=(TEMPORARY(Cross)/SQRT(TOTAL(Xd^2,Nan=Nan)*TOTAL(Yd^2,Nan=Nan)))<1.0

  endif else begin ; Compute cross-covariance or cross-correlation
    STOP
;    if Covariance eq -1 then begin
;      Xd=X & Yd=Y
;    endif else begin
;      Xd=X-mean(X,Double=useDouble,Nan=Nan);,/silent)
;      Yd=Y-mean(Y,Double=useDouble,Nan=Nan);,/silent)
;    endelse
;
;    for k = 0L, nLag-1 do begin
;      ; Note the reversal of the variables for negative lags.
;      Cross[k] = (Lag[k] ge 0) ? $
;        TOTAL(Xd[0:nX - Lag[k] - 1L] * Yd[Lag[k]:*],Nan=Nan) : $
;        TOTAL(Yd[0:nX + Lag[k] - 1L] * Xd[-Lag[k]:*],Nan=Nan)
;    endfor
;
;    ; Divide by N-abs(lags) for unbiased estimate or N for biased estimate
;
;    if KEYWORD_SET(UNBIASED) then begin
;      M=abs(lag)
;      if KEYWORD_SET(NAN) then N=(total(finite(Xd)*finite(Yd))-M)>1.0 $
;      else N=nX-M
;    endif else begin
;      if KEYWORD_SET(NAN) then N=total(finite(Xd)*finite(Yd)) $
;      else N=nX
;    endelse
;
;    Cross = TEMPORARY(Cross)/N

  endelse

  return, FLOAT(RP)

end