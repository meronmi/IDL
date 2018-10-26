FUNCTION phenot_p2_stat2v2

;  Purpose:
;     To compute and save the Lomb Normalized Periodogram for the FAPAR time
;     series which may contain missing values. This is performed to estimate
;     the number of significant frequencies in the signal (based on the power
;     spectrum), and to propose a method to reconstruct missing values that
;     preserves the frequency structure of the non-missing signal.

;  Outcome:
;     The results of the Lomb Normalized Periodogram are available in a text
;     file and the periodogram is optionally displayed and saved as
;     a separate graph.

;  Usage:
;     rc = phenot_p2_stat(freq, powr, np, ph, f)

;  Input parameters: None.

;  Output parameters: None.
; freq, powr, np, ph, f

;  Return values:
;     0: Normal completion.
;    10: Unrecognized data periodicity (should be 1, 10 or 30 for daily, decadal
;        or monthly data).

;  History:
;     Version 1.1: Port under Suse Linux (starting February 2010).

;  References:
;     Press et al. (1992) Numerical Recipes in FORTRAN, Second Edition, Cambridge
;     University Press, p. 569-577.

;  Include the necessary common block(s):
@cb_os.comm
@cb_job.comm
@cb_in.comm
@cb_stat.comm
;MM+
@cb_options.comm
@cb_pdhtf.comm
;  Initialize the return code:
retcode = 0

;MM15
IF (empty_season GT 0) THEN BEGIN       ;at least one season of the two found in the previous iteration
;resulted empty when after the fitting prcedure. So it is assumed that it was detected by lomb rat because there is 
;some period noise, that when fitted, is too smal to be fitted-
;First delate all the windows generated in the first iteration (if we are working at the pixel)
  IF (pltres eq 1) THEN BEGIN
    device, window=windows
    open=where(windows eq 1,count)
    if (count gt 0) then begin
      for i=0,count-1 do wdelete,open(i)
    endif
  ENDIF
  ;set ngspy to 1
  ngspy = 1
ENDIF

;MM+
IF (use_lomb EQ 0) AND (empty_season EQ 0) THEN BEGIN
  ;Implementation and plotting of autocorrelation
  lags=indgen(45) ;as suggest in MV paper ASR 2008
  ;retcode = autocorr (ts, lags, ignore_below, ignore_above, $
  ;   nval, mean, var, ncv, cov, cor)
  retcode = autocorr (fapar, lags, ibel , iabo, $
     nval, mean, var, ncv, cov, cor)
  
  ;check that they are 'big' enough and other constrains
  rc=modifiedEXTREMA('max', cor, max_ind)
  IF (rc eq 10) THEN STOP
  IF (rc eq 30) then return, rc ;unrecognized periodicity
  ngspy=N_ELEMENTS(max_ind)                      
  IF (vrbs eq 1) then begin
   print, 'autocorrelation peaks found at ', max_ind
  ENDIF
  IF (ngspy lt 1) OR (ngspy gt 2) then begin  ;if an anomalous number of gs was found, then return
    retcode=40
    return, retcode
  ENDIF
IF (pltres eq 1) then rc=gs6t_plt_acorr()
ENDIF
;MM-


IF (use_lomb EQ 1) AND (empty_season EQ 0) THEN BEGIN
  ;  Set the test flag to 1 to execute the IDL Lomb routine and display its results
  ;  on screen (for comparison with my routine), and to 0 to skip this step (The IDL
  ;  routine returns only the most significant peak in the power spectrum; while a
  ;  typical phenology application will require information on the most important 2
  ;  or 3 peaks):
  tstflg = 0
  ;  Notes on terminology, units and motivation:
  ;  If 'per' is the sampling interval, '1/per' is the sampling rate and the Nyquist
  ;  frequency 'f_c' for evenly spaced points is given by 1/(2*per).
  ;  Watch for units and the proper interpretation of numerical values:
  ;  A decadal time series (data points every 10 days nominally) will exhibit 36 points
  ;  per year; so the sampling interval is a decade, the sampling rate is one per decade
  ;  and the Nyquist frequency is (1 per (2 decades)) or 0.5 in those decadal units. A
  ;  frequency of once a year would be 1/36 or 0.027778 and a half-yearly cycle would have
  ;  a frequency of 1/18 or 0.055556.
  ;  Expressed in terms of days, the sampling interval is 10 days, the sampling rate is
  ;  once every 10 days and the Nyquist frequency is (1 per (20 days)) or 0.05. A
  ;  frequency of once a year would be 1/365 or 0.002740 and a half-yearly cycle would have
  ;  a frequency of 1/182.5 or 0.005479.
    ;  A regularly sampled signal contains complete information on all frequencies up to
  ;  f_c, and scrambled or aliased information on frequencies larger than f_c. The problem
  ;  is worse in the case of irregularly sampled data, as happens with missing values.
  ;  Note also that decadal and monthly data are never equally spaced, even when there are
  ;  no missing values, because of their differences in length during the year: A correct
  ;  handling of this issue would require basing all timing information to be given in
  ;  Julian days, where decadal values would be assigned to 5 days into each decade.
  
  ;  Missing values in a real record can be estimated (filled-in) by interpolation, but
  ;  experience shows that this may be unreliable; the longer the gaps in the data, the
  ;  higher the risk of errors in the estimation of power at low frequencies: Such an
  ;  approach cannot guarantee the preservation of the statistical properties of the
  ;  frequency spectrum that can be documented in the original data.
  
  ;  The Lomb Normalized Periodogram is a method that attempts to estimate the actual
  ;  power spectrum of the data despite the presence of missing values.
  ;  The Lomb method also allows for the rigorous estimation of the statistical significance
  ;  of a frequency (peak) detected in the power spectrum (against the null hypothesis
  ;  that the signal is purely Gaussian noise). Specifically, if M independent frequencies
  ;  are identified, the probability that none of them yields a power larger than P is
  ;  given by a negative exponential: The smaller P, the more significant these frequencies
  ;  are. M depends on the number of frequencies sampled, the number of contributing data
  ;  points nfap_nomiss, and their distribution in time. M is about equal to nfap_nomiss
  ;  when the data points are almost equally spaced and when the sampled frequencies
  ;  oversample the frequency range from 0 to the Nyquist frequency f_c. When a larger
  ;  frequency range is actually sampled (e.g., when some of the data points are closer
  ;  together document higher frequencies), M increases proportionally. The only case when
  ;  M differs from that for evenly spaced sampling is when data points are clustered (e.g.,
  ;  in groups of nn) in which cases M should be reduced by a factor of about nn.
  
  ;  Set the oversampling factor as recommended by Press et al., p. 572:
  ofac = 4.0
  
  ;  Set the upper frequency threshold as a multiplier of the Nyquist frequency.
  ;  For the purpose of defining phenology, which is concerned only or mostly with the
  ;  lower frequency range (seasonal cycles), it is not necessary to document the
  ;  significance of frequencies higher than the Nyquist frequency f_c, so hifac is
  ;  set to 1. Note: The routine numrec_period estimates M as hifac * nfap_nomiss.
  hifac = 1.0
  
  IF (tstflg NE 0) THEN BEGIN
  
  ;  Execute the IDL version of the Lomb periodogram routine and display the results:
     res = LNP_TEST(per_nomiss, fap_nomiss, HIFAC=hifac, JMAX=jmax, OFAC=ofac, $
        WK1=wk1, WK2=wk2)
     if (vrbs eq 1) then begin
       PRINT, 'Results from the IDL version of the Lomb periodogram routine:'
       PRINT
       PRINT, 'Maximum power (res[0]) = ', res[0]
       PRINT, 'Statistical significance of this max = ', res[1]
       PRINT, 'Index of maximum frequency (jmax) = ', jmax
       PRINT, 'Frequency at maximum power (wk1[jmax]) = ', wk1[jmax]
       PRINT, 'Array of frequencies (wk1) = '
       PRINT, wk1
       PRINT, 'Array of power values (wk2) = '
       PRINT, wk2
       WINDOW, /free
       PLOT, $
          wk1, $
          wk2, $
          XTITLE = 'Frequencies (wk1)', $
          YTITLE = 'Power values (wk2)'
     endif     
  ENDIF
  
  ;  Compute the Lomb Normalized Periodogram using my routine, which provides more
  ;  detailed information on the entire power spectrum (and in particular on multiple
  ;  possibly significant power peaks):
  rc = numrec_period(per_nomiss, fap_nomiss, nfap_nomiss, ofac, hifac, $
     freq, powr, np, ph, f, nout, jmax, prob)
  ;rc=gs6t_plt_acorr()
  
  ;  Compute standard significance levels to assess the reliability of the peaks
  ;  in this periodogram:
  effm = hifac * nfap_nomiss
  IF wantplotlomb THEN BEGIN  ;remove form image analysis to speed up the code
    sig_lev_000 = sig_lev(effm, 0.0001)
    sig_lev_001 = sig_lev(effm, 0.001)
    sig_lev_005 = sig_lev(effm, 0.005)
    sig_lev_010 = sig_lev(effm, 0.010)
    sig_lev_050 = sig_lev(effm, 0.050)
    sig_lev_100 = sig_lev(effm, 0.100)
    sig_lev_500 = sig_lev(effm, 0.500)
  ENDIF
  ;  Set the range of low frequencies to watch, from 0 to maxfpy, in cycles per year, and
  ;  convert this value to the correct unit (maxfpy per 365 days, maxfpy per 36 decadal
  ;  periods or maxfpy per 12 months):
  maxfpy = 4.0
  CASE period OF
     1: freq_low_tr = maxfpy / 365.0
     10: freq_low_tr = maxfpy / 36.0
     30: freq_low_tr = maxfpy / 12.0
     ELSE: BEGIN
        retcode = 10
        if (vrbs eq 1) then begin
          PRINT, '*** phenot_p2_stat2.pro [ERROR]:
          PRINT, '    Unrecognized data periodicity: period = ', period
          RETURN, retcode
        endif
     END
  ENDCASE
  
  ;  Subset the frequency and power arrays to conserve only the low frequency components,
  ;  in this case all frequencies lower than maxfpy per year, and save these frequencies
  ;  and the corresponding power values in separate arrays:
  freq_low_idx = WHERE(freq LE freq_low_tr, freq_low_cnt)
  freq_low = freq [freq_low_idx]
  powr_low = powr [freq_low_idx]
  
  IF wantplotlomb THEN BEGIN
    ;  Then set the desired statistical significance level to detect peaks and reset the
    ;  power values lower than this threshold to this minimum value (this is to make sure
    ;  numminmax will work correctly, even if only one significant frequency is detected
    ;  above the threshold):
    pow_tr = sig_lev_000
    powr_low_tr = powr_low
    reset_idx = WHERE(powr_low_tr LE pow_tr)
    powr_low_tr [reset_idx] = pow_tr
    
    ;  Detect the number of statistically significant power peaks:
    rc = numminmax(powr_low, pow_tr, 999.99, nseqs, $
       nmins, nmaxs, mins, maxs, imins, imaxs)
    
    ;  At this point, nmaxs significant peaks have been identified in the periodogram,
    ;  the array maxs contains their values and the array imaxs contains their indexes
    ;  (positions) in the original powr_low array, which is itself a subset of powr.
    ;  Convert the corresponding frequencies in cycles per year:
    freq_ss = freq[imaxs]
    CASE period OF
       1: freq_ss = freq_ss * 365.0
       10: freq_ss = freq_ss * 36.0
       30: freq_ss = freq_ss * 12.0
    ENDCASE
  ENDIF
  ;  Identify the largest frequency (in units of cycles per year) for which a
  ;  statistically significant power peak has been detected, and take it, rounded
  ;  to the nearest integer, as the expected number of peaks present per year in
  ;  the original FAPAR data. This is an initial estimate of the number of growing
  ;  seasons per year, although a significant semi-annual peak can be deteted even
  ;  with a very small 'bump' in the input record, if the main (annual) seasonal
  ;  cycle is itself very pronounced:
  ;  
  ;MM+ use the one from autocorrelation instead
  ;ngspy = ROUND(freq_ss [nmaxs - 1])
  ;MM-
   ;MM+
   ;the following can be wrong (nyears is not well defined..) 
   ;freq_low_cpy = freq_low * npts / nyears
    CASE period OF
      1:  freq_low_cpy = freq_low * 365.0
      10: freq_low_cpy = freq_low * 36.0
      30: freq_low_cpy = freq_low * 12.0
   ENDCASE
   ;freq_low_cpy = freq_low * npts / nyears 
   ;store the ratio of peak V=1 and peak V=2
   min1=MIN(ABS(freq_low_cpy-1.0), ind1, /NAN)
   min2=MIN(ABS(freq_low_cpy-2.0), ind2, /NAN)
   lombrat=powr_low[ind1]/powr_low[ind2]
   ;MM-
  ;  Save the results in a text file: M+ removed
  ;      
    ;  Optionally generate plots of the Lomb Normalized Periodogram, both on screen
    ;  and in an external graphics file:
  IF (wantplotlomb) THEN BEGIN
  
  ;  Set the array of frequencies in terms of cycles per year instead of cycles
  ;  per time period covered by the original data:
     ;MM+
     ;the following can be wrong (nyears is not well defined..) 
     ;freq_low_cpy = freq_low * npts / nyears
      CASE period OF
        1:  freq_low_cpy = freq_low * 365.0
        10: freq_low_cpy = freq_low * 36.0
        30: freq_low_cpy = freq_low * 12.0
     ENDCASE
     ;freq_low_cpy = freq_low * npts / nyears
     ;MM-
     SET_PLOT, screen
     wintit = 'Lomb Normalized Periodogram'
     WINDOW, /FREE, TITLE = wintit
     
  ;  Plot the Lomb Normalized Periodogram:
     PLOT, $
        freq_low_cpy, $
        powr_low, $
        XTITLE = 'Frequency [cycle per year]', $
        YTITLE = 'Lomb Normalized Periodogram', $
        TITLE = 'LOMB_RATIO v1/v2 = ' + STRTRIM(lombrat, 2), $
;        site_name + ' [' + $
;           STRTRIM(STRING(site_lat), 2) + ', ' + $
;           STRTRIM(STRING(site_lon), 2) + ']' + 
        BACKGROUND = 16777215, COLOR = 0 
  
  ;  Add a horizontal line to indicate the 0.0001 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_000, sig_lev_000], $
        LINESTYLE = 0,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.85, $
        sig_lev_000, $
        '0.0001',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.001 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_001, sig_lev_001], $
        LINESTYLE = 3,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.75, $
        sig_lev_001, $
        '0.001',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.005 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_005, sig_lev_005], $
        LINESTYLE = 2,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.85, $
        sig_lev_005, $
        '0.005',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.010 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_010, sig_lev_010], $
        LINESTYLE = 1,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.75, $
        sig_lev_010, $
        '0.010',  COLOR=0
  
  ;  Repeat these operations to save th graph in an external file:
     fn = 'LNP-graph'
     SET_PLOT, 'PS'
     DEVICE, $
        FILENAME = run_dir + fn + '.eps', $
        FONT_SIZE = 9, $
        /ENCAPSULATED
        !P.MULTI = [0, 0, 0, 0, 0]
  
  ;  Plot the Lomb Normalized Periodogram:
     PLOT, $
        freq_low_cpy, $
        powr_low, $
        XTITLE = 'Frequency [cycle per year]', $
        YTITLE = 'Lomb Normalized Periodogram', $
        TITLE = site_name + ' [' + $
           STRTRIM(STRING(site_lat), 2) + ', ' + $
           STRTRIM(STRING(site_lon), 2) + ']', $
         BACKGROUND = 16777215, COLOR = 0 
  
  ;  Add a horizontal line to indicate the 0.0001 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_000, sig_lev_000], $
        LINESTYLE = 0,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.85, $
        sig_lev_000, $
        '0.0001',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.001 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_001, sig_lev_001], $
        LINESTYLE = 3,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.75, $
        sig_lev_001, $
        '0.001',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.005 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_005, sig_lev_005], $
        LINESTYLE = 2,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.85, $
        sig_lev_005, $
        '0.005',  COLOR=0
  
  ;  Add a horizontal line to indicate the 0.010 significance level:
     OPLOT, $
        [!X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.70], $
        [sig_lev_010, sig_lev_010], $
        LINESTYLE = 1,  COLOR=0
     XYOUTS, $
        !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.75, $
        sig_lev_010, $
        '0.010',  COLOR=0
  
  ;  Close the file and return the graphics output to the screen:
     DEVICE, /CLOSE
     SET_PLOT, screen
  
  ENDIF
  IF lombrat LT lombRatThreshold THEN ngspy = 2 ELSE ngspy = 1
ENDIF ; use_lomb



;MM+
use_median_year=1
if use_median_year eq 1 then begin
  ;extract the median year from fap_nomiss
  
  ndec=N_ELEMENTS(fapar)
  ;indarray=indgen(ndec)+1 ;indarray starts at 1 [1,2,3...,ndec]
  ;treatment of first decade
  indarray=indgen(ndec)+fst_dec ;indarray starts at 1 [1,2,3...,ndec]
  ;build the median year
  medyear=fltarr(36)
  FOR i=1,36 DO BEGIN
    ind=where(((indarray-i) MOD 36.0) eq 0.0)
    rc50 = prctl(0.50, fapar[ind], 1, ibel, iabo, 0, val)
    ;case where only one obs is available, retain it:
    IF rc50 EQ 30 THEN BEGIN
      ;find where the good obs is
      ind2=WHERE(fapar[ind] GT ibel OR fapar[ind] GT iabo, count2)
      IF (count2 EQ 1) THEN val = fapar[ind[ind2]]
    ENDIF
    medyear[i-1]=val
  ENDFOR
  
  
  ;compute the moving avarage
  ;to avoid problems at the borders (failure of finding a minimum in an array where the first element is already 
  ; or near the minimum), replicate median year
  medyear=[medyear,medyear,medyear] 
  ;the smoothing window is set according to the number of GS
;  if (ngspy eq 1) then hwindow=9 else hwindow=6 
;  sgfilter = SAVGOL(hwindow, hwindow, 0, 3 , /DOUBLE ) 
;  y_smooth=CONVOL(medyear, sgfilter,/NAN, /EDGE_TRUNCATE)
  
  ;In the case that the time series is short (2-3 years) it may happen that medyear contain
  ;missing points (-999) because all data where -999. In this case, replace missing values with
  ;linear interpolation:
  indign=WHERE(medyear LT ibel OR medyear GT iabo, countign) 
  IF (countign GT 0) THEN medyear[indign]=!VALUES.F_NAN
  medyear = INTERPOL2(medyear, N_ELEMENTS(medyear))
  
  if (ngspy eq 1) then width=7 else width=5
  rc=smoothmv2(medyear, ibel, iabo, width, sfapar) ;use MV version because savgol with NaN has problem
  ;y_smooth=y_smooth[36:71]
  ;find the minima
  rc=modifiedEXTREMA('min', sfapar, min_ind)
  IF (rc EQ 0) THEN BEGIN
    ;retrieve only the ones in the central part of the window
    min_ind=min_ind[where((min_ind GE 36) AND (min_ind LE 71))]
    nmin=N_ELEMENTS(min_ind)
  ENDIF ELSE BEGIN
    RETURN, 30
  ENDELSE
  ;if it's not found, it is at the limits (???)
  if nmin lt 1 then min_ind=0

  ;Smooth more until the correct number of minima are found
  maxiter = 50
  IF (nmin GT ngspy) THEN BEGIN
    iter=0
    REPEAT BEGIN 
      iter = iter + 1
      rc=smoothmv2(sfapar, ibel, iabo, width, ssfapar) ;smooth again
      rc=modifiedEXTREMA('min', ssfapar, min_ind)
      ;retrieve only the ones in the central part of the window
      min_ind=min_ind[where((min_ind GE 36) AND (min_ind LE 71))]
      nmin=N_ELEMENTS(min_ind)
      sfapar=ssfapar
      IF iter EQ maxiter THEN BEGIN
        RETURN, 30  ;if no success after 10 iteration, return unrec data periodicity
      ENDIF 
    ENDREP UNTIL nmin EQ ngspy
  ENDIF 
  ;MM-
  ;choose the first minima as the offset of the time series
  xoffst=intarr(2)
  xoffst[0]=min_ind[0]-36-fst_dec
  ;Manage the case were I have two GS but only 1 breakpoint was found
  ;this means that autocorr found two peaks, but the minimum between the two is 
  ;resolved by modifiedExtrema, it's too little, the two seasons are mixed
  ;Reset GS to 1 in this case
  IF (ngspy EQ 2) AND (nmin LT 2) THEN BEGIN
    ngspy = 1
    retcode = 1 ;recorder that anomalies were found in determining the number of GS or setting breakpoints 
  ENDIF
  ;Assign the second breakpoint if there are still 2 gs
  IF (ngspy eq 2) THEN xoffst[1]=min_ind[1]-36-fst_dec
  
  ;Manage the case where there are two season and one of the interval is less than mingoodf
  IF (ngspy EQ 2) AND (xoffst[1]-xoffst[0] LE mingoodf) THEN  BEGIN
    ngspy=1;set 1 season
    retcode = 1 ;recorder that anomalies were found in determining the number of GS or setting breakpoints 
  ENDIF
    
    
  ;plot the result if requested
  if (pltres eq 1 ) then begin
    window, /free, TITLE='MEDIAN YEAR and SMOOTH'
    plot, sfapar, BACKGROUND = 16777215, COLOR = 0
    oplot, medyear, color=3000
    for i = 0, nmin-1 do oplot, [min_ind[i], min_ind[i]], [0,1], color=0
  endif
  ;for presentation
  if (pltres eq 1 ) then begin
    window, /free, TITLE='MEDIAN YEAR and SMOOTH2'
    plot, medyear[0:35], color=0, BACKGROUND = 16777215, xtitle='Dekads', ytitle='FAPAR', charsize = 1.2,  charthick = 1.2
    oplot, sfapar[0:35],  COLOR = 3000 
    for i = 0, nmin-1 do oplot, [min_ind[i]-36, min_ind[i]-36], [0,1], color=0, thick=3
    xyouts, 26, 0.05, '- Median year', /DATA, COLOR= 0 , charsize = 1.2
    xyouts, 26, 0.03, '- Smoothed median year', /DATA, COLOR= 3000, charsize = 1.2
    for i = 0, nmin-1 do xyouts, min_ind[i]-36+0.5, 0.35, 'Break '+strtrim(i+1,2), /DATA, COLOR= 0 , charsize = 2, charthick = 1.5
    
  endif
  ;compute the number of full years available
  IF (xoffst[0] LT 0) THEN temp = 0 ELSE temp = xoffst[0]
  n_full_solar_years=floor((ndec-(temp+1))/36.0)
  ;check if there are more dekads available (used in phenot_p3_pdhtf_mm.pro
  n_remaing_year= (ndec-(temp+1)) MOD 36.0
endif ;use_median_year
;MM-
;xoffst=[0,35]    only used for Niger
;MM15 set empty_season to no before going to optimization, this variable is set in p3_pdhtf
empty_season = 0
RETURN, retcode

END


;NOT USED CODE:
;
  ;window, /free
  ;plot, freq, powr, xtitle='freq', ytitle='power'
  ;
  ;diffpwr = wk2 - powr
  ;print, 'max diff power = ', max(diffpwr)
  ;window, /free
  ;plot, diffpwr
  ;
  ;print, 'max power my algo = ', max(powr, maxidx)
  ;print, 'index max = ', maxidx
  ;print, 'freq at max power my algo = ', freq[maxidx]
  ;
  ;;  Make an extra copy the complex Pseudo-Fourier spectrum:
  ;fo = f
  ;
  ;;conf = 98.0
  ;;alpha = (100.0 - conf) / 200.0
  ;alpha = 0.999
  ;level = IMSL_NORMALCDF(alpha, /INVERSE)
  ;nf = N_ELEMENTS(f)
  ;spec = f[1:FIX(nf / 4)]
  ;noise_limit = SQRT(level * MEAN(spec * CONJ(spec)))
  ;print, 'noise_limit = ', noise_limit
  ;
  ;window, /free, title = 'Full f'
  ;plot, f
  ;
  ;;  Remove the noise:
  ;ind = WHERE(ABS(f) LT noise_limit)
  ;f [ind] = 0.0
  ;f [0] = fo [0]
  ;window, /free, title = 'f without noise'
  ;plot, f
  ;
  ;;  Plot the spectrum:
  ;print, 'dim (freq) = ', N_ELEMENTS(freq)
  ;print, 'dim (f) = ', N_ELEMENTS(f)
  ;print, 'dim (fo) = ', N_ELEMENTS(fo)
  ;
  ;dim = N_ELEMENTS(freq)
  ;freq_cpy = freq * npts / nyears
  ;ao_spec = ABS(fo[1:dim+1]) * 2.0 / N_ELEMENTS(f)
  ;window, /free, title = 'ao_spec'
  ;plot, freq_cpy, ao_spec
  ;a_spec = ABS(f[1:dim+1]) * 2.0 / N_ELEMENTS(f)
  ;window, /free, title = 'a_spec'
  ;plot, freq_cpy, a_spec
  ;
  ;;;  Reconstruct a complete time series:
  ;;fb = FFT(f, /INVERSE)
  ;;yb = FLOAT(fb)
  ;;
  ;;;  and plot it on top of the original series (code borrowed from phenot_plt_fapar):
  ;;fst_day = JULDAY(1, 1, fst_year) - 1
  ;;lst_day = JULDAY(12, 31, lst_year) + 1
  ;;xtime = TIMEGEN(START = fst_day, FINAL = lst_day, DAYS = [1, 11, 21])
  ;;screen_dims = GET_SCREEN_SIZE(RESOLUTION = resolution)
  ;;SET_PLOT, screen
  ;;xs = FIX(screen_dims [0] / 100) * 100
  ;;ys = FIX(xs * 0.4)
  ;;!P.MULTI = [0, 0, 0, 0, 0]
  ;;rc = LABEL_DATE(DATE_FORMAT = ['%M', '%Y'])
  ;;WINDOW, /FREE, XSIZE = xs, YSIZE = ys, TITLE = 'Original and reconstructed TS'
  ;;
  ;;plot, xtime, yb [0:N_ELEMENTS(periods) - 1], /NODATA
  ;;oplot, julian1, yb [0:N_ELEMENTS(periods) - 1], LINESTYLE = 2
  ;;oplot, julian1, fapar, $
  ;;   MIN_VALUE = ibel, $
  ;;   MAX_VALUE = iabo, $
  ;;   LINESTYLE = 0
  ;;
  ;;
  ;;
  
  ;retcode = 111
  ;
  
  
  
  
    ;Manage the case where there are more minima than GS
  ;choose the minimum with the lowest fapar
  ;one GS and more than 1 minima
  ;MM+ improved on the 17 of august 2011
;  IF (ngspy EQ 1) AND (nmin GT 1) THEN BEGIN ;just choose the first ..
;    tmp=MIN(sfapar(min_ind), ind1)
;    min_ind=min_ind[ind1]
;    nmin=N_ELEMENTS(min_ind)
;  ENDIF
;  IF (ngspy EQ 2) AND (nmin GT 2) THEN BEGIN ;just choose the first two..
;    tmp=sfapar(min_ind)
;    min_ind=min_ind(sort(tmp))
;    min_ind=[min_ind[0],min_ind[1]]
;    min_ind=min_ind[sort(min_ind)]
;    nmin=N_ELEMENTS(min_ind)
;  ENDIF