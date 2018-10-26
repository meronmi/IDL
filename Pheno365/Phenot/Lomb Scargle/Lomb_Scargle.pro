Function Lomb_Scargle, x,y, MISSING=MISSING, CORRELOGRAM=CORRELOGRAM, LAG_SIZE=LAG_SIZE

;calling sequence:
;result = Lomb_Scargle(x,y) ; Just simple gap filling and smoothing
;result =  Lomb_Scargle(x,y,/Missing) ; Simple gap filling and smoothing, 
;          but appart from gap filled arrays (x,y) two more array are returned (xmissing,ymissing), 
;         which contains only missing data.  
;result = Lomb_Scargle(x,y,/CORRELOGRAM, LAG_SIZE=5) ; Simple gap filling and smoothing with correlogram of residuals
; to rerieve the results just type:
;result.x - for gap filled x/time array
;result.y - for gap filled and smooth y/data array   

; Here the range and precision of ALP parameter is set. This controls the number of interation which hase to done in order to retrieve the ALP parameter.
Prec = 0.05
Pmin = 0.05
Pmax = 2.0

;Sampling is the smallest time interval detected in the input data/x array
sampling = min(x[indgen(n_elements(x)-1)+1]-x[indgen(n_elements(x)-1)])
xo = x

;Jan: If You uncomment next three lines the premilinary smoothing and gap-filling with smoothing spline will take place.
;This is done due to optimize the cross-validation procedure which specifies the parameter ALP for KAISER-BESSEL window, in the interative way.
;     
;pp = IMSL_CSSMOOTH(x,y)
;x = FINDGEN((max(x)-min(x))/sampling+1)*sampling + x[0]
;y = IMSL_SPVALUE(x, pp)

;Jan: Here the arrays_compare procedure is invoked, due to find which elements are missing
;It is not necessary unless heayword missing is set.
IF KEYWORD_SET(MISSING) THEN BEGIN
compare = arrays_compare(xo,x)
xmissing_ind = compare.arr2.ind_dif 
ENDIF

end_flag = 0
xo = x
yo = y
REPEAT BEGIN

;JAN: here the interative optimization of ALP paramater for KAISER-BESSEL window starts

FOR ALP=Pmax, Pmin, (-1*(Pmax - Pmin)/4) DO BEGIN

IN_LOOP: $
x = xo
y = yo
mm = mean(y) 
y = y - mm 
window = FFT_TAPER(N_ELEMENTS(y),'KAISER-BESSEL',ALP) 
y = y * window
y = y + mm

matlab_lspr, x, y, wk1, wk2, ph, vari, hifac, ofac, f_uc

fo_uc = f_uc

;  First approach:
;  Set the confidence level to evaluate the significance of the power
;  spectrum peaks:
;conf = 99
;alpha = (100.0 - conf) / 200.0

;MMV: The Matlab call to norminv appears to compute at once the 2 values
;  of the Normal distribution corresponding to the two confidence levels
;  provided, but only the second one is used. The IMSL function works with
;  only one input, so it is called only once:
;level = IMSL_NORMALCDF(1.0 - alpha, /DOUBLE, /INVERSE)
;nf = N_ELEMENTS(f_uc)
;spec = f_uc [1:FIX(nf / 4.0) - 1]
;noise_limit = SQRT(level * MEAN(spec * CONJ(spec)))

;  Set the threshold at 1/3 of the highest peak in the the power spectrum:
noise_limit = MAX(ABS(f_uc[1:N_ELEMENTS(f_uc) - 1]))/3

;  Zero the power spectrum for those frequencies where the power is less
;  than the threshold, but keep the 0-frequency value:
ind = WHERE(ABS(f_uc) LT noise_limit)
f_uc [ind] = 0.0        ;removal of the noise
f_uc[0] = fo_uc[0]      ;

fb_uc = FFT(f_uc, /INVERSE)
yf = REAL_PART(fb_uc)
yf = yf/n_elements(fb_uc) ; Jan: This statment is written because the Matlab variable yf is devided by number of element of array fb_uc
xf = x[0] + INDGEN(N_ELEMENTS(f_uc)) / (wk1 [1] - wk1 [0]) / (N_ELEMENTS(f_uc)) 
ind = WHERE(xf LE x [N_ELEMENTS(x) - 1])
xf = xf [ind]
yf = yf [ind]

yf = yf / FFT_TAPER(N_ELEMENTS(yf),'KAISER-BESSEL',ALP) 

sc=mean(abs(yo-mean(yo)))/mean(abs(yf))

yf = sc * yf + REAL_PART(f_uc [0])
yf = yf + (mean(yo)-mean(yf))

; Jan: Here once more smoothin spline take place, but this time on smooth data which Lomb-Scargle rootine produce, so it has no impact on the end results. 
; This is done due to inconsistency which Lomb Scargle rootine produce. Instead of returning the original time interval and time values (x values) Lomb-Scargle procedure
; sometimes returns wrong time/x values. E.G. input = [0,10,20,30], Lomb_output = [2,14,25,31]. In this case smoothing spline is just used to interpolate data. 
pp = IMSL_CSSMOOTH(xf, yf)
x = FINDGEN((max(x)-min(x))/sampling+1)*sampling + x[0]
yf = IMSL_SPVALUE(x, pp)
compare = arrays_compare(xo,x)

; Jan: here the RMSE is calculated between the output array from Lomb-Scargle routine and original input array (only for corresponding elements of course).
residual = yo - yf[compare.arr2.ind_same]
MSE = mean(residual^2)
RMSE_ = SQRT(MSE)

IF end_flag EQ 1 THEN GOTO, EXIT_

IF n_elements(RMSE_ALL) EQ 0 THEN RMSE_ALL = RMSE_ ELSE RMSE_ALL = [RMSE_ALL,RMSE_]
IF n_elements(ALP_ALL) EQ 0 THEN ALP_ALL = ALP ELSE ALP_ALL = [ALP_ALL,ALP]
IF n_elements(N_ITER) EQ 0 THEN N_ITER = 1 ELSE N_ITER = N_ITER + 1

ENDFOR

IF (ABS(Pmax-Pmin)/4 LE PREC AND end_flag EQ 0) THEN BEGIN
end_flag=1
ALP = ALP_ALL[WHERE(RMSE_ALL EQ MIN(RMSE_ALL))]
ALP=ALP[0]
GOTO, IN_LOOP 
ENDIF

Pmax = ALP_ALL[WHERE(RMSE_ALL EQ MIN(RMSE_ALL))] + ABS(Pmax-Pmin)/4
Pmin = ALP_ALL[WHERE(RMSE_ALL EQ MIN(RMSE_ALL))] - ABS(Pmax-Pmin)/4 
Pmax = Pmax[0]
Pmin = Pmin[0] 

ENDREP UNTIL end_flag EQ 1 

EXIT_: $

window,/free
plot, ABS(f_uc[0:300])

;Drawing the corerelogram if specified

IF KEYWORD_SET(CORRELOGRAM) THEN BEGIN
IF KEYWORD_SET(LAG_SIZE) THEN lag = indgen(LAG_SIZE) ELSE lag = indgen(N_ELEMENTS(x)*0.2)
auto_corr = A_CORRELATE(residual,lag)
window, /free,title='CORRELOGRAM'
plot, lag, auto_corr 
endif

result = {name:'', x:x, y:yf, rmse:rmse_}

;If the keyword Missing was specified, routine returns also two array (xmissing, ymissing) with only missing values.
IF KEYWORD_SET(MISSING) THEN BEGIN

xmissing = x[xmissing_ind]
ymissing = yf[xmissing_ind]
result = {name:'', x:x, y:yf, rmse:rmse_, xmis:xmissing, ymis:ymissing}

ENDIF

print, 'ALP',ALP

return, result

END

;COPY
;Function Lomb_Scargle, x,y, Hamming=hanning
;;  Set the flag to decide whether to apply the Hamming window algorithmor not:
;IF KEYWORD_SET(hanning) THEN win = 1 ELSE win = 1
;
;ALP = 1
;
;xo = x
;yo = y
;
;;MMV: The Hann (Hanning) and Hamming window filters are implemented as
;;  separate functions in Matlab (actually in the Spectral Processing
;;  Toolbox). In IDL, the Hamming window is an option of the Hann(Hanning) algorithm.
;IF (win) THEN BEGIN
;
;   y = y - mean(y) 
;
;   window = FFT_TAPER(N_ELEMENTS(y),'KAISER-BESSEL',ALP) 
;
;   y = y * window
;   y = y + 1
;ENDIF
;
;;good filteres:
;;window = FFT_TAPER(N_ELEMENTS(y),'KAISER-BESSEL',ALP=0.78)
;
;matlab_lspr, x, y, wk1, wk2, ph, vari, hifac, ofac, f_uc
;
;
;fo_uc = f_uc
;
;;  First approach:
;;  Set the confidence level to evaluate the significance of the power
;;  spectrum peaks:
;conf = 99
;alpha = (100.0 - conf) / 200.0
;
;;MMV: The Matlab call to norminv appears to compute at once the 2 values
;;  of the Normal distribution corresponding to the two confidence levels
;;  provided, but only the second one is used. The IMSL function works with
;;  only one input, so it is called only once:
;level = IMSL_NORMALCDF(1.0 - alpha, /DOUBLE, /INVERSE)
;nf = N_ELEMENTS(f_uc)
;spec = f_uc [1:FIX(nf / 4.0) - 1]
;noise_limit = SQRT(level * MEAN(spec * CONJ(spec)))
;
;
;;  Set the threshold at 1/3 of the highest peak in the the power spectrum:
;
;;noise_limit = MAX(ABS(f_uc[1:N_ELEMENTS(f_uc) - 1]))/3
;;  Zero the power spectrum for those frequencies where the power is less
;;  than the threshold, but keep the 0-frequency value:
;ind = WHERE(ABS(f_uc) LT noise_limit)
;f_uc [ind] = 0.0          ;removal of the noise
;f_uc[0] = fo_uc[0]      ;
;
;f_uc1=f_uc
;
;;window,/free
;;plot, ABS(f_uc1[0:100])
;;oplot, ABS(f_uc1[0:100]),psym=2
;;oplot, [0,100000],[noise_limit,noise_limit], linestyle=1,color=1000
;
;window,/free
;plot, ABS(fo_uc[0:100])
;oplot, [0,100000],[noise_limit,noise_limit], linestyle=1,color=1000, thick=3
;
;fb_uc = FFT(f_uc1, /INVERSE)
;yf = REAL_PART(fb_uc)
;yf = yf/n_elements(fb_uc) ; Janek: This statment is written because the Matlab variable yf is devided by number of element of array fb_uc
;xf = x[0] + INDGEN(N_ELEMENTS(f_uc)) / (wk1 [1] - wk1 [0]) / (N_ELEMENTS(f_uc)) 
;ind = WHERE(xf LE x [N_ELEMENTS(x) - 1])
;xf = xf [ind]
;yf = yf [ind]
;
;IF (win) THEN BEGIN
;
;
;yf = yf / FFT_TAPER(N_ELEMENTS(yf),'KAISER-BESSEL',ALP) 
;
;ENDIF
;
;
;sc=mean(abs(yo-mean(yo)))/mean(abs(yf))
;
;yf = sc * yf + REAL_PART(f_uc [0])
;yf = yf + (mean(yo)-mean(yf))
;
;pp = IMSL_CSSMOOTH(xf, yf)
;
;sampling = min(x[indgen(n_elements(x)-1)+1]-x[indgen(n_elements(x)-1)])
;
;x = FINDGEN((max(x)-min(x)+1)/sampling)*sampling 
;yf = IMSL_SPVALUE(x, pp)
;compare = arrays_compare(xo,x)
;residual = yo - yf[compare.arr2.ind_same]
;MSE = mean(residual^2)
;RMSE = SQRT(MSE)
;
;
;;lag = indgen(75)
;;auto_corr = A_CORRELATE(residual,lag)
;;window, /free
;;plot, lag,ABS(auto_corr) 
;
;result = {name:'', x:x, y:yf}
;
;return, result
;
;END

;MMV: Upon returning from lspr,
;  wk1 = array of increasing frequencies, from 1.0 / (range(x) * ofac)
;     to ofac times the nyquist frequency f_c = 1/(2 * sampling interval),
;     in increments of 1.0 / (range(x) * ofac), where range(x) = MAX(x) - MIN(x).
;  wk2 = array of Lomb-Scargle Normalized power values for the
;     frequencies provided in wk1
;  ph = array of phases, in radians, for each of the frequencies
;     provided in wk1 (arguments of the COS function at x = 0)
;  vari = sigma^2, variance of y, used to derive the amplitude from
;     the normalized power wk2
;  hifac = an integer indicating up to which maximum frequency the
;     analysis proceeds, in units of Nyquist frequency (typically 1 or 2)
;  ofac = oversampling factor, typically 4
;  f_uc = complex Pseudo-Fourier spectrum

;  Save the original complex Pseudo-Fourier spectrum so that it can be
;  modified to retain only the desired frequencies: