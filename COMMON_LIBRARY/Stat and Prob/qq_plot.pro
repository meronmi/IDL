FUNCTION qq_plot, xx, TITLESTRING = titlestring, TESTNORM = testnorm
;xx=[8.84,10.26,12.39, 5.45, 9.94, 9.59, 7.23,10.08,11.96, 6.18, 6.79, 7.93,10.26,10.25,10.62,10.61, 7.90,15.88, 6.35, 9.22,10.98,12.33, 9.83, 7.35,10.56, 9.03, 8.63,11.51,11.91,14.19, 9.43, 7.64, 8.12,10.58, 9.69, 9.39, 8.34,12.98]
ind = WHERE(FINITE(xx), count)
IF (count LT 2) THEN RETURN, -1
x = xx[ind]
;compute nep
nep_x = NEP_array(x)
;now z from nep
z_x = x * !VALUES.F_NAN
FOR q = 0, count-1 DO z_x[q] = GAUSS_CVF(1.0-nep_x[q]/100.0)

;map x values to its theoretical (normal distributed) quatile
IF (N_ELEMENTS(titlestring) GT 0) THEN title = 'Q-Q Plot ' + titlestring ELSE title = 'Q-Q Plot'
gh0 = PLOT(z_x, x, SYMBOL='o', LINESTYLE='', XRANGE = [MIN(z_x)-(MAX(z_x)-MIN(z_x))/10.0, MAX(z_x)+(MAX(z_x)-MIN(z_x))/10.0], $
           TITLE=title, XTITLE='Standard Normal Quantiles', YTITLE='Quantiles of Input Sample')
IF KEYWORD_SET(TESTNORM) THEN BEGIN
  NormP = IMSL_NORMALITY(x)
  gtxt = TEXT(0.15,0.02,'p-value Shapiro-Wilk Normality test = ' + STRTRIM(NormP,2) + ' (reject Normality < alpha level)')
ENDIF

;now map the values a theoretical N would have
;compuet mean and sd of x
avg_x = MEAN(x)
sd_x = STDDEV(x)
;compute the quantile value associated to nep that a normal with avg_x and sd_x would have
nep_range = [0.5,99.5]
y = nep_range * !VALUES.F_NAN
z_nep_range =  nep_range * !VALUES.F_NAN
FOR q = 0, N_ELEMENTS(nep_range)-1 DO z_nep_range[q] = GAUSS_CVF(1.0-nep_range[q]/100.0)
FOR q = 0, N_ELEMENTS(nep_range)-1 DO y[q] = GAUSS_CVF(1.0-nep_range[q]/100.0)*sd_x+avg_x
gh1 = PLOT(z_nep_range,y, LINESTYLE='-', color = 'red', NAME = 'Normal reference quantiles', /OVERPLOT)
gh2 = LEGEND(TARGET = [gh1], LINESTYLE='', POSITION=[0.55,0.85])

RETURN, gh0
END