PRO test2
 a = FLOAT(INDGEN(1000))/1000.0 ;between 0-1
 sub_counter = LONARR(1000)
 FOR i = 0, 100000 DO BEGIN
  sub =  pps(1.0-a, 1)
  sub_counter[sub] = sub_counter[sub] + 1 
 ENDFOR
 hh = PLOT(a, sub_counter, SYMBOL='.', LINESTYLE='')
END

FUNCTION PPS, X, n
;probability proportional to size (PPS), https://en.wikipedia.org/wiki/Sampling_%28statistics%29#Probability-proportional-to-size_sampling
;given an array of value X and n elements to be sected, it returns the subscript of the
;random PPS selection
IF (SIZE(X, /N_DIMENSIONS)) NE 1 THEN STOP
XX = X
;;PPS is made for money, the minum is thus 1, here we use 1-RMSE so [0,1]
;;make it [1, 10001] more than 10000 bins is not necessary
;;- find the minimum)

;XX = ROUND((XX - MIN(XX)) / (MAX(XX) - MIN(XX))) * 10000.0 + 1 
;;minXX = MIN(XX)
;;XX = ROUND(XX/minXX, /L64)
;
;randomize the samples
rndSub =RandPerm(N_ELEMENTS(X), SEED=seed)
;rndSub = LONGscramble(N_ELEMENTS(X)) !!!! does not work properly with big arrays!!
;ind to be returned
sub = LONARR(n)
XX = XX[rndSub]

;total and sampling interval
BV = TOTAL(XX, /CUMULATIVE)
SI = BV[-1]/FLOAT(n)
;PRINT, SI
;generate random value between 1 and SI
start = ROUND(RANDOMU(seed, 1) * (SI-1)) + MIN(XX);+ 1
start = start[0]
;PRINT, start
;selctions = start * INDGEN(n)
FOR i = 0, n-1 DO BEGIN
  ind = WHERE(BV GE FLOAT(start+SI*i))
  ;store subscripts in the original X
  sub[i] = rndSub[ind[0]]
ENDFOR
;IF (TOTAL(sub[*]) LT 0) THEN STOP
RETURN, sub
END
 