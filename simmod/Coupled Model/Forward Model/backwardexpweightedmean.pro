FUNCTION BackwardExpWeightedMean, x, y, p, half_life, pcap
; Computes the exponetially weighted mean of y
; INPUTS
; - x is time in days, regularly gridded
; - y is the mean daily temperature of 
; - p is the cumulative daily precipitation 
; - half_life controls the steepness of the decay, it's the time (expressed in GDD units) in which
;   the weights fall to one half
; - pcap is the cap value for precipitation (p values > pcap are set to pcap)
; COMPUTED
;                                                                                                   k
; - g is GDD from the first element, the cumulative valued of y from the first element on, g(k) = SUM y
;                                                                                                   0
; therefore the delta GDD between day t and t-k is = g(t) - g(t-k)


;             u=0                                         u=0
; bewm(x) = SUM        (min[p(x+u), pcap])*EXP(u/tau) / SUM EXP(u/tau)
;             u=x-5*tau                                   u=x-5*tau   
; where tau is the mean lifetime, tau = 1.44*half_life

;test
;x = FINDGEN(25)+1 & y = x*0.0+20.0 & y[0]=0 & p = [0,10,20,0,0,0,0,20,0,10,0,0,0,0,0,0,0,0,0,20,10,30,0,0,0] & half_life=34.72222 & pcap = 50

;tau
tau = half_life / ALOG(2.0)   ;tau = 1.44*half_life
;lower integration limit
lil = 5.0 * tau

bewm  = FLTARR(N_ELEMENTS(y))*!VALUES.F_NAN

;compute g
g = TOTAL(DOUBLE(y), /CUMULATIVE)
;to compute the bewm of element i, z elements are needed before i,
;this z elments have to cover a span of 5*tau degree days
first_ind = WHERE(g GE lil+g[0])
first_ind = first_ind[0] 

FOR i = first_ind, N_ELEMENTS(x)-1 DO BEGIN
  ;WEIGHT array
  ;IF i EQ 24 then stop
  ;find all observations that are at leaf lil before i
  ii = WHERE((g[i]-g[0:i]) GE lil)
  ;take the nearest to i
  ii = ii[N_ELEMENTS(ii)-1]
  ;compute the integration variable
  u = g[ii:i]-g[i]
  ;compute the weights
  w = EXP(u/tau)
  pp = p[ii:i]
  ;cap precipitation
  ind2cap = WHERE(pp GT pcap, count2cap)
  IF (count2cap GT 0) THEN pp[ind2cap] = pcap
  ;compute the weithed average 
  bewm[i] = TOTAL(pp * w)/TOTAL(w)
ENDFOR


;the weight at 5*tau is negiglible 0.009 (less than 1 %)

RETURN, bewm
END