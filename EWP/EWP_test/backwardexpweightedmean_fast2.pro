FUNCTION BackwardExpWeightedMean_FAST2, x, y, p, half_life, pcap, option_weight

;version optimized for dekadal data

  ; Computes the exponetially weighted mean of p according to y
  ; INPUTS
  ; - x is time in days, regularly gridded, not used
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
  
  ;27/11: the constant sum was replaced by the Dominique's matrix computation, the exp mean as well,
  ;see functions
  
  y=REFORM(y)
  p=REFORM(p)
  ;compute g, total temperature
  g = TOTAL(DOUBLE(y), /CUMULATIVE)
  ;cap precipitation
  ind2cap = WHERE(p GT pcap, count2cap)
  IF (count2cap GT 0) THEN p[ind2cap] = pcap
  
  CASE option_weight OF
    'exp': BEGIN
      res = irreg_grid_back_exp_weight(g, p, half_life)
      ;old and probabibly wrong stuff:
;      ;tau
;      tau = half_life / ALOG(2.0)   ;tau = 1.44*half_life
;      ;lower integration limit
;      lil = 5.0 * tau
;      bewm  = FLTARR(N_ELEMENTS(y))*!VALUES.F_NAN    
;      ;fast alternative
;      ;make them regularly gridded
;      ;use a fixed 20 deg sampling distance
;      sdist = 50.0
;      g = ROUND(g)
;      ;pp = FLTARR(g[-1]-g[0]+1)
;      n_resampled = CEIL(((g[-1]-g[0])/sdist))
;      ;equally spaced vector ending with exactly g[-1]
;      gg = g[-1]- REVERSE(FINDGEN(n_resampled)*sdist)
;      pp = FLTARR(n_resampled)
;      used_ind = !NULL
;      FOR i = 0, N_ELEMENTS(g)-1 DO BEGIN
;        ind = WHERE(g[i] LE gg, count)
;        IF count EQ 0 THEN STOP
;        tmp = WHERE(ind[0] EQ used_ind, count_used)
;        IF count_used GT 0 THEN STOP ;we are rewriting on an already sued position
;        used_ind = [used_ind, ind[0]]
;        pp[ind[0]]=p[i]
;      ENDFOR
;      kernel = EXP(-FINDGEN(ROUND(lil/sdist))*sdist/tau)
;      res = CONVOL(FLOAT(pp), kernel, TOTAL(kernel), CENTER = 0, /EDGE_ZERO)
;      res2 = INTERPOL(res, gg, g) * 100/sdist
    END
    'const': BEGIN
      res = irreg_grid_cum(g,p,FLOAT(half_life))
      ;just the sum, not the mean
;      kernel = FLTARR(ROUND(half_life/sdist))*0.0+1.0
;      res = CONVOL(FLOAT(pp), kernel, CENTER = 0, /EDGE_ZERO)
;      res2 = INTERPOL(res, gg, g)

    END
    ELSE: STOP
  ENDCASE
  
  
  

  
  RETURN, res;res[(ROUND(g/sdist))] * 100/sdist
  
END


;*** BEGINNING OLD procudure using a FOR loop
;  ;to compute the bewm of element i, z elements are needed before i,
;  ;this z elments have to cover a span of 5*tau degree days, sp we can omly compute bewm from this on
;  first_ind = WHERE(g GE lil+g[0])
;  first_ind = first_ind[0]
;
;  FOR i = first_ind, N_ELEMENTS(x)-1 DO BEGIN
;    ;WEIGHT array
;    ;IF i EQ 24 then stop
;    ;find all observations that are at lil before i
;    ii = WHERE((g[i]-g[0:i]) GE lil)
;    ;take the nearest to i
;    ii = ii[N_ELEMENTS(ii)-1]
;    ;compute the integration variable
;    u = g[ii:i]-g[i]
;    ;compute the weights
;    w = EXP(u/tau)
;    pp = p[ii:i]
;    ;cap precipitation
;    ind2cap = WHERE(pp GT pcap, count2cap)
;    IF (count2cap GT 0) THEN pp[ind2cap] = pcap
;    ;compute the weithed average
;    bewm[i] = TOTAL(pp * w)/TOTAL(w)
;  ENDFOR
;*** BEGINNING OLD procudure using a FOR loop