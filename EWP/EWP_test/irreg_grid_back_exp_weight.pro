PRO test00
  ;x = FINDGEN(10)*30.0
  x = 10+[0.000000,30.0000,60.0000,88.0000,120.000,150.000,180.000,210.000,240.000,270.000]
  y = x*0.0+10.0;[2,2,2,2,0,0,0,0,4,4]
  ;test
  x = FINDGEN(25)+1 & y = x*0.0+20.0 & y[0]=0 & p = [0,10,20,0,0,0,0,20,0,10,0,0,0,0,0,0,0,0,0,20,10,30,0,0,0]
  & half_life=34.72222 
  PRINT, x
  PRINT, y
  TIC
  res =  REFORM(irreg_grid_back_exp_weight(x,y,34.72222))
  TOC
  PRINT, res
END

FUNCTION irreg_grid_back_exp_weight, x, y, hl
  ;27 /11 matrix version as suggested by Dominique,
  ;version optimized for dekadal data

  ; Computes the backward exponentially weighted mean of y with x irregularly gridded
  ; INPUTS
  ; - x is the dekadal cumulative temperature
  ; - y is the dekadal cumulative  precipitation
  ; - hl, half_life controls the steepness of the decay, it's the time (expressed in GDD units) in which
  ;   the weights fall to one half

  ; COMPUTED
  ;                                                                                                   k
  ; - g is GDD from the first element, the cumulative valued of y from the first element on, g(k) = SUM y
  ;                                                                                                   0
  ; therefore the delta GDD between day t and t-k is = g(t) - g(t-k)


  ;             u=5*tau                                     u=5*tau
  ; bewm(x) = SUM        (min[p(x-u), pcap])*EXP(-u/tau) / SUM EXP(-u/tau)
  ;             u=0                                         u=0
  ; where tau is the mean lifetime, tau = 1.44*half_life



  tau = hl / ALOG(2.0)   ;tau = 1.44*half_life
  ;lower integration limit
  lil = 5.0 * tau

  n = N_ELEMENTS(x)
  ;xcol = CMREPLICATE(FLOAT(x),  n)
  xcol = CMREPLICATE(x,  n)
  ;y = FLOAT(y)
  ;xrow = TRANSPOSE(xcol)
  ;PRINT, xcol
  ;PRINT, '**'
  ;PRINT, xrow
  ;xdiff is the distance betwee x elements (note that this  can be save once and for all)
  xdiff = (xcol-TRANSPOSE(xcol))
  ;here I have to take only the zero distance and the negative part up to win
  ;no sparse representation as there is no element by element multiplication over sparce matrix
  mask = (xdiff LE 0) AND (xdiff GE -lil)
  ;remove positive and big values (pos and neg), they will not be used and give floating error when exp
  xdiff = mask * xdiff
;  PRINT, 'xdiff'
;  PRINT, xdiff
  ;now the value of EXP(-u/tau), not ethat -u is the distance xdiff
  exp_mat = EXP(xdiff/tau) * mask
  
  ;xdiff = (xdiff LE 0) AND (xdiff GE -lil)
 
  ;now compute the summ of the weights, SUM EXP(u/tau)
  sum_wei = exp_mat ##  MAKE_ARRAY([1,n], VALUE=1, /FLOAT)
  ;now compute the "smoothed" array
  res = (exp_mat ## y) / sum_wei

  ;PRINT, '**'
  ;PRINT, xdiff
  ;here I have to take only the zero distance and the negative part up to win
  ;xdiff = (xdiff LE 0) AND (xdiff GT -win)
  ;use the sparse representation
  ;  sparse_xidiff = SPRSIN((xdiff LE 0) AND (xdiff GE -lil))
  ;PRINT, '**'
  ;PRINT, sparse_xidiff
  ;PRINT, y
  ;PRINT, REFORM(xdiff ## y)

  ;PRINT, SPRSAX(sparse_xidiff,y)
  ;RETURN, xdiff ## y
  RETURN, res
END