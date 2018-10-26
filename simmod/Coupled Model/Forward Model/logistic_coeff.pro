FUNCTION logistic_coeff, Yasymptotic, grow_decay_funct, x_min, x_max, x, cutoff
; for a given irregularly gridde x array (GDD)
; compute the decreasing logistic with the following characteristcs:
; - rescaling asymptotic maximum value:  Yasymptotic
; - 'grow' or 'decay' prescribed by grow_decay_funct
; - x_min: x value at which the logistic codomain (0,1) takes y = cutoff (e.g. 0.05 [0.95] if 
; the function is growing [decaying])  
; - x_max: x value at which the logistic codomain (0,1) takesy = cutoff (e.g. 0.05 [0.95] if 
; the function is growing [decaying])  
; - cutoff: the value used to truncate the function

; after that it snap the 0.05 and 0.95 to 0 and 1  

;compute x0 and k of the logistic
;C_min = ALOG(1/0.05-1)
C_min = ALOG(1.0d/DOUBLE(cutoff)-1)
;C_max= ALOG(1/0.95-1)
C_max= ALOG(1.0d/(1.0d - cutoff)-1)
k = (C_min-C_max)/(x_max-x_min)
x0 = 1.0d/k * C_min + x_min
CASE grow_decay_funct OF
  'grow': res = 1.0d / (1 + EXP(-k*(x-x0)))
  'decay': res = 1.0d / (1 + EXP(k*(x-x0)))
  ELSE: STOP
ENDCASE
;adjust outside cutoff values
;SPEED
res = (res GE cutoff) * res 
res = (res LE (1.0-cutoff)) * res + (res GT (1.0-cutoff)) * 1.0

;ind = WHERE(res LT cutoff, count)
;IF (count GT 0) THEN res[ind] = 0.0d
;
;ind = WHERE(res GT 1.0-cutoff, count)
;IF (count GT 0) THEN res[ind] = 1.0d
;;h = plot(x, DOUBLE(Yasymptotic)* res) 
RETURN, DOUBLE(Yasymptotic) * res
END