FUNCTION Jfunc1c, k,l,t

k = double(k)
l = double(l)
t = double(t)
;k = FLOAT(k) SLOWER!!
;l = FLOAT(l)
;t = FLOAT(t)
;compute once exp(-l*t) and exp(-k*t)
a = exp(-l*t) 
b = exp(-k*t)
;compute (k+l)
;c = (k + l)
;compute del
k_l = (k-l) 
j1=k_l*t;;

;j2 = (1.0 - a * b)/ c

gt_index = WHERE(ABS(j1) GT 1e-3, gt_count, COMPLEMENT = le_index, NCOMPLEMENT=le_count)
if (gt_count gt 0) then $
  j1[gt_index] = (a[gt_index]-b)/(k_l[gt_index])
if (le_count gt 0) then $
  j1[le_index] = 0.5*t*(b+a[le_index])*(1.0-(j1[le_index]^2)/12.0)  
RETURN, j1
END