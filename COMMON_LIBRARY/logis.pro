PRO logis, x, p, f
;res = CURVEFIT(l, ffa, l*0+1, [0.9, 1.0], FUNCTION_NAME='logis', /NODERIVATIVE)
;fg for lai - fapar: l=0.9 k=1
l = FLOAT(p[0])
k = FLOAT(p[1])
f = l * (1.0/(1+EXP(-k*x))-0.5)

END