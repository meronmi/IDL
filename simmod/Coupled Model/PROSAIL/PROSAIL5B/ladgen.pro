FUNCTION cum, a, b, t
; a nd b are LIDF parameter
; t is the angle in degrees
a = DOUBLE(a)
b = DOUBLE(b)
eps=1.0e-6
delx = 1.0d
x = 2.0d * !dtor * t
p = x
WHILE (delx GT eps) DO BEGIN
  y = a * SIN(x) + 0.5d * b * SIN(2.0d * x)
  dx = 0.5d * (y - x + p)
  x = x + dx
  delx = ABS(dx)
ENDWHILE

RETURN, (2.0d * y + p)/!dpi
END


; Original code written by: W. Verhoef, coded in DL by Michele Meroni (michele.meroni@gmail.com)
; DESCRIPTION:
; CALLING SEQUENCE:
;       output =  ladgen(a, b)
; INPUTS: alphadeg
; OUTPUTS: LIDF array of 13 elements
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; % LIDFa LIDF parameter a, which controls the average leaf slope
; % LIDFb LIDF parameter b, which controls the distribution's bimodality
; % LIDF type     a      b
; % Planophile    1      0
; % Erectophile   -1     0
; % Plagiophile   0      -1
; % Extremophile  0      1
; % Spherical     -0.35  -0.15
; % Uniform       0      0
; %   requirement: |LIDFa| + |LIDFb| <= 1
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:

; CODING NOTES:

FUNCTION ladgen, a, b
theta_deg = [10.,20.0,30.0,40.0,50.0,60.0,70.0,80.0,82.0,84.0,86.0,88.0,90.0]


n = N_ELEMENTS(theta_deg)
cumF = DBLARR(n)
F=dblarr(n)


FOR i=0, n-1 DO cumF[i] = cum(a, b, theta_deg[i])
cumF[n-1] = 1.0d
FOR i=1, n-1 do F[i]=cumF[i]-cumF[i-1]
F[0]=cumF[0]
RETURN, F
END