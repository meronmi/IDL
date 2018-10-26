FUNCTION Set_Prosail_param, SLA
;PROSAIL PARAMETERS
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

proSailVar = { $
  Cab    : 29.0d    ,$; chlorophyll content (µg.cm-2) (0-100 ug cm-2) for grassland (Atzberger et al., Suitability and
  ;adaptation of PROSAIL radiative transfer model for  hyperspectral grassland studies,
  ;RS Letters, Vol. 4, No. 1, January 2013, 55–64, DOI: 10.1080/2150704x.2012.689115
  Car    : 0.0d     ,$; 12.0d    ;; carotenoid content (µg.cm-2)  (0-25 ug cm-2)
  Cbrown : 0.0d     ,$;0.5d     ;; brown pigment content (0-1 arbitrary units)
  Cw     : 0.015     ,$;0.02, 0.01;0.015d   ;; EWT (cm) - Equivalent water thickness (0-0.05 cm) Vohland and Jarmer 2008 
  Cm     : 1.0d/(SLA*100*100)   ,$;0.1d     ;; LMA (g cm-2) - leaf mass per unit leaf area 
  ;Cm  (0.005-0.01 g cm-2 according to Darvishzadeh et al, 2008, RSE)
  ;SLA (0.02 - 0.01 m2 g-1)
  Ns     : 1.6d     ,$;; structure coefficient (1-3 dimensionless) Vohland and Jarmer 2008 
  lidf_a : -0.35    ,$
  lidf_b : -0.15    ,$
;  lidf_a : -1.0    ,$
;  lidf_b : 0.0    ,$
  lai    : 0.0d    ,$;; leaf area index
  hspot  : 0.05d    ,$; hot spot for grass (Atzberger et al., as above)
  tts    : 30.0d    ,$;; solar zenith angle (°)
  tto    : 0.0d     ,$;; observer zenith angle (°)
  psi    : 0.0d     ,$;; relative azimuth (°)
  rsoil  : FLTARR(2101)}     ; soil coefficient
RETURN, proSailVar
END