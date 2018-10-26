Function sd_vec0_36, array0_36
  ;used to compute the sd of angle of circular data expressed as 0-360 deg
  ;as in Zar, pag 603
  rad=!DTOR*double(array0_36*10)
  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
  r = SQRT(vn^2+ve^2)
  s0 = 180.0/!PI * SQRT(-4.60517*ALOG10(r))
  return, s0/10.0
End


;Function xxmean_vec0_36, array0_36
;  ;used to compute the mean angle of circular data expressed as 0-36
;  rad=!DTOR*double(array0_36*10)
;  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
;  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
;  mv=DOUBLE(ATAN(ve, vn))
;  IF mv LT 180.0d * 1.0d / !DTOR then mv=mv * !RADEG+180.0d
;  IF mv GT 180.0d * 1.0d / !DTOR then mv=mv * !RADEG-180.0d
;  ;the mean of 35 and 1 is 2.8421709430404009e-015
;  ;snap it to 36 if mean <0.01
;  IF ((mv/10.0) LT 0.01) THEN ret = 36.0 ELSE ret = mv/10.0
;  return, ret
;End