Function sd_vec, array_deg
  ;used to compute the sd of angle of circular data expressed as 0-360 deg
  ;as in Zar, pag 603
  rad=!DTOR*double(array_deg)
  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
  r = SQRT(vn^2+ve^2)
  s0 = 180.0/!PI * SQRT(-4.60517*ALOG10(r))
  return, s0
End