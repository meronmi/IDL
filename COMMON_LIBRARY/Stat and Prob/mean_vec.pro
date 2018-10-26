Function mean_vec, array_deg
  ;used to compute the mean angle of circular data expressed as 0-360 deg
  rad=!DTOR*double(array_deg)
  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
  mv=DOUBLE(ATAN(ve, vn))
  IF mv LT 180.0d * 1.0d / !DTOR then mv=mv * !RADEG+180.0d
  IF mv GT 180.0d * 1.0d / !DTOR then mv=mv * !RADEG-180.0d
  return, mv
End