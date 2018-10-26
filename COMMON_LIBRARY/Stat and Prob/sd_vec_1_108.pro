Function sd_vec_1_108, array
  ;used to compute the mean angle of circular data expressed as SPIRITS, 1-36, 37-72, 73-108
  ;Treat nan
  ind = WHERE(FINITE(array), count)
  IF (count GT 0) THEN x = array[ind] ELSE STOP
  ;check min max
  ma = MAX(x, MIN=mi)
  IF ((ma GT 108) OR (mi LT 0)) THEN STOP
  ;express everything in 1 to 36
  ind = WHERE((x GE 37) AND (x LE 72), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 36
  ind = WHERE((x GE 73) AND (x LE 108), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 72
  
  rad=!CONST.DtoR*double(x*10)  ; rad=!CONST.DtoR*double(x*10)
  ;used to compute the sd of angle of circular data expressed as 0-360 deg
  ;as in Zar, pag 603

  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
  r = SQRT(vn^2+ve^2)
  s0 = 180.0/!PI * SQRT(-4.60517*ALOG10(r))
  return, s0/10.0 ;return in the 1-36 (s 0 is 0 - 360)
End

