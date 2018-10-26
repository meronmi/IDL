Function mean_vec_1_108, array
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
  ve=-mean(sin(rad), /DOUBLE, /NAN) ;Y in the Zar
  vn=-mean(cos(rad), /DOUBLE, /NAN) ;X in the Zar
  mv=DOUBLE(ATAN(ve, vn))
  IF mv LT 180.0d * 1.0d / !DTOR then mv=mv * !CONST.RtoD+180.0d 
  IF mv GT 180.0d * 1.0d / !DTOR then mv=mv * !CONST.RtoD-180.0d
  ;the mean of 35 and 1 is 2.8421709430404009e-015
  ;snap it to 36 if mean <0.01
  IF ((mv/10.0) LT 0.01) THEN ret = 36.0 ELSE ret = mv/10.0
  ;keep only 2 decimals, no
  return, ret ;ROUND(mv*10)/100.0
End