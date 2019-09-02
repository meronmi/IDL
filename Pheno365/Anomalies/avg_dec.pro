Function avg_dec, array_dec
  ;return Nan if they are all Nan
  ind=WHERE(FINITE(array_dec) EQ 1, count_finite)
  IF (count_finite EQ 0) THEN RETURN, !VALUES.F_NAN
  ;average values expressed as decade of the year (circular data), going from 1 to 36
  ;first I compute the day of year (0-360) from dekads, always setting it to the
  ;first day of year of the dekad, note that it can be 0. Then I make the average of the angles.
  ret=ROUND(mean_vec(10.0d * (DOUBLE(array_dec) - 1.0d))/10.0d + 1.0d)
  IF ret GT 36.0 THEN ret = ret - 36.0d
  RETURN, ret
End

;Function ang_between_v2_and_v1b, v1, v2 ;v1,v2 in dekads
;;return the number of dekads, between v2 and v1, with sign
;  v1gra=10.0d * (DOUBLE(v1) - 1.0d)/10.0d + 1.0d
;  v2gra=10.0d * (DOUBLE(v2) - 1.0d)/10.0d + 1.0d
;  v1rad=!DTOR*DOUBLE(v1gra)
;  v2rad=!DTOR*DOUBLE(v2gra)
;  a=DOUBLE(atan(sin(v2rad),cos(v2rad)))
;  indlt=WHERE(a LT  180.0d * 1.0d / !DTOR, countlt)
;  IF (countlt GT 0) THEN a[indlt]=a[indlt] + !PI
;  indgt=WHERE(a GT  180.0d * 1.0d / !DTOR, countgt)
;  IF (countgt GT 0) THEN a[indgt]=a[indgt] - !PI
;  b=DOUBLE(atan(sin(v1rad),cos(v1rad)))
;  indlt=WHERE(b LT  180.0d * 1.0d / !DTOR, countlt)
;  IF (countlt GT 0) THEN b[indlt]=b[indlt] + !PI
;  indgt=WHERE(b GT  180.0d * 1.0d / !DTOR, countgt)
;  IF (countgt GT 0) THEN b[indgt]=b[indgt] - !PI
;  
;  ret = a - b
;  ret = ROUND(ret*!RADEG)
;  indgt18=WHERE(ret GT 18, coungt18)
;  IF (coungt18 NE 0) THEN ret[indgt18]=36-ret[indgt18]
;  indlt_18=WHERE(ret LT -18, counlt_18)
;  IF (counlt_18 NE 0) THEN ret[indlt_18]=-36-ret[indlt_18]
;  RETURN, ret
;End  

Function short_ang_between_v2_and_v1, v1, v2 ;v1,v2 in dekads
;return the number of dekads, between v2 and v1, with sign
  v1gra=10.0d * (DOUBLE(v1) - 1.0d)/10.0d + 1.0d
  v2gra=10.0d * (DOUBLE(v2) - 1.0d)/10.0d + 1.0d
  v1rad=!DTOR*DOUBLE(v1gra)
  v2rad=!DTOR*DOUBLE(v2gra)
  ret = atan(sin(v2rad),cos(v2rad))-atan(sin(v1rad),cos(v1rad))
  indf=WHERE(FINITE(ret) EQ 1, countf)
  IF (countf GT 0) THEN BEGIN
    retf=ret[indf]
    ;retf = ROUND(retf*!RADEG)
    retf = retf * !RADEG
    indgt18=WHERE(retf GT 18, coungt18)
    IF (coungt18 NE 0) THEN retf[indgt18]=retf[indgt18]-36
    indlt_18=WHERE(retf LT -18, counlt_18)
    ;IF (counlt_18 NE 0) THEN ret[indlt_18]=-36-ret[indlt_18]
    IF (counlt_18 NE 0) THEN retf[indlt_18]=36+retf[indlt_18]
    ret[indf]=retf
  ENDIF
  RETURN, ret
End  