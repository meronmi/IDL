FUNCTION treatSnowCover, u, winter_suppression_jd_min_max, maxFractOfS2SnowPixels, maxFractOfS2CloudyPixels
;when the snow cover is greater than maxFractOfS2SnowPixels, the obs is considre snow.
;Any snow obs is replace by the 5th percentile of NDVI. 
;During a winter period defined by winter_suppression_jd_min_max, snow may come and go, making it difficult to treat the period.
;Thefore during this period we replace all the observations from first to last snow appearance 

;obs with snow
indSnow = WHERE(u.fractSnowCovered GT maxFractOfS2SnowPixels, countSnow)
IF (countSnow GT 0) THEN BEGIN
  ;find the value of the 5th perecentile excluding snow and cloud obs
  indNoSnowNoCloud = WHERE((u.fractSnowCovered LE maxFractOfS2SnowPixels) AND (u.fractCloudCovered LE maxFractOfS2CloudyPixels), NoSnowNoCloud)
  IF (NoSnowNoCloud LT 10) THEN RETURN, u
  perc05NDVI = percentiles(u.mean[indNoSnowNoCloud],value=0.05)
  
  ;replace all snow obs with 5 percentile
  u.mean[indSnow] = perc05NDVI
  u.sd[indSnow] = !VALUES.F_NAN
  
  ;act on thos within winter_suppression_jd_min_max
  inSnowWinter = WHERE((u.dateJD[indSnow] GE winter_suppression_jd_min_max[0]) AND (u.dateJD[indSnow] LE  winter_suppression_jd_min_max[1]), countSnowWinter)
  IF (countSnowWinter GT 1) THEN BEGIN
    ;theer are at least two, replace all in between
    inSnowWinter = indSnow[inSnowWinter]
    indObs2replace = WHERE((u.dateJD GE u.dateJD[inSnowWinter[0]]) AND (u.dateJD LE u.dateJD[inSnowWinter[1]]), count2replace) ; at least the two found above
    u.mean[indObs2replace] = perc05NDVI
    u.sd[indObs2replace] = !VALUES.F_NAN
  ENDIF
ENDIF ELSE BEGIN
  ;no snow obs
  RETURN, u
ENDELSE
RETURN, u
END