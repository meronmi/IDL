FUNCTION findMinAwayFromMax_v5, subExtrema, sig, subRegMax, x, y, direction, holeMinFractionalImportance, yobs

;all variables except y have the number of elements of subExtrema, and the elements are subscript (points to) x and y

;given:
;- the position of a max (sub_subExtrema_Max),
;- a set of y minima given by subExtrema where sig = -1 
;- the x (time) and y (the index) values
;- a search direction (direction),
;- holeMinFractionalImportance: the minimum fractional importance of the hole made the minimum
;
; it takes the first min originalting a hole (in the max - min - next max sequence) with a fractional importance greater than a threshols 
; if max+1 is bigger than max, min1 is selcetd anyhow (no matter the thresold) 
;
;max0
; |                max1
; |        min1      |             max2
; |         |        |    min2      |
; |         |        |     |        |
;-0---------1--------2-----------------------------min Y val
;base line given by the overall min
;
;fractional importance of the hole = 1- A/B = 1 - area(max0-0-2-max1-min1)/ area(max0-0-2-max2)
;obs: the ratio between the two areas hase the limits: 0 (infinite hole), 1 (no hole)
;
;
; Computing the area down to the base level defined by the min level ensures that a small hole is negiglible if we are still at high y values, and gets selected when we go 
; at lower y values
; 
;########################

;v4: test dom's test
;v5: when a min is discarded, do not simply move to the next one. Keep the left max (Max0), omit the min (min1) and the folllowing Max (max1),
;    and consider Max0 - min2 - max2
doplot = 0


yExtrema = y[subExtrema]                                      ;get the y values of all the extrema
xExtrema = x[subExtrema]                                      ;get the x values of all the extrema
yOfSelectedMax = y[subRegMax]             ;y val of the position of the slected max
sub_subExtrema_Max = WHERE(subExtrema EQ subRegMax, count)
IF (count EQ 0) THEN STOP
;extract directional segments (for right or left serach) and adjust the left one for a forward search
IF (direction EQ 'left') THEN BEGIN
  ;ger everything from the selected max and mirror it to have the same strategy (i.e. forward search)
  ySegmnet =  REVERSE(yExtrema[0:sub_subExtrema_Max])
  ;time cannot be simply reversed, it would make a mess
  ;xSegmnet =  REVERSE(xExtrema[0:sub_subExtrema_Max])
  xSegmnet =  REVERSE(max(xExtrema[0:sub_subExtrema_Max])-xExtrema[0:sub_subExtrema_Max])
  sigSegment = REVERSE(sig[0:sub_subExtrema_Max])
  subExtremaSegment = subExtrema[0:sub_subExtrema_Max]
ENDIF ELSE BEGIN
  ;get everything from the selected max
  ySegmnet =  yExtrema[sub_subExtrema_Max:-1]
  xSegmnet =  xExtrema[sub_subExtrema_Max:-1]
  sigSegment = sig[sub_subExtrema_Max:-1]
  subExtremaSegment = subExtrema[sub_subExtrema_Max:-1]
ENDELSE

; compute the fractional importance of each minima (note: the last minima may not have a max afterwards, it will be considered only if no
; minima before stasfy the requirements)
subOfMin = WHERE((sigSegment EQ -1), countMin)  ;minima in the specified time range
;compute the base level
base_level = MIN(ySegmnet)
min_found = 0
FOR i = 0, countMin-1 DO BEGIN
  IF ((subOfMin[i] EQ N_ELEMENTS(sigSegment)-1) AND (min_found EQ 0)) THEN BEGIN
    ;this minimum is the last element, i.e. does not have a max afterwards.
    ;However, if I arrived here without a selection, there was nothing better before, so take it
    min_found = 1
    sub_of_mim_to_return = subOfMin[i]
    ;IF (min_found EQ 0) THEN sub_of_mim_to_return = subOfMin[i]
  ENDIF
  IF (min_found EQ 0) THEN BEGIN
    subSegmentLeft  = subOfMin[0]-1
    subSegmentRight = subOfMin[i]+1
    subSegmentMin = subOfMin[i]
    IF (doplot EQ 1) THEN BEGIN
      h = plot(x[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]],yobs[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]], symbol='o',LINESTYLE='')
      h = plot(x[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]],y[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]], symbol='+',LINESTYLE='-', COLOR='r',/OVERPLOT)
      h.close
    ENDIF
    ;test Dom test
    ;variance of smooth residuals = 1/(n-1) * RSS
;    den = TOTAL(FINITE(yobs[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]]))-1
;    varOfFit = 1.0/den $
;      * TOTAL((y[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]] - yobs[subExtremaSegment[subSegmentLeft]:subExtremaSegment[subSegmentRight]])^2,/NAN)
;    ;test statistics = (y_min - MAX(max0, max1))/varOfFit
;    stat = (ySegmnet[subSegmentMin]-MAX([ySegmnet[subSegmentLeft], ySegmnet[subSegmentRight]]))/sqrt(varOfFit)
;    critical_Student = T_CVF(1-0.05, den)
;    IF (stat LE critical_Student) THEN PRINT, 'Min OK' ELSE BEGIN
;      PRINT, 'Min NO!!!!!!!!!'
;      STOP
;    ENDELSE  
    ;end of Dom test
    xy_max0 = [xSegmnet[subSegmentLeft], ySegmnet[subSegmentLeft]]
    xy_0 =    [xSegmnet[subSegmentLeft], base_level]
    xy_2 =    [xSegmnet[subSegmentRight], base_level]
    xy_max1 = [xSegmnet[subSegmentRight], ySegmnet[subSegmentRight]]
    xy_min1 = [xSegmnet[subSegmentMin], ySegmnet[subSegmentMin]]
    minFractImportance = 1.0 - (POLY_AREA([xy_max0[0], xy_0[0], xy_2[0], xy_max1[0], xy_min1[0]],[xy_max0[1], xy_0[1], xy_2[1], xy_max1[1], xy_min1[1]]) $
                       / POLY_AREA([xy_max0[0], xy_0[0], xy_2[0], xy_max1[0]],            [xy_max0[1], xy_0[1], xy_2[1], xy_max1[1]]))
    ;2019 06 18: ADD A CONDITION ON (xy_max1[1] - xy_max0[1]) > 0.005 to avoid being trapped in very small variations (the curve is almost flat but it has
    ; a max1 bigger than max 0 of very very little)
    IF ((minFractImportance GT holeMinFractionalImportance) AND (min_found EQ 0)) OR $
       ((xy_max0[1] LT xy_max1[1]) AND ((xy_max1[1] - xy_max0[1]) GT 0.005) AND (min_found EQ 0)) THEN BEGIN
      min_found = 1
      sub_of_mim_to_return = subSegmentMin
    ENDIF
  ENDIF
ENDFOR
;if the search did not provided a good min, there was a min but di not passed the test, just take the last
IF (min_found EQ 0) THEN sub_of_mim_to_return = subOfMin[-1]

;be care, the retrieved min is pointing to the segment, not to the actula data
;remeber that when 'left', the array is mirrored
IF (direction EQ 'left') THEN BEGIN
  ;ind is sub from sub_subExtrema_Max going 0
  sub_of_mim_to_return = (sub_subExtrema_Max) - sub_of_mim_to_return
ENDIF ELSE BEGIN
  sub_of_mim_to_return =  sub_of_mim_to_return + sub_subExtrema_Max 
ENDELSE
RETURN, sub_of_mim_to_return

END