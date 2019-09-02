FUNCTION ComputeHoleFraction, subExtrema, sig, subRegMax, x, y, direction

;ABORTED


;yExtrema = y[subExtrema]                                      ;get the y values of all the extrema
;xExtrema = x[subExtrema]                                      ;get the x values of all the extrema
;yOfSelectedMax = y[subRegMax]                                 ;y val of the position of the slected max
;sub_subExtrema_Max = WHERE(subExtrema EQ subRegMax, count)
;IF (count EQ 0) THEN STOP
;;extract directional segments (for right or left serach) and adjust the left one for a forward search
;IF (direction EQ 'left') THEN BEGIN
;  ;ger everything from the selected max and mirror it to have the same strategy (i.e. forward search)
;  ySegmnet =  REVERSE(yExtrema[0:sub_subExtrema_Max])
;  ;time cannot be simply reversed, it would make a mess
;  ;xSegmnet =  REVERSE(xExtrema[0:sub_subExtrema_Max])
;  xSegmnet =  REVERSE(max(xExtrema[0:sub_subExtrema_Max])-xExtrema[0:sub_subExtrema_Max])
;  sigSegment = REVERSE(sig[0:sub_subExtrema_Max])
;ENDIF ELSE BEGIN
;  ;get everything from the selected max
;  ySegmnet =  yExtrema[sub_subExtrema_Max:-1]
;  xSegmnet =  xExtrema[sub_subExtrema_Max:-1]
;  sigSegment = sig[sub_subExtrema_Max:-1]  
;ENDELSE
;
;
;ySegmnetB = ySegmnet - MIN(ySegmnet)
;;cumulative area below the max-min-max-.. sereies
;cum_area = tsum_cumulative(xSegmnet, ySegmnetB)
;;cumlative area without the min: max- -max
;subOfMin = WHERE((sigSegment EQ -1), countMin)  ;minima in the specified time range
;ySegmnetB[subOfMin] = !VALUES.F_NAN
;cum_area_without_min = tsum_cumulative(xSegmnet, ySegmnetB, /NAN)
;
;subOfMax = WHERE((sigSegment EQ 1), countMax) 
;first = cum_area[]
;
;RETURN, fractArray
END