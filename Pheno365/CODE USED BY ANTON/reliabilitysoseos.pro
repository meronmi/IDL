FUNCTION reliabilitySOSEOS, xObs, xDayGrid, yfitDayGrid ;yObs
;compute an unreliability indicator for SOS and EOS
;We thus look at the two phases separately: the ascending one and the descending one
;idea: a gap in the time series will result in lower reliability.
;      The dcrease of reliability due to a gap is influenced by 2 factors:
;      - b (base), the x width of the gap (the longer, the worse)
;      - h (height), the y width of the gap (during a phase where NDVI is changing quickly -> lower relaiability; during a flta phase -> non big impact)
;To take into account both the above points, for a given gap we compute both the y and x gap
;This is computed for each pair of conscutive obs.
;
;unrel:
;The biggest h gap is selected.
;;Then, the h gap is normalised by the total y increase btewen min and PS90 (time at which it reaches 90% of amplitude), H.
;In this way we have a fractional importance of the biggest gap compared to the whole season (h/H).
;At this point, the fractional importance may be associate with a large x gap (if the season is large) or a small x gap (e.g. when the season is very steep). 
;For a steep season (20 day from min to max) we may have that the importance is large bu the gap is small (10 days). The SOS estimation is thus reliable. 
;To account for this, this fractional importance is the multyplied by the x width of the gap: h/H * b.
;
;fractUnrel:
;for each gap we compute h/H * b/B, then we select the maximum.


;1. locate the time of actual observations on xDayGrid
match, xObs, xDayGrid, sub_xObs, sub_xDayGrid
xDayGridIsObs = xDayGrid * 0
xDayGridIsObs[sub_xDayGrid] = 1 ;same length of xDayGrid, 1 when there is an obs, 0 otherwise

yfitMax = max(yfitDayGrid, indMax)                   ; return maximum NDVI value
indMax = indMax[0]
unrel = FLTARR(2)
fractUnrel = FLTARR(2)
FOR i = 0, 1 DO BEGIN ;treat ascending and then descending
  phaseCompleted = 0 ;turn it to 1 when no more computation needed for phase i
  IF (i EQ 0) AND (indMax EQ 0) THEN BEGIN
    ;I am looking at ascdending phase AND there is no no ascending phase
    unrel[i] = 1000
    fractUnrel[i] = 1000
    phaseCompleted = 1
  ENDIF
  IF (phaseCompleted EQ 0) AND (i EQ 1) AND (indMax EQ yfitDayGrid.LENGTH-1) THEN BEGIN
    ;I am looking at descending phase AND there is no no descending phase
    unrel[i] = 1000
    fractUnrel[i] = 1000
    phaseCompleted = 1
  ENDIF
  IF (phaseCompleted EQ 0) THEN BEGIN
    ;I am in one of teh two phase, and the phase exists
    ;mirror the descending phase so that we work only with an ascending one
    IF (i EQ 0) THEN BEGIN ;ascending
      xfitSeg = xDayGrid[0:indMax]
      yfitSeg = yfitDayGrid[0:indMax]
      xIsObsSeg = xDayGridIsObs[0:indMax]   
    ENDIF ELSE BEGIN
      ;be care with time (it is time away from the last date)
      xfitSeg = REVERSE(xDayGrid[-1] - xDayGrid[indMax+1:*])
      yfitSeg = REVERSE(yfitDayGrid[indMax+1:*])
      xIsObsSeg = REVERSE(xDayGridIsObs[indMax+1:*])
    ENDELSE 
    ;now, no matter the pahse, I have an ascending one (true ascending or mirrod of descending
    ;compute the amplitude of the phase
    amp = yfitMax - MIN(yfitSeg)  
    ;2. identify the time at which NDVI reaches 90% of min-max on the fitted curve
    shifted = SHIFT(yfitSeg,1) ;take care of circularity of shift
    shifted[0] = yfitSeg[0]
    indPS = WHERE((yfitSeg GT (0.9*amp + MIN(yfitSeg))) AND (yfitSeg GT shifted))
    indPS = indPS[0]
    IF (indPS EQ -1) THEN BEGIN
      ;PS can't be computed
      unrel[i] = 1001
      phaseCompleted = 1
    ENDIF
  ENDIF
  IF (phaseCompleted EQ 0) THEN BEGIN
    ;reduce to segment up to PS
    xfitSeg = xfitSeg[0:indPS]
    yfitSeg = yfitSeg[0:indPS]
    xIsObsSeg = xIsObsSeg[0:indPS]
    ;PS is available, go on
    ;compute the height and base of the phase, including PS as if it was a datapoint
       
    phaseHeight = FLOAT(MAX(yfitSeg[0:indPS]) - MIN(yfitSeg[0:indPS])) ;H
    phaseBase =  FLOAT(xfitSeg[-1] - xfitSeg[0])                        ;B
    ;compute all the gap height and base, including PS as if it was a datapoint. xIsObsSeg looks like [1,0,0,0,1,0,1,0,0,0,0,1], get h and b between each 1 pairs
    ;if PS does not correspond to an actual obs, make it an actual obs
    IF (xIsObsSeg[-1] NE 1) THEN xIsObsSeg[-1] = 1
    n_gaps = TOTAL(xIsObsSeg)-1
    subOnes = WHERE(xIsObsSeg EQ 1, /NULL)
    gapBase = FLTARR(n_gaps)
    gapHeight = FLTARR(n_gaps)
    sub_gapLeftObs = INTARR(n_gaps) ;store here the the subscrit of the left observation of the gap
    sub_gapRighttObs = INTARR(n_gaps)
    FOR j = 0,  n_gaps-1 DO BEGIN
      x = xfitSeg[subOnes[j]:subOnes[j+1]]
      y = yfitSeg[subOnes[j]:subOnes[j+1]]
      gapHeight[j] = y[-1] - y[0]                             ;h
      gapBase[j] = x[-1] - x[0]                               ;b
      sub_gapLeftObs[j] = subOnes[j]
      sub_gapRighttObs[j] = subOnes[j+1]
    ENDFOR
    ;compute fractional importance of the largest height
    maxGapHeight = MAX(gapHeight, indGapMax)
    maxGapFractImportanceH = maxGapHeight/phaseHeight
    ;multiply it by the gap length
    unrel[i] = maxGapFractImportanceH * (xfitSeg[sub_gapRighttObs[indGapMax]]-xfitSeg[sub_gapLeftObs[indGapMax]])
    
    ;compute h/H * b/B (the height gap fraction * the base gap fraction)
    gapFract = (gapHeight/phaseHeight) * (gapBase/phaseBase)
    ;select the largest
    maxGapFract = MAX(gapFract, indGapFract)
    fractUnrel[i] = gapFract[indGapFract]
  ENDIF
ENDFOR
;PRINT, unrel
RETURN, [unrel, fractUnrel] ;[unrelSOS, unrelEOS]
END