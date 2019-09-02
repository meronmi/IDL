FUNCTION findMinAwayFromMax2, subExtrema, sig, indOfMaxInExtrema, inXrange, y, direction
;all variables except y have the number of elements of subExtrema, and the elements are subscript (points to) y

;given:
;- the position of a max (indOfMaxInExtrema),
;- a set of y minima given by subExtrema where sig = -1 
;- a search direction (direction),
;
;it takes
;- the min happening in inXrange with the largest drop from the y of the selected max (y[subExtrema[indOfMaxInExtrema]]), if bigger than fractRange*(yOfSelectedMax-MIN(y))
;- if there are no mins in the range or the above mins are all smaller than fractRange*yrange extend the serach out of the range
;- after that, take as candidate the one with largest drop, whatever it is
;- replace this candidate if a min that still has a drop of fractDrop*largest_drop exists before the candidate, if more the one, takes the closest to the position of selected Max
;





; In a given direction we can compute the y values at all the minima, each of them associated with a reduction from the slected max.
; We can also compute the largest of such reduction yOfSelectedMax-MIN(ySegmnet) independnetly from where it happens (might be at a min far away or close
; We start moving away from the max and we consider each min one after the other, thus starting from the closer and then moving away
; we consider a minum to be a good candidate if the reduction achieved there is a at least "fractRange" of the maximum one in that direction. If this is 
; not teh case, the min is consdered too small 
fractRange = 0.2

fractDrop = 0.5               ;it has to be quite large, fraction of the biggest

;########################


yExtrema = y[subExtrema]                                      ;get the y values of all the extrema
yOfSelectedMax = y[subExtrema[indOfMaxInExtrema]]             ;y val of the position of the slected max

;extract directional segments (for right or left serach) and adjust the left one for a forward search
IF (direction EQ 'left') THEN BEGIN
  ;ger everything before the selected max and mirror it to have the same strategy (i.e. forward search)
  ySegmnet =  REVERSE(yExtrema[0:indOfMaxInExtrema-1])
  inRangeSegment = REVERSE(inXrange[0:indOfMaxInExtrema-1])
  sigSegment = REVERSE(sig[0:indOfMaxInExtrema-1])
ENDIF ELSE BEGIN
  ;get everything after the selected max
  ySegmnet =  yExtrema[indOfMaxInExtrema+1:-1]
  inRangeSegment = inXrange[indOfMaxInExtrema+1:-1]
  sigSegment = sig[indOfMaxInExtrema+1:-1]
ENDELSE

;now find the min with the largest drop that is in the specified wide range
;max drop = min val as the y of max is fixed
subOfMinInRange = WHERE((sigSegment EQ -1) AND (inRangeSegment EQ 1), count)
tooSmall = 0
IF count GT 0 THEN BEGIN
  ;there is at least one candidate
  res = MIN(ySegmnet[subOfMinInRange], indCandidate)
  indCandidate = subOfMinInRange[indCandidate[0]]    ;indCandidate[0] used because if there are two equally small take the one occurring first
  ;check that is not very very small, in that case look outside the range
  IF ((yOfSelectedMax-ySegmnet[indCandidate]) LT fractRange*(yOfSelectedMax-MIN(ySegmnet))) THEN tooSmall = 1
ENDIF
IF (count EQ 0) OR (tooSmall EQ 1) THEN BEGIN
  ;ther are no mins or they are too small, look outside as well and get what is available (there may not be anything better)
  subOfMin = WHERE((sigSegment EQ -1), count)
  ;here I do not check if a min is available, is there by def
  res = MIN(ySegmnet[subOfMin], indCandidate)
  indCandidate = subOfMin[indCandidate[0]]    
END

;now I have a candidate, compute its drop (dropOfCandidateMin) and then look backward if I find another min with a drop of at least fractDrop*dropOfCandidateMin
;Drop of the candidate
dropOfCandidateMin = yOfSelectedMax - ySegmnet[indCandidate] 
;this is the first candidate, takes a closer one only if it has a drop that is smaller than fraction*dropOfCandidateMin  
;get the position of minima before the candidate 
subOfMin = WHERE((sigSegment[0:indCandidate-1] EQ -1), count)
IF (count GT 0) THEN BEGIN
  ;there are minima, see if one can be selected (their drop GT fractDrop*dropOfCandidateMin)
  ;ind = WHERE((ySegmnet[subOfMin]-ySegmnet[indCandidate]) GT fractDrop*dropOfCandidateMin, count)
  ind = WHERE((yOfSelectedMax - ySegmnet[subOfMin]) GT fractDrop*dropOfCandidateMin, count)
  IF (count GT 0) THEN indCandidate = subOfMin[ind[0]] ;take the most-left of those complying
ENDIF
;Now indCandidate is the ind of my min (be case pointing to the segment)
;remeber that when 'left', the array is mirrored
IF (direction EQ 'left') THEN BEGIN
  ;ind is sub from indOfMaxInExtrema-1 going 0
  ind = (indOfMaxInExtrema-1) - indCandidate
ENDIF ELSE BEGIN
  ind =  indCandidate + indOfMaxInExtrema+1 
ENDELSE
RETURN, ind

END