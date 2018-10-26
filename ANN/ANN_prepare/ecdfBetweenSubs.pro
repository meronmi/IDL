FUNCTION lengthDry, vec, subStart, Substop
subStart0 = subStart - 36
IF (subStart0 LT 0) THEN subStart0 = 0
;definition (less than 10 mm) taken from https://gmao.gsfc.nasa.gov/research/subseasonal/atlas/Pindices-html/dryspell.html
ind = WHERE(~FINITE(vec[subStart0:subStop]), count)
IF (count EQ 0) THEN BEGIN
  dry = vec[subStart0:subStop] lt 10  ;dry dekads (with precipitation less than 10 are set to 1)
  accDry = dry * 0
  FOR i = 0, N_ELEMENTS(dry)-1 DO BEGIN
    IF (dry[i] EQ 1) THEN BEGIN
      IF (i GT 1) THEN BEGIN
        IF (dry[i-1] EQ 1) THEN accDry[i] = accDry[i-1] + 1 ELSE accDry[i] = 1
      ENDIF ELSE accDry[i] = 1
    ENDIF ELSE accDry[i] = 0
  ENDFOR
ENDIF ELSE RETURN, !VALUES.F_NAN
IF CHECK_MATH() NE 0 THEN STOP
;now look a the biggest number in the current season
RETURN, MAX(accDry[subStart-subStart0:*])
END


FUNCTION ecdfBetweenSubs, DataMatrix2D, subStart2D, subStop2D, oper
;ecdfBetweenSubs(rain, REFORM(rainSub.sos[*,line,*]-3, REFORM(rainSub.sos[*,line,*]-1))

;remeber that -9999 is no data

;variable to store the period cumulation or median or etc
his = subStart2D * !VALUES.F_NAN
FOR seas = 0, N_ELEMENTS(subStart2D[0,*])-1 DO BEGIN
  CASE oper OF
    'sum': BEGIN
      FOR sample = 0, N_ELEMENTS(subStart2D[*,0])-1 DO BEGIN
        subStart = subStart2D[sample,seas]
        subStop = subStop2D[sample,seas]
        ;manage the case where start and stop were originally equal, so they are sent here as start, stop-1
        ;and therefore stop is 1 before start
        IF (subStop EQ subStart-1) THEN subStop = subStart       
        IF ((subStart GE 0) AND (subStop GE 0)) THEN BEGIN
          his[sample,seas] = TOTAL(DataMatrix2D[sample, subStart:subStop],/NAN) 
        ENDIF ELSE BEGIN
          his[sample,seas] = !VALUES.F_NAN
        ENDELSE
      ENDFOR
    END
    'lengthDry': BEGIN
      FOR sample = 0, N_ELEMENTS(subStart2D[*,0])-1 DO BEGIN
        ;note: the dry spell may have started in the previous dekds (for instance, the current phase begins with 1 dry dek that is the last
        ;of a series of 10 dry started in a previous dekad. Consider one year befor to be sure
        subStart = subStart2D[sample,seas]
        subStop = subStop2D[sample,seas]
        IF (subStop EQ subStart-1) THEN subStop = subStart   
        IF ((subStart GE 0) AND (subStop GE 0)) THEN BEGIN
          his[sample,seas] = lengthDry(DataMatrix2D[sample,*],subStart,subStop)
        ENDIF ELSE BEGIN
          his[sample,seas] = !VALUES.F_NAN
        ENDELSE
      ENDFOR
    END
    'median': BEGIN
      FOR sample = 0, N_ELEMENTS(subStart2D[*,0])-1 DO BEGIN
        subStart = subStart2D[sample,seas]
        subStop = subStop2D[sample,seas]
        IF (subStop EQ subStart-1) THEN subStop = subStart   
        IF ((subStart GE 0) AND (subStop GE 0)) THEN BEGIN
          his[sample,seas] = MEDIAN(DataMatrix2D[sample, subStart:subStop])
        ENDIF ELSE BEGIN
          his[sample,seas] = !VALUES.F_NAN
        ENDELSE
      ENDFOR
    END
    'mean': BEGIN
      FOR sample = 0, N_ELEMENTS(subStart2D[*,0])-1 DO BEGIN
        subStart = subStart2D[sample,seas]
        subStop = subStop2D[sample,seas]
        IF (subStop EQ subStart-1) THEN subStop = subStart
        IF ((subStart GE 0) AND (subStop GE 0)) THEN BEGIN
          indFin = WHERE(FINITE(DataMatrix2D[sample, subStart:subStop]), countFin)
          IF (countFin NE 0) THEN $
            his[sample,seas] = MEAN(DataMatrix2D[sample, subStart:subStop], /NAN) $
          ELSE his[sample,seas] = !VALUES.F_NAN
          IF CHECK_MATH() NE 0 THEN STOP
        ENDIF ELSE BEGIN
          his[sample,seas] = !VALUES.F_NAN
        ENDELSE
      ENDFOR
    END
    ELSE: STOP
  ENDCASE
ENDFOR
prct = subStart2D * !VALUES.F_NAN
;now compute the the percentile fo4 each value //invPercentile, x, VAL = val
FOR seas = 0, N_ELEMENTS(subStart2D[0,*])-1 DO BEGIN
  FOR sample = 0, N_ELEMENTS(subStart2D[*,0])-1 DO BEGIN
     prct[sample,seas] = invPercentile(his[sample,*], VAL = his[sample,seas])
  ENDFOR
ENDFOR

RETURN, prct
END
