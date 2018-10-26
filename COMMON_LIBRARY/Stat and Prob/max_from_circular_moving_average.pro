FUNCTION max_from_circular_moving_average, array, winDim
  ;windDim has to be odd
  IF ((winDim MOD 2) EQ 0) THEN STOP
  ;Treat nan
  ind = WHERE(FINITE(array), count)
  IF (count GT 0) THEN x = array[ind] ELSE STOP
  
  ;check min max
  ma = MAX([x], MIN=mi)
  IF ((ma GT 108) OR (mi LT 1)) THEN STOP
  

  ;express everything in 1 to 36
  ind = WHERE((x GE 37) AND (x LE 72), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 36
  ind = WHERE((x GE 73) AND (x LE 108), count)
  IF (count GT 0) THEN x[ind] = x[ind] - 72
  ;PRINT, x
  
  density = INTARR(36)
  FOR i = 1, 36 DO BEGIN
    startI = i - (winDim-1)/2
    IF (startI LT 1) THEN startI = 36 + startI 
    endI = i +  (winDim-1)/2
    IF (endI GT 36) THEN endI =  endI - 36
    
    IF ((endI-startI) GE 0) THEN BEGIN
      ind = WHERE((x GE startI) AND (x LE endI), count)
      density[i-1] = count
    ENDIF ELSE BEGIN
       ind = WHERE(((x GE startI) AND (x LE 36)) OR $
                   ((x GE 1) AND (x LE endI)), count)
      density[i-1] = count
    ENDELSE
     ;PRINT, i, startI, endI, endI-startI, count
  ENDFOR
  ;find the largest chunk
  valMax = MAX(density, indMax)
  indMax = WHERE(density EQ valMax, count)
  IF (count EQ 1) THEN BEGIN
    RETURN, indMax
  ENDIF ELSE BEGIN
    dimChunk = INTARR(N_ELEMENTS(indmax)) * 0 + 1
    FOR i = 0, N_ELEMENTS(indmax)-1 DO BEGIN
      out = 0
      j = indmax[i] + 1
      IF (j EQ 36) THEN j = 1
      WHILE (out EQ 0) DO BEGIN
        IF (density[j] EQ valMax) THEN dimChunk[i] = dimChunk[i] + 1 ELSE out = 1
        j = j + 1
        IF (j EQ 36) THEN j = 1
      ENDWHILE
    ENDFOR
    valMaxChunk = MAX(dimChunk, indMaxChunk)
    pos = indMaxChunk+FLOOR(valMaxChunk/2)
    IF (pos GT (N_ELEMENTS(indMax)-1)) THEN pos = pos-(N_ELEMENTS(indMax)-1)
    posMax = indMax[pos] ; pos max is a sub of x (0, 35), I have to add 1 to have 1 36
    RETURN, posMax + 1
 ENDELSE



END