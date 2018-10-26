FUNCTION isLeap, year
IF (N_ELEMENTS(year) EQ 1) THEN BEGIN
  IF (((year MOD 4)) NE 0) THEN RETURN, 0
  IF (((year MOD 100)) NE 0) THEN RETURN, 1
  IF (((year MOD 400)) EQ 0) THEN RETURN, 1 ELSE RETURN, 0
ENDIF ELSE BEGIN
  ret = BYTARR(N_ELEMENTS(year))
  FOR i = 0, N_ELEMENTS(year)-1 DO BEGIN
    IF (((year[i] MOD 4)) NE 0) THEN ret[i] = 0
    IF (((year[i] MOD 100)) NE 0) THEN ret[i] = 1
    IF (((year[i] MOD 400)) EQ 0) THEN ret[i] = 1 ELSE ret[i] = 0
  ENDFOR
  RETURN, ret
ENDELSE
END