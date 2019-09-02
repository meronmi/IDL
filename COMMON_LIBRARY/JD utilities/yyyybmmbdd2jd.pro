FUNCTION MMbDDbYYYY2jd, date
  ;transform a data like "2/21/2016" into jd
  res = FLTARR(N_ELEMENTS(date))
  FOR i = 0, N_ELEMENTS(date)-1 DO BEGIN
    MMDDYYYY = FIX(STRTRIM(STRSPLIT(date[i], '/-', /EXTRACT),2))
    IF (MMDDYYYY[2] LT 1900) THEN STOP
    res[i] = JULDAY(MMDDYYYY[0], MMDDYYYY[1], MMDDYYYY[2], 12)
  ENDFOR
  RETURN, res
END

FUNCTION DDbMMbYYYY2jd, date
  ;transform a data like "2/21/2016" into jd
  res = FLTARR(N_ELEMENTS(date))
  FOR i = 0, N_ELEMENTS(date)-1 DO BEGIN
    DDMMYYYY = FIX(STRTRIM(STRSPLIT(date[i], '/-', /EXTRACT),2))
    IF (DDMMYYYY[2] LT 1900) THEN STOP
    res[i] = JULDAY(DDMMYYYY[1], DDMMYYYY[0], DDMMYYYY[2], 12)
  ENDFOR
  RETURN, res
END

FUNCTION YYYYbMMbDD2jd, date
  ;transform a data like "2/21/2016" into jd
  res = FLTARR(N_ELEMENTS(date))
  FOR i = 0, N_ELEMENTS(date)-1 DO BEGIN
    MMDDYYYY = FIX(STRTRIM(STRSPLIT(date[i], '/-', /EXTRACT),2))
    IF (MMDDYYYY[0] LT 1900) THEN STOP
    res[i] = JULDAY(MMDDYYYY[1], MMDDYYYY[2], MMDDYYYY[0], 12)
  ENDFOR
  RETURN, res
END