; get systime() and translate it into yyyymmdd format
FUNCTION Julian2yyyymmdd, julianDate       ; datestring should have format: yyyymmdd
  caldat, julianDate, month, day, year
  dateString = STRTRIM(year,2)+STRMID('0'+STRTRIM(month, 2), 1,2, /REVERSE_OFFSET)+STRMID('0'+STRTRIM(day, 2), 1,2, /REVERSE_OFFSET)
  RETURN, dateString
END ; FUNCTION DATESTRING2JULIAN