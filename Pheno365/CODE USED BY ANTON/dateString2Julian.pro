FUNCTION DATESTRING2JULIAN, datestring       ; datestring should have format: yyyymmdd
  year=FIX(STRMID(datestring,0,4))
  month=FIX(STRMID(datestring,4,2))
  day=FIX(STRMID(datestring,6,2))
  RETURN, JULDAY(month,day,year)
END ; FUNCTION DATESTRING2JULIAN