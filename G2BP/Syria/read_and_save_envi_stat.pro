PRO read_and_save_envi_stat
  OPENR, lunr, 'X:\SYRIA\statistics dek 09.txt', /GET_LUN
  OPENW, lunw, 'X:\SYRIA\statistics dek 09_2.txt', /GET_LUN
  WHILE ~ EOF(lunr) DO BEGIN
    str = ''
    READF, lunr, str
    initstr = STRMID(str, 0, 5)
    IF (initstr EQ 'Stats') THEN BEGIN
      PRINTF, lunw, str
      READF, lunr, str ;discrd hdr
      FOR i = 0, 3 DO BEGIN
        READF, lunr, str
        PRINTF, lunw, str
      ENDFOR
    ENDIF
  ENDWHILE
  FREE_LUN, lunr
  FREE_LUN, lunw
END