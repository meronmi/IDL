FUNCTION extractTimeFromHdr, fn, prefix

day_in_a_dek_normal = [10,10,11,10,10,8,10,10,11,10,10,10,10,10,11,10,10,10,10,10,11,10,10,11,10,10,10,10,10,11,10,10,10,10,10,11];
day_in_a_dek_leap = [10,10,11,10,10,9,10,10,11,10,10,10,10,10,11,10,10,10,10,10,11,10,10,11,10,10,10,10,10,11,10,10,10,10,10,11];

OPENR,lun, fn, /GET_LUN
line = ''
YY = !NULL
TT = !NULL
nTT = !NULL
WHILE NOT EOF(lun) DO BEGIN 
  READF, lun, line
  metathere = 1
  WHILE (metathere EQ 1) DO BEGIN
    posMeta = STRPOS(line, 'Meta')
    IF (posMeta NE -1) THEN BEGIN
    ;IF (STRMID(STRTRIM(line,2),0,4) EQ 'Meta') THEN BEGIN
      s = STRPOS(line, prefix) ;this will fine the first occurrence
      nchar = STRLEN(prefix)
      YYTT =  STRMID(line, s + nchar, 4)
      line = STRMID(line, s + nchar+ 4)
      YYYY = FIX(STRMID(YYTT,0,2))
      IF (YYYY GT 50) THEN YYYY = YYYY + 1900 ELSE YYYY = YYYY + 2000 
      YY = [YY, YYYY]
      TT = [TT, FIX(STRMID(YYTT,2,2))]
      ;PRINT, YY, TT
      ;is it leap?
      IF (YYYY MOD 4) EQ 0 THEN $
        nTT = [nTT, day_in_a_dek_leap[TT[-1]-1]] $;is leap
        ELSE nTT = [nTT, day_in_a_dek_normal[TT[-1]-1]];is not leap
      ;PRINT, YY[-1], TT[-1], nTT[-1]
      ;PRINT, 'xx'
    ENDIF ELSE metathere = 0
  ENDWHILE ;metathere
ENDWHILE ;eof

FREE_LUN, lun
RETURN, [TRANSPOSE([YY]),TRANSPOSE([TT]),TRANSPOSE([nTT])] ;3 coulmns (YYYY, TT, ndayInTT) x nb rows
END