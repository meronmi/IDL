FUNCTION iseven, num

;  Purpose:
;     To check whether a number is even or not.

;  Outcome:
;     Indicate whether the number num is even (1), odd (0) or not an
;     integer (9).

;  Usage:
;     rc = iseven (num)

;  Input parameters:
;     num {integer}: The number to test.

;  Output parameters: N/A.

;  Return codes:
;     0: num is an odd integer.
;     1: num is an even integer.
;     9: num is not an integer.

IF ((SIZE (num, /TYPE) EQ 2) OR $
   (SIZE (num, /TYPE) EQ 3) OR $
   (SIZE (num, /TYPE) GT 11)) THEN BEGIN
   nums = STRTRIM (STRING (num), 2)
   len = STRLEN (nums)
   IF ((STRMID (nums, len - 1, 1) EQ '0') OR $
      (STRMID (nums, len - 1, 1) EQ '2') OR $
      (STRMID (nums, len - 1, 1) EQ '4') OR $
      (STRMID (nums, len - 1, 1) EQ '6') OR $
      (STRMID (nums, len - 1, 1) EQ '8')) THEN BEGIN
      retcode = 1
   ENDIF ELSE BEGIN
      retcode = 0
   ENDELSE
ENDIF ELSE BEGIN
   retcode = 9
ENDELSE

RETURN, retcode

END