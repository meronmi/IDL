FUNCTION doy2dek, data
;list all first doy for each dek, from 1 to 36
;use with GE
fdoy = [1.0, 11, 21, 32, 42, 52, 60, 70, 80, 91, 101, 111, 121, 131, 141, 152, 162, 172, 182, 192, 202, 213, 223, 233, 244, 254, 264, 274, 284, 294, 305, 315, 325, 335, 345, 355]
;use with LT
ldoy = [fdoy[1:N_ELEMENTS(fadoy)-1], 366.0]

ind  = WHERE(data EQ -9999, count)
IF (count GT 0) THEN data[ind]=!VALUES.F_NAN

indf = WHERE(FINITE(data), countf)
indNaN = WHERE((FINITE(data) NE 1), countNaN)

; data are expressed in days since the first day of the cycle (an arbitrary day). If the first day is 1,
; the DOC value will be 1
data[indf] = FLOAT(FLOOR(data[indf]))
ind1 = WHERE(data[indf] GE 366, count1)
ind2 = WHERE(data[indf] LT 1, count2)


IF (count1+count2 NE 0) THEN STOP

check = data*0
data_out = data

FOR i = 0, N_ELEMENTS(fdoy)-1 DO BEGIN
  IF i GT 35 THEN STOP
  ;find the doy of dek i and substitute
  ind = WHERE((data[indf] GE fdoy[i]) AND (data[indf] LT ldoy[i]), count)
  ;PRINT, i+1, fdoy[i], ldoy[i]
  IF (count GT 0) THEN BEGIN
    data_out[indf[ind]] = FLOAT(i + 1)
    check[indf[ind]] = 1
  ENDIF
ENDFOR
IF (TOTAL(check[indf]) NE countf) THEN STOP
  
RETURN, data_out
END