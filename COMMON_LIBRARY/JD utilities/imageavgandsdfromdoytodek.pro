PRO ImageAVGandSDFromDoyToDek, ns, nl, dt, fn_avg_image, fn_sd_image, oper

; firstDOC: first day of the cycle at which data have been realigned. If data are aligned to DOY 1,
; the actual DOY is 1+FLOOR(value), so a value of 0.5 is 1+0 

; oper can be "plus", "minus" or "non" (in case of none the avg is just transformed in dekads)
;this is used to transform pheno results into dekadal units of the calendar year for ferdinando
; first limits SD to 30 days
; then perform the operation requeste ("plus" or "minus") as avg operation sd
; then check that is stille ge 1 or le 365
; then transform wisely to fekad

;Example:
; avgfn ='S:\Actions\FOODSEC\temporary_share\WA phenology\analysis 2013\1gspy_1s_A1eos-1997_sos1_DOC_TZPavg.img'
; sdfn = 'S:\Actions\FOODSEC\temporary_share\WA phenology\analysis 2013\1gspy_1s_A1eos-1997_sos1_DOC_TZPsd.img' 
; ImageAVGandSDFromDoyToDek, 4716, 1233, 4, avgfn, sdfn, 'minus'

;list all first doy for each dek, from 1 to 36
;use with GE
fdoy = [1.0, 11, 21, 32, 42, 52, 60, 70, 80, 91, 101, 111, 121, 131, 141, 152, 162, 172, 182, 192, 202, 213, 223, 233, 244, 254, 264, 274, 284, 294, 305, 315, 325, 335, 345, 355]
;use with LT
ldoy = [fdoy[1:N_ELEMENTS(fadoy)-1], 366.0] 
;ldoy = [10, 20, 31, 41, 51, 59, 69, 79, 90, 100, 110, 120, 130, 140, 151, 161, 171, 181, 191, 201, 212, 222, 232, 243, 253, 263, 273, 283, 293, 304, 314, 324, 334, 344, 354, 365]


IF FIX(dt) NE 4 THEN SOP  ;should be float
sd = FLTARR(ns, nl)
avg = sd

;open SD and limit it to 30 days

OPENR, lun, fn_sd_image, /GET_LUN
READU, lun, sd
FREE_LUN, lun
indfin = WHERE(FINITE(sd) EQ 1, countfin)
IF (countfin EQ 0) THEN STOP
indneg = WHERE(sd[indfin] LT 0.0, countneg)
IF (countneg GT 0) THEN STOP 
indat = WHERE(sd[indfin] GT 30.0, countat)  ;above threshold
IF (countat GT 0) THEN sd[indfin[indat]] = 30.0
 
;open AVG
OPENR, lun, fn_avg_image, /GET_LUN
READU, lun, avg
FREE_LUN, lun
;if only one data is present, avg may be finite and sd not, so compute finite on sd
;indf = WHERE(FINITE(avg), countf)
indf = WHERE(FINITE(sd), countf)
indNaN = WHERE((FINITE(sd) NE 1), countNaN)
;print, countf, countf2
;inddiff1 = SetDifference(indf, indf2) ;in avg and not in sd
;inddiff2 = SetDifference(indf2, indf) ;in sd and not in avg
IF (countNaN GT 0) THEN avg[indNaN] = !VALUES.F_NAN 
; data are expressed in days since the first day of the cycle (an arbitrary day). If the first day is 1,
; the DOC value will be 1                                        
avg[indf] = FLOAT(FLOOR(avg[indf]))  
ind1 = WHERE(avg[indf] GE 366, count1)  
ind2 = WHERE(avg[indf] LT 1, count2)
;temporay to remedy an error in a1_anomaly, REMOVE IT!!!!!!!!!!!!!!!!
avg[indf[ind2]] = avg[indf[ind2]] + 365.0  
ind2 = WHERE(avg[indf] LT 1, count2)
IF (count1+count2 NE 0) THEN STOP
 
;now compute the required operation with avg and sd and reposition values outside 1-365
CASE oper OF
  'plus': data = avg + sd
  'minus': data = avg - sd
  'none': data = avg
  ELSE: STOP
ENDCASE
;check range
indGE366 = WHERE(data[indf] GE 366, countGE366) 
indLT1 = WHERE(data[indf] LT 1, countLT1)
IF (countGE366 NE 0) THEN data[indf[indGE366]] = data[indf[indGE366]] - 365.0
IF (countLT1 NE 0) THEN data[indf[indLT1]] = data[indf[indLT1]] + 365.0
;double check
inddc = WHERE((data[indf] GE 366.0) OR (data[indf] LT 1.0), countdc)
IF (countdc GT 0) THEN STOP

check = INTARR(ns,nl)
data_out = data

FOR i = 0, N_ELEMENTS(fdoy)-1 DO BEGIN
  IF i GT 35 THEN STOP
  ;find the doy of dek i and substitute
  ind = WHERE((data[indf] GE fdoy[i]) AND (data[indf] LT ldoy[i]), count)
  PRINT, i+1, fdoy[i], ldoy[i]
  IF (count GT 0) THEN BEGIN 
    data_out[indf[ind]] = FLOAT(i + 1)
    check[indf[ind]] = 1
  ENDIF    
ENDFOR
IF (TOTAL(check[indf]) NE countf) THEN STOP
;    a = where(check[indf] EQ 0)
;    print, data[indf[a]]
;open output
fn1 = STRTRIM(STRSPLIT(fn_avg_image, '.', /EXTRACT),2)
fn1 = fn1[0] 
OPENW, lun, fn1+'_'+STRTRIM(oper,2)+'_sd_in_dekads.img', /GET_LUN
WRITEU, lun, data_out
FREE_LUN, Lun 
FILE_COPY, fn1+'.hdr',  fn1+'_'+STRTRIM(oper,2)+'_sd_in_dekads.hdr'

END
