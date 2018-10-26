FUNCTION find_first_season_JD, s0, s1, frstDoyOfCycleOut

;Of the two series paired s0 and s1 (expressed in Julian Days)
;find the one happening first according to the cycle out.
;To be robust consider that they may not have corresponding data first season present for 5 
;years without second season, then 5 years with second season without first

;OUTPUT: indication of first dekads is ordinal (first, second,..)




indf0 = WHERE((FINITE(s0) EQ 1) AND (s0 NE -999), count0)
indf1 = WHERE((FINITE(s1) EQ 1) AND (s1 NE -999), count1)
IF ((count0 EQ 0) OR (count1 EQ 0)) THEN BEGIN
 ;if one of the two season was detected to be present but the always failing
 ;possible cases: only one failing. make it as the second
 IF ((count1 EQ 0) AND (count0 GT 0)) THEN RETURN, 0
 IF ((count0 EQ 0) AND (count1 GT 0)) THEN RETURN, 1
 ;both failing, there is no difference which come first
 RETURN, 0
ENDIF

;express it in dekoc
s0doc = JD2DOC(s0, frstDoyOfCycleOut)
s1doc = JD2DOC(s1, frstDoyOfCycleOut)
 
;consider only finite values and avg it
avg0=MEAN(s0doc[indf0], /DOUBLE)
avg1=MEAN(s1doc[indf1], /DOUBLE)
IF (avg0 GT 365.0) THEN avg0 = avg0 - 365.0  
IF (avg0 LE 0.0)  THEN avg0 = avg0 + 365.0
IF (avg1 GT 365.0) THEN avg1 = avg1 - 365.0  
IF (avg1 LE 0.0)  THEN avg1 = avg1 + 365.0


;decide which comes first in the new cycle
IF (avg0 LT avg1) THEN RETURN, 0 ELSE RETURN, 1
END

;FUNCTION find_first_season, s0, s1, ord_Xfrst_dek, Xfrst_year, ord_Yfrst_dek, Yfrst_year
;
;;of the two series paired s0 and s1 find the one happening first
;;to be robust consider that they may not have corresponding data
;;first season present for 5 years without second season, then 5 years with 
;;second season without first
;
;;indication of first dekads is ordinal (first, second,..)
;
;;transform into abs for use when required
;abs_Xfrst_dek = ord_Xfrst_dek - 1
;abs_Yfrst_dek = ord_Yfrst_dek - 1
;
;
;indf0 = WHERE((FINITE(s0) EQ 1) AND (s0 NE -999), count0)
;indf1 = WHERE((FINITE(s1) EQ 1) AND (s1 NE -999), count1)
;IF ((count0 EQ 0) OR (count1 EQ 0)) THEN BEGIN
; ;if one of the two season was detected to be present but the always failing
; ;possible cases:
; ;only one failing. make it as the second
; IF ((count1 EQ 0) AND (count0 GT 0)) THEN RETURN, 0
; IF ((count0 EQ 0) AND (count1 GT 0)) THEN RETURN, 1
; ;both failing, it's indiffrent which come first
; RETURN, 0
;ENDIF 
;; both seasons are represented
;s00=s0[indf0]
;s11=s1[indf1]
;
;
;
;;Express series in progressive dekads from dekad 0 (first dekad) of the new cycle
;s0pdekad_Ycycle = s00 +  abs_Xfrst_dek + (Xfrst_year - Yfrst_year) * 36 - abs_Yfrst_dek + 1
;s1pdekad_Ycycle = s11 +  abs_Xfrst_dek + (Xfrst_year - Yfrst_year) * 36 - abs_Yfrst_dek + 1
;
;;Express it as dekad of year
;s0dekocY = pdekad2dekoy(s0pdekad_Ycycle, 0, 0)
;s1dekocY = pdekad2dekoy(s1pdekad_Ycycle, 0, 0)
;
;;average it
;avg0 = avg_dec(s0dekocY)
;avg1 = avg_dec(s1dekocY)
;;decide which comes first in the new cycle
;IF (avg0 LT avg1) THEN RETURN, 0 ELSE RETURN, 1
;END