FUNCTION compute_gsavgTcJD, cs, gsrel, gsTc
; This function compute the average occurrence of a given event (gsTc in this case)


;INPUT
;- cs : current season index (0 for first, 1 for second)
;- gsrel: reliability variable
;- gsTc: gs sart or stop computed with a low thershold Tc (usually 0.05) 
;EXAMPLE: season_completeness(0, gsrel, gsstrtTc)  
 
;computation of completeness
;find those season with reliability <1000 (Nan should not be present)
indr=WHERE(gsrel[cs,*] LT 1000, countr)  ;reliable seasons
;indu=WHERE(gsrel[cs,*] GE 1000, countu)  ;unrelieble seasons
;if there are no reliable seasons it means I am anylizing 2 years, both not totally complete..
;consider them reliable (and issue a warning ? not implemented..)
IF (countr EQ 0) THEN BEGIN
  indr = INDGEN(N_ELEMENTS(gsrel[cs,*]))
  countr = N_ELEMENTS(gsrel[cs,*])
ENDIF 
;for such seasons, compute the avg position of sos and eos Tc threshold (normally 5%)

avg_vec = 1
gsTcDoc=JD2DOC(gsTc[cs,*],1,avg_vec = avg_vec)

RETURN, avg_vec
END

;FUNCTION compute_gsavgTcPdek, cs, gsrel, gsTc
;; This function compute teh average occurrence of a given event (gsTc in this case)
;
;;INPUT
;;- cs : current season index (0 for first, 1 for second)
;;- gsrel: reliability variable
;;- gsTc: gs sart or stop computed with a low thershold Tc (usually 0.05) 
;;EXAMPLE: season_completeness(0, gsrel, gsstrtTc)  
; 
;;computation of completeness
;;find those season with reliability <1000 (Nan should not be present)
;indr=WHERE(gsrel[cs,*] LT 1000, countr)  ;reliable seasons
;;indu=WHERE(gsrel[cs,*] GE 1000, countu)  ;unrelieble seasons
;;if there are no reliable seasons it means I am anylizing 2 years, both not totally complete..
;;consider them reliable (and issue a warning ? not implemented..)
;IF (countr EQ 0) THEN BEGIN
;  indr = INDGEN(N_ELEMENTS(gsrel[cs,*]))
;  countr = N_ELEMENTS(gsrel[cs,*])
;ENDIF 
;;for such seasons, compute the avg position of sos and eos Tc threshold (normally 5%)
;;gsTcDek=pdekad2dekoy00(gsTc[cs,*]) ;OBSOLATE
;;avg=ROUND(avg_dec(gsTcDek[indr,0]))
;;IF (avg EQ 0) THEN avg = 36
;
;avg_vec = 1
;gsTcDek2=pdekad2dekoc(gsTc[cs,*],1,1, avg_vec = avg_vec)
;ind999=WHERE(gsTcDek2 EQ -999, count999)
;IF (count999 GT 0) THEN gsTcDek2[ind999]=!VALUES.F_NAN
;;avg2=MEAN(gsTcDek2, /NAN)
;gsavgTcPdek2=avg_vec
;
;RETURN, gsavgTcPdek2
;END


FUNCTION season_completeness, cs, gsrel, gsStartTc, gsStopTc, gsinistart, gsiniend
;For the first and the last season, this function provide an estimate of the data availability.
;For every season compares the the period used for optimizing the model with that usually required.
;The one ususally required is identified by computing the avarage SOS and EOS (computed with a very low
;threshold, e.g. 0.05) and reporting this avg period on the carrent year. If for example I have 90% of coverage it
;means that I have 90 % of the data I usually employ to detect the season.
;INPUT
;- cs : current season index (0 for first, 1 for second)
;- gsrel: reliability variable
;- gsStartTc/gsStopTc: gs sart or stop computed with a low thershold Tc (usually 0.05) 
;EXAMPLE: season_completeness(0, gsrel, gsstrtTc, gsstopTc, gsinistart, gsiniend)

indf = WHERE(FINITE(gsStartTc[cs,*]) EQ 1, countf)
IF (countf EQ 0) then RETURN, REFORM(gsStartTc[cs,*]*!VALUES.F_NAN)
gsavgsosTcJD=compute_gsavgTcJD(cs, gsrel, gsStartTc)
;gsavgsosTcPDek=compute_gsavgTcPdek(cs, gsrel, gsStartTc)
gsavgeosTcJD=compute_gsavgTcJD(cs, gsrel, gsStopTc)
;gsavgeosTcPDek=compute_gsavgTcPdek(cs, gsrel, gsStopTc)

;here the start and stop may be equal?
;compute the % of data present in each season 
;bound it to 100% by not considering the part of a ow exceeding the avg
sos_diff=gsavgsosTcJD-gsinistart[cs,*] 
indf=WHERE(FINITE(sos_diff), countf)
IF (countf GT 0) THEN BEGIN
  indfgt0=WHERE(sos_diff[indf] GT 0, countfgt0)
  IF (countfgt0 GT 0) THEN sos_diff[indf[indfgt0]]=0.0
ENDIF  
;sos_diff = sos_diff < 0
eos_diff = gsiniend[cs,*]-gsavgeosTcJD
indf=WHERE(FINITE(eos_diff), countf)
IF (countf GT 0) THEN BEGIN
  indfgt0=WHERE(eos_diff[indf] GT 0, countfgt0)
  IF (countfgt0 GT 0) THEN eos_diff[indf[indfgt0]]=0.0
ENDIF  
;eos_diff = eos_diff < 0

;check that there is no zero division, in that case set ret to 0
denom = (gsavgeosTcJD-gsavgsosTcJD)
indn0 = WHERE(denom NE 0, countn0)
ret = gsavgsosTcJD*0.0
IF (countn0 EQ 0) THEN BEGIN
  ret[*]=0.0
ENDIF ELSE BEGIN
  ret[indn0]=100.0 * reform(((gsavgeosTcJD[indn0]-gsavgsosTcJD[indn0]) + $
    (sos_diff[indn0]) + (eos_diff[indn0])) / $
    (denom[indn0]))
ENDELSE 


;it may happen that  this ratio is negative meaning that the current season
;was deemed to stop X dekads before avgerage eosTc and that X dekads is bigger
;than the average lenTc. Set it to 0, it just means that it is not covered at all
indf = WHERE(FINITE(ret), countf)
IF (countf GT 0) THEN BEGIN
  indn = WHERE(ret[indf] LT 0, countn)
  IF (countn GT 0) THEN ret[indf[indn]]=0.0 
ENDIF
;There is not logical reason for having a completeness LT 100 in the inner seasons. But it may happen 
;because of mathematical manipulation (inistart was moved beyond the mean for example.
;Set inner seasons to 100
;new code  (8 May 2012)
;ret can contain NaN and real number 0.0-100.0
; find the index of the first and last season with real value
IF (countf GT 0) THEN BEGIN
  ind1st = indf[0]
  indlst = indf[N_ELEMENTS(indf)-1]
  ;if there are elements in between, set them to 0
  IF (indlst GT ind1st+1) THEN ret[ind1st+1:indlst-1]=100
ENDIF 
indN = WHERE(FINITE(gsStartTc[cs,*]) EQ 0, countN)
IF (countN GT 0) THEN ret[indN]=!VALUES.F_NAN
;IF N_ELEMENTS(ret) GT 2 THEN $  ;if there are just two elements, leave it like it is
;  IF (cs EQ 0) THEN ret[1:N_ELEMENTS(ret)-2]=100 ELSE ret[0:N_ELEMENTS(ret)-2]=100 
RETURN, ret
END
