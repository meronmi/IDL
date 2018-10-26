

;
;FUNCTION build_avg_vec4, p_in, period
;;Purpose: to build a pdek (progressive day, dekad) vector containing the average timing of the ecent from:
;;-p: progressive time (can be pdekad, pdays) 
;
;;SOS or EOS should happen once a year (because seasons are analysed seperately). So, the year oof SOS or EOS should be like
;;INDGEN(N_ELEMENTS(gsTcDek[*,1])). Some SOS or EOS may jump the previous or following year. For example, if SOS EQ 35, one year it may go
;;to SOS EQ 1 in the next year. To build an average vector of SOS it has to be asigned to the correct year.
;;A majority criterion is used. I look for best agreement with an INDGEN() vector. This indegn can start at -1, 0 or +1
;
;;History
;;22/11 bug fixed (if it was starting form more than 36) and period added
;CASE period OF
;  1:   oyip = 365   ;one year period
;  10 : oyip = 36
;  ELSE: STOP
;ENDCASE
;
;pt = p_in                               ;progressive time
;pyid =  INDGEN(N_ELEMENTS(p_in))        ;progressive year id            
;py = pyid * !VALUES.F_NAN               ;true progressive year
;py_out = py                     
;ct = pt  * !VALUES.F_NAN                ;true circular time
;indf = WHERE((FINITE(pt) EQ 1) AND (pt NE -999), count)
;IF (count EQ 0) then STOP   ;not very elegant.. it should never happen
;
;py[indf] = FLOOR(pt[indf]/36.0)         ;true progressive year   
;ct[indf] = pt[indf] MOD oyip             ;crircular time (dekad of year)
;ind0 = WHERE(ct[indf] EQ 0, count0)
;IF (count0 NE 0) THEN ct[indf[ind0]] = oyip 
;
;;Find the alignemnt best matching the py
;cost = FLTARR(3)
;FOR s = -1, 1 DO BEGIN
;  cost = ABS(py[indf]-(pyid[indf]+s))
;ENDFOR
;mincost = MIN(cost, overallshift)
;diff = py[indf] - pyid[indf] + overallshift
;
;;now assign the new year
;py_out[indf] = py[indf] - diff
;
;avgtcc = avg_dec(ct[indf])
;RETURN, ROUND(avgtcc)+oyip*py_out
;END



FUNCTION build_avg_vec3, p_in, period
;Purpose: to build a pdek (progressive day, dekad) vector containing the average timing of the ecent from:
;-p: progressive time (can be pdekad, pdays) 

;SOS or EOS should happen once a year (because seasons are analysed seperately). So, the year oof SOS or EOS should be like
;INDGEN(N_ELEMENTS(gsTcDek[*,1])). Some SOS or EOS may jump the previous or following year. For example, if SOS EQ 35, one year it may go
;to SOS EQ 1 in the next year. To build an average vector of SOS it has to be asigned to the correct year.
;A majority criterion is used. I look for best agreement with an INDGEN() vector. This indegn can start at -1, 0 or +1

;History
;22/11 bug fixed (if it was starting form more than 36) and period added
p = p_in

CASE period OF
  1:   oyip = 365   ;one year period
  10 : oyip = 36
  ELSE: STOP
ENDCASE

indf = WHERE((FINITE(p) EQ 1) AND (p NE -999), count)
IF (count EQ 0) then STOP   ;not very elegant.. it should never happen

goodtp = p[indf]                       ;good time progressive
year = INTARR(N_ELEMENTS(goodtp))      
goodtcc = goodtp MOD oyip               ;good time circular calendar (1 is first dekad or first year)
ind = WHERE(goodtcc EQ 0, count)
IF (count NE 0) THEN goodtcc[indf] = oyip

year = FLOOR(goodtp/36.0)

avgtcc = avg_dec(goodtcc)

;Find the years that would require a variation in the reference year to confrom with the mean
;For example p=[35,3,5] with year=[0,2,4], the mean is 2.3, and I know that they require a
;variation when the absolute diff of the original is at least one unit longer than the circular
;difference

;the following does not work with days! so add a stop
IF period EQ 1 THEN STOP
shortdiff = short_ang_between_v2_and_v1(goodtcc,goodtcc*0.0+avgtcc)
ind = WHERE(ABS(goodtcc-avgtcc) - $
      ABS(shortdiff) GT 1, count)
      
IF (count) GT 0 THEN BEGIN  ;there are samples to be treated 
  tmp = year
  shiftyear = shortdiff * 0 + 1
  ind0 = WHERE(shortdiff LT 0, count0)
  IF (count0 GT 0) THEN shiftyear[ind0] = -1
  tmp[ind] = year[ind] +  shiftyear[ind]
  ;intersection = SetIntersection(tmp[ind], year)  ;check that they are not already used
  ;IF (intersection NE -1) THEN STOP
  year = tmp
ENDIF

;Now the array year should not have repeated elements. If there are still equal elements
;it means that I could not recognize a needed shift in the year by comparing the absolute 
;difference with the circular one, example [12,32,36+20] -> goodtcc [12,32,20], year [0,0,1]
;In this rarw case some subjective decision has to be taken. 
;I just remove the first occurrence of repeated elements until no repeated element are present..


;find if there are repeated elements and where
repind = find_rep_elements(year)
year0 = year
indf0 = indf
IF (repind[0] NE -1) THEN BEGIN
  REPEAT BEGIN
    year0 = remove_sub_from_array(year0, repind[0])
    ;update indexs
    indf0 = remove_sub_from_array(indf0, repind[0])
    repind = find_rep_elements(year0)
  ENDREP UNTIL (repind[0] EQ -1) 
ENDIF

p=ROUND(avgtcc)+oyip*year0
p_out=p_in*!VALUES.F_NAN
p_out[indf0]=p
;print, pdekad
;print, pdek_avg_vec

RETURN, p_out
END

FUNCTION build_avg_vec2, pdekad
;Purpose: to build a pdek (progressive dekad) vector containing the average timing of the ecent from:
;-pdekad: progressive dekad 

;SOS or EOS should happen once a year (because seasons are analysed seperately). So, the year oof SOS or EOS should be like
;INDGEN(N_ELEMENTS(gsTcDek[*,1])). Some SOS or EOS may jump the previous or following year. For example, if SOS EQ 35, one year it may go
;to SOS EQ 1 in the next year. To build an average vector of SOS it has to be asigned to the correct year.
;A majority criterion is used. I look for best agreement with an INDGEN() vector. This indegn can start at -1, 0 or +1

ind=WHERE((FINITE(pdekad) EQ 1) AND (pdekad NE -999), count)
IF (count EQ 0) then STOP   ;not very elegant.. it should never happen
arrayX=INDGEN(N_ELEMENTS(pdekad))
x=arrayX[ind]
y=pdekad[ind]
c=MEAN((y-36.0*x), /DOUBLE) ;changed because if one is missing at the begininng it may be a problem
;c = avg_dec(y)
pdek_avg_vec=ROUND(c)+36.0*arrayX
;print, pdekad
;print, pdek_avg_vec

RETURN, pdek_avg_vec
END


FUNCTION build_avg_vec, dekoy_vec, year_vec
;Purpose: to build a pdek (progressive dekad) vector containing the average timing of the ecent from:
;-a vector of dekade of year and corresponding
;-vector of years 
;- the avg occurrence of the event in dekoy


;SOS or EOS should happen once a year (because seasons are analysed seperately). So, the year oof SOS or EOS should be like
;INDGEN(N_ELEMENTS(gsTcDek[*,1])). Some SOS or EOS may jump the previous or following year. For example, if SOS EQ 35, one year it may go
;to SOS EQ 1 in the next year. To build an average vector of SOS it has to be asigned to the correct year.
;A majority criterion is used. I look for best agreement with an INDGEN() vector. This indegn can start at -1, 0 or +1

ind=WHERE((FINITE(dekoy_vec) EQ 1) AND (dekoy_vec NE -999), count)
IF (count EQ 0) then STOP   ;not very elegant.. it should never happen
avg=ROUND(avg_dec(dekoy_vec[ind]))
IF (avg EQ 0) THEN avg = 36

;build a vector with a smooth sequence of years
first_year=year_vec[ind[0]]-ind[0]
;first_year=
year_vec0=INDGEN(N_ELEMENTS(year_vec))+first_year

yy=[-2,-1,0,1,2]
year_shift = FLTARR(N_ELEMENTS(yy))  ;to store the summation of the difference with INDGEN, starting -1,0,+1
FOR i = 0, N_ELEMENTS(yy)-1 DO year_shift[i]=TOTAL(year_vec[ind]-(year_vec0[ind]+yy[i]), /NAN)
min = MIN(ABS(year_shift), indmin, /NAN)
;compute the corrected yer
cor_year = INDGEN(N_ELEMENTS(year_vec))+yy[indmin]
pdek_avg_vec = cor_year*36+avg-1

RETURN, pdek_avg_vec
END