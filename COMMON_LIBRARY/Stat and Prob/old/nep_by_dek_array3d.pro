FUNCTION NEP_by_dek_ARRAY3D, Xin, first_dek, last_dek, XHIS = XhisIn, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset, $
                             STATFNAME = statfname
                             
;%INVPRCTILE  Inverse percentiles of a sample.
;% Calculates the nonexceedance probability p (NEP) for xq values from
;% sample of data x. 
;
; Adapted from MATLAB invepercebtile
; Uses  Weibull
;%  Param        Equation            Description
;%  'Weibull'    i/(n+1)             Unbiased exceedance probabilities for
;%                                   all distributions
;
;That is p = i/(n+1) * 100.
;IF xq LT MIN(x), p = 0
;IF xq GT MAX(x), p = 100
;                            
; Compute it on the "z" dimesnion of the 3D array X


; if Xhis is provide, it computes stats on this matrix instead of on X
; if STATFNAME is provided, stats  are saved as files

;avoid making changes to input arrays

X = FLOAT(Xin)

IF (N_ELEMENTS(XhisIn) EQ 0) THEN BEGIN  
  IF (KEYWORD_SET(mixed_sd) EQ 1) THEN STOP   ;the mixed sd can computed only if the stats are computed over a differnt archive (I need 2 archives to compute mixed stats)
ENDIF

; Determine dimension and datatype, make some checks
sz = SIZE(X)
IF (sz[0] NE 3) THEN BEGIN
  PRINT, 'NEP_by_dek_ARRAY3D.pro: Error, NEP requested something that is not 3d'
  STOP
ENDIF

IF ((sz[0] EQ 3) AND sz[3] LT 4) THEN BEGIN
  PRINT, 'NEP_by_dek_ARRAY3D.pro: Error, NEP requested on less then 4 obs'
  STOP
ENDIF

;make an array listing all the dekad numebr and check that it matches last_dek
deks = INDGEN(sz[sz[0]])
deks[0] = first_dek
FOR i = 1, N_ELEMENTS(deks)-1 DO BEGIN
  deks[i] =deks[i-1] + 1
  IF (deks[i] GT 36) THEN deks[i] = 1 
ENDFOR
IF (deks[-1] NE last_dek) THEN BEGIN
  PRINT, 'Dimensions do not match with first and last dekads provided'
  STOP
ENDIF
;make it float if it is not float or double
dt = sz[-2]
IF ((dt NE 4) AND (dt NE 5)) THEN X = FLOAT(X)

; Determine if keywords were passed and act consequently
IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
  Xhis = FLOAT(XhisIn)
  szHis = SIZE(Xhis)
  IF (TOTAL(sz-szHis) NE 0) THEN BEGIN
    PRINT, 'NEP_by_dek_ARRAY3D.pro: Error, X and Xhis do not have the same size'
    STOP
  ENDIF
ENDIF ;ELSE BEGIN
;  Xhis = X
;ENDELSE

;treat no data
IF N_ELEMENTS(minval) THEN BEGIN
  ind = WHERE(X LT minval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
    ind = WHERE(Xhis LT minval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF  
ENDIF
IF N_ELEMENTS(maxval) THEN BEGIN
  ind = WHERE(X GT maxval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
    ind = WHERE(Xhis GT maxval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF
ENDIF  
IF N_ELEMENTS(nodataval) THEN BEGIN
  ind = WHERE(X EQ nodataval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
    ind = WHERE(Xhis EQ nodataval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF
ENDIF
IF N_ELEMENTS(gain) THEN BEGIN
  X = TEMPORARY(X) * gain
  IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
    Xhis = TEMPORARY(Xhis) * gain
  ENDIF
ENDIF
IF N_ELEMENTS(offset) THEN  BEGIN
  X = TEMPORARY(X) + offset
  IF (N_ELEMENTS(XhisIn) GT 0) THEN BEGIN
    Xhis = TEMPORARY(Xhis) + offset
  ENDIF
ENDIF
ind = 0
;compute min max and VCI it
IF (N_ELEMENTS(statfname) GT 0) THEN minamxXdek = FLTARR(sz[1], sz[2], 36, 2) 
  
!except=0 ; avoid issuing error when all data are NaN
IF (N_ELEMENTS(XhisIn) GT 0) THEN  X4STATS = Xhis ELSE X4STATS = X


;variabble to store results
nep = FLTARR(sz[1],sz[2],sz[3]) *!VALUES.F_NAN

FOR i = 0, 35 DO BEGIN
  ind = WHERE(deks EQ i+1, count)
  ;get the values for dekad i
  dekI4STATS = X4STATS[*,*,ind]
  ;loop on the image get the values for dekad i
  FOR s = 0, sz[1]-1 DO BEGIN
    FOR l = 0, sz[2]-1 DO BEGIN
      values = X4STATS[s,l,ind]
      ;remove NAN
      indFin = WHERE(FINITE(values), countFin)
      ;at least 2 obs to compute inv percentile
      IF (countFin GE 2) THEN BEGIN
        values = values[indFin]
        ;order it
        values = values[SORT(values)];values[UNIQ(values, SORT(values))]
        ;compute corresponding p
        p = (FINDGEN(N_ELEMENTS(values))+1)/(1+N_ELEMENTS(values))*100.0
        ;find the index of non duplicated ones (in case of duplication always take the last element as unique is doing)
        ind0 = UNIQ(values)
        nep[s,l,ind] = INTERPOL(p[ind0],values[ind0],X[s,l,ind])
         ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
        indFin = WHERE(FINITE(nep[s,l,ind]), countFin)
        IF (countFin GT 0) THEN nep[s,l,ind[indFin]] = FLOAT(ROUND(nep[s,l,ind[indFin]]*100)/100.)
        ;check if some of the observation are outside range and assign 0 and 100
        ;look fo observations below the min
        ind0 = WHERE(X[s,l,ind] LT values[0], count0)
        IF (count0 GT 0) THEN  nep[s,l,ind[ind0]] = 0
        ;look for above the max 
        ind0 = WHERE(X[s,l,ind] GT values[-1], count0)
        IF (count0 GT 0) THEN  nep[s,l,ind[ind0]] = 100
      ENDIF
    ENDFOR
  ENDFOR
ENDFOR
clear = CHECK_MATH() ;set back to normal
!except=1
RETURN, nep
END