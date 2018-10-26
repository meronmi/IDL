FUNCTION a_by_dek_array1d, Xin, a_type, first_dek, last_dek, XHIS = Xhis, $
            MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset, $
            STATFNAME = statfname, INCLUDECURRENTDATE = includecurrentdate
            
; Compute the anomaly on the "z" dimesnion of the array X
; Xin: 1d array on which to compute anomaly
; a_type: type of anomaly: 'z' is Z-score, 'v' is VCI, 'n' is NEP (also calle VPI)
; first_dek: dekad of the first image
; last_dek: dekad of the last image
; OPTIONAL PARAMETERS
; 
; XHIS: 1d array (same dimension of Xin) on which to compute the statiscts used in the anomaly 
; MINVAL: Xin (and Xhis) are in DN, minval is the minimum valid DN
; MAXVAL = Xin (and Xhis) are in DN, minval is the maximum valid DN
; NODATAVAL = DN reserved for nodata 
; GAIN = gain of physical value = gain * DN + offset
; OFFSET = offset of physical value = gain * DN + offset
; STATFNAME = statfname

;Include Current Date ot not. 
IF KEYWORD_SET(includecurrentdate) THEN ICD = 1 ELSE ICD = 0

; Avoid making changes to input arrays, make it float
X = FLOAT(Xin)
szX = SIZE(X)

; Make some preliminary checks checks
IF (szX[0] NE 1) THEN BEGIN
  PRINT, 'a_by_dek_array1d.pro: Error, z-score requested something that is not 1d' & STOP
ENDIF
IF ((szX[0] EQ 1) AND szX[1] LT 4) THEN BEGIN
  PRINT, 'a_by_dek_array1d.pro: Error, z-score requested on less then 4 obs' & STOP
ENDIF
;make an array listing all the dekad numebr and check that it matches last_dek
deks = INDGEN(szX[szX[0]])
deks[0] = first_dek
FOR i = 1, N_ELEMENTS(deks)-1 DO BEGIN
  deks[i] =deks[i-1] + 1
  IF (deks[i] GT 36) THEN deks[i] = 1 
ENDFOR
IF (deks[-1] NE last_dek) THEN BEGIN
  PRINT, 'a_by_dek_array31.pro. Dimensions do not match with first and last dekads provided' & STOP
ENDIF

; Determine if keywords were passed and act consequently
IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
  ; Avoid making changes to input arrays, make it float 
  Xhis0 = FLOAT(Xhis)
  szHis = SIZE(Xhis0)
  IF (MAX(szX-szHis) NE 0) THEN BEGIN
    PRINT, 'a_by_dek_array1d.pro: Error, X and Xhis do not have the same size' & STOP
  ENDIF
ENDIF 
IF N_ELEMENTS(minval) THEN BEGIN
  ind = WHERE(X LT minval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis0) GT 0) THEN BEGIN
    ind = WHERE(Xhis0 LT minval, count)
    IF (count NE 0) THEN Xhis0[ind] = !VALUES.F_NAN
  ENDIF  
ENDIF
IF N_ELEMENTS(maxval) THEN BEGIN
  ind = WHERE(X GT maxval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis0) GT 0) THEN BEGIN
    ind = WHERE(Xhis0 GT maxval, count)
    IF (count NE 0) THEN Xhis0[ind] = !VALUES.F_NAN
  ENDIF
ENDIF  
IF N_ELEMENTS(nodataval) THEN BEGIN
  ind = WHERE(X EQ nodataval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis0) GT 0) THEN BEGIN
    ind = WHERE(Xhis0 EQ nodataval, count)
    IF (count NE 0) THEN Xhis0[ind] = !VALUES.F_NAN
  ENDIF
ENDIF
IF N_ELEMENTS(gain) THEN BEGIN
  X = TEMPORARY(X) * gain
  IF (N_ELEMENTS(Xhis0) GT 0) THEN BEGIN
    Xhis0 = TEMPORARY(Xhis0) * gain
  ENDIF
ENDIF
IF N_ELEMENTS(offset) THEN  BEGIN
  X = TEMPORARY(X) + offset
  IF (N_ELEMENTS(Xhis0) GT 0) THEN BEGIN
    Xhis0 = TEMPORARY(Xhis0) + offset
  ENDIF
ENDIF
;use Xhis0 or X for stats 
IF (N_ELEMENTS(Xhis0) GT 0) THEN  X4STATS = Xhis0 ELSE X4STATS = X

;variabble to store results
Y = FLTARR(szX[1]) *!VALUES.F_NAN
CASE a_type OF
  'z': BEGIN
    meanXdek = FLTARR(36)
    sdXdek = FLTARR(36)
    stat_suffix = ['_mean','_sd']
  END 
  'v': BEGIN
    minXdek = FLTARR(36)
    maxXdek = FLTARR(36)
    stat_suffix = ['_min','_max']
  END
  'n':BEGIN
    stat_suffix = !NULL ;just to put something here..
  END 
ENDCASE

!except=0 ; avoid issuing error when all data are NaN on the z dimension
FOR i = 0, 35 DO BEGIN
  ind = WHERE(deks EQ i+1, count)
  CASE a_type OF
    'z': BEGIN
      ;general mean and sd are computed anyhow to be saved as general statistics
      meanXdek[i] = MEAN(X4STATS[ind], /NAN)
      sdXdek[i] =  STDDEV(X4STATS[ind], /NAN)
      IF (ICD EQ 1) THEN BEGIN
        Y[ind] = (X[ind] -  meanXdek[i]) / $
                     sdXdek[i]
      ENDIF ELSE BEGIN
        ;Exclude the element being normalized from the computation of statistics
        ;loop on the total elements (belonging to dek i) that requires anomaly
        FOR j = 0, N_ELEMENTS(ind) - 1 DO BEGIN
          ;retain all ind index excluding j
          ind2retain = WHERE(ind NE ind[j])
          meanXdekExcludingJ = MEAN(X4STATS[ind[ind2retain]], /NAN)
          sdXdekExcludingJ =  STDDEV(X4STATS[ind[ind2retain]], /NAN)
          Y[ind[j]] = (X[ind[j]] -  meanXdekExcludingJ) / sdXdekExcludingJ
        ENDFOR
      ENDELSE
    END
    'v': BEGIN
      ;general min and max are computed anyhow to be saved as general statistics
      minXdek[i] = MIN(X4STATS[ind], /NAN)
      maxXdek[i] = MAX(X4STATS[ind], /NAN)
      IF (ICD EQ 1) THEN BEGIN
        ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
        tmp  = 100.0 * (X[ind] - minXdek[i]) / $
                       (maxXdek[i] - minXdek[i])
        indFin = WHERE(FINITE(tmp), countFin)
        IF (countFin GT 0) THEN tmp[indFin] = FLOAT(ROUND(tmp[indFin]*100)/100.)
        Y[ind] = tmp
      ENDIF ELSE BEGIN
        ;Exclude the element being normalized from the computation of statistics
        ;;loop on the total elements (belonging to dek i) that requires anomaly
        FOR j = 0, N_ELEMENTS(ind) - 1 DO BEGIN
          ;retain all ind index excluding j
          ind2retain = WHERE(ind NE ind[j])
          minXdekExcludingJ = MIN(X4STATS[ind[ind2retain]], /NAN)
          maxXdekExcludingJ = MAX(X4STATS[ind[ind2retain]], /NAN)
          ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
;          tmp  = 100.0 * (X[*,*,ind[j]] - REBIN(REFORM(minXdekExcludingJ), szX[1], szX[2], count)) / $
;            (REBIN(REFORM(maxXdekExcludingJ), szX[1], szX[2], count) - REBIN(REFORM(minXdekExcludingJ), szX[1], szX[2], count))
          tmp  = 100.0 * (X[ind[j]] - minXdekExcludingJ) / $
                         (maxXdekExcludingJ - minXdekExcludingJ)
          indFin = WHERE(FINITE(tmp), countFin)
          IF (countFin GT 0) THEN tmp[indFin] = FLOAT(ROUND(tmp[indFin]*100)/100.)
          Y[ind[j]] = tmp
          tmp = 0
        ENDFOR
      ENDELSE    
    END
    'n':BEGIN

      IF (TOTAL(FINITE(X[ind])) GT 0) THEN BEGIN
        IF (ICD EQ 1) THEN BEGIN
          Y[ind] = NEP_array(X[ind], VALHIS = X4STATS[ind])
        ENDIF ELSE BEGIN
          FOR j = 0, N_ELEMENTS(ind) - 1 DO BEGIN
            ;retain all ind index excluding j
            ind2retain = WHERE(ind NE ind[j])
            Y[ind[j]] = NEP_array(X[ind[j]], VALHIS = X4STATS[ind[ind2retain]])
          ENDFOR ;j
        ENDELSE
      ENDIF ;finite    
    END
    ELSE: STOP
  ENDCASE
ENDFOR
clear = CHECK_MATH() ;set back to normal
!except=1
stat1 = !VALUES.F_NAN
stat1name = 'none'
stat2 = !VALUES.F_NAN
stat2name = 'none'

IF (a_type EQ 'z') THEN BEGIN
  stat1 = meanXdek
  stat1name = 'avg'
  meanXdek = 0
  stat2 = sdXdek
  stat2name = 'SD'
  sdXdek = 0
END
IF (a_type EQ 'v') THEN BEGIN
  stat1 = minXdek
  stat1name = 'min'
  minXdek = 0
  stat2 = maxXdek
  stat2name = 'max'
  maxXdek = 0
END

Yout = CREATE_STRUCT('type',a_type,'value', Y, 'stat1', stat1, 'stat1name', stat1name, 'stat2', stat2, 'stat2name', stat2name)  
;IF ((N_ELEMENTS(statfname) GT 0) AND ((a_type EQ 'z') OR (a_type EQ 'v')))THEN BEGIN
;  res = write_envi_img(stat1, statfname + stat_suffix[0] + '.img')
;  res = write_envi_hdr(statfname + stat_suffix[0] + '.hdr', szX[1], szX[2], 4, NBANDS= 36)
;  res = write_envi_img(stat2, statfname + stat_suffix[1] + '.img')
;  res = write_envi_hdr(statfname + stat_suffix[1] + '.hdr', szX[1], szX[2], 4, NBANDS= 36)
;ENDIF
RETURN, Yout
END