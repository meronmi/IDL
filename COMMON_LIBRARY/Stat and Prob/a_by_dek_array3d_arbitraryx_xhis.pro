PRO debug
dataVal=[1, 2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 2]
hisVal= [1, 2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 2,  4,  6,  8,  10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 3,  6,  9,  12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99, 102,  105,  108]
 
X = INTARR(3,3, N_ELEMENTS(dataVal))
for i = 0,2 do for j = 0,2 do X[i,j,*] = dataVal
a_type='n'
first_dek = 1
last_dek =1 
first_year = 2000
Xhisdata = INTARR(3,3, N_ELEMENTS(hisVal))
for i = 0,2 do for j = 0,2 do Xhisdata[i,j,*] = hisVal
XHIS_first_dek = 1
XHIS_last_dek = 36
XHIS_first_year = 2000
gain = 1.0d
offset = 0.0
INCLUDECURRENTDATE = 0
res = a_by_dek_array3d_arbitraryX_XHIS(X, a_type, first_dek, last_dek, first_year, XHISdata = Xhisdata, XHIS_first_dek = XHIS_first_dek, XHIS_last_dek = XHIS_last_dek, $
  XHIS_first_year = XHIS_first_year, GAIN = gain, OFFSET = offset, $
  INCLUDECURRENTDATE = includecurrentdate)
PRINT, 'debug'
END

PRO debug_real_data
;test with real fapar data, a pixel in Spain
rt0 =  READ_ASCII('\\ies\d5\foodsec\Share\COP_FAPAR\thinned\debug_anomaly_computation\pix_RT0_1634-1833.txt', DATA_START=3)
rt0 = REFORM(rt0.FIELD1[1,*])
rt6 =  READ_ASCII('\\ies\d5\foodsec\Share\COP_FAPAR\thinned\debug_anomaly_computation\pix_RT6_9901-1836.txt', DATA_START=3)
rt6 = REFORM(rt6.FIELD1[1,*])
mat0 = FLTARR(3,3,N_ELEMENTS(rt0))
for i = 0,2 do for j = 0,2 do mat0[i,j,*] = rt0
mat6 = FLTARR(3,3,N_ELEMENTS(rt6))
for i = 0,2 do for j = 0,2 do mat6[i,j,*] = rt6
mat6_first_dek = 1
mat6_last_dek = 36
mat6_first_year = 1999  
;first test get Z anomaly from mat6 using the the old and new function, with an withou include, various anomalies 
;tests using no His, v, z, n ok, incl or excl ok
incl = 0
a_type = 'z'
zMat6Old = a_by_dek_array3d(REFORM((mat6)), a_type, mat6_first_dek, mat6_last_dek, $; XHIS = Xhis, NODATAVAL = nodataval, STATFNAME = statfname$
                            MINVAL = 0, MAXVAL = 200, GAIN = 0.005, OFFSET = 0.0, $
                            INCLUDECURRENTDATE = incl)
zMat6Old = zMat6Old[0,0,645:711]
;now extract mat 6 for the small period
mat6_short = mat6[*,*,645:711] 
short_first_dek = 34
short_last_dek = 28
short_first_year = 2016 
zMat6New = a_by_dek_array3d_arbitraryX_XHIS(REFORM(mat6_short), a_type, short_first_dek, short_last_dek, short_first_year, $
                           XHISdata = mat6, XHIS_first_dek = mat6_first_dek, XHIS_last_dek = mat6_last_dek, XHIS_first_year = mat6_first_year, $
                           MINVAL = 0, MAXVAL = 200, GAIN = 0.005, OFFSET = 0.0, $
                           INCLUDECURRENTDATE = incl)                   
zMat6New = zMat6New[0,0,*]
PRINT, TOTAL(ABS(zMat6Old-zMat6New))
h = PLOT(zMat6Old, DIMENSIONS=[900,300])
h = PLOT(zMat6New, COLOR='r', /OVERPLOT)                           

PRINT, 'debug real data'
END



FUNCTION a_by_dek_array3d_arbitraryX_XHIS, X, a_type, first_dek, last_dek, first_year, XHISdata = Xhis, XHIS_first_dek = XHIS_first_dek, XHIS_last_dek = XHIS_last_dek, $
            XHIS_first_year = XHIS_first_year, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset, $
            STATFNAME = statfname, INCLUDECURRENTDATE = includecurrentdate

;Michele Meroni 18/01/2019
            
; Compute the anomaly on the "z" dimesnion of the array X
; X: 3d array on which to compute anomaly
; a_type: type of anomaly: 'z' is Z-score, 'v' is VCI, 'n' is NEP (also calle VPI)
; first_dek: dekad of the first image
; last_dek: dekad of the last image
; first_year: year of the first dek
; OPTIONAL PARAMETERS
; 
; includecurrentdate = in order to normalize item j, include its value (or not) in the computation of stats
; XHIS: 3d array (same x and y dimension of X, but different number of deks and different start dek and end dek) on which to compute the statiscts used in the anomaly
; XHIS_first_dek: dekad of the first image
; XHIS_last_dek: dekad of the last image
; XHIS_first_year: year of the first dek
; MINVAL: X (and Xhis) are in DN, minval is the minimum valid DN
; MAXVAL = X (and Xhis) are in DN, minval is the maximum valid DN
; NODATAVAL = DN reserved for nodata 
; GAIN = gain of physical value = gain * DN + offset
; OFFSET = offset of physical value = gain * DN + offset
; STATFNAME = statfname

;Include Current Date ot not. 
IF KEYWORD_SET(includecurrentdate) THEN ICD = 1 ELSE ICD = 0

; Avoid making changes to input arrays, make it float
;removed, variables must be passed by reference
X = FLOAT(TEMPORARY(X))
szX = SIZE(X)

; Make some preliminary checks checks
IF (szX[0] NE 3) THEN BEGIN
  PRINT, 'a_by_dek_array3d.pro: Error, z-score requested something that is not 3d' & STOP
ENDIF
IF ((szX[0] EQ 3) AND szX[3] LT 4) THEN BEGIN
  PRINT, 'a_by_dek_array3d.pro: Error, z-score requested on less then 4 obs' & STOP
ENDIF

;make an array listing all the dekad numebr and check that it matches last_dek
deks = INDGEN(szX[szX[0]])
years = INDGEN(szX[szX[0]])
deks[0] = first_dek
years[0] = first_year

FOR i = 1, N_ELEMENTS(deks)-1 DO BEGIN
  deks[i] =deks[i-1] + 1
  years[i] = years[i-1] 
  IF (deks[i] GT 36) THEN BEGIN
    deks[i] = 1
    years[i] = years[i] + 1
  ENDIF
ENDFOR
IF (deks[-1] NE last_dek) THEN BEGIN
  PRINT, 'a_by_dek_array3d.pro. Dimensions do not match with first and last dekads provided' & STOP
ENDIF

; Determine if keywords were passed and act consequently
IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
  ; Avoid making changes to input arrays, make it float 
  Xhis = FLOAT(TEMPORARY(Xhis))
  szHis = SIZE(Xhis)
  ;check only x and y
  IF (MAX([szX[1],szX[2]]-[szHis[1],szHis[2]]) NE 0) THEN BEGIN
    PRINT, 'a_by_dek_array3d.pro: Error, X and Xhis do not have the same sample and lines' & STOP
  ENDIF
  IF ~(N_ELEMENTS(XHIS_first_dek) GT 0) THEN BEGIN
    PRINT, 'a_by_dek_array3d.pro: Error, Xhis first dekad missing' & STOP
  ENDIF
  IF ~(N_ELEMENTS(XHIS_last_dek) GT 0) THEN BEGIN
    PRINT, 'a_by_dek_array3d.pro: Error, Xhis last dekad missing' & STOP
  ENDIF
  IF ~(N_ELEMENTS(XHIS_first_year) GT 0) THEN BEGIN
    PRINT, 'a_by_dek_array3d.pro: Error, Xhis first year missing' & STOP
  ENDIF
  ;also for Xhis make an array listing all the dekad numebr and check that it matches last_dek
  deksHis = INDGEN(szHis[szHis[0]])
  yearsHis = INDGEN(szHis[szHis[0]])
  deksHis[0] = XHIS_first_dek
  yearsHis[0] = XHIS_first_year
  FOR i = 1, N_ELEMENTS(deksHis)-1 DO BEGIN
    deksHis[i] =deksHis[i-1] + 1
    yearsHis[i] = yearsHis[i-1]
    IF (deksHis[i] GT 36) THEN BEGIN
      deksHis[i] = 1
      yearsHis[i] = yearsHis[i] + 1
    ENDIF
  ENDFOR
  IF (deksHis[-1] NE XHIS_last_dek) THEN BEGIN
    PRINT, 'a_by_dek_array3d.pro. Dimensions of Xhis do not match with first and last dekads provided' & STOP
  ENDIF
ENDIF 

;fix values and scaling
IF N_ELEMENTS(minval) THEN BEGIN
  ind = WHERE(X LT minval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
    ind = WHERE(Xhis LT minval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF  
ENDIF
IF N_ELEMENTS(maxval) THEN BEGIN
  ind = WHERE(X GT maxval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
    ind = WHERE(Xhis GT maxval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF
ENDIF  
IF N_ELEMENTS(nodataval) THEN BEGIN
  ind = WHERE(X EQ nodataval, count)
  IF (count NE 0) THEN X[ind] = !VALUES.F_NAN
  IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
    ind = WHERE(Xhis EQ nodataval, count)
    IF (count NE 0) THEN Xhis[ind] = !VALUES.F_NAN
  ENDIF
ENDIF
IF N_ELEMENTS(gain) THEN BEGIN
  X = TEMPORARY(X) * gain
  IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
    Xhis = TEMPORARY(Xhis) * gain
  ENDIF
ENDIF
IF N_ELEMENTS(offset) THEN  BEGIN
  X = TEMPORARY(X) + offset
  IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
    Xhis = TEMPORARY(Xhis) + offset
  ENDIF
ENDIF

;use Xhis or X for stats 
IF (N_ELEMENTS(Xhis) GT 0) THEN  X4STATS = Xhis ELSE X4STATS = X

;variabble to store results
Y = FLTARR(szX[1],szX[2],szX[3]) *!VALUES.F_NAN
CASE a_type OF
  'z': BEGIN
    meanXdek = FLTARR(szX[1], szX[2], 36)
    sdXdek = FLTARR(szX[1], szX[2], 36)
    stat_suffix = ['_mean','_sd']
  END 
  'v': BEGIN
    minXdek = FLTARR(szX[1], szX[2], 36)
    maxXdek = FLTARR(szX[1], szX[2], 36)
    stat_suffix = ['_min','_max']
  END
  'n':BEGIN
    stat_suffix = !NULL ;just to put something here..
  END 
ENDCASE


;COMPUTATION OF ANOMALY ##########################################################################################
!except=0 ; avoid issuing error when all data are NaN on the z dimension
FOR i = 0, 35 DO BEGIN
  ;Index of "NRT" data
  ind = WHERE(deks EQ i+1, count)
  ;Index of "Historical" time series, if an historical X was provided by the user, treat it separately
  IF (N_ELEMENTS(Xhis) GT 0) THEN $
    indHis = WHERE(deksHis EQ i+1, countHis) ELSE $
    indHis = WHERE(deks EQ i+1, count)              ;it was not provide, X4STATS is just a copy of X
  
  IF  (a_type EQ 'z') OR (a_type EQ 'v') THEN BEGIN   
    ;specific part
    CASE a_type OF
      'z': BEGIN
        ;general mean and sd are computed anyhow on the full his, to be saved as general statistics
        meanXdek[*,*,i] = MEAN(X4STATS[*,*,indHis], DIMENSION = 3, /NAN)
        sdXdek[*,*,i] =  STDDEV(X4STATS[*,*,indHis], DIMENSION = 3, /NAN)
        ;mangae include current dekad or not in the computation of statistics
        IF (ICD EQ 1) THEN BEGIN ;include
          Y[*,*,ind] = (X[*,*,ind] -  REBIN(REFORM(meanXdek[*,*,i]), szX[1], szX[2], count)) / $
                       REBIN(REFORM(sdXdek[*,*,i]),  szX[1], szX[2], count)
        ENDIF
      END
      'v': BEGIN
        ;general min and max are computed anyhow to be saved as general statistics
        minXdek[*,*,i] = MIN(X4STATS[*,*,indHis], DIMENSION = 3, /NAN)
        maxXdek[*,*,i] = MAX(X4STATS[*,*,indHis], DIMENSION = 3, /NAN)
        ;mangae include current dekad or not in the computation of statistics
        IF (ICD EQ 1) THEN BEGIN ;include
          ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
          tmp  = 100.0 * (X[*,*,ind] - REBIN(REFORM(minXdek[*,*,i]), szX[1], szX[2], count)) / $
            (REBIN(REFORM(maxXdek[*,*,i]), szX[1], szX[2], count) - REBIN(REFORM(minXdek[*,*,i]), szX[1], szX[2], count))
          indFin = WHERE(FINITE(tmp), countFin)
          IF (countFin GT 0) THEN tmp[indFin] = FLOAT(ROUND(tmp[indFin]*100)/100.)
          Y[*,*,ind] = tmp & tmp = 0
        ENDIF
      END
    ENDCASE
    IF (ICD EQ 0) THEN BEGIN ;do not include
      ;Exclude the element being normalized from the computation of statistics
      ;loop on the total elements (belonging to dek i) that requires anomaly
      FOR j = 0, N_ELEMENTS(ind) - 1 DO BEGIN
        ;here, if a differnt His is passed, I have to locate the X elements into Xhis, that may be different;
        ;retain all ind index excluding j
        IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
          ;in this case ind and indHis are different, I have to locate ind[j] in indHis to exclude it
          res = WHERE((deksHis[indHis] EQ deks[ind[j]]) AND (yearsHis[indHis] EQ years[ind[j]]), COMPLEMENT=ind2retain)
          ind2retain = indHis[ind2retain]
        ENDIF ELSE BEGIN
          ind2retain = WHERE(indHis NE indHis[j]) ;His is just a copy of X
          ind2retain = indHis[ind2retain]
        ENDELSE
        CASE a_type OF
          'z': BEGIN
            meanXdekExcludingJ = MEAN(X4STATS[*,*,ind2retain], DIMENSION = 3, /NAN)
            sdXdekExcludingJ =  STDDEV(X4STATS[*,*,ind2retain], DIMENSION = 3, /NAN)
            Y[*,*,ind[j]] = (X[*,*,ind[j]] -  REFORM(meanXdekExcludingJ)) / $
                             REFORM(sdXdekExcludingJ)
          END
          'v': BEGIN
            minXdekExcludingJ = MIN(X4STATS[*,*,ind2retain], DIMENSION = szX[0], /NAN)
            maxXdekExcludingJ = MAX(X4STATS[*,*,ind2retain], DIMENSION = szX[0], /NAN)
            ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
            tmp  = 100.0 * (X[*,*,ind[j]] - REFORM(minXdekExcludingJ)) / $
              (REFORM(maxXdekExcludingJ) - REFORM(minXdekExcludingJ))
            indFin = WHERE(FINITE(tmp), countFin)
            IF (countFin GT 0) THEN tmp[indFin] = FLOAT(ROUND(tmp[indFin]*100)/100.)
            Y[*,*,ind[j]] = tmp & tmp = 0
          END
        ENDCASE
      ENDFOR  ;j
    ENDIF 
  ENDIF ELSE BEGIN ;end of z and v anomaly treatment
    IF (a_type NE 'n') THEN STOP ;anomaly not implemented
    ;n anomaly requested, it is treated differently
    ;loop on the image get the values for dekad i
    FOR s = 0, szX[1]-1 DO BEGIN
      FOR l = 0, szX[2]-1 DO BEGIN
        IF (TOTAL(FINITE(X[s,l,ind])) GT 0) THEN BEGIN
          IF (ICD EQ 1) THEN BEGIN
            Y[s,l,ind] = NEP_array(X[s,l,ind], VALHIS = X4STATS[s,l,indHis])
          ENDIF ELSE BEGIN
            FOR j = 0, N_ELEMENTS(ind) - 1 DO BEGIN
              ;retain all ind index excluding j
              ;here, if a differnt His is passed, I have to locate the X elements into Xhis, that may be different;
              ;retain all ind index excluding j
              IF (N_ELEMENTS(Xhis) GT 0) THEN BEGIN
                ;in this case ind and indHis are different, I have to locate ind[j] in indHis to exclude it
                res = WHERE((deksHis[indHis] EQ deks[ind[j]]) AND (yearsHis[indHis] EQ years[ind[j]]), COMPLEMENT=ind2retain)
                ind2retain = indHis[ind2retain]
              ENDIF ELSE BEGIN
                ind2retain = WHERE(indHis NE indHis[j]) ;His is just a copy of X
                ind2retain = indHis[ind2retain]
              ENDELSE
              Y[s,l,ind[j]] = NEP_array(X[s,l,ind[j]], VALHIS = X4STATS[s,l,ind2retain])
            ENDFOR ;j
          ENDELSE
        ENDIF ;finite
      ENDFOR ;l
    ENDFOR  ;s
  ENDELSE
ENDFOR
clear = CHECK_MATH() ;set back to normal
!except=1

IF (a_type EQ 'z') THEN BEGIN
  stat1 = meanXdek
  meanXdek = 0
  stat2 = sdXdek
  sdXdek = 0
END
IF (a_type EQ 'v') THEN BEGIN
  stat1 = minXdek
  minXdek = 0
  stat2 = maxXdek
  maxXdek = 0
END

IF ((N_ELEMENTS(statfname) GT 0) AND ((a_type EQ 'z') OR (a_type EQ 'v')))THEN BEGIN
  res = write_envi_img(stat1, statfname + stat_suffix[0] + '.img')
  res = write_envi_hdr(statfname + stat_suffix[0] + '.hdr', szX[1], szX[2], 4, NBANDS= 36)
  res = write_envi_img(stat2, statfname + stat_suffix[1] + '.img')
  res = write_envi_hdr(statfname + stat_suffix[1] + '.hdr', szX[1], szX[2], 4, NBANDS= 36)
ENDIF
RETURN, Y
END


