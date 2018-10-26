FUNCTION VCI_by_dek_ARRAY3D, Xin, first_dek, last_dek, XHIS = XhisIn, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset, $
                             STATFNAME = statfname
; Compute the VCI on the "z" dimesnion of the array X
; first_dek and last_dek are the dekad of the first and last dimesnion (corresponding to the last dimension of Xin)
; if X is 1D the "z" dimesnion is 1 (it is a vector)
; if X is 2D the "z" dimesnion is 2 (it is likely a line of a bil file) 
; if X is 3D the "z" dimesnion is 3 (it is likely a bsq)

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
  PRINT, 'VCI_by_dek_ARRAY3D.pro: Error, VCI requested something that is not 3d'
  STOP
ENDIF

IF ((sz[0] EQ 3) AND sz[3] LT 4) THEN BEGIN
  PRINT, 'VCI_by_dek_ARRAY3D.pro: Error, VCI requested on less then 4 obs'
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
    PRINT, 'VCI_by_dek_ARRAY3D.pro: Error, X and Xhis do not have the same size'
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

minX = FLTARR(sz[1],sz[2])
maxX = FLTARR(sz[1],sz[2])
Y = FLTARR(sz[1],sz[2],sz[3])

FOR i = 0, 35 DO BEGIN
  ind = WHERE(deks EQ i+1, count)
  minX = MIN(X4STATS[*,*,ind], DIMENSION = sz[0], /NAN)
  maxX = MAX(X4STATS[*,*,ind], DIMENSION = sz[0], /NAN) 
  
  IF (N_ELEMENTS(statfname) GT 0) THEN BEGIN
    minamxXdek[*,*,i,0] = minX
    minamxXdek[*,*,i,1] = maxX
  ENDIF
  ;make sure to keep only 2 decimal, unless I get small noise like 100.00001
  ;Y[*,*,ind] = 100.0 * (X[*,*,ind] - REBIN(minX, sz[1], sz[2], count)) / (REBIN(maxX, sz[1], sz[2], count) - REBIN(minX, sz[1], sz[2], count))
  tmp  = 100.0 * (X[*,*,ind] - REBIN(minX, sz[1], sz[2], count)) / (REBIN(maxX, sz[1], sz[2], count) - REBIN(minX, sz[1], sz[2], count))
  indFin = WHERE(FINITE(tmp), countFin)
  IF (countFin GT 0) THEN tmp[indFin] = FLOAT(ROUND(tmp[indFin]*100)/100.)
  Y[*,*,ind] = tmp
ENDFOR
clear = CHECK_MATH() ;set back to normal
!except=1
IF (N_ELEMENTS(statfname) GT 0) THEN BEGIN
  res = write_envi_img(minamxXdek[*,*,*,0], statfname + '_min.img')
  res = write_envi_hdr(statfname + '_min.hdr', sz[1], sz[2], 4, NBANDS= 36)
  res = write_envi_img(minamxXdek[*,*,*,1], statfname + '_max.img')
  res = write_envi_hdr(statfname + '_max.hdr', sz[1], sz[2], 4, NBANDS= 36)
ENDIF
RETURN, Y
END