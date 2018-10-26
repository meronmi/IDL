FUNCTION ZARRAY1D2D3D, Xin, XHIS = XhisIn, MINVAL = minval, MAXVAL = maxval, NODATAVAL = nodataval, GAIN = gain, OFFSET = offset, SIMPLEDIFF = simplediff
; Compute the z score on the "z" dimesnion of the array X
; if X is 1D the "z" dimesnion is 1 (it is a vector)
; if X is 2D the "z" dimesnion is 2 (it is likely a line of a bil file) 
; if X is 3D the "z" dimesnion is 3 (it is likely a bsq)

; if Xhis is provide, it computes mean and SD on this matrix instead of on X

; if /SIMPLEDIFF is set it does not fivide by sd (return the simple diff
; example 
; a = findgen(3,4,5)
; (a[0,0,*] - mean(a[0,0,*]))/stddev(a[0,0,*])

;avoid making changes to input arrays
X = FLOAT(Xin)



; Determine dimension and datatype, make some checks
sz = SIZE(X)
IF (sz[0] EQ 0) THEN BEGIN
  PRINT, 'ZARRAY1D2D3D.pro: Error, z-score requested on a scalar'
  STOP
ENDIF

IF ((sz[0] EQ 1) AND sz[1] LT 4) OR $
   ((sz[0] EQ 2) AND sz[2] LT 4) OR $
   ((sz[0] EQ 3) AND sz[3] LT 4) THEN BEGIN
  PRINT, 'ZARRAY1D2D3D.pro: Error, z-score requested on less then 4 obs'
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
    PRINT, 'ZARRAY1D2D3D.pro: Error, X and Xhis do not have the same size'
    STOP
  ENDIF
ENDIF ;ELSE BEGIN
;  Xhis = X
;ENDELSE


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

;X is now ready for z-scoring
!except=0 ; avoid issuing error when all data are NaN
IF (N_ELEMENTS(XhisIn) GT 0) THEN  BEGIN
  meanX = MEAN(Xhis, DIMENSION = sz[0], /NAN) ;mean always on the last dimension
  IF (N_ELEMENTS(simplediff) EQ 0) THEN sdX = STDDEV(Xhis, DIMENSION = sz[0], /NAN) ;mean always on the last dimension
ENDIF ELSE BEGIN
  meanX = MEAN(X, DIMENSION = sz[0], /NAN) ;mean always on the last dimension
  IF (N_ELEMENTS(simplediff) EQ 0) THEN sdX = STDDEV(X, DIMENSION = sz[0], /NAN) ;mean always on the last dimension
endelse
clear = CHECK_MATH() ;set back to normal
!except=0

CASE sz[0] OF
  ;1: do nothong
  2: BEGIN
    meanX = REBIN(meanX, [sz[1], sz[2]])
    IF (N_ELEMENTS(simplediff) EQ 0) THEN sdX = REBIN(sdX, [sz[1], sz[2]])
    END
  3: BEGIN
    meanX = REBIN(meanX, [sz[1], sz[2], sz[3]])
    IF (N_ELEMENTS(simplediff) EQ 0) THEN sdX = REBIN(sdX, [sz[1], sz[2], sz[3]])
    END
ENDCASE
IF (N_ELEMENTS(simplediff) EQ 0) THEN Z = (X-meanX)/sdX ELSE Z = (X-meanX)  
RETURN, Z
END