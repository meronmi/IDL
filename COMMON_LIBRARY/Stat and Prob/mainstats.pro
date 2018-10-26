FUNCTION mainStats, xin, VALIDRANGE=validRange, GAIN_OFFSET = gain_offset, CIRCULARPHENO = circularpheno
;compute basic stats on the x array
;INPUT
; - xin is the array
;OPTIONS
; VALIDRANGE is the min max to be considered valid
; for scaling the data AFTER having checked valida range
; /CIRCULAR if dtat are circular (phenology)

;RETURN
;'n','avg','sd','min','max','median','q1','q3'
x = FLOAT(xin)
xStats = CREATE_STRUCT('n',0L,'avg',0.0,'sd',0.0,'min',0.0,'max',0.0,'median',0.0,'q1',0.0,'q3',0.0)
;retain only finite data
indFin = WHERE(FINITE(x), countFIN)
IF (countFIN LT 3) THEN BEGIN
  xStats.n = countFIN
  xStats.avg = !VALUES.F_NAN
  xStats.sd = !VALUES.F_NAN
  xStats.min = !VALUES.F_NAN
  xStats.q1 =  !VALUES.F_NAN
  xStats.median = !VALUES.F_NAN
  xStats.q3 =  !VALUES.F_NAN
  xStats.max = !VALUES.F_NAN
  RETURN, xStats
  
ENDIF
x = x[indFin]
indFin = 0

;clean input data
IF (N_ELEMENTS(validRange) GT 0) THEN BEGIN
  IF (validRange[1] LE validRange[0]) THEN STOP
  indFin = WHERE(((x GE validRange[0]) AND (x LE validRange[1])), countFIN)
  IF (countFIN LT 3) THEN BEGIN
    xStats.n = countFIN
    xStats.avg = !VALUES.F_NAN
    xStats.sd = !VALUES.F_NAN
    xStats.min = !VALUES.F_NAN
    xStats.q1 =  !VALUES.F_NAN
    xStats.median = !VALUES.F_NAN
    xStats.q3 =  !VALUES.F_NAN
    xStats.max = !VALUES.F_NAN
    RETURN, xStats
    RETURN, xStats
  ENDIF
  x = x[indFin]
  indFin = 0
ENDIF

;scale it if necessary
IF (N_ELEMENTS(gain_offset) GT 0) THEN BEGIN
  IF (N_ELEMENTS(gain_offset) NE 2) THEN STOP
  x = gain_offset[0] * x + gain_offset[1]
ENDIF



IF  (N_ELEMENTS(x) LT 5) THEN BEGIN
  xStats.n = countFIN
  xStats.avg = !VALUES.F_NAN
  xStats.sd = !VALUES.F_NAN
  xStats.min = !VALUES.F_NAN
  xStats.q1 =  !VALUES.F_NAN
  xStats.median = !VALUES.F_NAN
  xStats.q3 =  !VALUES.F_NAN
  xStats.max = !VALUES.F_NAN
  RETURN, xStats
ENDIF

IF (KEYWORD_SET(circularpheno)) THEN BEGIN
  xStats.n = countFIN
  xStats.avg= mean_vec_1_108(x)
  xStats.sd = sd_vec_1_108(x)
  xStats.min = !VALUES.F_NAN
  xStats.q1 =  !VALUES.F_NAN
  xStats.median = !VALUES.F_NAN
  xStats.q3 =  !VALUES.F_NAN
  xStats.max = !VALUES.F_NAN
ENDIF ELSE BEGIN
  res = CREATEBOXPLOTDATA(REBIN(x, N_ELEMENTS(x)))   ; controllare rebin
  xStats.n = countFIN
  xStats.avg = MEAN(x) 
  xStats.sd = STDDEV(x)
  xStats.min = res[0]
  xStats.q1 =  res[1]
  xStats.median = res[2]
  xStats.q3 =  res[3]
  xStats.max = res[4]
ENDELSE

RETURN, xStats

END