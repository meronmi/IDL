PRO radiation_interim_ope
;open files
int_fname = 'K:\radiation\stack files\interim'
ope_fname = 'K:\radiation\stack files\ope'
bias_fname = 'K:\radiation\stack files\ope-int'
avgbias_fname = 'K:\radiation\stack files\avg_ope-int'
relAvgBias_fname = 'K:\radiation\stack files\rel_avg_ope-int'
b0_fname = 'K:\radiation\stack files\b0'
b1_fname = 'K:\radiation\stack files\b1'
r2_fname = 'K:\radiation\stack files\r2' 
s = 1440
l = 501
b = 108
image = LONARR(s, l)
int = FLTARR(s, l, b)
ope = int

OPENR, lun, int_fname, /GET_LUN
ass = ASSOC(lun, image)
FOR i = 0, b-1 DO int[*,*,i] = ass[i]
FREE_LUN, lun

OPENR, lun, ope_fname, /GET_LUN
ass = ASSOC(lun, image)
FOR i = 0, b-1 DO ope[*,*,i] = ass[i]
FREE_LUN, lun

;compute bias
bias = FLTARR(s, l, b)
relbias = bias
avg_bias = FLTARR(s, l)
FOR i = 0, b-1 DO BEGIN
  bias[*,*,i] = ope[*,*,i]-int[*,*,i]
  relbias[*,*,i] = bias[*,*,i] / ope[*,*,i] *100.0
ENDFOR

avg_bias = TOTAL(bias, 3)/FLOAT(b)

avgope = TOTAL(ope, 3)/FLOAT(b)

relAvgBias = TOTAL(relbias, 3)/FLOAT(b)
ind = WHERE(avgope EQ 0.0)
relAvgBias[ind] = !VALUES.F_NAN


OPENW, lun, bias_fname, /GET_LUN, APPEND
FOR i = 0, b-1 DO WRITEU, lun,  bias[*,*,i]
FREE_LUN, lun

OPENW, lun, avgbias_fname, /GET_LUN
WRITEU, lun,  avg_bias
FREE_LUN, lun

OPENW, lun, relAvgBias_fname, /GET_LUN
WRITEU, lun,  relAvgBias
FREE_LUN, lun


;compute reg parameters
b0 = FLTARR(s, l)
b1 = FLTARR(s, l)
r2 = FLTARR(s, l)
FOR ss=0, s-1 DO BEGIN
  FOR ll=0, l-1 DO BEGIN
    x = REFORM(ope[ss,ll,*])
    y = REFORM(int[ss,ll,*])
    ind = WHERE((x NE 0.0) AND (y NE 0.0), count)
    IF count GT 4 THEN BEGIN
      coeff = REGRESS(x, y, CORRELATION=corr, CONST=const, /DOUBLE) 
    ENDIF ElSE BEGIN
      coeff = [!VALUES.F_NAN]
      const = !VALUES.F_NAN 
      corr = !VALUES.F_NAN
    ENDELSE 
    b1[ss,ll] = coeff[0]
    b0[ss,ll] = const
    r2[ss,ll] = corr^2
  ENDFOR
ENDFOR

OPENW, lun, b0_fname, /GET_LUN
WRITEU, lun,  b0
FREE_LUN, lun

OPENW, lun, b1_fname, /GET_LUN
WRITEU, lun,  b1
FREE_LUN, lun

OPENW, lun, r2_fname, /GET_LUN
WRITEU, lun,  r2
FREE_LUN, lun


PRINT, 'finito'
END

