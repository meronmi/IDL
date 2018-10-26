PRO RUN_BBB_regXYMat
dirY = '\\ies\d5\asap\TEST_PREDICTORS\DATA_Y\cNDVI'
varNameY = 'zcNDVI_200701-201714'
out_dir = '\\ies\d5\asap\TEST_PREDICTORS\YX_correlation'
dirX = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\Indicator_at_progress'  
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PUT THEM ALL
varNameX = ['chirpsSPI1', 'chirpsSPI3'];['zBECsm'];['SPI1', 'SPI3'];, 'zSWI015'];, 'zSWI040', 'zSWI060']
;varNameX = ['zSWI015', 'zSWI040', 'zSWI060']
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
progNameX = ['_atProg0','_atProg25','_atProg50','_atProg75','_atProg100'] + '_bil'
fnTmp = dirY + '\season' + STRTRIM(1,2) + varNameY 
ns = FIX(read_info('samples', fnTmp + '.hdr'))
nl = FIX(read_info('lines', fnTmp + '.hdr'))
nb = FIX(read_info('bands', fnTmp + '.hdr'))
mapinfo_string = read_info('map info', fnTmp + '.hdr')
coord_string = read_info('coordinate system string', fnTmp + '.hdr')
;form the names and send it to regXYmat
FOR v = 0, N_ELEMENTS(varNameX) - 1 DO BEGIN
  FOR s = 1, 2 DO BEGIN
    FOR p = 0, N_ELEMENTS(progNameX) - 1 DO BEGIN
      out_dir_tmp = out_dir + '\' + varNameX[v]
      FILE_MKDIR, out_dir_tmp
      ;fnX = dirX + '\season' + STRTRIM(s,2) +  varNameX[v] + progNameX[p] 
      ;fnY = dirY + '\season' + STRTRIM(s,2) + varNameY
      fnX = 'season' + STRTRIM(s,2) +  varNameX[v] + progNameX[p]
      fnY = 'season' + STRTRIM(s,2) + varNameY
      BBB_regXYmat, dirX, fnX, dirY, fnY, ns, nl, nb, out_dir_tmp, mapinfo_string, coord_string
    ENDFOR
  ENDFOR
ENDFOR  
END


PRO BBB_regXYmat, dirX, fnX, dirY, fnY, ns, nl, nb, out_dir, mapinfo_string, coord_string
;saturate values at 2.5 (-2.5, 2.5)
doSat = 1
;compute per pixel r^2, b and significance of two stack images over two seasons (separately and then toghether)
nLineTile = 200 
;define a fraction of samples to be store for graphical representation
storeFract = 0.05

Y0 = FLTARR(ns,nl, nb)       ;this is for the bsq
corr = FLTARR(ns,nl)*!VALUES.F_NAN
cod = FLTARR(ns,nl)*!VALUES.F_NAN ;coefficient of determination
b = FLTARR(ns,nl)*!VALUES.F_NAN ;slope
p =  FLTARR(ns,nl)*!VALUES.F_NAN

;X files are bil
OPENR, lunX, dirX + '\' + fnX, /GET_LUN
lineX = ASSOC(lunX, FLTARR(ns, nb))

;Y is bsq
OPENR, lunY, dirY + '\' + fnY, /GET_LUN
READU, lunY, Y0
FREE_LUN, lunY

;open files for output
;fnX = fnX + 'XXXX-DEBUG-XXXX'
suffixes = ['-corr_with-zcNDVI', '-cod_with-zcNDVI', '-gain_with-zcNDVI', '-sig_with-zcNDVI']
OPENW, luncor,out_dir + '\' + fnX + suffixes[0], /GET_LUN
OPENW, luncod, out_dir + '\' + fnX + suffixes[1], /GET_LUN
OPENW, lunb, out_dir + '\' + fnX + suffixes[2], /GET_LUN
OPENW, lunsig, out_dir + '\' + fnX + suffixes[3], /GET_LUN


;variables to store all values and plot correlation
xarr = [!NULL]
yarr = [!NULL]

;prepare the lookup table for the Dom approach
;critical t value fo P = 0.05, two-tailed for differnt digrees of freedom
; df = n - 2 bebcause the linear fir has two parameters (a nd b)
; n can range from 4 (no reg is computed if ther are less than 4 data points) to nb at maximum
t_critical = FLTARR(2, nb-4+1)  ;first column is number of data, secons is critical val
FOR i = 4, nb DO BEGIN
  t_critical[1,i-4] = i 
  t_critical[1,i-4] = T_CVF((0.05/2.0), i-2) ;P is divided by two to get the two-tailed value
ENDFOR



;linesNotProcesses =  nl-1 MOD nLineTile
;FOR l=0, nl-1, nLineTile DO BEGIN
eofReached = 0
TileLineCounter = LONARR(nLineTile)     ;tempory for debug, used to store the actual image line
FOR l=0, nl-1 DO BEGIN
  x = FLTARR(ns,nLineTile, nb) ;this is for the bil
  y = x
  ;this is to process (and thius read) lines in groups of nLineTile
  ;making sure that the last bit that may not be of nLineTile is corretctly processed
  FOR ll = l, l + nLineTile -1 DO BEGIN
     ;debug
     ;store the actual ine
;     TileLineCounter[ll-l] =  ll
;     IF (ll EQ 3471) THEN BEGIN
;      PRINT, 'debug'
;     ENDIF
     IF ((ll MOD 5000) EQ 0) THEN BEGIN
      PRINT, fnX + ', Reading Line: ' + STRTRIM(ll)
     ENDIF
     IF (ll LE nl-1) THEN BEGIN
      x[*,ll-l,*] = lineX[ll] 
      y[*,ll-l,*] = Y0[*,ll,*] 
      lastLine = ll
      nActualLines = ll - l +1 
    ENDIF ELSE BEGIN
      eofReached = 1   
    ENDELSE
  ENDFOR
  IF (eofReached EQ 1) THEN BEGIN
    ;truncate x and y to the last line
    x = x[*,0 : nActualLines-1,*]
    y = y[*,0 : nActualLines-1,*]
  ENDIF
  l = lastLine
  
  ;Matrix version as suggested by Dom, now on 5 lines, faster on bigger tiles
  ; Y = a + b X
  ;0. first keep only data points that are present in both x and y
  indBothFin = WHERE(FINITE(x) AND FINITE(y), countBothFinite, COMPLEMENT=indOneNotFin, NCOMPLEMENT=countOneNotFin)
  IF (countBothFinite GT 0) THEN BEGIN
    IF (countOneNotFin GT 0) THEN BEGIN
      x[indOneNotFin] = !VALUES.F_NAN
      y[indOneNotFin] = !VALUES.F_NAN
    ENDIF
    ;0; make the comparable (X variables are typically truncated at -2.5 to  2.5
    IF (doSat EQ 1) THEN BEGIN
      indSat = WHERE(x GT 2.5, countSat) & IF (countSat GT 0) THEN x[indSat] = 2.5
      indSat = WHERE(x LT -2.5, countSat) & IF (countSat GT 0) THEN x[indSat] = -2.5
      indSat = WHERE(y GT 2.5, countSat) & IF (countSat GT 0) THEN y[indSat] = 2.5
      indSat = WHERE(y LT -2.5, countSat) & IF (countSat GT 0) THEN y[indSat] = -2
    ENDIF
    ;1.compute matrix mean over the z dimension
    !except=0 ;avoid issuing an error when computing the mean on all NaN (try:  mean([!VALUES.F_NAN,!VALUES.F_NAN,!VALUES.F_NAN],/NaN)
    xm = MEAN(x, DIMENSION=3,/NAN)
    ym = MEAN(y, DIMENSION=3,/NAN)
    ;make sure that at least 4 valid couples of obs are present, if not set to NaN
    FiniteXY = FINITE(x) AND FINITE(y) 
    nFin = TOTAL(FiniteXY, 3, /NAN)
    indLT4 = WHERE(nFin LT 4, countLT4, COMPLEMENT=indGE4)
    IF (countLT4 GT 0) THEN BEGIN
      xm[indLT4] = !VALUES.F_NAN
      ym[indLT4] = !VALUES.F_NAN
    ENDIF
    clear = CHECK_MATH() ;set back to normal
    !except=1
      
    ;2. center the variables
    xc = x - REBIN(xm,[ns, nActualLines, nb])
    yc = y - REBIN(ym,[ns, nActualLines, nb])
    
    ;3. compute numerator and denominator for estimating b   
    numerator = TOTAL((xc * yc), 3, /NAN)
    ind0 = WHERE(numerator EQ 0.0, count0)
    IF (count0 GT 0) THEN numerator[ind0] = !VALUES.F_NAN
    denominatorXC2 = TOTAL((xc^2), 3, /NAN)
    ind0 = WHERE(denominatorXC2 EQ 0.0, count0)
    IF (count0 GT 0) THEN denominatorXC2[ind0] = !VALUES.F_NAN
    b = numerator / denominatorXC2
    
    ;4. compute a fron b amd means
    a = ym - b * xm
    
    ;5. compute r and R2 (cod)
    numerator = TOTAL(xc^2, 3, /NAN)
    ind0 = WHERE(numerator EQ 0.0, count0)
    IF (count0 GT 0) THEN numerator[ind0] = !VALUES.F_NAN
    denominator = TOTAL(yc^2, 3, /NAN)
    ind0 = WHERE(denominator EQ 0.0, count0)
    IF (count0 GT 0) THEN denominator[ind0] = !VALUES.F_NAN
    r = b * SQRT(numerator/denominator)
     
    ;6. compute significance
    ;6a variance of b
    ;be care in computing the totall (y - y_fit)^2. A zero is ambiguous because it may be a sum of all nan, but also a perefct model
    y_fit = REBIN(a,[ns,nActualLines,nb]) + REBIN(b,[ns,nActualLines,nb]) * x
    !except=0 ;avoid issuing an error when computing the mean on all NaN (try:  mean([!VALUES.F_NAN,!VALUES.F_NAN,!VALUES.F_NAN],/NaN)
    numerator = TOTAL((y-y_fit)^2, 3, /NAN)/ (nFin-2)
    clear = CHECK_MATH() ;set back to normal
    !except=1
    varb = numerator/denominatorXC2
    indNaN = WHERE(~FINITE(b), countNaN)
    IF (countNaN GT 0) THEN varb[indNaN] = !VALUES.F_NAN
    ;6b compute the ratio b to SQRT(varb)
    ratio = b / SQRT(varb)
    ;6c compare to appropriate student, using the lookup table and number of data points nFin
    t = FLTARR(ns,nLineTile)
    sig = t *!VALUES.F_NAN
    FOR i = 4, nb DO BEGIN
      ind = WHERE(nFin EQ i)
      t[ind] = t_critical[1,i-4]
    ENDFOR
    indFin = WHERE(FINITE(ratio))
    ind = WHERE(ratio[indFin] GT t[indFin])
    sig[indFin[ind]] = 1
    ind = WHERE(ratio[indFin] LE t[indFin])
    sig[indFin[ind]] = 0
    
    ;7. store all x and y values used
    ; indGE4 is the index to the 2d array elemnts having at least 4 obs
    ; I want to store a fraction of this (storeFract)
    ;regularly sample them
    
    ;manage teh case where there are no obs at all (in this case there is nothing to store
    IF (indGE4[0] NE -1) THEN BEGIN
      ind = indGE4[0:-1:LONG(1/storeFract)]
      ;extract the z profile of those
      dims = SIZE(b, /Dimensions)
      colrow = Array_Indices(dims, ind, /Dimensions)
      dims = Size(x, /DIMENSIONS )
      x2d = Reform(x, dims[0]*dims[1], dims[2])
      y2d = Reform(y, dims[0]*dims[1], dims[2])
      ind2d = colrow[0,*] + (colrow[1,*] * dims[0])
      xarr = [xarr, REFORM(x2d[ind2d,*], N_ELEMENTS(ind2d)*dims[2])]
      yarr = [yarr, REFORM(y2d[ind2d,*], N_ELEMENTS(ind2d)*dims[2])]
    ENDIF
    ;write files
    WRITEU, lunb, b
    WRITEU, luncor, r
    WRITEU, luncod, r^2
    WRITEU, lunsig, sig
  ENDIF ELSE BEGIN ;ther is no data at all
    WRITEU, lunb, x*!VALUES.F_NAN
    WRITEU, luncor, x*!VALUES.F_NAN
    WRITEU, luncod, x*!VALUES.F_NAN
    WRITEU, lunsig, x*!VALUES.F_NAN
  ENDELSE
  

  ;100. DEBUG check on s= 1026, l = 0:

;  PRINT, b[1026,0] ;b is ok
;  PRINT, a[1026,0] ;a is ok
;  PRINT, r[1026,0] ;r is ok
;  PRINT, r[1026,0]^2  ;r2 follows
;  PRINT, sig[1026,0]
;  xx = x[1026,0,*]
;  yy = y[1026,0,*]
;  
;  indFin = WHERE(FINITE(xx) AND FINITE(yy))
;  res = linregstat(xx[indFin], yy[indFin]) ;RETURN, [offset, gain, corr, P-value for gain]
;  PRINT, res
;  ;check a significant one
;  ;sig[1755,0] is 1
;  xx = x[1755,0,*]
;  yy = y[1755,0,*]
;  indFin = WHERE(FINITE(xx) AND FINITE(yy))
;  res = linregstat(xx[indFin], yy[indFin])
;  PRINT, res
;  print, ''
  ;now I have to write the results of thsi tile
ENDFOR
FREE_LUN, lunX

FREE_LUN, luncor
FREE_LUN, luncod
FREE_LUN, lunb
FREE_LUN, lunsig

;write the hdrs
FOR i = 0, N_ELEMENTS(suffixes)-1 DO $
  res = write_envi_hdr(out_dir + '\' + fnX + suffixes[i] + '.hdr', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq', MAPINFO=mapinfo_string, COORDINFO=coord_string)
xarr = xarr[WHERE(FINITE(xarr))]
yarr = yarr[WHERE(FINITE(yarr))]

;DensityAndFit_log_scale, xarr, yarr, fnX, 'czNDVI', out_dir, [-2.5,2.5], [-2.5,2.5], 20, 30
DensityAndFit_log_scale, xarr, yarr,fnX, 'czNDVI',  out_dir, [-2.5,2.5], [-2.5,2.5], 20, 30, $
  TITLE = '', DOFIT=1, NOWIN = 0, RGBTAB = 72, SIMPLE = 0, DOLOG = 1
PRINT, 'BBB_regXY finished'


END