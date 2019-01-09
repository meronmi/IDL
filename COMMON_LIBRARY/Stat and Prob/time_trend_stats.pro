Function time_trend_stats, fnameY, inPath, outPath, outBaseName, validRange,  YSCALING = yscaling
;  Purpose:
;     Given an image multiband Y, save 
;     - the correlation (r) with time (years)
;     - the stat significance of Y = aX + b (p)
;     - the Mann-Kendal correlation (tau) and test of significance (see for example https://ftian2016.wordpress.com/2016/11/18/theil-sen-slope-and-mann-kendall-significance-test-in-idl/)
;     - the Theil-Sen slope (https://en.wikipedia.org/wiki/Theil%E2%80%93Sen_estimator)
;     
;  Restrictions:
;     - files should be bil
;  Usage:
;     print, trendY (fnameY, inPath, ns, nl, nb, outPath, outBaseName, val999)
;     print, trendY ('A1sos-1997_acc1', 'Q:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis', 7841, 1458, 16, 'Q:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis\Trends', 'acc1trend999is0', 0)

;  Input parameters: None.
;     fnameY: input bil file for Y 
;     inPath: path for input files
;     outPath: outputh path
;     outBaseName: output basename (e.g., 'SosDltVsTime')
;     validRange:  [x1, y1] valid range (above or below is set to nan, e.g. [0,250]
;     YSCALING, [offset, gain] to be applied to y

;  Return values:
;     0: normal 
;     10: one of the file could not be opened
;     20: an error was encountered

;  History:
;     Version 1.0: created by MM

dirsep = '\'

IF (N_ELEMENTS(validRange) NE 2) THEN STOP
IF (validRange[1] LE validRange[0]) THEN STOP

; check and open the bil files
fnameY = inPath + dirsep + fnameY
IF (STRTRIM(read_info('interleave', remove_ext_from_fn(fnameY) + '.hdr'),2) NE 'bil') THEN STOP
  
IF FILE_TEST(fnameY) EQ 0 THEN RETURN, 10

dt = FIX(read_info('data type',  remove_ext_from_fn(fnameY) + '.hdr'))
ns = LONG(read_info('samples',  remove_ext_from_fn(fnameY) + '.hdr')) 
nl = LONG(read_info('lines',  remove_ext_from_fn(fnameY) + '.hdr')) 
nb = FIX(read_info('bands',  remove_ext_from_fn(fnameY) + '.hdr'))

IF (nb LE 4) THEN STOP


OPENR, lunY, fnameY, /GET_LUN
assoY = ASSOC(lunY, MAKE_ARRAY(ns,nb, TYPE=dt))
; open outputs
FILE_MKDIR, outPath

fnameGain = outPath+dirsep+outBaseName+'_gain'
fnameOffset = outPath+dirsep+outBaseName+'_offset'
fnamePGain = outPath+dirsep+outBaseName+'_Pval_gain'
fnameCorr = outPath+dirsep+outBaseName+'_corr'
fnameTSslope = outPath+dirsep+outBaseName+'_TSslope'
fnameMKtau = outPath+dirsep+outBaseName+'MKtau'
fnameMKp = outPath+dirsep+outBaseName+'MKp'
OPENW, lunGain, fnameGain, /GET_LUN
OPENW, lunOffset, fnameOffset, /GET_LUN
OPENW, lunPGain, fnamePGain, /GET_LUN
OPENW, lunCorr, fnameCorr, /GET_LUN
OPENW, lunTSslope, fnameTSslope, /GET_LUN
OPENW, lunMKtau, fnameMKtau, /GET_LUN
OPENW, lunMKp, fnameMKp, /GET_LUN

; Loop on the image lines 
FOR line = 0, nl-1, 1L DO BEGIN
  IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
  yline=float(assoY[line])
  gainline = FLTARR(ns) * !VALUES.F_NAN
  offline = FLTARR(ns) * !VALUES.F_NAN
  pgainline = FLTARR(ns) * !VALUES.F_NAN
  corrline = FLTARR(ns) * !VALUES.F_NAN
  
  TSsline = FLTARR(ns) * !VALUES.F_NAN
  MKtline = FLTARR(ns) * !VALUES.F_NAN
  MKpline = FLTARR(ns) * !VALUES.F_NAN
  
  ;loop on samples
  FOR sample = 0, ns-1, 1L DO BEGIN
    x0 = FINDGEN(N_ELEMENTS(REFORM(yline[sample,*])))
    y0 = REFORM(yline[sample,*])
    indNaN = WHERE(((y0 LT validRange[0]) OR (y0 GT validRange[1])), countNaN,  NCOMPLEMENT = countFIN)
    IF (countFIN GE 3) THEN BEGIN
      IF (countNaN GT 0) THEN y0[indNaN] = !VALUES.F_NAN
      IF (N_ELEMENTS(yscaling) GT 0) THEN y0 = yscaling[0] + yscaling[1] * y0 
      ;linear regresstion stats
      ret = linregstat(x0, y0)
      gainline[sample] = ret[1]
      offline[sample] = ret[0]
      pgainline[sample] = ret[3]
      corrline[sample] = ret[2]
      ;Theil_sen slope
      indFin = WHERE((FINITE(x0) AND FINITE(y0)), countFin)
      ret = theil_sen(x0[indFin],y0[indFin])
      TSsline[sample] = ret[1]
      ;Mann-Kendall tau
      ret = R_CORRELATE(x0[indFin], y0[indFin], /KENDALL)
      MKtline[sample] = ret[0]
      MKpline[sample] = ret[1]
    ENDIF 
  ENDFOR 
  WRITEU, lunGain, gainline
  WRITEU, lunOffset, offline
  WRITEU, lunPGain, pgainline
  WRITEU, lunCorr, corrline
  WRITEU, lunTSslope, TSsline
  WRITEU, lunMKtau, MKtline
  WRITEU, lunMKp, MKpline
ENDFOR
FREE_LUN, lunY
FREE_LUN, lunGain
FREE_LUN, lunOffset
FREE_LUN, lunPGain
FREE_LUN, lunCorr
FREE_LUN, lunTSslope
FREE_LUN, lunMKtau
FREE_LUN, lunMKp
;write hdrs

tmp =[fnameGain, fnameOffset, fnamePGain, fnameCorr, fnameTSslope, fnameMKtau, fnameMKp] + '.hdr'
mapinfo = read_info('map info',  remove_ext_from_fn(fnameY) + '.hdr')
FOR i = 0, N_ELEMENTS(tmp)-1 DO res = write_envi_hdr(tmp[i], ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
RETURN, 0  ;normal
End

