Function trendY, fnameY, inPath, ns, nl, nb, outPath, outBaseName, val999
;  Purpose:
;     Given an image multiband Y 
;     - the correlation (r) with time (years)
;     - the stat significance of Y = aX + b (p)
;     
;  Restrictions:
;     - files should be floating point bil (results from anomalies are bsq, change it with ENVI)
;  Usage:
;     print, trendY (fnameY, inPath, ns, nl, nb, outPath, outBaseName, val999)
;     print, trendY ('A1sos-1997_acc1', 'Q:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis', 7841, 1458, 16, 'Q:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\ANOMALIES\Correlation_analysis\Trends', 'acc1trend999is0', 0)

;  Input parameters: None.
;     fnameX: input bil file for X 
;     inPath: path for input files
;     ns
;     nl
;     nb
;     outPath: outputh path
;     outBaseName: output basename (e.g., 'SosDltVsAcc')
;     val999:      how to consider -999, can be 0 or !VALUES.FNAN


;  Return values:
;     0: normal 
;     10: one of the file could not be opened
;     20: an error was encountered

;  History:
;     Version 1.0: created by MM

; check and open the bil files
fnameY = inPath + '\' + fnameY
IF (STRTRIM(read_info('interleave', fnameY+'.hdr'),2) NE 'bil') THEN STOP
  
IF FILE_TEST(fnameY) EQ 0 THEN RETURN, 10
OPENR, lunY, fnameY, /GET_LUN
assoY = ASSOC(lunY, FLTARR(ns,nb))

; open outputs
FILE_MKDIR, outPath
fnameGain = outPath+'\'+outBaseName+'_gain'
fnameOffset = outPath+'\'+outBaseName+'_offset'
fnamePGain = outPath+'\'+outBaseName+'_Pval_gain'
fnameCorr = outPath+'\'+outBaseName+'_corr'
OPENW, lunGain, fnameGain, /GET_LUN
OPENW, lunOffset, fnameOffset, /GET_LUN
OPENW, lunPGain, fnamePGain, /GET_LUN
OPENW, lunCorr, fnameCorr, /GET_LUN

; Loop on the image 
FOR line = 0, nl-1, 1L DO BEGIN
  IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
  yline=float(assoY[line])
  gainline = FLTARR(ns) * !VALUES.F_NAN
  offline = FLTARR(ns) * !VALUES.F_NAN
  pgainline = FLTARR(ns) * !VALUES.F_NAN
  corrline = FLTARR(ns) * !VALUES.F_NAN
  
  FOR sample = 0, ns-1, 1L DO BEGIN
    x0 = FINDGEN(N_ELEMENTS(REFORM(yline[sample,*])))
    y0 = REFORM(yline[sample,*])
    ind999 = WHERE(y0 EQ -999, count999)
    IF (count999 GT 0) THEN y0[ind999] = val999
    ret = linregstat(x0, y0)
    
    gainline[sample] = ret[1]
    offline[sample] = ret[0]
    pgainline[sample] = ret[3]
    corrline[sample] = ret[2]
  ENDFOR 
  WRITEU, lunGain, gainline
  WRITEU, lunOffset, offline
  WRITEU, lunPGain, pgainline
  WRITEU, lunCorr, corrline
ENDFOR
FREE_LUN, lunGain
FREE_LUN, lunOffset
FREE_LUN, lunPGain
FREE_LUN, lunCorr
;write hdrs
tmp =[fnameGain, fnameOffset, fnamePGain, fnameCorr] + '.hdr'
FOR i = 0, 3 DO BEGIN
  OPENW, lun, tmp[i], /GET_LUN
  PRINTF,lun,'ENVI'
  PRINTF,lun,'description = {corr analysis}'
  PRINTF,lun,'samples ='+STRCOMPRESS(ns)
  PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
  PRINTF,lun,'bands   ='+STRCOMPRESS(1)
  PRINTF,lun,'header offset = 0'
  PRINTF,lun,'file type = ENVI Standard'
  PRINTF,lun,'data type = 4'
  PRINTF,lun,'interleave = bil'
  PRINTF,lun,'byte order = 0'
  FREE_LUN, lun
ENDFOR

RETURN, 0  ;normal
End

