Function time_stats, fnameY, inPath, outPath, outBaseName, validRange,  YSCALING = yscaling
;  Purpose:
;     Given an image multiband Y, save 
;     - the mean and sd

;  Restrictions:
;     - files should be bil


;  Input parameters: None.
;     fnameX: input bil file for X 
;     inPath: path for input files
;     outPath: outputh path
;     outBaseName: output basename (e.g., 'SosDltVsAcc')
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

fnameAVG = outPath+dirsep+outBaseName+'_avg'
fnameSD = outPath+dirsep+outBaseName+'_sd'

OPENW, lunAVG, fnameAVG, /GET_LUN
OPENW, lunSD, fnameSD, /GET_LUN


; Loop on the image lines 
FOR line = 0, nl-1, 1L DO BEGIN
  IF ((line MOD (nl/5)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
  yline=FLOAT(assoY[line])
  avgline = FLTARR(ns) * !VALUES.F_NAN
  sdline = FLTARR(ns) * !VALUES.F_NAN
  indNaN = WHERE(((yline LT validRange[0]) OR (yline GT validRange[1])), countNaN,  NCOMPLEMENT = countFIN)
  IF (countNaN GT 0) THEN yline[indNaN] = !VALUES.F_NAN
  avgline = MEAN(yline, DIMENSION = 2, /NAN)
  sdline =  STDDEV(yline, DIMENSION = 2, /NAN)
  WRITEU, lunAVG, avgline
  WRITEU, lunSD, sdline
ENDFOR
FREE_LUN, lunAVG
FREE_LUN, lunSD
;write hdrs

tmp =[fnameAVG, fnameSD] + '.hdr'
mapinfo = read_info('map info',  remove_ext_from_fn(fnameY) + '.hdr')
FOR i = 0, N_ELEMENTS(tmp)-1 DO res = write_envi_hdr(tmp[i], ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
RETURN, 0  ;normal
End

