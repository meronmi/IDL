Function detect_equals, fnameX, inPath, ns, nl, nb, outPath, outBaseName
  
  ; res = detect_equals('PV_1333_1413i_sc', 'X:\Active Projects\MARSOP and transition\Proba V 1 km',7841,8289,17,'X:\Active Projects\MARSOP and transition\Proba V 1 km\corr','PV_n_of_equals')
  
  
  ;  Input parameters: None.
  ;     fnameX: input bil file for X
  ;     fnameY: input bil file for Y
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
  fnameX = inPath + '\' + fnameX
  
  IF (STRTRIM(read_info('interleave', fnameX+'.hdr'),2) NE 'bil') THEN STOP
  
  
  IF FILE_TEST(fnameX) EQ 0 THEN RETURN, 10
  OPENR, lunX, fnameX, /GET_LUN
  assoX = ASSOC(lunX, FLTARR(ns,nb))

  ; open outputs
  FILE_MKDIR, outPath
  fnameout = outPath+'\'+outBaseName+'_count_repeated'
  OPENW, lun, fnameout, /GET_LUN
  
  
  ; Loop on the image
  FOR line = 0, nl-1, 1L DO BEGIN
    IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
    xline=float(assoX[line])
    diffline = FLTARR(ns, nb)+100
    eqline = FLTARR(ns, nb)*0.0
    FOR b =1, nb-1 DO BEGIN
      diffline(*, b) = xline(*, b) - xline(*, b-1)
    ENDFOR
    ind = WHERE(diffline EQ 0.0, count)
    IF (count GT 0) THEN eqline[ind]=1
    
    WRITEU, lun, TOTAL(eqline, 2)
  ENDFOR
  FREE_LUN, lun
  
  ;write hdrs
  tmp =fnameout + '.hdr'
  
  OPENW, lun, tmp, /GET_LUN
  PRINTF,lun,'ENVI'
  PRINTF,lun,'description = {repeated pixels}'
  PRINTF,lun,'samples ='+STRCOMPRESS(ns)
  PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
  PRINTF,lun,'bands   ='+STRCOMPRESS(1)
  PRINTF,lun,'header offset = 0'
  PRINTF,lun,'file type = ENVI Standard'
  PRINTF,lun,'data type = 4'
  PRINTF,lun,'interleave = bil'
  PRINTF,lun,'byte order = 0'
  FREE_LUN, lun
  
  
  RETURN, 0  ;normal
End