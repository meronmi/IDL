Function makeCV, fnameX, fnameY, inPath, ns, nl, outBaseName
;  Purpose:
;     Given two images X (avg)and Y(sd) it returns the Coefficient of Variation


; fnameX='1gspy_1s_A1sos-1997_acc1_TZPavg' & fnameY ='1gspy_1s_A1sos-1997_acc1_TZPsd' & pathin='X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\acc'
; print, makeCV (fnameX, fnameY, pathin, 7841, 1458, '1gspy_1s_A1sos-1997_acc1_TZP')

;  Input parameters: None.
;     fnameX: input  file for AVG 
;     fnameY: input  file for SD
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
fnameY = inPath + '\' + fnameY

avg = FLTARR(ns,nl)
sd = FLTARR(ns,nl)

IF FILE_TEST(fnameX) EQ 0 THEN RETURN, 10
OPENR, lunX, fnameX, /GET_LUN
READU, lunX, avg
FREE_LUN, lunX  

IF FILE_TEST(fnameY) EQ 0 THEN RETURN, 10
OPENR, lunY, fnameY, /GET_LUN
READU, lunY, sd
FREE_LUN, lunY
; open outputs

fnameCV = inPath+'\'+outBaseName+'_CV'
OPENW, lun, fnameCV, /GET_LUN
WRITEU, lun, sd/avg*100.0
FREE_LUN, lun
;write hdrs
tmp = fnameCV + '.hdr'

OPENW, lun, tmp, /GET_LUN
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


RETURN, 0  ;normal
End

