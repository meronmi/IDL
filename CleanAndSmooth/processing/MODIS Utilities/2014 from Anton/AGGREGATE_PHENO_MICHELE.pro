; _____________________________________________________________________________________________
;
; NAME:
;   AGGREGATE_PHENO_MICHELE
;
; PURPOSE:
;   Calculate the aggregated phenological parameters from Michele's SPOT-VGT analysis
;
; INPUTS:
;   - 1 file to reflect if no clear seasonality (0), single season (1), or double season (2)
;   - 12 files to reflect per-pixel avgSOS, stdSOS, avgEOS, stdEOS (4 files in case single season, 8 files if double season)
;   - Image with ID codes of admin polygons (converted to raster) in same geometry as the other images
;
; CONDITIONS USED (otherwise result = mask value):
;   - at least x% of the pixels in an admin-unit should have a seasonality value of 1 or 2 to get results
; 
; OUTPUT:
;   - A CSV-file with:
;      ibliID, %0, %1, %2, avgSOS_2L, avgEOS_2L, avgSOS_2S, avgEOS_2S, sdS_SOS_2L, sdS_EOS_2L, sdS_SOS_2S, sdS_EOS_2S, sdT_SOS_2L, sdT_EOS_2L, sdT_SOS_2S, sdT_EOS_2S, avgSOS_1, avgEOS_1, sdS_SOS_1, sdS_EOS_1, sdT_SOS_1, sdT_EOS_1
;    (ref)  -,  0,  1,  2,         3,         4,         5,         6,          7,         8,           9,         10,         11,         12,         13,         14,       15,       16,        17,        18,        19,        20
;    ;      where:
;      %0, %1, %2 = the percentage of pixels within a division that have no seasonality (0), single season (1), or double season (2)
;      SOS = start of season
;      EOS = end of season
;      avg = the spatial average of the temporal SOS/EOS average as given in Michele's files
;      sdS = the (spatial=S) standard deviation of the temporal SOS/EOS average as given in Michele's files
;      sdT = the spatial average of the temporal SOS/EOS averages as given in Michele's files
;      2L  = the long season (approximately March-May) for areas with two seasons  (indicated as 2gspy_1s in Michele's files) 
;      2S  = the short season (approximately October-December) for areas with two seasons (indicated as 2gspy_2s in Michele's files) 
;      1   = the single season for areas with one season
;      *Note the (ref) numbers below is just for internal reference of #parameters
;      
; CALLING SEQUENCE:
;   AGGREGATE_PHENO_MICHELE
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, November, 2014
;
; _____________________________________________________________________________________________


PRO AGGREGATE_PHENO_MICHELE

STARTTIME = SYSTIME(1)

; General settings (to be changed for different input)
dataPath='G:\BACKUP\IMAGES\IBLI\pheno\Michele\ngspy_lr6_iter_v1\'      ; location where phenology files are located
adminPath='G:\BACKUP\IMAGES\IBLI\pheno\Michele\ancillary\'             ; location where administrative region-file is located

adminFile = adminPath+'ibliID_K+E.img'                           ; input File for division codes
nl=4124                            ; number of lines
ns=4538                            ; number of samples
nParameters=21                     ; number of parameters in output statistics
minPercPixels=3.                   ; minimum % of pixels that should be covered to get results 
                                   ; (note: for now 3 was chosen, so that Turkana Central still gets results, next lowest is Katilu = 15.7%)

; general activities & writing of a StringArray to hold season identifiers
workPath=dataPath+'aggregate\'                                   ; location where output will be written to
IF FILE_TEST(workPath) eq 0 THEN FILE_MKDIR, workPath

; Open the Admin1-file
OPENR, R1, adminFile, /GET_LUN
adminRef = ASSOC(R1, BYTARR(ns,nl)) ; note: if later we use INTARR for adminFile, then change!
adminRaster = adminRef[0]                                 ; the raster file holding the admin1 codes
free_lun, R1
adminList = adminRaster[UNIQ(adminRaster, SORT(adminRaster))]   ; array with unique admin codes (first=0) in file
noAdminCode =255                                                ; this is the value which does not correspond to an admin unit
index=WHERE(adminList NE noAdminCode)                           ; NOTE: perhaps this value needs changing??
IF index[0] ne -1 THEN adminList=adminList[index]               ; remove this value from the adminList

; Make the fileList of phenological files to access
fileList = dataPath + ['ngspy_lr6_iter_v1.img', $                          ; for parameter 0, 1, 2
                       '2gspy_1s_15A1sos-1997_sos1_DOC_TZPavg.img', $      ; for parameter 3, 7
                       '2gspy_1s_15A1sos-1997_eos1_DOC_TZPavg.img', $      ; for parameter 4, 8 
                       '2gspy_2s_15A1sos-1997_sos2_DOC_TZPavg.img', $      ; for parameter 5, 9   
                       '2gspy_2s_15A1sos-1997_eos2_DOC_TZPavg.img', $      ; for parameter 6, 10
                       '1gspy_1s_15A1sos-1997_sos1_DOC_TZPavg.img', $      ; for parameter 15, 17
                       '1gspy_1s_15A1sos-1997_eos1_DOC_TZPavg.img', $      ; for parameter 16, 18
                       '2gspy_1s_15A1sos-1997_sos1_DOC_TZPsd.img', $       ; for parameter 11
                       '2gspy_1s_15A1sos-1997_eos1_DOC_TZPsd.img', $       ; for parameter 12
                       '2gspy_2s_15A1sos-1997_sos2_DOC_TZPsd.img', $       ; for parameter 13
                       '2gspy_2s_15A1sos-1997_eos2_DOC_TZPsd.img', $       ; for parameter 14
                       '1gspy_1s_15A1sos-1997_sos1_DOC_TZPsd.img', $       ; for parameter 19
                       '1gspy_1s_15A1sos-1997_eos1_DOC_TZPsd.img']         ; for parameter 20

; Create an outputArray to hold all statistics per admin (ibli) unit and set values standard to -99.
outArray = FLTARR(nParameters,N_ELEMENTS(adminlist))-99.

; Open the number of seasons file and aggregate output
nSeasonsFile = fileList[0]
OPENR, R1, nSeasonsFile, /GET_LUN               ; Open the stack-file for reading
nSeasonsRef = ASSOC(R1, INTARR(ns,nl))         ; integer is used...
nSeasonsRaster = BYTE(nSeasonsRef[0])           ; the raster file holding per pixel the seasonality information
free_lun, R1
FOR admin=0,N_ELEMENTS(adminlist)-1, 1L DO BEGIN
  adminCode = adminList[admin]                          ; assign adminCode-region to process
  indexTot=WHERE(adminRaster eq adminCode, nTotPixels)                         ; total number of pixels in admin Unit
  index0 = WHERE(adminRaster eq adminCode AND nSeasonsRaster eq 0, number0)    ; total number of pixels in admin Unit with seasonality 0
  index1 = WHERE(adminRaster eq adminCode AND nSeasonsRaster eq 1, number1)    ; total number of pixels in admin Unit with seasonality 1
  index2 = WHERE(adminRaster eq adminCode AND nSeasonsRaster eq 2, number2)    ; total number of pixels in admin Unit with seasonality 2
  outArray[0,admin]=100.*FLOAT(number0)/nTotPixels
  outArray[1,admin]=100.*FLOAT(number1)/nTotPixels
  outArray[2,admin]=100.*FLOAT(number2)/nTotPixels
ENDFOR
nSeasonsRaster = 0  & indexTot =0 & index0 = 0 & index1=0 & index2=0  ; free memory

; now loop over the average phenology files
FOR file=1,6,1L DO BEGIN
  phenFile = fileList[file]
  OPENR, R1, phenFile, /GET_LUN               ; Open the stack-file for reading
  phenRef = ASSOC(R1, FLTARR(ns,nl))          ; integer is used...
  phenRaster = phenRef[0]                     ; the raster file holding per pixel the seasonality information
  free_lun, R1
  FOR admin=0,N_ELEMENTS(adminlist)-1, 1L DO BEGIN        ; loop over admin-units
    adminCode = adminList[admin]                          ; assign adminCode-region to process
    index = WHERE(adminRaster eq adminCode AND FINITE(phenRaster))       ; check for pixels falling in Admin that have no NaN values
    IF index[0] ne -1 THEN BEGIN
      ; NOW WE HAVE TO CALCULATE THE MEAN/STDEV BUT ACCOUNT FOR VALUES AROUND DEC/JAN!
      phenValuesAdmin = phenRaster[index]
      maxDate = MAX(phenValuesAdmin,MIN=minDate)     ; first get min and max values
      IF maxDate-minDate GT 250 THEN BEGIN           ; if larger than 200 it means that we have a mix of values in Dec (or before) and Jan (or after)
        index2 = WHERE(phenValuesAdmin GT 250)       ; select the large dates 
        phenValuesAdmin[index2]=phenValuesAdmin[index2]-365.     ; subtract a full year (we temporally get negative values)
      ENDIF
      ; we can now simply calculate the average and stdev and add 365 in case the average is below 0
      avgPhen = MEAN(phenValuesAdmin)
      sdS_Phen= STDDEV(phenValuesAdmin)
      IF avgPhen LE 0.0 THEN avgPhen = avgPhen + 365.
      
      IF outArray[2,admin] ge minPercPixels THEN BEGIN
        CASE file OF
          1: BEGIN
               outArray[3,admin] = avgPhen
               outArray[7,admin] = sdS_Phen
             END
          2: BEGIN
               outArray[4,admin] = avgPhen
               outArray[8,admin] = sdS_Phen
             END
          3: BEGIN
               outArray[5,admin] = avgPhen
               outArray[9,admin] = sdS_Phen
             END
          4: BEGIN
               outArray[6,admin] = avgPhen
               outArray[10,admin] = sdS_Phen
             END 
          ELSE: ; do nothing         
        ENDCASE
      ENDIF
      IF outArray[1,admin] ge minPercPixels THEN BEGIN
        CASE file OF
          5: BEGIN
             outArray[15,admin] = avgPhen
             outArray[17,admin] = sdS_Phen
             END
          6: BEGIN
             outArray[16,admin] = avgPhen
             outArray[18,admin] = sdS_Phen
             END
          ELSE: ; do nothing
        ENDCASE
      ENDIF
    ENDIF
  ENDFOR  
ENDFOR

; now loop over the standard deviation phenology files
FOR file=7,12,1L DO BEGIN
  phenFile = fileList[file]
  OPENR, R1, phenFile, /GET_LUN               ; Open the stack-file for reading
  phenRef = ASSOC(R1, FLTARR(ns,nl))          ; integer is used...
  phenRaster = phenRef[0]                     ; the raster file holding per pixel the seasonality information
  free_lun, R1
  FOR admin=0,N_ELEMENTS(adminlist)-1, 1L DO BEGIN        ; loop over admin-units
    adminCode = adminList[admin]                          ; assign adminCode-region to process
    index = WHERE(adminRaster eq adminCode AND FINITE(phenRaster))       ; check for pixels falling in Admin that have no NaN values
    IF index[0] ne -1 THEN BEGIN
      ; NOW WE CALCULATE THE MEAN of the temporal standard deviations
      sdT_Phen = MEAN(phenRaster[index])
      IF outArray[2,admin] ge minPercPixels THEN BEGIN
        CASE file OF
           7: outArray[11,admin] = sdT_Phen
           8: outArray[12,admin] = sdT_Phen
           9: outArray[13,admin] = sdT_Phen
          10: outArray[14,admin] = sdT_Phen
          ELSE: ; do nothing
        ENDCASE
      ENDIF
      IF outArray[1,admin] ge minPercPixels THEN BEGIN
        CASE file OF
          11: outArray[19,admin] = sdT_Phen
          12: outArray[20,admin] = sdT_Phen 
          ELSE: ; do nothing
        ENDCASE
      ENDIF
    ENDIF
  ENDFOR
ENDFOR


; Write away the output to a CSV-file
outFile = workPath+'ibliID_phenoSummary_SPOT_Michele'+ '.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
PRINTF,W1, 'ibliID, %0, %1, %2, avgSOS_2L, avgEOS_2L, avgSOS_2S, avgEOS_2S, sdS_SOS_2L, sdS_EOS_2L, sdS_SOS_2S, sdS_EOS_2S, sdT_SOS_2L, sdT_EOS_2L, sdT_SOS_2S, sdT_EOS_2S, avgSOS_1, avgEOS_1, sdS_SOS_1, sdS_EOS_1, sdT_SOS_1, sdT_EOS_1'
FOR i=0, N_ELEMENTS(adminList)-1, 1L DO BEGIN
  writeline = STRCOMPRESS(FIX(adminList[i]),/REMOVE_ALL)+','+STRJOIN(STRCOMPRESS(outArray[*,i],/REMOVE_ALL),',')
  printf, W1, writeline
ENDFOR
CLOSE, W1

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED AGGREGATING'

END ;Procedure AGGREGATE_CUM_SPOT