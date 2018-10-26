; _____________________________________________________________________________________________
;
; NAME:
;   CUMULATE_PER_PIXEL
;
; PURPOSE:
;   Cumulate NDVI for fixed seasons (for different NDVI series) --> also MAXIMUM NDVI as output!!!
;
; INPUTS:
;   - filtered BIL-stack of NDVI for Kenya
;   - which NDVI series
;
; CONDITIONS USED (otherwise result = mask value):
;   - ...
;
; CALLING SEQUENCE:
;   CUMULATE_PER_PIXEL
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, April 2013
;   Modified by Michele Meroni to process MODIS C5 data, April 2013
;
; _____________________________________________________________________________________________


PRO CREATE_HEADER, outFile, nl, ns, COUNT, bandNameList, dataSet, type      ; Procedure to generate generic header for each output
;HEADER_OUT=STRMID(outFile,0,STRLEN(outFile)-3)+'hdr'
HEADER_OUT=outFile+'.hdr'
OPENW,3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {'
descriptionString = '  '+type+' NDVI derived from '+dataset+' per season for Kenya}'
printf,3,'  '+descriptionString
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(COUNT)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
dataType = 2      ; the general case
;MM+
IF (dataSet EQ 'MODIS_C5_Terra') OR (dataSet EQ 'MODIS_C5_Aqua') THEN BEGIN
  mapInfo = '{Geographic Lat/Lon, 1.0000, 1.0000, 33.50000000, 5.60000000, 2.2321428600e-003, 2.2321428600e-003, WGS-84, units=Degrees}'
  IF type eq 'maximum' THEN dataType = 2 ELSE dataType = 3      ; long
ENDIF
IF (dataSet EQ 'BOKU_C5_Terra') OR (dataSet EQ 'BOKU_C5_TerraAqua') THEN BEGIN
  mapInfo = '{Geographic Lat/Lon, 1.0000, 1.0000, 33.50000000, 5.60000000, 2.0199619772e-003, 2.0202020202e-003, WGS-84, units=Degrees}'
  IF type eq 'maximum' THEN dataType = 2 ELSE dataType = 3      ; long
ENDIF
IF (type eq 'maximum' AND (dataSet eq 'SPOT' OR dataSet eq 'eMODIS')) THEN dataType=1
printf,3,'data type ='+STRCOMPRESS(dataType)
printf,3,'interleave = bil'
printf,3,'sensor type ='+dataSet
printf,3,'byte order = 0'
IF dataSet eq 'GIMMS' THEN mapInfo = '{Geographic Lat/Lon, 1.0000, 1.5000, 33.00000000, 6.000000000, 0.08333333333333333333, 0.08333333333333333333, WGS-84, units=Degrees}'
IF dataSet eq 'SPOT' THEN mapInfo = '{Geographic Lat/Lon, 1.0000, 1.0000, 33.0000, 6.00000, 0.0089285714285714, 0.0089285714285714, WGS-84, units=Degrees}'

; NOTE!! HERE eMODIS and MODIS should be added!
printf,3,'map info = '+STRCOMPRESS(mapInfo)
printf,3,'band names = {'
printf,3,STRJOIN(bandNameList,', ')+'}'
CLOSE,3
END ; CREATE_HEADER
; _____________________________________________________________________________________________


PRO CUMULATE_PER_PIXEL

STARTTIME = SYSTIME(1)

;MM dataSet = ['GIMMS', 'SPOT', 'MODIS', 'eMODIS']
dataSet = ['GIMMS', 'SPOT', 'MODIS_C5_Terra', 'BOKU_C5_Terra', 'BOKU_C5_TerraAqua', 'eMODIS', 'MODIS_C5_Aqua']
dataSet = dataSet[6]
dataPath= 'K:\ILRI\'    ;base path

; below provide the dataset-specific settings
IF dataSet eq 'GIMMS' THEN BEGIN
  nl=132                             ; number of lines
  ns=112                             ; number of samples
  nTimePeriods = 732                 ; total #bands in file
  startYearData = 1981               ; startYear of data
  nImagesYear = 24                   ; number of images per year (bi-monthly)
  periodLag   = 13                   ; first image of the year - July (note, for IDL do -1)
  startPeriodLong = 5                ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsLong = 14             ; how many periods (15-day) are in the long season
  startPeriodShort = 19              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
  noDataValue = -10000

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'GIMMS\'
  inFile = dataPath+'Kenya_NDVI3g_filteredSG_cm1.stk'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, INTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF

IF dataSet eq 'SPOT' THEN BEGIN
  nl=1232                            ; number of lines
  ns=1008                            ; number of samples
  nTimePeriods = 516                 ; total #bands in file
  startYearData = 1998               ; startYear of data
  nImagesYear = 36                   ; number of images per year (three per month)
  periodLag   = 10                   ; first image of the year - July (note, for IDL do -1) 
  startPeriodLong = 7                ; start Period for long rains (1=first of January, 7 is first of March here)
  numberPeriodsLong = 21             ; how many periods (15-day) are in the long season
  startPeriodShort = 28              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 15            ; how many periods (15-day) are in the long season
  noDataValue = 0                    ; Note: we don't really have one, but this should be fine!

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'SPOT\'
  inFile = dataPath+'SPOT_VGT_filtered_Kenya.img'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, BYTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF

;MM+
IF dataSet eq 'MODIS_C5_Terra' THEN BEGIN
  nl = 4658                          ; number of lines
  ns = 3808                          ; number of samples
  nTimePeriods = 301                 ; total #bands in file
  startYearData = 2000               ; startYear of data
  nImagesYear = 23                   ; number of images per year (16 day composite)
  periodLag   = 4                    ; first image of the year - July (note, for IDL do -1) MM+:it's the ordinal period number of the first image (MODIS is doy 49, thr fourth period
  startPeriodLong = 5                ; start Period for long rains (1=first of January, 5 is 6 of March here)
  numberPeriodsLong = 13             ; how many periods (16-day) are in the long season
  startPeriodShort = 18              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
  noDataValue = -999

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'MODIS_C5\bil_files\'
  inFile = dataPath+'MODIS_C5_Terra_NDVIx10000_bil_cl_Sm_Ch'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, INTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF

IF dataSet eq 'MODIS_C5_Aqua' THEN BEGIN
  nl = 4658                          ; number of lines
  ns = 3808                          ; number of samples
  nTimePeriods = 242                 ; total #bands in file
  startYearData = 2002               ; startYear of data
  nImagesYear = 23                   ; number of images per year (16 day composite)
  periodLag   = 12                    ; first image of the year - July (note, for IDL do -1) MM+:it's the ordinal period number of the first image (MODIS is doy 49, thr fourth period
  startPeriodLong = 4                ; start Period for long rains (1=first of January, 5 is 6 of March here)
  numberPeriodsLong = 13             ; how many periods (16-day) are in the long season
  startPeriodShort = 17              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
  noDataValue = -999

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'MODIS_C5_aqua\bil_files\'
  inFile = dataPath+'MODIS_C5_Aqua_NDVI_bil_cl_Sm_Ch'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, INTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF

IF dataSet eq 'BOKU_C5_Terra' THEN BEGIN
  nl = 5148                          ; number of lines
  ns = 4208                          ; number of samples
  nTimePeriods = 301                 ; total #bands in file
  startYearData = 2000               ; startYear of data
  nImagesYear = 23                   ; number of images per year (16 day composite)
  periodLag   = 4                    ; first image of the year - July (note, for IDL do -1) MM+:it's the ordinal period number of the first image (MODIS is doy 49, thr fourth period
  startPeriodLong = 5                ; start Period for long rains (1=first of January, 5 is 6 of March here)
  numberPeriodsLong = 13             ; how many periods (16-day) are in the long season
  startPeriodShort = 18              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
  noDataValue = 0

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'BOKU\bil files\'
  inFile = dataPath+'BOKU_C5_NDVIx10000_T_sm_bil'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, INTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF

IF dataSet eq 'BOKU_C5_TerraAqua' THEN BEGIN
  nl = 5148                          ; number of lines
  ns = 4208                          ; number of samples
  nTimePeriods = 492                 ; total #bands in file
  startYearData = 2002               ; startYear of data
  nImagesYear = 46                   ; number of images per year (16 day composite)
  periodLag   = 23                    ; first image of the year - July (note, for IDL do -1) MM+:it's the ordinal period number of the first image (MODIS is doy 49, thr fourth period
  startPeriodLong = 9                ; start Period for long rains (1=first of January, 5 is 6 of March here)
  numberPeriodsLong = 26             ; how many periods (16-day) are in the long season
  startPeriodShort = 35              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 20            ; how many periods (15-day) are in the long season
  noDataValue = 0

  ; dataAccess to a lineAssNDVI
  dataPath=dataPath+'BOKU\bil files\'
  inFile = dataPath+'BOKU_C5_NDVIx10000_TA_sm_bil_from_2002'
  OPENR, R1, inFile, /GET_LUN               ; Open the stack-file for reading
  lineAssNDVI = ASSOC(R1, INTARR(ns,nTimePeriods))    ; Associate a line of data (with all bands)
ENDIF
;MM-







; define the precise access points (periods) in the file (in IDL-units), and create bandNameList
nYears = CEIL(FLOAT(nTimePeriods)/nImagesYear)            ; number of years to analyse
FOR year = 0, nYears, 1L DO BEGIN                         ; usually it would be till nYears-1, but just to be sure (we subset later)
  ; first create a bandNameList to be subsetted later
  sYear = STRCOMPRESS(year+startYearData,/REMOVE_ALL)
  startPeriodYear = [startPeriodLong,startPeriodShort]+year*nImagesYear-periodLag ;MM+ compute the index (from 0) to the correct bands in the file
  endPeriodYear = [startPeriodLong+numberPeriodsLong,startPeriodShort+numberPeriodsShort]-1+year*nImagesYear-periodLag
  IF year eq 0 THEN BEGIN                  ; do separately to first create the list, and later to update the list
    bandNameList=[sYear+'_long',sYear+'_short']
    startPeriods = startPeriodYear
    endPeriods = endPeriodYear
  ENDIF ELSE BEGIN
    bandNameList=[bandNameList, sYear+'_long',sYear+'_short']
    startPeriods = [startPeriods, startPeriodYear]
    endPeriods = [endPeriods,endPeriodYear]
  ENDELSE
ENDFOR
; now subset the above based on whether it fits in the data
index = WHERE(startPeriods ge 0 AND endPeriods lt nTimePeriods, COUNT)
IF index[0] ne -1 THEN BEGIN
  startPeriods = startPeriods[index]
  endPeriods   = endPeriods[index]
  bandNameList = bandNameList[index]
ENDIF

; open two files for writing, and gradually (per line) write all maxNDVI and cumNDVI values for all seasons
cumFile = dataPath+'cumNDVI_'+FILE_BASENAME(inFile)
maxFile = dataPath+'maxNDVI_'+FILE_BASENAME(inFile)
IF FILE_TEST(cumFile) eq 1 THEN FILE_DELETE, cumFile  ; Delete output file if an earlier version exists
IF FILE_TEST(maxFile) eq 1 THEN FILE_DELETE, maxFile  ; Delete output file if an earlier version exists
OPENW, W1, cumFile, /GET_LUN, /APPEND
OPENW, W2, maxFile, /GET_LUN, /APPEND

FOR j=0,nl-1,1L DO BEGIN                 ; Loop over lines
  NDVIline = lineAssNDVI[j]              ; get one line of NDVI time series
  FOR season=0,COUNT-1,1L DO BEGIN       ; loop over all seasons that are fully contained in NDVI time series
    NDVIseasonLine = NDVIline[*,startPeriods[season]:endPeriods[season]]
    ;MM+ make it long because it may reach a value bigger than 32767 (max for integer)
    ;cumNDVIline = TOTAL(NDVIseasonLine,2)
    cumNDVIline = LONG(TOTAL(LONG(NDVIseasonLine),2))
    maxNDVIline = FIX(MAX(NDVIseasonLine,DIMENSION=2,MIN=minNDVIline))   ; calculate maximum and minimum for season
    IF dataSet eq 'GIMMS' OR dataset eq 'MODIS' then cumNDVIline = FIX(cumNDVIline/10)     ; in case of GIMMS divide by 10 to fit into integer
    IF dataSet eq 'SPOT' OR dataSet eq 'eMODIS' then cumNDVIline = FIX(cumNDVIline)
    ; if the minimum-value is the noData-value (assume that noData is always lowest value!), then set output to that
    ; pixel to noData
    index = WHERE(minNDVIline eq noDataValue)
    IF index[0] NE -1 THEN BEGIN
      cumNDVIline[index] = 0
      maxNDVIline[index] = 0
    ENDIF
    WRITEU, W1, cumNDVIline
    WRITEU, W2, maxNDVIline
  ENDFOR
ENDFOR
CLOSE, /ALL

CREATE_HEADER, cumFile, nl, ns, COUNT, bandNameList, dataSet, 'cumulated'
CREATE_HEADER, maxFile, nl, ns, COUNT, bandNameList, dataSet, 'maximum'


; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED CUMULATING'

END ;Procedure CUMULATE_PER_PIXEL