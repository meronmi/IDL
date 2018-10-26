; _____________________________________________________________________________________________
;
; NAME:
;   CUMULATE_Z_PER_DIVISION
;
; PURPOSE:
;   Cumulate zNDVI for fixed seasons (for different aggregated zNDVI series)
;
; INPUTS:
;   - csv-file with aggregated division-level z-scores for each period (dekad, 15days)
;   - which NDVI series
;
; CONDITIONS USED (otherwise result = mask value):
;   - none
;
; CALLING SEQUENCE:
;   CUMULATE_Z_PER_DIVISION
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, May 2013
;
; _____________________________________________________________________________________________


PRO CREATE_HEADER, outFile, nl, ns, COUNT, bandNameList, dataSet, type      ; Procedure to generate generic header for each output
HEADER_OUT=STRMID(outFile,0,STRLEN(outFile)-3)+'hdr'
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


PRO CUMULATE_Z_PER_DIVISION

STARTTIME = SYSTIME(1)

dataSet = ['GIMMS', 'SPOT', 'MODIS', 'eMODIS']
dataSet = dataSet[3]
dataPath= 'G:\BACKUP\IMAGES\IBLI\'

; below provide the dataset-specific settings
IF dataSet eq 'GIMMS' THEN BEGIN
  startYearData = 1981               ; startYear of data
  nImagesYear = 24                   ; number of images per year (bi-monthly)
  periodLag   = 13                   ; first image of the year - July (note, for IDL do -1)
  startPeriodLong = 5                ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsLong = 14             ; how many periods (15-day) are in the long season
  startPeriodShort = 19              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
ENDIF

IF dataSet eq 'SPOT' THEN BEGIN
  startYearData = 1998               ; startYear of data
  nImagesYear = 36                   ; number of images per year (three per month)
  periodLag   = 10                   ; first image of the year - July (note, for IDL do -1)
  startPeriodLong = 7                ; start Period for long rains (1=first of January, 7 is first of March here)
  numberPeriodsLong = 21             ; how many periods (15-day) are in the long season
  startPeriodShort = 28              ; start Period for long rains (1=first of January, 5 is first of March here)
  numberPeriodsShort = 15            ; how many periods (15-day) are in the long season
ENDIF

IF dataSet eq 'eMODIS' THEN BEGIN
  startYearData = 2001               ; startYear of data
  nImagesYear = 36                   ; number of images per year (three per month)
  periodLag   =  1                   ; first image of the year - July (note, for IDL do -1)
  startPeriodLong = 7                ; start Period for long rains (1=first of January, 7 is first of March here)
  numberPeriodsLong = 21             ; how many periods (10-day) are in the long season
  startPeriodShort = 28              ; start Period for long rains (1=first of January, 28 is first of October here)
  numberPeriodsShort = 15            ; how many periods (15-day) are in the long season
ENDIF

; access the input csv-file
inFile=dataPath+dataSet+'\z-scoring-first\aggregate\'+'zNDVI_aggregated_'+dataSet+'.csv'
; read data
read = READ_ASCII(inFile,data_start=2,delimiter=',',HEADER=firstLine)
firstLine=firstLine[1]                 ; get rid of description
array=read.(0)
first2Columns=LONG(array[0:1,*])
zNDVIdata=array[2:*,*]
dimensions=size(zNDVIdata)
nTimePeriods=dimensions[1]
nDivisions=dimensions[2]

; define the precise access points (periods) in the file (in IDL-units), and create bandNameList
nYears = CEIL(FLOAT(nTimePeriods)/nImagesYear)            ; number of years to analyse
FOR year = 0, nYears, 1L DO BEGIN                         ; usually it would be till nYears-1, but just to be sure (we subset later)
  ; first create a bandNameList to be subsetted later
  sYear = STRCOMPRESS(year+startYearData,/REMOVE_ALL)
  startPeriodYear = [startPeriodLong,startPeriodShort]+year*nImagesYear-periodLag
  endPeriodYear = [startPeriodLong+numberPeriodsLong,startPeriodShort+numberPeriodsShort]-1+year*nImagesYear-periodLag
  IF year eq 0 THEN BEGIN                  ; do separately to first create the list, and later to update the list
    bandNameList=[sYear+'L',sYear+'S']
    startPeriods = startPeriodYear
    endPeriods = endPeriodYear
  ENDIF ELSE BEGIN
    bandNameList=[bandNameList, sYear+'L',sYear+'S']
    startPeriods = [startPeriods, startPeriodYear]
    endPeriods = [endPeriods,endPeriodYear]
  ENDELSE
ENDFOR
; now subset the above based on whether it fits in the data
index = WHERE(startPeriods ge 0 AND endPeriods lt nTimePeriods, nSeasons)
IF index[0] ne -1 THEN BEGIN
  startPeriods = startPeriods[index]
  endPeriods   = endPeriods[index]
  bandNameList = bandNameList[index]
ENDIF


; create an array to hold z-scores
zCumNDVIdata=FLTARR(nSeasons,nDivisions)

; CALCULATE!!!
FOR season=0,nSeasons-1,1L DO BEGIN
  zNDVIseasonLine = zNDVIdata[startPeriods[season]:endPeriods[season],*]
  cumZNDVIline = TOTAL(zNDVIseasonLine,1)
  zCumNDVIdata[season,*]=cumZNDVIline
ENDFOR

; Write the output to a CSV-file
outFile = dataPath+dataSet+'\z-scoring-first\aggregate\'+'zCumNDVI_aggregated_'+dataSet+'.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
PRINTF,W1, ';Seasonal cumulation of Z-scored aggregated data for '+dataset+' of Kenyan divisions'
PRINTF,W1, 'adminID, pixels,'+STRJOIN(bandNameList,',')
FOR i=0, nDivisions-1, 1L DO BEGIN
  writeline = STRJOIN(STRCOMPRESS(first2Columns[*,i],/REMOVE_ALL),',')+','+STRJOIN(STRCOMPRESS(zCumNDVIdata[*,i],/REMOVE_ALL),',')
  printf, W1, writeline
ENDFOR
CLOSE, W1


; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED CUMULATING'

END ;Procedure CUMULATE_Z_PER_DIVISION