; _____________________________________________________________________________________________
;
; NAME:
;   AVERAGE_NDVI_PER_DIVISION_adaptiveSeasons
;
; PURPOSE:
;   Average NDVI for two seasons (for different aggregated NDVI series)
;   Instead of the original fixed seasons for all divisions, we now use a CSV-file with start/end 
;     dates of both seasons
;
; INPUTS:
;   - csv-file with aggregated division-level NDVI for each period (dekad, 15days)
;   - csv-file with start/end dates of both seasons
;
; CONDITIONS USED (otherwise result = mask value):
;   - none
;
; CALLING SEQUENCE:
;   AVERAGE_NDVI_PER_DIVISION_adaptiveSeason
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, May 2013
;   Adapted by:  Anton Vrieling, November 2014
;
; _____________________________________________________________________________________________

PRO AVERAGE_NDVI_PER_DIVISION_adaptiveSeasons

STARTTIME = SYSTIME(1)

dataSet = ['GIMMS', 'SPOT', 'MODIS', 'eMODIS']
dataSet = dataSet[3]
dataPath= 'G:\BACKUP\IMAGES\IBLI\'
adminLevel = ['division','kebele','ibli','woreda', '108divisions']
adminLevel = adminLevel[2]

; below provide the dataset-specific settings
IF dataSet eq 'GIMMS' THEN BEGIN
  startYearData = 1981               ; startYear of data
  nImagesYear = 24                   ; number of images per year (bi-monthly)
  periodLag   = 13                   ; first image of the year - July (note, for IDL do -1)
  ;startPeriodLong = 5                ; start Period for long rains (1=first of January, 5 is first of March here)
  ;numberPeriodsLong = 14             ; how many periods (15-day) are in the long season
  ;startPeriodShort = 19              ; start Period for long rains (1=first of January, 5 is first of March here)
  ;numberPeriodsShort = 10            ; how many periods (15-day) are in the long season
ENDIF

IF dataSet eq 'SPOT' THEN BEGIN
  startYearData = 1998               ; startYear of data
  nImagesYear = 36                   ; number of images per year (three per month)
  periodLag   = 10                   ; first image of the year - April (note, for IDL do -1)
  ;startPeriodLong = 7                ; start Period for long rains (1=first of January, 7 is first of March here)
  ;numberPeriodsLong = 21             ; how many periods (15-day) are in the long season
  ;startPeriodShort = 28              ; start Period for long rains (1=first of January, 5 is first of March here)
  ;numberPeriodsShort = 15            ; how many periods (15-day) are in the long season
ENDIF

IF dataSet eq 'eMODIS' THEN BEGIN
  startYearData = 2001               ; startYear of data
  nImagesYear = 36                   ; number of images per year (three per month)
  periodLag   =  1                   ; first image of the year - January (note, for IDL do -1)
  ;startPeriodLong = 7                ; start Period for long rains (1=first of January, 7 is first of March here)
  ;numberPeriodsLong = 21             ; how many periods (10-day) are in the long season
  ;numberPeriodsLong = 12       ; TO CREATE UPDATE LONG SEASON, FROM MARCH-MAY (9) OR MARCH-JUNE (12)
  ;startPeriodShort = 28              ; start Period for long rains (1=first of January, 28 is first of October here)
  ;numberPeriodsShort = 15            ; how many periods (10-day) are in the short season
  ;numberPeriodsShort = 9            ; at time of update Nov2013 we only have 5 dekads for numberPeriodsShort
ENDIF

; open the average phenology file (in dekad numbers)
phenFile='G:\BACKUP\IMAGES\IBLI\pheno\Michele\ngspy_lr6_iter_v1\aggregate\ibliID_phenoSummary_dekad.csv'
read = READ_ASCII(phenFile,data_start=1,delimiter=',',HEADER=firstLine)
array=read.(0)
adminList=TRANSPOSE(FIX(array[0,*]))         ; get a list of admin units
phenData=FIX(array[1:4,*])                   ; get only the start/end dates for the two seasons

; access the input NDVI csv-file
whichUpdate='update_20141028'
inFile=dataPath+dataSet+'\aggregation-first\'+whichUpdate+'\'+'NDVI_aggregated_'+dataSet+'_'+adminLevel+'.csv'

; read the NDVI data
read = READ_ASCII(inFile,data_start=2,delimiter=',',HEADER=firstLine)
firstLine=firstLine[1]                 ; get rid of description
array=read.(0)
first2Columns=LONG(array[0:1,*])
NDVIdata=array[2:*,*]
IF dataset eq 'eMODIS' THEN NDVIdata=(array[2:*,*]-100.)/100.         ; note this is only for eMODIS to change to NDVI units!
IF dataset eq 'GIMMS' THEN NDVIdata=(array[2:*,*])/10000.  
dimensions=size(NDVIdata)
nTimePeriods=dimensions[1]
nDivisions=dimensions[2]

; create a bandNameList that can accommodate all possibly occurring seasons
nYears = CEIL(FLOAT(nTimePeriods)/nImagesYear)+1            ; number of years to analyse (add a year to be sure!)
;maxStartPeriodLong = MAX(phenData[0,*])
;maxStartPeriodShort = MAX(phenData[2,*])
bandNameList=STRCOMPRESS(REBIN(INDGEN(nYears)+startYearData, nYears*2, /sample),/REMOVE_ALL)
seasonNameList=REPLICATE('S',nYears*2)
tempList = INDGEN(nYears*2)
index = WHERE(2*(tempList/2) eq tempList)
seasonNameList[index]='L'
bandNameList=bandNameList+seasonNameList
;IF maxStartPeriodLong LT periodLag THEN bandNameList=bandNameList[1:*]  ; two statements to shorten the bandNameList if the series does not start before the latest startdate
;IF maxStartPeriodLong LT periodLag THEN bandNameList=bandNameList[1:*]
nSeasons = N_ELEMENTS(bandNameList)

; create an array to hold temporal-averaged NDVI for fullSeason
avgNDVIdata=FLTARR(nSeasons,nDivisions)-99.

; now loop over the admin units, get the phenological access points and calculate averages
FOR admin = 0, nDivisions-1, 1L DO BEGIN
  SOS_L=phenData[0,admin] & EOS_L=phenData[1,admin] & SOS_S=phenData[2,admin] & EOS_S=phenData[3,admin]  ; get relevant parameters
  ; some planning to get the sequences right!
  IF EOS_L LT SOS_L THEN EOS_L=EOS_L+nImagesYear
  IF EOS_S LT SOS_S THEN EOS_S=EOS_S+nImagesYear
  ; get access points and account for periodLag
  startPeriods = reform(rebin([SOS_L, SOS_S],2,nYears),2*nYears)+nImagesYear*REBIN(INDGEN(nYears), nYears*2, /sample)-periodLag
  endPeriods = reform(rebin([EOS_L, EOS_S],2,nYears),2*nYears)+nImagesYear*REBIN(INDGEN(nYears), nYears*2, /sample)-periodLag
  index = WHERE(startPeriods GE 0 AND endPeriods LT nTimePeriods)    ; create an index to just allow for full seasons
  IF index[0] NE -1 THEN BEGIN
    startPeriods = startPeriods[index]
    endPeriods = endPeriods[index]
  ENDIF
  ; CALCULATE finally!!!  ;-)  CONTINUE FROM HERE!
  avgNDVIadmin = FLTARR(N_ELEMENTS(startPeriods))
  FOR season=0,N_ELEMENTS(startPeriods)-1,1L DO BEGIN
    NDVIseasonLine = NDVIdata[startPeriods[season]:endPeriods[season],admin]
    numberPeriodsInSeason = endPeriods[season] - startPeriods[season] + 1      ; quick calculation to get the number of periods in season under consideration
    avgNDVIadmin[season] = TOTAL(NDVIseasonLine)/numberPeriodsInSeason
  ENDFOR
  avgNDVIdata[index,admin] = avgNDVIadmin
  
ENDFOR

; Write the output to a CSV-file
outFile = dataPath+dataSet+'\aggregation-first\'+whichUpdate+'\'+'avgNDVI_aggregated_'+dataSet+'_'+adminLevel+'_ADAPTIVE_SEASONS.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
IF adminLevel eq 'division' THEN PRINTF,W1, ';Seasonal average of aggregated NDVI data for '+dataSet+' of Kenyan divisions and Ethiopian woredas'
IF adminLevel eq 'kebele' THEN PRINTF,W1, ';Seasonal average of aggregated NDVI data for '+dataSet+' of Ethiopian kebeles (Borana)'
IF adminLevel eq 'ibli' THEN PRINTF,W1, ';Seasonal average of aggregated NDVI data for '+dataSet+' of Kenyan divisions and Ethiopian clustered kebeles'
IF adminLevel eq 'woreda' THEN PRINTF,W1, ';Seasonal average of aggregated NDVI data for '+dataSet+' of Ethiopian woredas (Borana)'
IF adminLevel eq '108divisions' THEN PRINTF,W1, ';Seasonal average of aggregated NDVI data for '+dataSet+' of the old 108 Kenyan divisions'
PRINTF,W1, 'adminID, pixels,'+STRJOIN(bandNameList,',')
FOR i=0, nDivisions-1, 1L DO BEGIN
  writeline = STRJOIN(STRCOMPRESS(first2Columns[*,i],/REMOVE_ALL),',')+','+STRJOIN(STRCOMPRESS(avgNDVIdata[*,i],/REMOVE_ALL),',')
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