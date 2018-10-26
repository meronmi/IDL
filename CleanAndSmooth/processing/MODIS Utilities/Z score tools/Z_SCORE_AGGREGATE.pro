; _____________________________________________________________________________________________
;
; NAME:
;   Z_SCORE_AGGREGATE
;
; PURPOSE:
;   Based on aggregated cumulated NDVI (or maximum NDVI) calculate the z-score
;
; INPUTS:
;   - csv-file with admin-codes and alternate long and short season
;
; CALLING SEQUENCE:
;   Z_SCORE_AGGREGATE
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, April 2013
;
; _____________________________________________________________________________________________


PRO Z_SCORE_AGGREGATE

STARTTIME = SYSTIME(1)

; General settings (to be changed for different input)
dataPath='G:\BACKUP\IMAGES\IBLI\'               ; location where csv-file is
source=['GIMMS','SPOT','MODIS','eMODIS']        ; select source
source=source[0]
which = ['cumNDVI','maxNDVI']                   ; select which to aggregate
which = which[1]

; general activities
dataPath=dataPath+source+'\aggregate\'
inFile=dataPath+which+'_aggregated_'+source+'.csv'

; read data
read = READ_ASCII(inFile,data_start=2,delimiter=',',HEADER=firstLine)
firstLine=firstLine[1]                 ; get rid of description
array=LONG(read.(0))
first2Columns=array[0:1,*]
cumNDVIdata=array[2:*,*]
dimensions=size(cumNDVIdata)
nSeasons=dimensions[1]
nDivisions=dimensions[2]

; create an array to hold z-scores
zCumNDVIdata=FLTARR(nSeasons,nDivisions)

; calculate z-scores (per season! we do not need to know which season comes first here)
FOR season=0,1,1 DO BEGIN  
  index=INDGEN(CEIL(nSeasons/2.))*2+season      ; for the first season
  check=WHERE(index NE nSeasons)                ; in case of uneven seasons, we have to get rid of the last index
  IF check[0] ne -1 THEN index=index[check]
  cumNDVIseason=cumNDVIdata[index,*]
  avgCumNDVIseason=TOTAL(cumNDVIseason,1)/N_ELEMENTS(index)
  stdCumNDVIseason=STDDEV(cumNDVIseason,DIMENSION=1)
  ; below some cumbersome array multiplications and transposing to get same array as cumNDVIseason
  avgCumNDVIseason = avgCumNDVIseason # replicate(1,N_ELEMENTS(index))
  avgCumNDVIseason = TRANSPOSE(avgCumNDVIseason)
  stdCumNDVIseason = stdCumNDVIseason # replicate(1,N_ELEMENTS(index))
  stdCumNDVIseason = TRANSPOSE(stdCumNDVIseason) 
  zCumNDVIseason=(cumNDVIseason-avgCumNDVIseason)/stdCumNDVIseason
  zCumNDVIdata[index,*]=zCumNDVIseason
ENDFOR

; Write the output to a CSV-file
outFile = dataPath+'Zscore_'+which+'_aggregated_'+source+ '.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
PRINTF,W1, ';Z-scored aggregated '+which+' data for '+source+' of Kenyan divisions'
PRINTF,W1, firstLine
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
PRINT, 'FINISHED Z-SCORING THE AGGREGATES'

END ;Procedure AGGREGATE_CUM_GIMMS