; _____________________________________________________________________________________________
;
; NAME:
;   PHENO_MICHELE_TO_DEKAD
;
; PURPOSE:
;   Based on average phenology per admin unit (AGGREGATE_PHENO_MICHELE.pro output), and assuming double season,
;   write a file with dekad number for start/end of the two seasons
;
; INPUTS:
;   - csv-file with admin-codes and phenological summary
;
; CALLING SEQUENCE:
;   PHENO_MICHELE_TO_DEKAD
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, November 2014
;
; _____________________________________________________________________________________________


PRO PHENO_MICHELE_TO_DEKAD

STARTTIME = SYSTIME(1)

; General settings (to be changed for different input)
dataPath = 'G:\BACKUP\IMAGES\IBLI\pheno\Michele\ngspy_lr6_iter_v1\aggregate\'
inFile = dataPath + 'ibliID_phenoSummary_SPOT_Michele.csv'

; read data
read = READ_ASCII(inFile,data_start=1,delimiter=',',HEADER=firstLine)
array=read.(0)
adminList=TRANSPOSE(FIX(array[0,*]))    ; get a list of admin units
phenData=array[4:7,*]                   ; get only the start/end dates for the two seasons
nDivisions = N_ELEMENTS(adminList)


; DOY separator values per dekad:   jan      feb      mar      apr         may         jun         jul         aug         sep         oct         nov         dec
;  corresponding dekadNr:            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 
doySeparatorList =               [0,10,20,31,41,51,59,69,79,90,100,110,120,130,140,151,161,171,181,191,201,212,222,232,243,253,263,273,283,293,304,314,324,334,344,354,365]
dekadNrList = INDGEN(37)+1       ; corresponding list of dekad numbers

; create an array to hold the dekad numbers per admin and start off with giving no data value of -99
phenDekadArray=INTARR(4,nDivisions)

; translate sos/eos to dekad number
; NOTE!!! We may eventually want to allow some flexibility, e.g. if DOY=9.5, we could set first dekad to 2 instead of 1 (and other way around for EOS)
; NOTE!!! We may eventually want to take into account the sdT (temporal standard deviation)
FOR param=1,4, 1L DO BEGIN    ; split per parameter in file because for EOS/SOS we may want to do different things eventually
                              ; for now we keep things equal as SOS means "start that dekad", and EOS means "end with that dekad and include it"
  phenParamValues = phenData[param-1,*]  ; get list of only relevant parameter
  phenParamDekad = TRANSPOSE(INTARR(nDivisions))-99
  IF (2*(param/2) eq param) THEN even = 1 ELSE even = 0          ; 0 for even (EOS), 1 for odd (SOS)
  CASE even OF
    0: BEGIN      ; this is for EOS
         FOR i=0, 35, 1L DO BEGIN
           index = WHERE(phenParamValues GT doySeparatorList[i])
           IF index[0] NE -1 THEN phenParamDekad[index] = dekadNrList[i]
         ENDFOR
       END
    1: BEGIN      ; this is for SOS
         FOR i=0, 35, 1L DO BEGIN
           index = WHERE(phenParamValues GT doySeparatorList[i])
           IF index[0] NE -1 THEN phenParamDekad[index] = dekadNrList[i]
         ENDFOR
       END
  ENDCASE
  phenDekadArray[param-1,*]=phenParamDekad
ENDFOR


; Write the output to a CSV-file
outFile = dataPath+'ibliID_phenoSummary_dekad.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
PRINTF,W1, 'ibliID, avgSOS_2L, avgEOS_2L, avgSOS_2S, avgEOS_2S
FOR i=0, nDivisions-1, 1L DO BEGIN
  writeline = STRCOMPRESS(FIX(adminList[i]),/REMOVE_ALL)+','+STRJOIN(STRCOMPRESS(phenDekadArray[*,i],/REMOVE_ALL),',')
  printf, W1, writeline
ENDFOR
CLOSE, W1

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED TRANSLATING PHENOLOGY TO DEKADS'

END ;Procedure Z_SCORE_AGGREGATE_SIMPLE