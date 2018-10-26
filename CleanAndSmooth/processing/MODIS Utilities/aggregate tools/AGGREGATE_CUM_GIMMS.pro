; _____________________________________________________________________________________________
;
; NAME:
;   AGGREGATE_CUM_GIMMS
;
; PURPOSE:
;   Calculate the aggregated cumulated NDVI (or maximum NDVI) for different seasons
;
; INPUTS:
;   - BIL-stacks of yearly cumNDVI (or other)
;   - Image with ID codes of admin polygons (converted to raster) in same geometry as the BIL-stacks
;      but with 10x10 times higher resolution
;   - season 1 (=long: March-September) or 2 (=short: October-February)
;
; CONDITIONS USED (otherwise result = mask value):
;   - cumNDVI should exist (>0)
;
; CALLING SEQUENCE:
;   AGGREGATE_CUM_GIMMS
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, April 2013
;
; _____________________________________________________________________________________________


PRO AGGREGATE_CUM_GIMMS

STARTTIME = SYSTIME(1)

; General settings (to be changed for different input)
dataPath='G:\BACKUP\IMAGES\IBLI\GIMMS\'         ; location where administrative region-file is located
which = ['cumNDVI','maxNDVI']                   ; select which to aggregate
which = which[1]
imageFileBase = 'Kenya_NDVI3g_filteredSG_cm1.stk'  ; original filtered NDVI stack file
imageFile = dataPath+which+'_'+imageFileBase    ; input file (can be changed: later for max/cum SPOT VGT matters for INTARR or BYTARR!)
adminFile = dataPath+'divisionID.img'                           ; input File for division codes
nl=132                             ; number of lines
ns=112                             ; number of samples
nSeasons=60                        ; number of seasons in cumulated NDVI file
firstYear=1981                     ; first year in the cumulated dataset
firstSeason=2                      ; 1=long rains (March-September), 2=short rains (October-February)
ratioAdmin=10                      ; number indicates how many times the spatial resolution of Admin-layer is higher than NDVI

; general activities & writing of a StringArray to hold season identifiers
workPath=dataPath+'aggregate\'                                   ; location where output will be written to
IF FILE_TEST(workPath) eq 0 THEN FILE_MKDIR, workPath
; create a stringArray to hold identifiers for season: 1981S, 1982L, etc... to be printed to file later
yearArray=REBIN(INDGEN(CEIL(nSeasons/2.)+1)+firstYear,2*(CEIL(nSeasons/2.)+1))   ;clumsy code, but gets always array with each year twice
seasonArray=REPLICATE(['L'],nSeasons+1)
index=1+INDGEN(CEIL(nSeasons/2.))*2
seasonArray[index]='S'
seasonArray=STRCOMPRESS(yearArray,/REMOVE_ALL)+seasonArray
IF firstSeason eq 2 THEN seasonArray=seasonArray[1:nSeasons] ELSE seasonArray=seasonArray[0:nSeasons-1]

; Open the Admin1-file
OPENR, lun, adminFile, /GET_LUN
adminRef = ASSOC(lun, BYTARR(ratioAdmin*ns,ratioAdmin*nl)) ; note: if later we use INTARR for adminFile, then change!
adminRaster = adminRef[0]                                 ; the raster file holding the admin1 codes
free_lun, lun
adminList = adminRaster[UNIQ(adminRaster, SORT(adminRaster))]   ; array with unique admin codes (first=0) in file
noAdminCode =255                                                ; this is the value which does not correspond to an admin unit
index=WHERE(adminList NE noAdminCode)                           ; NOTE: perhaps this value needs changing??
IF index[0] ne -1 THEN adminList=adminList[index]               ; remove this value from the adminList

; Create access to lines of phenology-data: Depends if byte or integer (NOTE: CHANGE FOR SPOT maximum!!)
OPENR, R1, imageFile, /GET_LUN               ; Open the stack-file for reading
lineAssCumNDVI = ASSOC(R1, INTARR(ns,nSeasons))    ; Associate a line of data (with all bands)

; Create the arrays where all aggregated values will be stored. One column for Admin1-units
; (standard value is mask value -1) --> therefore substract 1 from LONARR
cumNDVI_array = LONARR(2+nSeasons,N_ELEMENTS(adminlist))-1
cumNDVI_array[0,*] = [adminList]
; note: in second column (cumNDVI_array[1,*]) we will write the total number of pixels (*100)

; Here the average cumNDVI per Admin1-region is calculated.
FOR admin=0,N_ELEMENTS(adminlist)-1, 1L DO BEGIN
  adminCode = adminList[admin]                          ; assign adminCode-region to process
  ; create a weight-file in original GIMMS geometry that indicates % of adminArea per original GIMMS-pixel
  adminWeights=BYTARR(ns,nl)
  FOR i=0,ns-1,1L DO BEGIN                             ; Loop over lines of GIMMS-geometry
    FOR j=0,nl-1,1L DO BEGIN                           ; Loop over sample of GIMMS-geometry
    ; create 1st an 10*10 window to calculate percentages
    window = adminRaster[i*ratioAdmin:i*ratioAdmin+(ratioAdmin-1), j*ratioAdmin:j*ratioAdmin+(ratioAdmin-1)]
    ; For the 100 pixels, sum #pixels of admin-unit within GIMMS pixel.
    test = where(window eq adminCode, CNT1)
    adminWeights[i,j] = BYTE(CNT1)                 ; BYTE because maximum is 100 (with ratio 10, with ratios above 15: change!)
    ENDFOR
  ENDFOR

  ; NOTE!!! WE WILL NEED THIS TOTAL WEIGHT, BUT PERHAPS BETTER TO CHECK AFTERWARDS IN CASE IRRELEVANT GIMMS PIXELS (WATER) ARE INCLUDED!
  ; another option is to critically revise the admin raster-file and remove strange pixels (water) from the admin-file itself.
  
  ; Here first find out which lines to access
  index = where(adminWeights gt 0, COUNT)    ; for now include all with slight overlap with admin Unit: possible to set threshold higher
                                             ; for ratio 10--> 100 would mean only pixels fully falling within Unit.
  IF index[0] NE -1 THEN BEGIN   ; Admin-code may not exist for some reason or not enough points in admin region with good weight --> output remains -1
    weightArrayAdmin = adminWeights[index]   ; put the weight for later multiplication
    ; here loop over relevant lines & build temporary array of all relevant pixels
    lines = index/ns
    uniqlines = lines[UNIQ(lines)]
    j = 0
    ; loop over lines of data where Admin region falls in
    WHILE j lt N_ELEMENTS(uniqlines) DO BEGIN
      cumNDVIdataLine = lineAssCumNDVI[uniqlines[j]]
      check = where(lines eq uniqlines[j], CNT)
      index2 = index[check] mod ns                 ; index for the relevant samples in the line
      IF j eq 0 THEN BEGIN
        cumNDVIArrayAdminComplete = cumNDVIdataLine[index2,*]
      ENDIF ELSE BEGIN
        cumNDVIArrayAdminComplete = [cumNDVIArrayAdminComplete, cumNDVIdataLine[index2,*]]
      ENDELSE
      j++
    ENDWHILE
    ; check if during some season cum or maxNDVI is set to 0, if so, remove pixel time series completely
    minCheck = MIN(cumNDVIArrayAdminComplete,DIMENSION=2)
    index = WHERE(minCheck GT 0)
    IF index[0] NE -1 THEN BEGIN
      weightArrayAdmin = weightArrayAdmin[index]
      cumNDVIArrayAdminComplete = cumNDVIArrayAdminComplete[index,*]
      totalWeight = LONG(TOTAL(weightArrayAdmin))
      cumNDVI_array[1,admin]=totalWeight
      ; the final step of combining the weights with the cumNDVIArrayAdminComplete
      weights = weightArrayAdmin # replicate(1,nSeasons)      ; array with weights 
      cumNDVIAdminAvg = LONG(TOTAL((FLOAT(cumNDVIArrayAdminComplete)* weights),1)/totalWeight)
      cumNDVI_array[2:*,admin] = cumNDVIAdminAvg
    ENDIF
  ENDIF

ENDFOR
CLOSE, /ALL

; Write away the output to a CSV-file
outFile = workPath+which+'_aggregated'+'_GIMMS'+ '.csv'
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, width=2000                        ; set width to get all data in one row
PRINTF,W1, ';Aggregated '+which+' data for GIMMS of Kenyan divisions'
PRINTF,W1, 'adminID, pixels*100,'+STRJOIN(seasonArray,',')
FOR i=0, N_ELEMENTS(adminlist)-1, 1L DO BEGIN
  writeline = STRJOIN(STRCOMPRESS(cumNDVI_array[*,i],/REMOVE_ALL),',')
  printf, W1, writeline
ENDFOR
CLOSE, W1

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED AGGREGATING'

END ;Procedure AGGREGATE_CUM_GIMMS