; NAME:
;   SCREENING
;
; PURPOSE:
;   IDL wrapper for screening 10-day composite AVHRR LTDR series to remove strange outliers (clouds, but also strange high values)
;
; INPUTS:
;   NDVI stack --> here 0 stands for cloud!
;   number of samples, lines, & bands
;
; OUTPUTS:
;   Filtered NDVI using Savitzky-Golay
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, January 2008 (based on Allard de Wit, November 2004)
;   Adapted by:  Anton Vrieling, August 2010 for SPOT VGT
;   Adapted by:  Anton Vrieling, January 2016 for LTDR AVHRR
;
; last run took 166 minutes


function cloudFilter_v1, data, noDataValue, periodIDs, threshold, medianDifThr
  ; from a single pixel NDVI time series, clouds and outliers are masked and set to noData
  ; periodIDs are the dekad-identifiers (without year) and contain the same number of elements as data
  tmp = data  ; create a temporary copy of the data
  ; assess first (accounting for noData) which values are outside the quartiles +- 1.5* the IQR for each specific dekad
  ; because of sometimes high-deviations we use now interdecile range (10-90%) and multiply by 3.0
  uniqIDs = periodIDs[UNIQ(periodIDs, SORT(periodIDs))] 
  medianTmp = INTARR(N_ELEMENTS(tmp))+noDataValue         ; initiate a vector to hold medians (same length as data)
  FOR i=0, N_ELEMENTS(uniqIDs)-1, 1L DO BEGIN
    dekadID = uniqIDs[i]
    ind1 = WHERE(periodIDs EQ dekadID AND tmp NE noDataValue)
    IF ind1[0] NE -1 THEN BEGIN
      IF N_ELEMENTS(ind1) LT 5 THEN tmp[ind1]=noDataValue ELSE BEGIN ; in case only 4 observations have a value, discard them
        tmp2 = tmp[ind1]
        perc = percentiles(tmp2,VALUE=[0.10,0.50,0.90])
        ;idr = perc[2]-perc[0]
        ;ind2 = WHERE(tmp2 LT (perc[0]-2.5*idr) OR tmp2 GT (perc[2]+2.5*idr))
        ;IF ind2[0] NE -1 THEN BEGIN
        ;  ind3 = ind1[ind2]
        ;  tmp[ind3] = noDataValue
        ;ENDIF
        ind1 = WHERE(periodIDs EQ dekadID)           ; also write median to medianTmp vector
        medianTmp[ind1]=perc[1]
      ENDELSE
    ENDIF
  ENDFOR
  ; now implement thresholding: in principle we delete the point where an increase of more than the threshold occurs
  ; however, here first we test for such an increase (if the increase occurs)
  ; second, we evaluate the absolute distance between medianTmp and point just before increase  (lowD)
  ; third, we evaluate the absolute distance between medianTmp and point just after increase   (highD)
  ; if highD is 3*lowD, then highD is labelled as erroneously high value  (and set to noDataValue), else lowD
  ; !!! account for noData
  tmp2 = SHIFT(tmp,-1)             ; shifted
  tmpMedianDif1 = ABS(medianTmp-tmp)             ; difference between median and low value
  tmpMedianDif2 = ABS(SHIFT(medianTmp,-1)-tmp2)  ; difference between median and high value // THIS IS AS 55 BUT SHIFTED of 1 to the lest
  ; two wheres probably to assess the case where low value needs to be set to noData, or high value needs to be set to noData
  ; as compared to v0, add a condition about the minimum tmpMedianDif before acting
  ind1 = WHERE(tmp2-tmp GE threshold AND tmp NE noDataValue AND tmpMedianDif1 GE medianDifThr AND tmpMedianDif2 LT 3*tmpMedianDif1)
  IF ind1[N_ELEMENTS(ind1)-1] eq N_ELEMENTS(tmp)-1 THEN ind1=ind1[0:N_ELEMENTS(ind1)-2]     ; get rid of the last value in series
  IF ind1[0] NE -1 AND ind1[0] NE N_ELEMENTS(tmp)-1 THEN $       ; if the previous if only yielded the last dekad in file, it would not have been omitted
     tmp[ind1] = noDataValue
  ind1 = WHERE(tmp2-tmp GE threshold AND tmp NE noDataValue AND tmpMedianDif2 GE medianDifThr AND tmpMedianDif2 GE 3*tmpMedianDif1) + 1  ; (+1 because we need to put the tmp2 at noDataValue)
  IF ind1[N_ELEMENTS(ind1)-1] eq N_ELEMENTS(tmp) THEN ind1=ind1[0:N_ELEMENTS(ind1)-2]     ; get rid of the last value in series
  IF ind1[0] NE -1 AND ind1[0] NE N_ELEMENTS(tmp) THEN $       ; if the previous if only yielded the last dekad in file, it would not have been omitted
    tmp[ind1] = noDataValue
  ;Mic spike detection
  ;desesonalize with the median
  ;treat that both tmp and medianTmp can contain noDataValue
  y = FLOAT(tmp)  ;a copy of the original
  indNaN = WHERE(y EQ noDataValue, countNaN)
  IF (countNaN EQ N_ELEMENTS(y)) THEN BEGIN
    y = tmp
  ENDIF ELSE BEGIN
    IF (countNaN GT 0) THEN y[indNaN] = !VALUES.F_NAN
    medianTmp = FLOAT(medianTmp)
    indNaN = WHERE(medianTmp EQ noDataValue, countNaN)
    IF (countNaN GT 0) THEN medianTmp[indNaN] = !VALUES.F_NAN
    dy = y - medianTmp
    ;detect those that jump up and come back in just one dek 0-2000-200, y1-y2-y3
    ;the jump up thresold for 1 dekad is set so tpeak, note that thi sjump is retained if dos not go back to less than minValAfterPeak = y2-(y2-y1)/2, at leaft half of the jump
    tpeak = 2000
    indFin = WHERE(FINITE(dy) EQ 1)
    ;detect values that are far from the LTMedian (they might be jump or not)
    indPeak = WHERE(dy[indFin] GT tpeak, countPeak)
    IF (countPeak) GT 0 THEN BEGIN
      ;index dy directly
      indPeak = indFin[indPeak]
      ;analyse one by one
      FOR i = 0, countPeak-1 DO BEGIN
        isJump = 0
        ;I am looking at value far from the LTMedian, conditions to be a jump:
        ;1) the previous value was finite and low (tpeak less) and it is not sub = -1
        ;2) the next value is finite and below minValAfterPeak and it si not sub = N_ELEMENTS(dy) 
        
        ;condition 1)
        sub = indPeak[i]
        subOfPrevious = indPeak[i]-1
        subOfNext = indPeak[i]+1
        IF ((subOfPrevious GE 0) AND (subOfNext LT N_ELEMENTS(dy))) THEN BEGIN ;is not at borders 
          IF ((FINITE(dy[subOfPrevious]) EQ 1) AND (FINITE(dy[subOfNext]) EQ 1)) THEN BEGIN  ;has no Nan before or after
            IF ((dy[sub]-dy[subOfPrevious]) GT tpeak) THEN BEGIN  ;is going up
              ;condition 2
              IF ((dy[subOfNext] LT (dy[sub]-(dy[sub]-dy[subOfPrevious])/2.0))) THEN BEGIN  ;and gond down after
                isJump = 1
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (isJump EQ 1) THEN y[sub] = noDataValue 
      ENDFOR
    ENDIF
    indNaN = WHERE(~FINITE(y), countNaN)
    IF (countNaN GT 0) THEN y[indNaN] = noDataValue
    y = FIX(y)
  ENDELSE
  return, y
end

;*************************************************************************************************************************


PRO SCREENING

STARTTIME = SYSTIME(1)

dataPath='D:\LTDR\' ;G:\IMAGES\LTDR\V5\processing\'                 ; location where LTDR 10-day stack is
workPath='D:\LTDR\';'G:\IMAGES\LTDR\V5\processing\'                 ; location where output will be written to
dataFile=dataPath+'LTDR10day_MVC_NDVIstack_V5.bil'
ns=7200                   ; number of samples
nl=2780                   ; number of lines
nTimePeriods=1334         ; number of bands / time periods in file
noDataValue=-9999s
threshold = 3000          ; maximum increase in NDVI (*10000) that is accepted, if not value before is classified as cloud
medianDifThr = 1500       ; if the difference between the dekad NDVI value and the LTA median NDVI for that dekad is LE this number, then it will be retained in any case

; Input files are opened by performing ASSOC to one line.
OPENR, R1, dataFile, /GET_LUN
assData = ASSOC(R1, INTARR(ns,nTimePeriods))

; Create files for output
outFile=workPath+'LTDR10day_MVC_NDVIstack_V5screened_v3'       ; file to hold screened timeseries
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN, /APPEND

; create bandList from original file names
;fileList = FILE_SEARCH('G:\IMAGES\LTDR\V5\dekadMVC\AVH13C10.NDVI*.img')
;; sort to be sure the order is correct
;sortList=LONG(STRMID(FILE_BASENAME(fileList),13,6)+STRMID(FILE_BASENAME(fileList),22,1))
;sorted = SORT(sortList)                      ; sorting just to be sure
;fileList=fileList[sorted]
; create bandList and a dekad identifier (11 mean first dekad of january, 12 second dekad of january, 123 is third dekad of december)
;bandList = STRMID(FILE_BASENAME(fileList),13,14)
;if I don't have access to individual files
bandList=['198106-dk3.N07', '198107-dk1.N07', '198107-dk2.N07', '198107-dk3.N07', '198108-dk1.N07', '198108-dk2.N07', '198108-dk3.N07', '198109-dk1.N07', '198109-dk2.N07', '198109-dk3.N07', '198110-dk1.N07', '198110-dk2.N07', '198110-dk3.N07', '198111-dk1.N07', '198111-dk2.N07', '198111-dk3.N07', '198112-dk1.N07', '198112-dk2.N07', '198112-dk3.N07', '198201-dk1.N07', '198201-dk2.N07', '198201-dk3.N07', '198202-dk1.N07', '198202-dk2.N07', '198202-dk3.N07', '198203-dk1.N07', '198203-dk2.N07', '198203-dk3.N07', '198204-dk1.N07', '198204-dk2.N07', '198204-dk3.N07', '198205-dk1.N07', '198205-dk2.N07', '198205-dk3.N07', '198206-dk1.N07', '198206-dk2.N07', '198206-dk3.N07', '198207-dk1.N07', '198207-dk2.N07', '198207-dk3.N07', '198208-dk1.N07', '198208-dk2.N07', '198208-dk3.N07', '198209-dk1.N07', '198209-dk2.N07', '198209-dk3.N07', '198210-dk1.N07', '198210-dk2.N07', '198210-dk3.N07', '198211-dk1.N07', '198211-dk2.N07', '198211-dk3.N07', '198212-dk1.N07', '198212-dk2.N07', '198212-dk3.N07', '198301-dk1.N07', '198301-dk2.N07', '198301-dk3.N07', '198302-dk1.N07', '198302-dk2.N07', '198302-dk3.N07', '198303-dk1.N07', '198303-dk2.N07', '198303-dk3.N07', '198304-dk1.N07', '198304-dk2.N07', '198304-dk3.N07', '198305-dk1.N07', '198305-dk2.N07', '198305-dk3.N07', '198306-dk1.N07', '198306-dk2.N07', '198306-dk3.N07', '198307-dk1.N07', '198307-dk2.N07', '198307-dk3.N07', '198308-dk1.N07', '198308-dk2.N07', '198308-dk3.N07', '198309-dk1.N07', '198309-dk2.N07', '198309-dk3.N07', '198310-dk1.N07', '198310-dk2.N07', '198310-dk3.N07', '198311-dk1.N07', '198311-dk2.N07', '198311-dk3.N07', '198312-dk1.N07', '198312-dk2.N07', '198312-dk3.N07', '198401-dk1.N07', '198401-dk2.N07', '198401-dk3.N07', '198402-dk1.N07', '198402-dk2.N07', '198402-dk3.N07', '198403-dk1.N07', '198403-dk2.N07', '198403-dk3.N07', '198404-dk1.N07', '198404-dk2.N07', '198404-dk3.N07', '198405-dk1.N07', '198405-dk2.N07', '198405-dk3.N07', '198406-dk1.N07', '198406-dk2.N07', '198406-dk3.N07', '198407-dk1.N07', '198407-dk2.N07', '198407-dk3.N07', '198408-dk1.N07', '198408-dk2.N07', '198408-dk3.N07', '198409-dk1.N07', '198409-dk2.N07', '198409-dk3.N07', '198410-dk1.N07', '198410-dk2.N07', '198410-dk3.N07', '198411-dk1.N07', '198411-dk2.N07', '198411-dk3.N07', '198412-dk1.N07', '198412-dk2.N07', '198412-dk3.N07', '198501-dk1.N07', '198501-dk2.N07', '198501-dk3.N07', '198502-dk1.N07', '198502-dk2.N09', '198502-dk3.N09', '198503-dk1.N09', '198503-dk2.N09', '198503-dk3.N09', '198504-dk1.N09', '198504-dk2.N09', '198504-dk3.N09', '198505-dk1.N09', '198505-dk2.N09', '198505-dk3.N09', '198506-dk1.N09', '198506-dk2.N09', '198506-dk3.N09', '198507-dk1.N09', '198507-dk2.N09', '198507-dk3.N09', '198508-dk1.N09', '198508-dk2.N09', '198508-dk3.N09', '198509-dk1.N09', '198509-dk2.N09', '198509-dk3.N09', '198510-dk1.N09', '198510-dk2.N09', '198510-dk3.N09', '198511-dk1.N09', '198511-dk2.N09', '198511-dk3.N09', '198512-dk1.N09', '198512-dk2.N09', '198512-dk3.N09', '198601-dk1.N09', '198601-dk2.N09', '198601-dk3.N09', '198602-dk1.N09', '198602-dk2.N09', '198602-dk3.N09', '198603-dk1.N09', '198603-dk2.N09', '198603-dk3.N09', '198604-dk1.N09', '198604-dk2.N09', '198604-dk3.N09', '198605-dk1.N09', '198605-dk2.N09', '198605-dk3.N09', '198606-dk1.N09', '198606-dk2.N09', '198606-dk3.N09', '198607-dk1.N09', '198607-dk2.N09', '198607-dk3.N09', '198608-dk1.N09', '198608-dk2.N09', '198608-dk3.N09', '198609-dk1.N09', '198609-dk2.N09', '198609-dk3.N09', '198610-dk1.N09', '198610-dk2.N09', '198610-dk3.N09', '198611-dk1.N09', '198611-dk2.N09', '198611-dk3.N09', '198612-dk1.N09', '198612-dk2.N09', '198612-dk3.N09', '198701-dk1.N09', '198701-dk2.N09', '198701-dk3.N09', '198702-dk1.N09', '198702-dk2.N09', '198702-dk3.N09', '198703-dk1.N09', '198703-dk2.N09', '198703-dk3.N09', '198704-dk1.N09', '198704-dk2.N09', '198704-dk3.N09', '198705-dk1.N09', '198705-dk2.N09', '198705-dk3.N09', '198706-dk1.N09', '198706-dk2.N09', '198706-dk3.N09', '198707-dk1.N09', '198707-dk2.N09', '198707-dk3.N09', '198708-dk1.N09', '198708-dk2.N09', '198708-dk3.N09', '198709-dk1.N09', '198709-dk2.N09', '198709-dk3.N09', '198710-dk1.N09', '198710-dk2.N09', '198710-dk3.N09', '198711-dk1.N09', '198711-dk2.N09', '198711-dk3.N09', '198712-dk1.N09', '198712-dk2.N09', '198712-dk3.N09', '198801-dk1.N09', '198801-dk2.N09', '198801-dk3.N09', '198802-dk1.N09', '198802-dk2.N09', '198802-dk3.N09', '198803-dk1.N09', '198803-dk2.N09', '198803-dk3.N09', '198804-dk1.N09', '198804-dk2.N09', '198804-dk3.N09', '198805-dk1.N09', '198805-dk2.N09', '198805-dk3.N09', '198806-dk1.N09', '198806-dk2.N09', '198806-dk3.N09', '198807-dk1.N09', '198807-dk2.N09', '198807-dk3.N09', '198808-dk1.N09', '198808-dk2.N09', '198808-dk3.N09', '198809-dk1.N09', '198809-dk2.N09', '198809-dk3.N09', '198810-dk1.N09', '198810-dk2.N09', '198810-dk3.N09', '198811-dk1.N09', '198811-dk2.N1', '198811-dk3.N1', '198812-dk1.N1', '198812-dk2.N1', '198812-dk3.N1', '198901-dk1.N1', '198901-dk2.N1', '198901-dk3.N1', '198902-dk1.N1', '198902-dk2.N1', '198902-dk3.N1', '198903-dk1.N1', '198903-dk2.N1', '198903-dk3.N1', '198904-dk1.N1', '198904-dk2.N1', '198904-dk3.N1', '198905-dk1.N1', '198905-dk2.N1', '198905-dk3.N1', '198906-dk1.N1', '198906-dk2.N1', '198906-dk3.N1', '198907-dk1.N1', '198907-dk2.N1', '198907-dk3.N1', '198908-dk1.N1', '198908-dk2.N1', '198908-dk3.N1', '198909-dk1.N1', '198909-dk2.N1', '198909-dk3.N1', '198910-dk1.N1', '198910-dk2.N1', '198910-dk3.N1', '198911-dk1.N1', '198911-dk2.N1', '198911-dk3.N1', '198912-dk1.N1', '198912-dk2.N1', '198912-dk3.N1', '199001-dk1.N1', '199001-dk2.N1', '199001-dk3.N1', '199002-dk1.N1', '199002-dk2.N1', '199002-dk3.N1', '199003-dk1.N1', '199003-dk2.N1', '199003-dk3.N1', '199004-dk1.N1', '199004-dk2.N1', '199004-dk3.N1', '199005-dk1.N1', '199005-dk2.N1', '199005-dk3.N1', '199006-dk1.N1', '199006-dk2.N1', '199006-dk3.N1', '199007-dk1.N1', '199007-dk2.N1', '199007-dk3.N1', '199008-dk1.N1', '199008-dk2.N1', '199008-dk3.N1', '199009-dk1.N1', '199009-dk2.N1', '199009-dk3.N1', '199010-dk1.N1', '199010-dk2.N1', '199010-dk3.N1', '199011-dk1.N1', '199011-dk2.N1', '199011-dk3.N1', '199012-dk1.N1', '199012-dk2.N1', '199012-dk3.N1', '199101-dk1.N1', '199101-dk2.N1', '199101-dk3.N1', '199102-dk1.N1', '199102-dk2.N1', '199102-dk3.N1', '199103-dk1.N1', '199103-dk2.N1', '199103-dk3.N1', '199104-dk1.N1', '199104-dk2.N1', '199104-dk3.N1', '199105-dk1.N1', '199105-dk2.N1', '199105-dk3.N1', '199106-dk1.N1', '199106-dk2.N1', '199106-dk3.N1', '199107-dk1.N1', '199107-dk2.N1', '199107-dk3.N1', '199108-dk1.N1', '199108-dk2.N1', '199108-dk3.N1', '199109-dk1.N1', '199109-dk2.N1', '199109-dk3.N1', '199110-dk1.N1', '199110-dk2.N1', '199110-dk3.N1', '199111-dk1.N1', '199111-dk2.N1', '199111-dk3.N1', '199112-dk1.N1', '199112-dk2.N1', '199112-dk3.N1', '199201-dk1.N1', '199201-dk2.N1', '199201-dk3.N1', '199202-dk1.N1', '199202-dk2.N1', '199202-dk3.N1', '199203-dk1.N1', '199203-dk2.N1', '199203-dk3.N1', '199204-dk1.N1', '199204-dk2.N1', '199204-dk3.N1', '199205-dk1.N1', '199205-dk2.N1', '199205-dk3.N1', '199206-dk1.N1', '199206-dk2.N1', '199206-dk3.N1', '199207-dk1.N1', '199207-dk2.N1', '199207-dk3.N1', '199208-dk1.N1', '199208-dk2.N1', '199208-dk3.N1', '199209-dk1.N1', '199209-dk2.N1', '199209-dk3.N1', '199210-dk1.N1', '199210-dk2.N1', '199210-dk3.N1', '199211-dk1.N1', '199211-dk2.N1', '199211-dk3.N1', '199212-dk1.N1', '199212-dk2.N1', '199212-dk3.N1', '199301-dk1.N1', '199301-dk2.N1', '199301-dk3.N1', '199302-dk1.N1', '199302-dk2.N1', '199302-dk3.N1', '199303-dk1.N1', '199303-dk2.N1', '199303-dk3.N1', '199304-dk1.N1', '199304-dk2.N1', '199304-dk3.N1', '199305-dk1.N1', '199305-dk2.N1', '199305-dk3.N1', '199306-dk1.N1', '199306-dk2.N1', '199306-dk3.N1', '199307-dk1.N1', '199307-dk2.N1', '199307-dk3.N1', '199308-dk1.N1', '199308-dk2.N1', '199308-dk3.N1', '199309-dk1.N1', '199309-dk2.N1', '199309-dk3.N1', '199310-dk1.N1', '199310-dk2.N1', '199310-dk3.N1', '199311-dk1.N1', '199311-dk2.N1', '199311-dk3.N1', '199312-dk1.N1', '199312-dk2.N1', '199312-dk3.N1', '199401-dk1.N1', '199401-dk2.N1', '199401-dk3.N1', '199402-dk1.N1', '199402-dk2.N1', '199402-dk3.N1', '199403-dk1.N1', '199403-dk2.N1', '199403-dk3.N1', '199404-dk1.N1', '199404-dk2.N1', '199404-dk3.N1', '199405-dk1.N1', '199405-dk2.N1', '199405-dk3.N1', '199406-dk1.N1', '199406-dk2.N1', '199406-dk3.N1', '199407-dk1.N1', '199407-dk2.N1', '199407-dk3.N1', '199408-dk1.N1', '199408-dk2.N1', '199408-dk3.N1', '199409-dk1.N1', '199409-dk2.N1', '199409-dk3.N1', '199410-dk1.N1', '199410-dk2.N1', '199410-dk3.N1', '199411-dk1.N1', '199411-dk2.N1', '199411-dk3.N1', '199412-dk1.N1', '199412-dk2.N1', '199412-dk3.N1', '199501-dk1.N14', '199501-dk2.N14', '199501-dk3.N14', '199502-dk1.N14', '199502-dk2.N14', '199502-dk3.N14', '199503-dk1.N14', '199503-dk2.N14', '199503-dk3.N14', '199504-dk1.N14', '199504-dk2.N14', '199504-dk3.N14', '199505-dk1.N14', '199505-dk2.N14', '199505-dk3.N14', '199506-dk1.N14', '199506-dk2.N14', '199506-dk3.N14', '199507-dk1.N14', '199507-dk2.N14', '199507-dk3.N14', '199508-dk1.N14', '199508-dk2.N14', '199508-dk3.N14', '199509-dk1.N14', '199509-dk2.N14', '199509-dk3.N14', '199510-dk1.N14', '199510-dk2.N14', '199510-dk3.N14', '199511-dk1.N14', '199511-dk2.N14', '199511-dk3.N14', '199512-dk1.N14', '199512-dk2.N14', '199512-dk3.N14', '199601-dk1.N14', '199601-dk2.N14', '199601-dk3.N14', '199602-dk1.N14', '199602-dk2.N14', '199602-dk3.N14', '199603-dk1.N14', '199603-dk2.N14', '199603-dk3.N14', '199604-dk1.N14', '199604-dk2.N14', '199604-dk3.N14', '199605-dk1.N14', '199605-dk2.N14', '199605-dk3.N14', '199606-dk1.N14', '199606-dk2.N14', '199606-dk3.N14', '199607-dk1.N14', '199607-dk2.N14', '199607-dk3.N14', '199608-dk1.N14', '199608-dk2.N14', '199608-dk3.N14', '199609-dk1.N14', '199609-dk2.N14', '199609-dk3.N14', '199610-dk1.N14', '199610-dk2.N14', '199610-dk3.N14', '199611-dk1.N14', '199611-dk2.N14', '199611-dk3.N14', '199612-dk1.N14', '199612-dk2.N14', '199612-dk3.N14', '199701-dk1.N14', '199701-dk2.N14', '199701-dk3.N14', '199702-dk1.N14', '199702-dk2.N14', '199702-dk3.N14', '199703-dk1.N14', '199703-dk2.N14', '199703-dk3.N14', '199704-dk1.N14', '199704-dk2.N14', '199704-dk3.N14', '199705-dk1.N14', '199705-dk2.N14', '199705-dk3.N14', '199706-dk1.N14', '199706-dk2.N14', '199706-dk3.N14', '199707-dk1.N14', '199707-dk2.N14', '199707-dk3.N14', '199708-dk1.N14', '199708-dk2.N14', '199708-dk3.N14', '199709-dk1.N14', '199709-dk2.N14', '199709-dk3.N14', '199710-dk1.N14', '199710-dk2.N14', '199710-dk3.N14', '199711-dk1.N14', '199711-dk2.N14', '199711-dk3.N14', '199712-dk1.N14', '199712-dk2.N14', '199712-dk3.N14', '199801-dk1.N14', '199801-dk2.N14', '199801-dk3.N14', '199802-dk1.N14', '199802-dk2.N14', '199802-dk3.N14', '199803-dk1.N14', '199803-dk2.N14', '199803-dk3.N14', '199804-dk1.N14', '199804-dk2.N14', '199804-dk3.N14', '199805-dk1.N14', '199805-dk2.N14', '199805-dk3.N14', '199806-dk1.N14', '199806-dk2.N14', '199806-dk3.N14', '199807-dk1.N14', '199807-dk2.N14', '199807-dk3.N14', '199808-dk1.N14', '199808-dk2.N14', '199808-dk3.N14', '199809-dk1.N14', '199809-dk2.N14', '199809-dk3.N14', '199810-dk1.N14', '199810-dk2.N14', '199810-dk3.N14', '199811-dk1.N14', '199811-dk2.N14', '199811-dk3.N14', '199812-dk1.N14', '199812-dk2.N14', '199812-dk3.N14', '199901-dk1.N14', '199901-dk2.N14', '199901-dk3.N14', '199902-dk1.N14', '199902-dk2.N14', '199902-dk3.N14', '199903-dk1.N14', '199903-dk2.N14', '199903-dk3.N14', '199904-dk1.N14', '199904-dk2.N14', '199904-dk3.N14', '199905-dk1.N14', '199905-dk2.N14', '199905-dk3.N14', '199906-dk1.N14', '199906-dk2.N14', '199906-dk3.N14', '199907-dk1.N14', '199907-dk2.N14', '199907-dk3.N14', '199908-dk1.N14', '199908-dk2.N14', '199908-dk3.N14', '199909-dk1.N14', '199909-dk2.N14', '199909-dk3.N14', '199910-dk1.N14', '199910-dk2.N14', '199910-dk3.N14', '199911-dk1.N14', '199911-dk2.N14', '199911-dk3.N14', '199912-dk1.N14', '199912-dk2.N14', '199912-dk3.N14', '200001-dk1.N14', '200001-dk2.N14', '200001-dk3.N14', '200002-dk1.N14', '200002-dk2.N14', '200002-dk3.N14', '200003-dk1.N14', '200003-dk2.N14', '200003-dk3.N14', '200004-dk1.N14', '200004-dk2.N14', '200004-dk3.N14', '200005-dk1.N14', '200005-dk2.N14', '200005-dk3.N14', '200006-dk1.N14', '200006-dk2.N14', '200006-dk3.N14', '200007-dk1.N14', '200007-dk2.N14', '200007-dk3.N14', '200008-dk1.N14', '200008-dk2.N14', '200008-dk3.N14', '200009-dk1.N14', '200009-dk2.N14', '200009-dk3.N14', '200010-dk1.N14', '200010-dk2.N14', '200010-dk3.N14', '200011-dk1.N16', '200011-dk2.N16', '200011-dk3.N16', '200012-dk1.N16', '200012-dk2.N16', '200012-dk3.N16', '200101-dk1.N16', '200101-dk2.N16', '200101-dk3.N16', '200102-dk1.N16', '200102-dk2.N16', '200102-dk3.N16', '200103-dk1.N16', '200103-dk2.N16', '200103-dk3.N16', '200104-dk1.N16', '200104-dk2.N16', '200104-dk3.N16', '200105-dk1.N16', '200105-dk2.N16', '200105-dk3.N16', '200106-dk1.N16', '200106-dk2.N16', '200106-dk3.N16', '200107-dk1.N16', '200107-dk2.N16', '200107-dk3.N16', '200108-dk1.N16', '200108-dk2.N16', '200108-dk3.N16', '200109-dk1.N16', '200109-dk2.N16', '200109-dk3.N16', '200110-dk1.N16', '200110-dk2.N16', '200110-dk3.N16', '200111-dk1.N16', '200111-dk2.N16', '200111-dk3.N16', '200112-dk1.N16', '200112-dk2.N16', '200112-dk3.N16', '200201-dk1.N16', '200201-dk2.N16', '200201-dk3.N16', '200202-dk1.N16', '200202-dk2.N16', '200202-dk3.N16', '200203-dk1.N16', '200203-dk2.N16', '200203-dk3.N16', '200204-dk1.N16', '200204-dk2.N16', '200204-dk3.N16', '200205-dk1.N16', '200205-dk2.N16', '200205-dk3.N16', '200206-dk1.N16', '200206-dk2.N16', '200206-dk3.N16', '200207-dk1.N16', '200207-dk2.N16', '200207-dk3.N16', '200208-dk1.N16', '200208-dk2.N16', '200208-dk3.N16', '200209-dk1.N16', '200209-dk2.N16', '200209-dk3.N16', '200210-dk1.N16', '200210-dk2.N16', '200210-dk3.N16', '200211-dk1.N16', '200211-dk2.N16', '200211-dk3.N16', '200212-dk1.N16', '200212-dk2.N16', '200212-dk3.N16', '200301-dk1.N16', '200301-dk2.N16', '200301-dk3.N16', '200302-dk1.N16', '200302-dk2.N16', '200302-dk3.N16', '200303-dk1.N16', '200303-dk2.N16', '200303-dk3.N16', '200304-dk1.N16', '200304-dk2.N16', '200304-dk3.N16', '200305-dk1.N16', '200305-dk2.N16', '200305-dk3.N16', '200306-dk1.N16', '200306-dk2.N16', '200306-dk3.N16', '200307-dk1.N16', '200307-dk2.N16', '200307-dk3.N16', '200308-dk1.N16', '200308-dk2.N16', '200308-dk3.N16', '200309-dk1.N16', '200309-dk2.N16', '200309-dk3.N16', '200310-dk1.N16', '200310-dk2.N16', '200310-dk3.N16', '200311-dk1.N16', '200311-dk2.N16', '200311-dk3.N16', '200312-dk1.N16', '200312-dk2.N16', '200312-dk3.N16', '200401-dk1.N16', '200401-dk2.N16', '200401-dk3.N16', '200402-dk1.N16', '200402-dk2.N16', '200402-dk3.N16', '200403-dk1.N16', '200403-dk2.N16', '200403-dk3.N16', '200404-dk1.N16', '200404-dk2.N16', '200404-dk3.N16', '200405-dk1.N16', '200405-dk2.N16', '200405-dk3.N16', '200406-dk1.N16', '200406-dk2.N16', '200406-dk3.N16', '200407-dk1.N16', '200407-dk2.N16', '200407-dk3.N16', '200408-dk1.N16', '200408-dk2.N16', '200408-dk3.N16', '200409-dk1.N16', '200409-dk2.N16', '200409-dk3.N16', '200410-dk1.N16', '200410-dk2.N16', '200410-dk3.N16', '200411-dk1.N16', '200411-dk2.N16', '200411-dk3.N16', '200412-dk1.N16', '200412-dk2.N16', '200412-dk3.N16', '200501-dk1.N16', '200501-dk2.N16', '200501-dk3.N16', '200502-dk1.N16', '200502-dk2.N16', '200502-dk3.N16', '200503-dk1.N16', '200503-dk2.N16', '200503-dk3.N16', '200504-dk1.N16', '200504-dk2.N16', '200504-dk3.N16', '200505-dk1.N16', '200505-dk2.N16', '200505-dk3.N16', '200506-dk1.N16', '200506-dk2.N16', '200506-dk3.N16', '200507-dk1.N16', '200507-dk2.N16', '200507-dk3.N16', '200508-dk1.N16', '200508-dk2.N16', '200508-dk3.N16', '200509-dk1.N16', '200509-dk2.N16', '200509-dk3.N16', '200510-dk1.N16', '200510-dk2.N16', '200510-dk3.N16', '200511-dk1.N16', '200511-dk2.N16', '200511-dk3.N16', '200512-dk1.N16', '200512-dk2.N16', '200512-dk3.N16', '200601-dk1.N16', '200601-dk2.N16', '200601-dk3.N16', '200602-dk1.N16', '200602-dk2.N16', '200602-dk3.N16', '200603-dk1.N16', '200603-dk2.N16', '200603-dk3.N16', '200604-dk1.N16', '200604-dk2.N16', '200604-dk3.N16', '200605-dk1.N16', '200605-dk2.N16', '200605-dk3.N16', '200606-dk1.N16', '200606-dk2.N16', '200606-dk3.N16', '200607-dk1.N16', '200607-dk2.N16', '200607-dk3.N16', '200608-dk1.N16', '200608-dk2.N16', '200608-dk3.N16', '200609-dk1.N16', '200609-dk2.N16', '200609-dk3.N16', '200610-dk1.N16', '200610-dk2.N16', '200610-dk3.N16', '200611-dk1.N16', '200611-dk2.N16', '200611-dk3.N16', '200612-dk1.N16', '200612-dk2.N16', '200612-dk3.N16', '200701-dk1.N16', '200701-dk2.N16', '200701-dk3.N16', '200702-dk1.N16', '200702-dk2.N16', '200702-dk3.N16', '200703-dk1.N16', '200703-dk2.N16', '200703-dk3.N16', '200704-dk1.N16', '200704-dk2.N16', '200704-dk3.N16', '200705-dk1.N16', '200705-dk2.N16', '200705-dk3.N16', '200706-dk1.N16', '200706-dk2.N16', '200706-dk3.N16', '200707-dk1.N16', '200707-dk2.N16', '200707-dk3.N16', '200708-dk1.N16', '200708-dk2.N16', '200708-dk3.N16', '200709-dk1.N16', '200709-dk2.N16', '200709-dk3.N16', '200710-dk1.N16', '200710-dk2.N16', '200710-dk3.N16', '200711-dk1.N16', '200711-dk2.N16', '200711-dk3.N16', '200712-dk1.N16', '200712-dk2.N16', '200712-dk3.N16', '200801-dk1.N18', '200801-dk2.N18', '200801-dk3.N18', '200802-dk1.N18', '200802-dk2.N18', '200802-dk3.N18', '200803-dk1.N18', '200803-dk2.N18', '200803-dk3.N18', '200804-dk1.N18', '200804-dk2.N18', '200804-dk3.N18', '200805-dk1.N18', '200805-dk2.N18', '200805-dk3.N18', '200806-dk1.N18', '200806-dk2.N18', '200806-dk3.N18', '200807-dk1.N18', '200807-dk2.N18', '200807-dk3.N18', '200808-dk1.N18', '200808-dk2.N18', '200808-dk3.N18', '200809-dk1.N18', '200809-dk2.N18', '200809-dk3.N18', '200810-dk1.N18', '200810-dk2.N18', '200810-dk3.N18', '200811-dk1.N18', '200811-dk2.N18', '200811-dk3.N18', '200812-dk1.N18', '200812-dk2.N18', '200812-dk3.N18', '200901-dk1.N18', '200901-dk2.N18', '200901-dk3.N18', '200902-dk1.N18', '200902-dk2.N18', '200902-dk3.N18', '200903-dk1.N18', '200903-dk2.N18', '200903-dk3.N18', '200904-dk1.N18', '200904-dk2.N18', '200904-dk3.N18', '200905-dk1.N18', '200905-dk2.N18', '200905-dk3.N18', '200906-dk1.N18', '200906-dk2.N18', '200906-dk3.N18', '200907-dk1.N18', '200907-dk2.N18', '200907-dk3.N18', '200908-dk1.N18', '200908-dk2.N18', '200908-dk3.N18', '200909-dk1.N18', '200909-dk2.N18', '200909-dk3.N18', '200910-dk1.N18', '200910-dk2.N18', '200910-dk3.N18', '200911-dk1.N18', '200911-dk2.N18', '200911-dk3.N18', '200912-dk1.N18', '200912-dk2.N18', '200912-dk3.N18', '201001-dk1.N19', '201001-dk2.N19', '201001-dk3.N19', '201002-dk1.N19', '201002-dk2.N19', '201002-dk3.N19', '201003-dk1.N19', '201003-dk2.N19', '201003-dk3.N19', '201004-dk1.N19', '201004-dk2.N19', '201004-dk3.N19', '201005-dk1.N19', '201005-dk2.N19', '201005-dk3.N19', '201006-dk1.N19', '201006-dk2.N19', '201006-dk3.N19', '201007-dk1.N19', '201007-dk2.N19', '201007-dk3.N19', '201008-dk1.N19', '201008-dk2.N19', '201008-dk3.N19', '201009-dk1.N19', '201009-dk2.N19', '201009-dk3.N19', '201010-dk1.N19', '201010-dk2.N19', '201010-dk3.N19', '201011-dk1.N19', '201011-dk2.N19', '201011-dk3.N19', '201012-dk1.N19', '201012-dk2.N19', '201012-dk3.N19', '201101-dk1.N19', '201101-dk2.N19', '201101-dk3.N19', '201102-dk1.N19', '201102-dk2.N19', '201102-dk3.N19', '201103-dk1.N19', '201103-dk2.N19', '201103-dk3.N19', '201104-dk1.N19', '201104-dk2.N19', '201104-dk3.N19', '201105-dk1.N19', '201105-dk2.N19', '201105-dk3.N19', '201106-dk1.N19', '201106-dk2.N19', '201106-dk3.N19', '201107-dk1.N19', '201107-dk2.N19', '201107-dk3.N19', '201108-dk1.N19', '201108-dk2.N19', '201108-dk3.N19', '201109-dk1.N19', '201109-dk2.N19', '201109-dk3.N19', '201110-dk1.N19', '201110-dk2.N19', '201110-dk3.N19', '201111-dk1.N19', '201111-dk2.N19', '201111-dk3.N19', '201112-dk1.N19', '201112-dk2.N19', '201112-dk3.N19', '201201-dk1.N19', '201201-dk2.N19', '201201-dk3.N19', '201202-dk1.N19', '201202-dk2.N19', '201202-dk3.N19', '201203-dk1.N19', '201203-dk2.N19', '201203-dk3.N19', '201204-dk1.N19', '201204-dk2.N19', '201204-dk3.N19', '201205-dk1.N19', '201205-dk2.N19', '201205-dk3.N19', '201206-dk1.N19', '201206-dk2.N19', '201206-dk3.N19', '201207-dk1.N19', '201207-dk2.N19', '201207-dk3.N19', '201208-dk1.N19', '201208-dk2.N19', '201208-dk3.N19', '201209-dk1.N19', '201209-dk2.N19', '201209-dk3.N19', '201210-dk1.N19', '201210-dk2.N19', '201210-dk3.N19', '201211-dk1.N19', '201211-dk2.N19', '201211-dk3.N19', '201212-dk1.N19', '201212-dk2.N19', '201212-dk3.N19', '201301-dk1.N19', '201301-dk2.N19', '201301-dk3.N19', '201302-dk1.N19', '201302-dk2.N19', '201302-dk3.N19', '201303-dk1.N19', '201303-dk2.N19', '201303-dk3.N19', '201304-dk1.N19', '201304-dk2.N19', '201304-dk3.N19', '201305-dk1.N19', '201305-dk2.N19', '201305-dk3.N19', '201306-dk1.N19', '201306-dk2.N19', '201306-dk3.N19', '201307-dk1.N19', '201307-dk2.N19', '201307-dk3.N19', '201308-dk1.N19', '201308-dk2.N19', '201308-dk3.N19', '201309-dk1.N19', '201309-dk2.N19', '201309-dk3.N19', '201310-dk1.N19', '201310-dk2.N19', '201310-dk3.N19', '201311-dk1.N19', '201311-dk2.N19', '201311-dk3.N19', '201312-dk1.N19', '201312-dk2.N19', '201312-dk3.N19', '201401-dk1.N19', '201401-dk2.N19', '201401-dk3.N19', '201402-dk1.N19', '201402-dk2.N19', '201402-dk3.N19', '201403-dk1.N19', '201403-dk2.N19', '201403-dk3.N19', '201404-dk1.N19', '201404-dk2.N19', '201404-dk3.N19', '201405-dk1.N19', '201405-dk2.N19', '201405-dk3.N19', '201406-dk1.N19', '201406-dk2.N19', '201406-dk3.N19', '201407-dk1.N19', '201407-dk2.N19', '201407-dk3.N19', '201408-dk1.N19', '201408-dk2.N19', '201408-dk3.N19', '201409-dk1.N19', '201409-dk2.N19', '201409-dk3.N19', '201410-dk1.N19', '201410-dk2.N19', '201410-dk3.N19', '201411-dk1.N19', '201411-dk2.N19', '201411-dk3.N19', '201412-dk1.N19', '201412-dk2.N19', '201412-dk3.N19', '201501-dk1.N19', '201501-dk2.N19', '201501-dk3.N19', '201502-dk1.N19', '201502-dk2.N19', '201502-dk3.N19', '201503-dk1.N19', '201503-dk2.N19', '201503-dk3.N19', '201504-dk1.N19', '201504-dk2.N19', '201504-dk3.N19', '201505-dk1.N19', '201505-dk2.N19', '201505-dk3.N19', '201506-dk1.N19', '201506-dk2.N19', '201506-dk3.N19', '201507-dk1.N19', '201507-dk2.N19', '201507-dk3.N19', '201508-dk1.N19', '201508-dk2.N19', '201508-dk3.N19', '201509-dk1.N19', '201509-dk2.N19', '201509-dk3.N19', '201510-dk1.N19', '201510-dk2.N19', '201510-dk3.N19', '201511-dk1.N19', '201511-dk2.N19', '201511-dk3.N19', '201512-dk1.N19', '201512-dk2.N19', '201512-dk3.N19', '201601-dk1.N19', '201601-dk2.N19', '201601-dk3.N19', '201602-dk1.N19', '201602-dk2.N19', '201602-dk3.N19', '201603-dk1.N19', '201603-dk2.N19', '201603-dk3.N19', '201604-dk1.N19', '201604-dk2.N19', '201604-dk3.N19', '201605-dk1.N19', '201605-dk2.N19', '201605-dk3.N19', '201606-dk1.N19', '201606-dk2.N19', '201606-dk3.N19', '201607-dk1.N19', '201607-dk2.N19', '201607-dk3.N19', '201608-dk1.N19', '201608-dk2.N19', '201608-dk3.N19', '201609-dk1.N19', '201609-dk2.N19', '201609-dk3.N19', '201610-dk1.N19', '201610-dk2.N19', '201610-dk3.N19', '201611-dk1.N19', '201611-dk2.N19', '201611-dk3.N19', '201612-dk1.N19', '201612-dk2.N19', '201612-dk3.N19', '201701-dk1.N19', '201701-dk2.N19', '201701-dk3.N19', '201702-dk1.N19', '201702-dk2.N19', '201702-dk3.N19', '201703-dk1.N19', '201703-dk2.N19', '201703-dk3.N19', '201704-dk1.N19', '201704-dk2.N19', '201704-dk3.N19', '201705-dk1.N19', '201705-dk2.N19', '201705-dk3.N19', '201706-dk1.N19', '201706-dk2.N19', '201706-dk3.N19', '201707-dk1.N19', '201707-dk2.N19', '201707-dk3.N19', '201708-dk1.N19', '201708-dk2.N19', '201708-dk3.N19', '201709-dk1.N19', '201709-dk2.N19', '201709-dk3.N19', '201710-dk1.N19', '201710-dk2.N19', '201710-dk3.N19', '201711-dk1.N19', '201711-dk2.N19', '201711-dk3.N19', '201712-dk1.N19', '201712-dk2.N19', '201712-dk3.N19', '201801-dk1.N19', '201801-dk2.N19', '201801-dk3.N19', '201802-dk1.N19', '201802-dk2.N19', '201802-dk3.N19', '201803-dk1.N19', '201803-dk2.N19', '201803-dk3.N19', '201804-dk1.N19', '201804-dk2.N19', '201804-dk3.N19', '201805-dk1.N19', '201805-dk2.N19', '201805-dk3.N19', '201806-dk1.N19', '201806-dk2.N19', '201806-dk3.N19', '201807-dk1.N19']

;dekadList = FIX(STRMID(FILE_BASENAME(fileList),17,2)+STRMID(FILE_BASENAME(fileList),22,1))
;dekadList = FIX(STRMID(FILE_BASENAME(bandList),17,2)+STRMID(FILE_BASENAME(bandList),22,1))
dekadList = [63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 101, 102, 103, 111, 112, 113, 121, 122, 123, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 62, 63, 71]
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ;debug
  ;line = 1403
  data = assData[line]
  scrNDVI=INTARR(ns,nTimePeriods)+noDataValue               ; line of Savitzky-Golay smoothed NDVI
  FOR sample=0, ns-1, 1L DO BEGIN
    ;debug
    ;sample = 3767
    dataSample = REFORM(data[sample,*])
    ; apply a cloud filter function that should 1) remove outliers, 2) apply threshold
    dataSample = cloudFilter_v1(dataSample,noDataValue,dekadList,threshold,medianDifThr)
    
    scrNDVI[sample,*] = dataSample
;    ; perform Savitzy-Golay filtering (after a few checks)
;    ind1 = WHERE(dataSample EQ noDataValue, CNT)
;    IF CNT LT nTimePeriods*0.75 THEN BEGIN  ; this means that if missing values is 25% or more, output is noDataValue
;      sago_interpol, dataSample, noDataValue, smNDVI=tmp2
;      sgNDVI[sample,*] = tmp2
;    ENDIF
  ENDFOR
  WRITEU, W1, scrNDVI
  
ENDFOR

CLOSE, /ALL


; WRITE HEADER OF THE OUTPUT
HEADER_OUT=STRMID(outFile,0,STRLEN(outFile)-3)+'hdr'  ;here diffrent if I dont use .bil
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {'
printf,3,'  Screened 10-DAY composites from LTDR AVHRR V5 (screened for outliers like clouds or erroneous high values}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nTimePeriods)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'sensor type = AVHRR'
printf,3,'byte order = 0'
printf,3,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -180.0, 83.0, 0.05, 0.05, WGS-84, units=Degrees}'
printf,3,'band names = {'+STRJOIN(bandList,', ')+'}'
CLOSE, 3


; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED SCREENING AVHRR LTDR DATA'

END ;Procedure SAGO_WRAPPER

