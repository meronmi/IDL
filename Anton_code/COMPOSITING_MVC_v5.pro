; NAME:
;   COMPOSITING
;
; PURPOSE:
;   COMPOSITE DAILY AVHRR FROM LTDR INTO 10-DAY PRODUCTS  --> VERSION 5
;     USING A CONSTRAINT VIEW ANGLE MAXIMUM VALUE COMPOSITE (CVMVC) LIKE IN http://modis.gsfc.nasa.gov/data/atbd/atbd_mod13.pdf
;            Land pixels with clouds, shadow, and bad data integrity will be excluded from the VI composite.
;            
;            The NDVI is computed for the observations with the two smallest view zenith
;            angles, after which the MVC rule is applied and the most optimal observation selected.
;            This will be referred to as the constraint view angle maximum value composite (CVMVC)
;            approach. If only one cloud-free observation is available over the composite
;            period, this observation will automatically be selected to represent the composite
;            period. If all data during a 16-day period were affected by clouds, based on the cloud
;            flags, the view angle constraint for the MVC method will be released and the VI will be
;            calculated for all days and the best pixel chosen based on the maximum value of the
;            NDVI among all observations.
;  
;            
;            ALTERNATIVE TO TRY!!!!
;            - set to no data all pixels that only have water in all 10-days (only for this use quality bits for now)
;            - perform MVC (adapted)
;                 - get the highest two NDVI values
;                 - look at the one with lower vzen value --> evaluate if this is not less than 90% of the value with higher vzen
;                        - if not --> take the one with lower vzen
;                        - if yes --> take the one with higher vzen and higher NDVI
;            
;            
;            
; INPUTS:
;   DAILY AVHRR LTDR FILES (NDVI AND SURFACE REFLECTANCE)
;
; USAGE:
;   VGT_READ
;
; OUTPUT:
;   10-day NDVI images
;
; NOTES:
;
;
; MODIFICATION HISTORY:
;   Written by:  Anton Vrieling, December 2015


;*************************************************************************************************************************

FUNCTION READ_HDF, INFILE, SDS_VAR        ; Function to read data from HDF given the Input HDF file and the SDS_VAR name

hdfid = hdf_sd_start(INFILE)                        ; open a HDF file (containing data)
index = hdf_sd_nametoindex(hdfid, SDS_VAR)          ; obtain data from an SDS
varid = hdf_sd_select(hdfid, index)
hdf_sd_getdata, varid, data
hdf_sd_endaccess, varid
hdf_sd_end, hdfid
return, data

END ; FUNCTION READ_HDF

;*************************************************************************************************************************

FUNCTION DOYtoJulian, doy, year, refDate 
; get Julian date in IDL-reference (accounting for reference date)
  julian = JULDAY(1,1,year)+doy-1 - refDate
  return, julian
END ; FUNCTION READ_HDF

;*************************************************************************************************************************

function pixelCompositeMVC, ndviSeries, qualSeries, vzenSeries, julianList, noDataValue
  ; create composite and corresponding julian date from ndvi and vzen series
  ; also create flags for NDVI that are written to the last digit
  ; flag 0 = only a single valid value existed in the 10-day window
  ; flag 1 = first maximum selected
  ; flag 2 = second maximum selected
  ; flag-value + 4 if brdf-issues were identified...
  output= {$
    NDVI: noDataValue, $                      ; set standard to -9999
    JulDate: noDataValue}
  flag = 0
  julianSeries = julianList       ; do this first to avoid that the function changes the values of julianList
  ; first case 
  IF N_ELEMENTS(ndviSeries) EQ 1 AND ndviSeries[0] GT 0 THEN BEGIN
    output.NDVI = ndviSeries[0]
    output.JulDate = julianSeries[0]
    IF qualImages[0] EQ 2 THEN flag=flag+4
  ENDIF
  IF N_ELEMENTS(ndviSeries) GT 1 AND max(ndviSeries) GT 0 THEN BEGIN
    index = REVERSE(SORT(ndviSeries))           ; get an index of highest first
    ndviSeries = ndviSeries[index[0:1]]         ; get the highest two NDVI values
    julianSeries = julianSeries[index[0:1]]     ; get the corresponding julian date values
    vzenSeries = ABS(vzenSeries[index[0:1]])    ; get the corresponding absolute vzen values
    qualSeries = qualSeries[index[0:1]]
    ; first assign standard case (take the highest NDVI)
    output.NDVI = ndviSeries[0]
    output.JulDate = julianSeries[0]
    IF ndviSeries[1] NE noDataValue THEN flag=1           ; if second would be noDataValue, then flag remains 0
    ; now check if we need to adapt because second value is similar (within 90 percent of highest NDVI) and has lower absolute vzen
    IF vzenSeries[1] LE vzenSeries[0] AND ndviSeries[1] GE 0.90*ndviSeries[0] THEN BEGIN    ; we want smallest VZEN
      output.NDVI = ndviSeries[1]
      output.JulDate = julianSeries[1]
      flag=2
    ENDIF 
    ; update flags in case of BRDF-issues. Note that we do not take into account the brdf in our selection criteria, but just update in flag...
    IF flag le 1 AND qualSeries[0] EQ 2 THEN flag = flag+4
    IF flag eq 2 and qualSeries[1] EQ 2 THEN flag = flag+4
  ENDIF
  ; incorporate flags in NDVI value
  IF output.NDVI NE noDataValue THEN output.NDVI = 10*ROUND(output.NDVI/10.)+flag
 
  return, output
end

;*************************************************************************************************************************

PRO CREATE_HEADER, outFile, nlong, nlat, dataTypeCode, bandNameString     ; Procedure to generate generic header for each output
HEADER_OUT=STRMID(outFile,0,STRLEN(outFile)-3)+'hdr'
IF FILE_TEST(HEADER_OUT) eq 1 THEN FILE_DELETE, HEADER_OUT
OPENW,3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {'
printf,3,'  '+'Generic ENVI-header for '+FILE_BASENAME(outFile)+'}'
printf,3,'samples ='+STRCOMPRESS(nlong)
printf,3,'lines   ='+STRCOMPRESS(nlat)
printf,3,'bands   = 1'
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = '+STRTRIM(dataTypeCode,2)
printf,3,'interleave = bsq'
printf,3,'sensor type = AVHRR'
printf,3,'byte order = 0'
printf,3,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -180.0, 90.0, 0.05, 0.05, WGS-84, units=Degrees}'
printf,3,'band names = {'+bandNameString+'}'
CLOSE,3
END ; CREATE_HEADER

;*************************************************************************************************************************


PRO COMPOSITING_MVC_v5

STARTTIME = SYSTIME(1)

; First set general attributes
dataPath = 'R:\LTDR\V5\'
workPath = 'G:\IMAGES\LTDR\dekadMVC\'
IF FILE_TEST(workPath) eq 0 THEN FILE_MKDIR, workPath
nlong = 7200                                   ; number of samples in longitude direction
nlat  = 3600                                   ; number of lines is latitude direction
noDataValue = -9999s
startYear = 2017
endYear = 2018
referenceDate = JULDAY(12,31,1980)             ; reference date to be used for writing date output (i.e. JULDAY minus referenceDate): 1 corresponds to 1 January 1981
bitsOfInterest = [1,2,3,4,6,8,9,14]            ; bits of interest in quality file, see http://ltdr.nascom.nasa.gov/ltdr/docs/AVHRR_LTDR_V4_Document.pdf
bitsOfInterest = [1,2,3,6,8,9]            ; bits of interest in quality file, see http://ltdr.nascom.nasa.gov/ltdr/docs/AVHRR_LTDR_V4_Document.pdf, for selection bits, see: http://cordis.europa.eu/result/rcn/58544_en.html (under task 3.5)
bitsOfInterest = 3
bitsOfInterest = FIX(TOTAL(2^bitsOfInterest))

FOR year=startYear,endYear,1L DO BEGIN
  dateList=STRMID('0'+STRTRIM(INDGEN(36)/3S+1, 2), 1,2, /REVERSE_OFFSET)+'-dk'+STRTRIM(REFORM(TRANSPOSE(REFORM(REBIN(INDGEN(3)+1,36),12,3)),36),2)
  compositeStartJulianDates = [1,11,21,32,42,52,60,70,80,91,101,111,121,131,141,152,162,172,182,192,202,213,223,233,244,254,264,274,284,294,305,315,325,335,345,355,367] ; last number added as endDate
  IF year MOD 4 EQ 0 THEN compositeStartJulianDates[6:*]=compositeStartJulianDates[6:*]+1
  compositeNameList = STRTRIM(year,2)+dateList
  IF year EQ 1981 THEN BEGIN
    compositeStartJulianDates = compositeStartJulianDates[17:*]
    dateList=dateList[17:*]
  ENDIF
  ;; NOTE = TEMPORARY, IN ORDER TO RESUME AFTER PROBLEM WITH DATA FILE!!!!
  IF year EQ 2004 THEN BEGIN
    compositeStartJulianDates = compositeStartJulianDates[16:*]
    dateList=dateList[16:*]
  ENDIF
  srFileList = FILE_SEARCH(dataPath+'AVH09C1\N??\'+STRTRIM(year,2)+'*\AVH09C1.A'+STRTRIM(year,2)+'*.hdf')
  doyList = FIX(STRMID(FILE_BASENAME(srFileList),13,3))
    
  FOR dekad=0, N_ELEMENTS(compositeStartJulianDates)-2, 1L DO BEGIN
    dateString=STRTRIM(year,2)+dateList[dekad]
    ; initiate output files for writing (also write if no data at all exists for consistent time series)
    mvcImage = MAKE_ARRAY(nlong, nlat, /INTEGER, VALUE=noDataValue)
    dateImage = MAKE_ARRAY(nlong, nlat, /INTEGER, VALUE=noDataValue)
    ; get a subset of the relevant files
    ind = WHERE(doyList GE compositeStartJulianDates[dekad] AND doyList LT compositeStartJulianDates[dekad+1])
    IF ind[0] NE -1 THEN BEGIN
      srFiles  =srFileList[ind]
      julianList = DOYtoJulian(doyList[ind],year,referenceDate)
      ndviImages = INTARR(nlong,nlat,N_ELEMENTS(srFiles))+noDataValue
      qualImages = BYTARR(nlong,nlat,N_ELEMENTS(srFiles))          ; store here values 1 for water (always), 2 for brdf issues (if no water was identified)
      vzenImages = INTARR(nlong,nlat,N_ELEMENTS(srFiles))+noDataValue
      ; iteratively open all files within composite period & write to stack for analysis
      FOR t=0,N_ELEMENTS(srFiles)-1,1L DO BEGIN
        ; open and save quality image to a file where 0=good quality, 1=water, 2=brdf issues
        print, srFiles[t]
        qualImage = READ_HDF(srFiles[t],'QA')
        index1 = WHERE((qualImage AND 2^14) NE 0)            ; this is to test for BRDF
        index2 = WHERE((qualImage AND bitsOfInterest) NE 0)  ; this tests for water
        qualImage = BYTARR(nlong,nlat)
        IF index1[0] NE -1 THEN qualImage[index1]=2B      ; brdf issues
        IF index2[0] NE -1 THEN qualImage[index2]=1B      ; water

        qualImages[*,*,t] = qualImage
        
        ; open surface reflectance images and calculate NDVI
        srefl1Image = READ_HDF(srFiles[t],'SREFL_CH1')
        srefl2Image = READ_HDF(srFiles[t],'SREFL_CH2')
        ndviImage = 0s*srefl1Image + noDataValue
        index = WHERE(srefl1Image GE 0 AND srefl2Image GE 0 AND qualImage NE 1 AND srefl2Image LT 10000 AND srefl1Image LT 10000)     ; those with brdf issues continue...
        ndviImage[index] = FIX(10000*FLOAT(srefl2Image[index]-srefl1Image[index])/(srefl2Image[index]+srefl1Image[index]))
        ndviImages[*,*,t] = ndviImage
        
        ; open the view zenith angle (VZA) and write to file
        vzenImages[*,*,t] = READ_HDF(srFiles[t],'VZEN')
      ENDFOR
      ; now loop over pixels to evaluate best observation in time period & write date + NDVI to files
      FOR i=0,nLong-1,1L DO BEGIN
        FOR j=0,nLat-1,1L DO BEGIN
          ndviPixelSeries = REFORM(ndviImages[i,j,*])
          qualPixelSeries = REFORM(qualImages[i,j,*])
          vzenPixelSeries = REFORM(vzenImages[i,j,*])
          b=JulianList
          result = pixelCompositeMVC(ndviPixelSeries,qualPixelSeries,vzenPixelSeries,julianList,noDataValue)
          mvcImage[i,j,*]=result.NDVI
          dateImage[i,j,*]=result.JulDate
        ENDFOR
      ENDFOR
    ENDIF
    ; write the NDVI composite and the JulianDate file
    IF ind[0] NE -1 THEN outfileNDVI = workPath + 'AVH13C10.NDVI'+dateString+STRMID(FILE_BASENAME(srFiles[0]),16,9)+'img' ELSE $
                         outfileNDVI = workPath + 'AVH13C10.NDVI'+dateString+'.XXX.XXX.'+'img'
    IF ind[0] NE -1 THEN outfileDate = workPath + 'AVH13C10.date'+dateString+STRMID(FILE_BASENAME(srFiles[0]),16,9)+'img' ELSE $
                         outfileDate = workPath + 'AVH13C10.date'+dateString+'.XXX.XXX.'+'img'
    IF FILE_TEST(outFileNDVI) eq 1 THEN FILE_DELETE, outFileNDVI
    OPENW, W1, outFileNDVI, /GET_LUN
    WRITEU, W1, mvcImage
    FREE_LUN, W1
    CLOSE, W1
    CREATE_HEADER, outFileNDVI, nlong, nlat, 2, 'NDVI'
    IF FILE_TEST(outFileDate) eq 1 THEN FILE_DELETE, outFileDate
    OPENW, W1, outFileDate, /GET_LUN
    WRITEU, W1, dateImage
    FREE_LUN, W1
    CLOSE, W1
    CREATE_HEADER, outFileDate, nlong, nlat, 2, 'JulDate'
  ENDFOR

ENDFOR




;*************************************************************************************************************************
; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
PRINT, 'FINISHED COMPOSITING AVHRR LTDR FILES'

END ; {PROCEDURE VGT_READ.pro}

;*************************************************************************************************************************
