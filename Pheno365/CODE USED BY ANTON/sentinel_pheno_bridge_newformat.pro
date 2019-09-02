PRO Sentinel_pheno_sequential_caller_newFormat, fn_sav_s1, fn_sav_s2
;with this procedure pheno is called sequentially without bridges

RESTORE, fn_sav_s1
;RESTORE, fn_sav_s2 ;not needed here
cropTypes = s1.crop.Uniq()
s1 = 0
;s2 = 0
ncrops = N_ELEMENTS(cropTypes)
c = 0
PRINT, 'n unique crops = ', STRING(cropTypes.LENGTH)
selected_crops = cropTypes
;put common wheat first for easy debug
selected_crops = [selected_crops[3], selected_crops[0:2], selected_crops[4:-1]]
PRINT, selected_crops
use_time_constrains_per_crop = 1     ;set it to 1 to have time period (used by locate_largest_season_in_jd_range_v2) per crop
;time constrain per crop if required
IF (use_time_constrains_per_crop EQ 1) THEN BEGIN
  fn = FILE_DIRNAME(fn_sav_s1) + '\time_constrains_per_crop_v3.csv'
  res = READ_CSV(fn, HEADER=hdr)
  timeRangesPerCrop = rename_tags(res, TAG_NAMES(res), hdr)
  fn_timeConstrains = FILE_DIRNAME(fn) + '\' +FILE_BASENAME(fn, '.csv') + '.sav';STRSPLIT(csv_file, '.', /EXTRACT)
  SAVE, timeRangesPerCrop, FILENAME = fn_timeConstrains
ENDIF ELSE fn_timeConstrains = ''

stime = JD2DDMMYYYY(SYSTIME(/JULIAN))
stimeStr = STRTRIM(stime[2],2) + '_' + STRTRIM(stime[1],2) + '_' + STRTRIM(stime[0],2)
dir_out = FILE_DIRNAME(fn_sav_s1) + '\Results_' + stimeStr
dir_out = FILE_DIRNAME(fn_sav_s1) + '\Sequential_caller_results'
IF FILE_SEARCH(dir_out) EQ '' THEN BEGIN
  FILE_MKDIR, dir_out
ENDIF ELSE BEGIN
  PRINT, 'Directory already exists, the program will overwrite it'
  FILE_MKDIR, dir_out
ENDELSE
dir_out_graphs = dir_out + '\graphs'
FILE_MKDIR, dir_out_graphs
FOR i = 0, N_ELEMENTS(selected_crops)-1 DO Sentinel_pheno_bridge_newFormat, selected_crops[i], i, fn_sav_s1, fn_sav_s2, dir_out, dir_out_graphs, use_time_constrains_per_crop, fn_timeConstrains
END


FUNCTION Sentinel_pheno_bridge_caller_newFormat, fn_sav_s1, fn_sav_s2
;fn_sav = 'D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190520_v3.sav'
nPro = 18                                         ;number of process, up to a maximum number equal the number of virtual processor cores
nPerChunk = 150
use_time_constrains_per_crop = 1     ;set it to 1 to have time period (used by locate_largest_season_in_jd_range_v2) per crop
;time constrain per crop if required
IF (use_time_constrains_per_crop EQ 1) THEN BEGIN
  fn = FILE_DIRNAME(fn_sav_s1) + '\time_constrains_per_crop_v3.csv'
  res = READ_CSV(fn, HEADER=hdr)
  timeRangesPerCrop = rename_tags(res, TAG_NAMES(res), hdr)
  fn_timeConstrains = FILE_DIRNAME(fn) + '\' +FILE_BASENAME(fn, '.csv') + '.sav';STRSPLIT(csv_file, '.', /EXTRACT)
  SAVE, timeRangesPerCrop, FILENAME = fn_timeConstrains
ENDIF ELSE fn_timeConstrains = ''



RESTORE, fn_sav_s1
cropTypes = s1.crop[UNIQ(s1.crop,SORT(s1.crop))]
PRINT, 'n unique crops = ', STRING(N_ELEMENTS(cropTypes))
selected_crops = cropTypes
;put common wheat first for easy debug
selected_crops = [selected_crops[3], selected_crops[0:2], selected_crops[4:-1]]
PRINT, selected_crops
;split big crops in chunks
splitted_crops = !NULL

PRINT, 'SPLITTING BIG CROP GROUPS'
FOR i = 0, N_ELEMENTS(selected_crops) -1 DO BEGIN
  indCrop = WHERE(s1.crop EQ selected_crops[i], countCrop)
  tmp = s1.pointID[indCrop]
  uniqIds = tmp[UNIQ(tmp, SORT(tmp))]
  nUniqIds = N_ELEMENTS(uniqIds)
  IF (nUniqIds GT nPerChunk) THEN BEGIN
    ;make n chunks of about nPerChunk (300)
    nc = CEIL(nUniqIds/FLOAT(nPerChunk))
    FOR j = 0, nc-1 DO BEGIN
      IF (j EQ (nc-1)) THEN BEGIN ;we are at the last chunk that may be incomplete 
        splitted_crops = [splitted_crops, selected_crops[i] + '<' + STRTRIM(j*nPerChunk,2) + ':' + STRTRIM(nUniqIds-1,2) + '>'] 
      ENDIF ELSE BEGIN
        splitted_crops = [splitted_crops, selected_crops[i] + '<' + STRTRIM(j*nPerChunk,2) + ':' + STRTRIM((j+1)*nPerChunk-1,2) + '>'] 
      ENDELSE
      PRINT, splitted_crops[-1]
    ENDFOR
  ENDIF ELSE BEGIN
    splitted_crops = [splitted_crops, selected_crops[i]]
  ENDELSE
ENDFOR
y = 0
ncrops = N_ELEMENTS(splitted_crops)


;Set up of IDL bridge

;c=7 ;floric only 2 samples ok for debug
parallelProName = 'Sentinel_pheno_bridge_newFormat'     ;name of the function to be executed in parallel.
                                                ;IMPORTANT: this function MUST be positioned in the IDL path (Window-Preferences IDL-Paths) with all the required functions
;Some housekeeping for the bridges
oIDLBridgeArray = OBJARR(nPro)      ;array of Bridge objects, one object = one child process that can be executed
status = LONARR(nPro)               ;array for storing the status of the childs
ErrorString=STRARR(nPro)            ;array for storing potential problems of the child
inactive = BYTARR(nPro)             ;array to store the lifecycle of each child, it is turned to 1 when no more results are expected from this child
lastAssigment = STRARR(nPro)        ;array for storing the last crop assigned
stime = JD2DDMMYYYY(SYSTIME(/JULIAN))
stimeStr = STRTRIM(stime[2],2) + '_' + STRTRIM(stime[1],2) + '_' + STRTRIM(stime[0],2)
dir_out = FILE_DIRNAME(fn_sav_s1) + '\Results_' + stimeStr
IF FILE_SEARCH(dir_out) EQ '' THEN BEGIN
  FILE_MKDIR, dir_out
ENDIF ELSE BEGIN
  PRINT, 'Directory already exists, the program will overwrite it'
  FILE_MKDIR, dir_out
ENDELSE
;dir to output graphs
dir_out_graphs = dir_out + '\graphs'
FILE_MKDIR, dir_out_graphs
;for debug
;Sentinel_pheno_bridge, splitted_crops[9], 9, fn_sav, dir_out, dir_out_graphs, use_time_constrains_per_crop, fn_timeConstrains
;Initialize childs
FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i] = OBJ_NEW("IDL_IDLBridge", OUTPUT=dir_out+'\out_'+STRTRIM(i,2)+'.txt')
  IF (~OBJ_VALID(oIDLBridgeArray[i])) THEN BEGIN
    void=DIALOG_MESSAGE(/ERROR,'Unable to create an IDL_IDLBridge session')
    WIDGET_CONTROL, wWrapper, /DESTROY
    RETURN, -9999
  ENDIF
  PRINT, 'Child process ' + STRTRIM(i,2) + ' was initialized'
ENDFOR
tt=systime(/sec)

;Start by assigning a a crop to each child
c = 0
FOR i = 0, nPro-1 DO BEGIN
  PRINT, 'Child ' + STRTRIM(i, 2) + ': assigned to ' + splitted_crops[c]
  lastAssigment[i] = splitted_crops[c]
  oIDLBridgeArray[i]->SetVar, 'crop2process', splitted_crops[c]
  oIDLBridgeArray[i]->SetVar, 'c', c
  oIDLBridgeArray[i]->SetVar, 'fn_sav_s1', fn_sav_s1
  oIDLBridgeArray[i]->SetVar, 'fn_sav_s2', fn_sav_s2
  oIDLBridgeArray[i]->SetVar, 'dir_out', dir_out
  oIDLBridgeArray[i]->SetVar, 'dir_out_graphs', dir_out_graphs
  oIDLBridgeArray[i]->SetVar, 'use_time_constrains_per_crop', use_time_constrains_per_crop
  oIDLBridgeArray[i]->SetVar, 'fn_timeConstrains', fn_timeConstrains
  oIDLBridgeArray[i]->Execute, parallelProName + ', crop2process, c, fn_sav_s1, fn_sav_s2, dir_out, dir_out_graphs, use_time_constrains_per_crop, fn_timeConstrains', /NOWAIT
  c = c + 1 ;get reday for the next crop type
ENDFOR



eop = 0     ;end of processes: it turns 1 when there are no more processes to be assigned to a child (but childs may be running)
eor = 0     ;end of results: it turns 1 when eop = 1 (nothing to assign) and the results were gathered for all child, so exit the loop

;now test who is idle and let him process a new line, in the test loop take care of eor and eop conditions
WHILE eor EQ 0 DO BEGIN
  ;get the status of the childs
  FOR i = 0, nPro-1 DO BEGIN
    error = ''
    status[i]=oIDLBridgeArray[i]->Status(ERROR=error)
    ErrorString[i] = error
;    WAIT, 0.2
;    PRINT,'err = ' + err
  ENDFOR

  ;check if I have problems
  indFail = WHERE(status GE 3, countFail)
  IF (countFail GT 0) THEN BEGIN    ;strange: casting status[k] to string resulted in this symbol 
    FOR k = 0, countFail-1 DO BEGIN
      PRINT, 'Problem with child ' + STRTRIM(indFail[k],2) + ', Status = '+ STRTRIM(status[indFail[k]],2) + ', ErrorString: ' + ErrorString[indFail[k]]
      OBJ_DESTROY, oIDLBridgeArray
      STOP
;      IF (ErrorString[indFail[k]] NE 'Type conversion error: Unable to convert given STRING to Long.') THEN STOP
    ENDFOR
    ;STOP
  ENDIF

  ;get inedex of those that have finished
  indIdle = WHERE(status EQ 0, countIdle)
  ;if some of them has finished
  IF (countIdle GT 0) THEN BEGIN
    FOR j = 0, countIdle-1 DO BEGIN
      ;in case we have finished the jobs, set them to inactive
      IF (eop EQ 1) THEN BEGIN
        ;no more new executions needed, set it to inactive
        inactive[indIdle[j]] = 1       
      ENDIF
      ;if all are inactive there is nothing left to do, set eor to 1 to get out of the cycle
      IF (TOTAL(inactive) EQ nPro) THEN eor = 1
    ENDFOR
  ENDIF
  IF ((countIdle GT 0) AND (eop EQ 0)) THEN BEGIN  
    ;if there are still jobs to be processed, launch a new execution
    ;run a new process using the  idle childs
    FOR j = 0, countIdle-1 DO BEGIN
      PRINT, 'Child ' + STRTRIM(indIdle[j], 2) + ' (' + lastAssigment[indIdle[j]] + '): ended'
      IF (eop EQ 0) THEN BEGIN
        PRINT, 'Child ' + STRTRIM(indIdle[j], 2) + ' assigned to ' + splitted_crops[c]
        lastAssigment[indIdle[j]] = splitted_crops[c]
        oIDLBridgeArray[indIdle[j]]->SetVar, 'crop2process', splitted_crops[c]
        oIDLBridgeArray[indIdle[j]]->SetVar, 'c', c
        oIDLBridgeArray[indIdle[j]]->SetVar, 'fn_sav_s1', fn_sav_s1
        oIDLBridgeArray[indIdle[j]]->SetVar, 'fn_sav_s2', fn_sav_s2
        oIDLBridgeArray[indIdle[j]]->SetVar, 'dir_out', dir_out
        oIDLBridgeArray[indIdle[j]]->SetVar, 'dir_out_graphs', dir_out_graphs
        oIDLBridgeArray[indIdle[j]]->SetVar, 'use_time_constrains_per_crop', use_time_constrains_per_crop
        oIDLBridgeArray[indIdle[j]]->SetVar, 'fn_timeConstrains', fn_timeConstrains
        oIDLBridgeArray[indIdle[j]]->Execute, parallelProName + ', crop2process, c, fn_sav_s1, fn_sav_s2, dir_out, dir_out_graphs, use_time_constrains_per_crop, fn_timeConstrains', /NOWAIT
      ENDIF ;eops
      ;check if there are still jobs to do, in case set eop = 1
      IF (c EQ ncrops-1) THEN BEGIN
        eop = 1
        PRINT, 'No more childs to launch, the program is waiting the active child to end'
      ENDIF ELSE BEGIN
        c = c + 1 ;get reday for the next line
      ENDELSE
    ENDFOR  
  ENDIF
ENDWHILE
;wait for all to end and do the rest
OBJ_DESTROY, oIDLBridgeArray
PRINT, 'Parallel processing time with '+STRTRIM(nPro,2) + ' processors (hours):', (systime(/sec)-tt)/60.0/60.0
RETURN, dir_out
END


PRO Sentinel_pheno_bridge_newFormat, crop2process, c, fn_sav_s1, fn_sav_s2, dir_out, dir_out_graphs, use_time_constrains_per_crop, fn_timeConstrains
  ;COMPILE_OPT IDL2
  ;note, crops may or may not be splitted into parts like 'Maize - B16 <300:465>'
  @cb_optimization.comm
  yearOfInterst = 2018
  winter_suppression_jd_min_max =  [JULDAY(12, 1, yearOfInterst-1), JULDAY(4, 30, yearOfInterst)]   ;during this winter period all the obs between cloudy obs will be replaced by the 5th NDVI percentile
  s1ENL = 4.4 ;Equivalent number of looks
  ;Thresholds for cloud presence
  maxFractOfS2CloudyPixels = 0.05
  ;Thresholds for snow presence
  maxFractOfS2SnowPixels = 0.05
  ;Thresholds used to consider the signal homogeneous
  heteroNdviMaxCv = 0.2 ;CAP consider a parcel obs to be heterogenous if CV > 0.2. Here I compute the fraction of observation during the season that are heterog. according to this criterion
  heteroVHMinVarRatio = 1.0 ;ratio between actual and thoretical (correcte for Dom test) VH variance, above is heterogenous
  dpi_fig = 150
  buffer_plots = 1 ;(0 to see them)
  dims_fit_plot = [900,300] ;[900,300]
  pos_legend_fit=[0.5,0.065]
  
  extract_pheno = 1  ;with 0 just plots the input data in a single graph
  k_sub_max = 1;6     ;subscript of the last index to consider 0 is only first, 1 is vh vv ratio, and so on
  dlmtr = ','
  
  ;read the date with the new format (one file for S1 and one for S2)
  RESTORE, fn_sav_s1
  RESTORE, fn_sav_s2
  
  IF (use_time_constrains_per_crop EQ 1) THEN RESTORE, fn_timeConstrains
  ;COMPULSORY TO USE '_' to separete the index and the orbit
  selected_indices = ['NDVI', 'RVI_ad', 'CR_ad', 'CR_a', 'RVI_a', 'CR_d', 'RVI_d']

  ;for plotting color with latitude, make color table for lat
  ctable = COLORTABLE(['red','blue'], NCOLORS = 256, /TRANSPOSE)
  minLat = MIN(s1.y)
  maxLat = MAX(s1.y)
  strOut = crop2process + ', ' + STRTRIM(c)  
  PRINT, 'START: '+crop2process
  ;analyse the crop to be processed
  crop_splitted = 0 ;0 if it is splitted
  label_splitted =''
  ;1 treat the possible split
  IF (STRMATCH(crop2process,'*<*:*>*') EQ 1) THEN BEGIN
    ;it is splitted
    crop_splitted = 1
    startID = STRMID(crop2process,STREGEX(crop2process,'<')+1,STREGEX(crop2process,':')-STREGEX(crop2process,'<')-1)
    stopID = STRMID(crop2process,STREGEX(crop2process,':')+1,STREGEX(crop2process,'>')-STREGEX(crop2process,':')-1)
    crop2process = STRMID(crop2process,0,STREGEX(crop2process,'<'))
    label_splitted = '_'+ startID + '-' + stopID
    PRINT, crop2process, label_splitted
    PRINT,  dir_out+'\pheno_results_' + crop2process + label_splitted +'_'+STRTRIM(c,2)+'.csv'
  ENDIF
  indCropS1 = WHERE(s1.crop EQ crop2process, countCropS1)
  indCropS2 = WHERE(s2.crop EQ crop2process, countCropS2)
  ;get the unique IDs of the crop
  uniqIdsS1 = s1.pointID[indCropS1].Uniq()
  uniqIdsS2 = s2.pointID[indCropS2].Uniq()
  IF (crop_splitted EQ 1) THEN BEGIN
    ;uniqIds = uniqIds[LONG(startID):LONG(stopID)]
    uniqIdsS1 = uniqIdsS1[LONG(startID):LONG(stopID)]
  ENDIF
  ;check that they are same length and exaclty the same stuff 
  PRINT, '*************************************************************************************************************'
  PRINT, 'BE CARE: check on equal sample disabled for debugging at line 279... This should be back in force with new data'
  ;IF (uniqIdsS1.LENGTH NE uniqIdsS2.LENGTH) OR (TOTAL(uniqIdsS1.Compare(uniqIdsS2) NE 0)) THEN STOP ELSE uniqIds = uniqIdsS1
  ;buffer_plots = 0  ;comment this after debugging!!!!!!!!!!!!!!!
  ;for now process only where both are present
  match, uniqIdsS1, uniqIdsS2, subS1, subS2
  IF (subS1.LENGTH LT uniqIdsS1.LENGTH) THEN BEGIN
    PRINT, 'Number of polyggons in S1 that were not present in S2=', uniqIdsS1.LENGTH - subS1.LENGTH
    PRINT, 'List of IDs'
    tmp = uniqIdsS1
    tmp[subS1]  = -9999
    PRINT, uniqIdsS1[WHERE(tmp NE -9999)] 
  ENDIF
  uniqIds = uniqIdsS1[subS1]
  PRINT, '*************************************************************************************************************'
  PRINT, 'Elements for crop ' + crop2process + ': ' + STRTRIM(N_ELEMENTS(uniqIds),2)
  initHandlerInputDataGraphs = BYTARR(k_sub_max+1) ;initialize it at zero
  ;handlerInputDataGraphs = !NULL
  handlerInputDataGraphs = OBJARR(k_sub_max+1)
  
  
  ;prepare for output
  IF (extract_pheno EQ 1) THEN BEGIN
    OPENW, lun, dir_out+'\pheno_results_' + crop2process + label_splitted +'_'+STRTRIM(c,2)+'.csv',/GET_LUN
    OPENW, lunMiss,dir_out+'\pheno_missing_data_' + crop2process + label_splitted + '_'+STRTRIM(c,2)+'.txt',/GET_LUN
    OPENW, lunFail,dir_out+'\pheno_failures_' + crop2process + label_splitted + '_'+STRTRIM(c,2)+'.txt',/GET_LUN
    ;new with structure
    phenoRes = sentinel_phenoRes_CREATE_STRUCT()
    hdr = TAG_NAMES(phenoRes)
    hdr = [hdr[0:6],'G1_' + hdr[7:-1],'G2_' + hdr[7:-1]]
    PRINTF, lun, STRJOIN(hdr+dlmtr)
    ;PRINTF, lun, STRJOIN(strHdrPhenoRes+dlmtr)
    PRINTF, lunFail, STRJOIN(['Index','Crop','ID','Message_GS1','Message_GS2']+dlmtr)
    PRINTF, lunMiss, STRJOIN(['Index','Crop','ID','Message']+dlmtr)
  ENDIF

 
  FOR j = 0, uniqIds.LENGTH-1 DO BEGIN
    ;here we have k_sub_max+1 indexes to deal with
    FOR k = 0, k_sub_max DO BEGIN
      ;u = where(uniqIds eq 54382052) & j = u[0] & buffer_plots = 0 & k = 0
      phenoRes = sentinel_phenoRes_CREATE_STRUCT()
      phenoRes.CROP_TYPE = crop2process
      phenoRes.ID = uniqIds[j]
      ;treat the case of using ascending and descending seperately that do no exist as indexes (ASCENDING, DESCENDING are attributes of each obs
      index2retrieve = selected_indices[k]
      phenoRes.Index = selected_indices[k]
      index2retrieve = STRSPLIT(index2retrieve,'_',/EXTRACT)    ;selected_indices = ['NDVI', 'CR_ad', 'RVI_ad', 'CR_a', 'RVI_a', 'CR_d', 'RVI_d']
      ;if I get an string array of two elements it is S1 and the second elemnt tells me what orbit to use
      IF (index2retrieve.LENGTH EQ 1) THEN BEGIN
        ;it is NDVI from S2
        subsOfPoly = WHERE((s2.pointID EQ uniqIds[j]), countObsOfPoly)
        ;order date ascending
        subAsc = SORT(s2.dateJD[subsOfPoly])
        subsOfPoly = subsOfPoly[subAsc]
        u = CREATE_STRUCT($
            'sentinel', 2, $
            'dateJD', s2.dateJD[subsOfPoly], 'mean', s2.NDVI_mean[subsOfPoly], 'sd', s2.NDVI_sd[subsOfPoly], $
            'count', s2.NDVI_count[subsOfPoly], 'fractCloudCovered', 1.0-s2.Fract_cloudfree[subsOfPoly], $
            'Snow_count', s2.Snow_count[subsOfPoly], 'fractSnowCovered',s2.Snow_count[subsOfPoly]/FLOAT(MAX(s2.NDVI_count[subsOfPoly])))
          ;get the latitude
        phenoRes.lat = s2.y[subsOfPoly[0]]
        phenoRes.lon = s2.x[subsOfPoly[0]]
        phenoRes.nuts0 = s2.nuts0[subsOfPoly[0]]
      ENDIF ELSE BEGIN
        ;it is S1 and we have 3 cases: use asc/desc, use asc only, use desc only
        CASE index2retrieve[1] OF
          'ad': subsOfPoly = WHERE((s1.pointID EQ uniqIds[j]), countObsOfPoly)
          'a':  subsOfPoly = WHERE((s1.pointID EQ uniqIds[j]) AND (s1.orbit EQ 'ASCENDING'), countObsOfPoly)
          'd':  subsOfPoly = WHERE((s1.pointID EQ uniqIds[j]) AND (s1.orbit EQ 'DESCENDING'), countObsOfPoly)
          ELSE: stop
        ENDCASE
        orbit = index2retrieve[1]
        index2retrieve = index2retrieve[0]
        ;order date ascending
        subAsc = SORT(s1.dateJD[subsOfPoly])
        subsOfPoly = subsOfPoly[subAsc]
        u = CREATE_STRUCT($
          'sentinel', 1, $
          'dateJD', s1.dateJD[subsOfPoly], 'mean', FLTARR(countObsOfPoly), 'sd', s1.VH_sd[subsOfPoly],$ 
          'vh_var_ratio', (s1.VH_sd[subsOfPoly])^2 / (((s1.VH_mean[subsOfPoly])^2)/s1ENL), $; * (1.0+(1.6449/SQRT(s1ENL*s1.VH_count[subsOfPoly])))^2 ), $ ;ratio between actual VH variance and the theoretical one 
                                                                                                              ;for the homogeneous target (MEAN^2 / ENL) 
                                                                                                              ;adjusted for the test of Dom (ratio > 1, heterogeneous)
          'vv_var_ratio', (s1.VV_sd[subsOfPoly])^2 / (((s1.VV_mean[subsOfPoly])^2)/s1ENL), $; * (1.0+(1.6449/SQRT(s1ENL*s1.VH_count[subsOfPoly])))^2 ), $
          'count', s1.VH_count[subsOfPoly], 'fract_ugly', 1.0-s1.VH_fract_available[subsOfPoly], $
          'View_angle', s1.View_angle[subsOfPoly], 'orbit', s1.orbit[subsOfPoly], 'rel_orbit', s1.rel_orbit[subsOfPoly])
        CASE index2retrieve[0] OF
          'RVI': u.mean =  s1.RVI[subsOfPoly]
          'CR': u.mean =  s1.CR[subsOfPoly]      
          ELSE: stop
        ENDCASE   
        ;get the latitude
        phenoRes.lat = s1.y[subsOfPoly[0]]
        phenoRes.lon = s1.x[subsOfPoly[0]]
        phenoRes.nuts0 = s1.nuts0[subsOfPoly[0]] 
      ENDELSE   
      phenoRes.nImages_input = countObsOfPoly
      
      
      IF (countObsOfPoly GT 0) THEN BEGIN
        IF (u.sentinel EQ 1) THEN BEGIN
          ;s1 data, reduce with mean when there are more than one obs per day
          ;1 get unique jds
          uniqJDs = u.dateJD.Uniq()
          v = CREATE_STRUCT($
            'sentinel', 1, $
            'dateJD', uniqJDs, 'mean', FLTARR(uniqJDs.LENGTH), 'sd', FLTARR(uniqJDs.LENGTH), 'min', FLTARR(uniqJDs.LENGTH), 'max', FLTARR(uniqJDs.LENGTH), $
            'vh_var_ratio', FLTARR(uniqJDs.LENGTH), 'vv_var_ratio', FLTARR(uniqJDs.LENGTH), $
            'count', FLTARR(uniqJDs.LENGTH), 'fract_ugly',FLTARR(uniqJDs.LENGTH), $
            'View_angle', FLTARR(uniqJDs.LENGTH), 'orbit', orbit)
          FOR ijd = 0, uniqJDs.LENGTH-1 DO BEGIN
            indJD = WHERE(u.dateJD EQ uniqJDs[ijd],/NULL)
            v.mean[ijd] = u.mean[indJD].Mean(/NAN)
            v.sd[ijd] = u.sd[indJD].Mean(/NAN)
            v.min[ijd] = u.mean[indJD].Min(/NAN)
            v.max[ijd] = u.mean[indJD].Max(/NAN)
            v.vh_var_ratio[ijd] = u.vh_var_ratio[indJD].Mean(/NAN)
            v.vv_var_ratio[ijd] = u.vv_var_ratio[indJD].Mean(/NAN)
            v.count[ijd] = u.count[indJD].Mean(/NAN)
            v.fract_ugly[ijd] = u.fract_ugly[indJD].Mean(/NAN)
            ;it has happened that the mean view angle is NaN
            IF (TOTAL(FINITE(u.View_angle[indJD])) EQ 0) THEN BEGIN
              v.View_angle[ijd] = !VALUES.F_NAN 
            ENDIF ELSE BEGIN
              v.View_angle[ijd] = u.View_angle[indJD].Mean(/NAN)
            ENDELSE           
          ENDFOR
          u = v
          v = 0
        ENDIF     
        ;now remove possible NaN (important, it may cause troubles later when fitting)
        indFin = WHERE(FINITE(u.mean EQ 1))
        IF (u.sentinel EQ 1) THEN BEGIN
          u = CREATE_STRUCT($
            'sentinel', 1, 'dateJD', u.dateJD[indFin], 'mean', u.mean[indFin], 'sd', u.sd[indFin], 'min', u.min[indFin], 'max', u.max[indFin], $
            'vh_var_ratio', u.vh_var_ratio[indFin], 'vv_var_ratio',  u.vv_var_ratio[indFin], $
            'count',  u.count[indFin], 'fract_ugly', u.fract_ugly[indFin], $
            'View_angle', u.View_angle[indFin], 'orbit',  u.orbit)
        ENDIF ELSE BEGIN
          u = CREATE_STRUCT($
            'sentinel', 2, $
            'dateJD',  u.dateJD[indFin], 'mean', u.mean[indFin], 'sd', u.sd[indFin], $
            'count', u.count[indFin], 'fractCloudCovered', u.fractCloudCovered[indFin], 'Snow_count', u.Snow_count[indFin], 'fractSnowCovered', u.fractSnowCovered[indFin])
        ENDELSE
       
        ;scale it in order to use it as a subscript of the colortable
        latSub = ROUND((phenoRes.lat-minLat)/FLOAT(maxLat-minLat)*255)
        ;make a plot with orginal data (all), extract title
        tmpTitle = STRTRIM(STRSPLIT(crop2process, '-', /EXTRACT),2)
        IF (initHandlerInputDataGraphs[k] EQ 0) THEN BEGIN
          handlerInputDataGraphs[k] = PLOT(u.dateJd, u.mean, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=crop2process, COLOR = ctable[*,latSub], TRANSPARENCY= 95, $;LAYOUT=[1,3,i+1], $
            XTITLE='Time', YTITLE=phenoRes.Index, DIMENSIONS=[1200,300], TITLE=tmpTitle[0], BUFFER = buffer_plots, XMINOR=5)
          initHandlerInputDataGraphs[k] = 1
        ENDIF ELSE BEGIN
          handlerInputDataGraphs[k].Select
          handlerInputDataGraphs[k] = PLOT(u.dateJd, u.mean, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=crop2process, COLOR = ctable[*,latSub], TRANSPARENCY= 95, /OVERPLOT)
        ENDELSE
      ENDIF ELSE BEGIN
        u = !VALUES.F_NAN
      ENDELSE
      
      PRINT, uniqIds[j], ', ',crop2process, ', ', phenoRes.Index
      
      IF (extract_pheno EQ 1) THEN BEGIN    
        fit_title = crop2process + ', ID ' + STRTRIM(uniqIds[j],2) + '; ' + STRING(phenoRes.lat, FORMAT='(f6.2)') + ' N, ' + $
                    STRING(phenoRes.lon, FORMAT='(f6.2)') + ' E, npix max = ' +  STRTRIM(ROUND(u.count.Max()),2)
        posTop = [0.1, 0.7, 0.9, 0.9] ;[X1, Y1, X2, Y2]
        posBot = [0.1, 0.15, 0.9, 0.7] ;[X1, Y1, X2, Y2] 
        xrg= [u.dateJd.Min()-10,u.dateJd.Max()+10]
        IF (u.sentinel EQ 2) THEN BEGIN
          ;top short plot for fract and snow
          h0 = PLOT(u.dateJd, u.fractCloudCovered, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', TITLE= fit_title, LINESTYLE='-', SYMBOL='+',YTITLE='Frct cover', BUFFER = buffer_plots, DIMENSIONS=dims_fit_plot, $
            XSTYLE=1,XRANGE=xrg,XSHOWTEXT=0, XMINOR=5, YRANGE=[0,1.0],YMAJOR=3,YMINOR=0,YSHOWTEXT=0, NAME='Clouds', POSITION=posTop) ;YTICKNAME=['','0.25','0.5','0.75',''] 
          h01 = PLOT(u.dateJd, u.fractSnowCovered, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE='-', COLOR='b',SYMBOL='o', SYM_COLOR='b',NAME='Snow', /OVERPLOT)           
          yax = AXIS('Y', LOCATION='right', TITLE='Frct', MAJOR=5, MINOR=0)
          h0l = LEGEND(TARGET=[h0,h01], POSITION=[0.005,posTop[3]], LINESTYLE='', TRANSPARENCY = 75, HORIZONTAL_ALIGNMENT=0) ;ORIENTATION = 1
          ;for s2 plot sd
          ;plot mean, use  a different symbol for obs data are cloudy
          h1 = ERRORPLOT(u.dateJd, u.mean, u.sd, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XTICKUNITS='Months',XTICKINTERVAL=6, $
             XSTYLE=1,XRANGE=xrg, LINESTYLE='', SYMBOL='o', YTITLE=phenoRes.Index, BUFFER = buffer_plots, DIMENSIONS=dims_fit_plot, XMINOR=5, POSITION = posBot, /NODATA, /CURRENT)
          indCloudy = WHERE(u.fractCloudCovered GT maxFractOfS2CloudyPixels, countCloudy, COMPLEMENT= indClFree, NCOMPLEMENT= countClFree)   
          ;Now that raw data are plotted (snow may be present
          ;use different symbol datapoints where the fraction of cloudy pixels is greater than maxFractOfS2CloudyPixels 
          IF (countClFree GT 0) THEN h1 = ERRORPLOT(u.dateJd[indClFree], u.mean[indClFree], u.sd[indClFree], LINESTYLE='', SYMBOL='o', COLOR = 'light grey', ERRORBAR_COLOR= 'light grey', NAME='obs', /OVERPLOT)
          IF (countCloudy GT 0) THEN h1 = ERRORPLOT(u.dateJd[indCloudy], u.mean[indCloudy], u.sd[indCloudy], LINESTYLE='', SYMBOL='tu', COLOR = 'light grey', ERRORBAR_COLOR= 'light grey', NAME='obs', /OVERPLOT)
          ;Treat snow pixels as well
          u = treatSnowCover(u, winter_suppression_jd_min_max, maxFractOfS2SnowPixels, maxFractOfS2CloudyPixels)
          ;replot with possibly replaced values
          IF (countClFree GT 0) THEN h1 = ERRORPLOT(u.dateJd[indClFree], u.mean[indClFree], u.sd[indClFree], LINESTYLE='', SYMBOL='o', COLOR = 'grey', ERRORBAR_COLOR= 'grey', NAME='obs', /OVERPLOT)
          IF (countCloudy GT 0) THEN h1 = ERRORPLOT(u.dateJd[indCloudy], u.mean[indCloudy], u.sd[indCloudy], LINESTYLE='', SYMBOL='tu', COLOR = 'grey', ERRORBAR_COLOR= 'grey', NAME='obs', /OVERPLOT)
          
          IF (countCloudy GT 0) AND (countClFree GT 0) THEN BEGIN
            u = CREATE_STRUCT('sentinel', 2, 'dateJD',  u.dateJD[indClFree], 'mean', u.mean[indClFree], 'sd', u.sd[indClFree], $
              'count', u.count[indClFree], 'fractCloudCovered', u.fractCloudCovered[indClFree], $
              'Snow_count', u.Snow_count[indClFree], 'fractSnowCovered', u.fractSnowCovered[indClFree])
          ENDIF
          IF (countCloudy GT 0) AND (countClFree EQ 0) THEN BEGIN
            u = CREATE_STRUCT('sentinel', 2, 'dateJD',  !VALUES.F_NAN, 'mean', !VALUES.F_NAN, 'sd', !VALUES.F_NAN, $
              'count', !VALUES.F_NAN, 'fractCloudCovered', !VALUES.F_NAN, $
              'Snow_count', !VALUES.F_NAN, 'fractSnowCovered', !VALUES.F_NAN)
          ENDIF
          
          ;IF (countSnow GT 0) THEN STOP
        ENDIF ELSE BEGIN
          ;top short plot for fract and VV and VH varriance ratio
          h0 = PLOT(u.dateJd, u.fract_ugly, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', TITLE= fit_title, LINESTYLE='-', SYMBOL='+',YTITLE='Frct cover', BUFFER = buffer_plots, DIMENSIONS=dims_fit_plot, $
            XSTYLE=1,XRANGE=xrg,XSHOWTEXT=0, XMINOR=5, YRANGE=[0,1.5],YMAJOR=0,YMINOR=0,YSHOWTEXT=0, NAME='Ugly', POSITION=posTop) ;YTICKNAME=['','0.25','0.5','0.75','']
          h01 = PLOT(u.dateJd, u.vh_var_ratio, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE='-', COLOR='b',SYMBOL='o', SYM_COLOR='b',NAME='VH_var_rat', /OVERPLOT)
          h02 = PLOT(u.dateJd, u.vv_var_ratio, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE='-', COLOR='r',SYMBOL='tu', SYM_COLOR='r',NAME='VV_var_rat', /OVERPLOT)
          htmp = PLOT(xrg, [1,1], XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE='-', COLOR='grey',/OVERPLOT)
          yax = AXIS('Y', LOCATION='right', TITLE='Frct', MAJOR=4, MINOR=0)
          h0l = LEGEND(TARGET=[h0,h01,h02], POSITION=[0.005,posTop[3]+0.025], LINESTYLE='', TRANSPARENCY = 75, HORIZONTAL_ALIGNMENT=0, FONT_SIZE = 7, SAMPLE_WIDTH=0.075) ;ORIENTATION = 1
          ;plot mean, use  adifferent symbol for obs passing the VH varainece test
          h1 = PLOT(u.dateJd, u.mean, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))',XTICKUNITS='Months',XTICKINTERVAL=6, $
            XSTYLE=1,XRANGE=xrg, YTITLE=phenoRes.Index, XMINOR=5, POSITION = posBot, /NODATA, /CURRENT)
          indHetObs = WHERE(u.vh_var_ratio GT heteroVHMinVarRatio, countHetObs, COMPLEMENT=indHomObs, NCOMPLEMENT=countHomObs)
          IF (countHomObs GT 0) THEN h1 = PLOT(u.dateJd[indHomObs], u.mean[indHomObs], LINESTYLE='', SYMBOL='o', COLOR = 'grey', NAME='obs', /OVERPLOT)
          IF (countHetObs GT 0) THEN  h1 = PLOT(u.dateJd[indHetObs], u.mean[indHetObs], LINESTYLE='', SYMBOL='tu', COLOR = 'grey', NAME='obs', /OVERPLOT)
          h1b = PLOT(u.dateJd, u.min, LINESTYLE='', SYMBOL='+', COLOR = 'grey', SYM_SIZE = 0.5, /OVERPLOT)
          h1b = PLOT(u.dateJd, u.max, LINESTYLE='', SYMBOL='+', COLOR = 'grey', SYM_SIZE = 0.5, /OVERPLOT)   
        ENDELSE
        lgndTargets = [h1]
        ;Check that there are at least 10 obs, if not add it to missing
        IF (u.mean.LENGTH LT 10) THEN BEGIN
          PRINTF, lunMiss, phenoRes.Index + ',' + crop2process + ', ID ' + STRTRIM(uniqIds[j],2) + ', had less than 10 obs and it was not processed (n = ' + STRTRIM(u.mean.LENGTH,2) + ')'
          phenoRes.LocateSeason_msg[*] = 'Less than 10 images in total'
          ;Ihave to vrite a line anyhow
          tmp = !NULL
          FOR t = 0, 6 DO tmp = [tmp, STRTRIM(phenoRes.(t),2)]
          FOR t = 7, N_TAGS(phenoRes)-1 DO tmp = [tmp, STRTRIM(phenoRes.(t)[0],2)]
          FOR t = 7, N_TAGS(phenoRes)-1 DO tmp = [tmp, STRTRIM(phenoRes.(t)[1],2)]
          tmp = STRJOIN(tmp+dlmtr)
          tmp = tmp.Replace('NaN','-9999')
          PRINTF, lun, tmp
          IF (u.mean.LENGTH GT 0) THEN BEGIN
            h1.save, dir_out_graphs + '\fit_' + crop2process + '_ID_' + STRTRIM(uniqIds[j],2)+'_'+phenoRes.Index+'.png'
            h1.close
          ENDIF
          pheno = 0
        ENDIF ELSE BEGIN

          ;Here I have to set the possible time. Let's try to understand if its summer or winter crop
          ;Find biggest max in the year of interest (given by Jd range) and the local minima at left and right, assuming that the max has to be located in the period of interest
          ;= year of interest betwen 1/1 and september
          IF use_time_constrains_per_crop EQ 1 THEN BEGIN
            ;get the limits
            crop_code = STRSPLIT(crop2process,'-', /EXTRACT)
            crop_code = STRTRIM(crop_code[-1], 2)
            indCrop = WHERE(timeRangesPerCrop.Code EQ crop_code, count)
            IF (count NE 1) THEN STOP 
            DDMMrelY = STRSPLIT(timeRangesPerCrop.LEFT_WIDE_RANGE[indCrop], '#', /EXTRACT)
            lmin = JULDAY(DDMMrelY[1],DDMMrelY[0],yearOfInterst+FIX(DDMMrelY[2]))
            DDMMrelY = STRSPLIT(timeRangesPerCrop.RIGHT_WIDE_RANGE[indCrop], '#', /EXTRACT)
            rmin = JULDAY(DDMMrelY[1],DDMMrelY[0],yearOfInterst+FIX(DDMMrelY[2]))
            DDMMrelY = STRSPLIT(timeRangesPerCrop.LEFT_NARROW_RANGE[indCrop], '#', /EXTRACT)
            lmax = JULDAY(DDMMrelY[1],DDMMrelY[0],yearOfInterst+FIX(DDMMrelY[2]))
            DDMMrelY = STRSPLIT(timeRangesPerCrop.RIGHT_NARROW_RANGE[indCrop], '#', /EXTRACT)
            rmax = JULDAY(DDMMrelY[1],DDMMrelY[0],yearOfInterst+FIX(DDMMrelY[2]))
            IF (timeRangesPerCrop.Type[indCrop] EQ 'winter crop standard') THEN doubleSeasonAdmitted = 1 ELSE doubleSeasonAdmitted = 0
            ;doubleSeasonAdmitted = 1
;            IF (sentinel EQ 1) THEN BEGIN ;s1 maxima tends to be delayed, but nothing shoud go beyond end of august rmax = MIN([rmax + 30, JULDAY(8, 31, yearOfInterst)])
            season = locate_largest_season_in_jd_range_single_double(u.dateJd, u.mean, [lmin, rmin], [lmax,rmax], index2retrieve, doubleSeasonAdmitted, yearOfInterst)
          ENDIF ELSE BEGIN
            ;season = locate_largest_season_in_jd_range_v3(u.dateJd, u.mean, [JULDAY(01,01,yearOfInterst), JULDAY(09,30,yearOfInterst)], [JULDAY(03,01,yearOfInterst), JULDAY(07,31,yearOfInterst)], index2retrieve)
            season = locate_largest_season_in_jd_range_v5(u.dateJd, u.mean, [JULDAY(01,01,yearOfInterst), JULDAY(09,30,yearOfInterst)], [JULDAY(03,01,yearOfInterst), JULDAY(07,31,yearOfInterst)], index2retrieve)
          ENDELSE
          ;the smoothing is available also in case of locate failure
          h1.Select
          xjd_reg = FINDGEN(u.dateJd[-1]-u.dateJd[0]+1)+u.dateJd[0]
          ;now treat the possibility of having 2 seasons
          legend_added = 0
          FOR s = 0, season.n_seasons-1 DO BEGIN 
            IF (season.code[s] NE -100) THEN BEGIN ;with -100 tehere was less than 10 data, no smoothing performed
              h7 = PLOT(xjd_reg, season.y_reg_smooth, COLOR = 'grey', NAME='Smooth', /OVERPLOT) ;TRANSPARENCY= 70,
              lgndTargets = [lgndTargets, h7]
              ;results of locate might be availble, plot them (retr may be 0 for other reason but the submax is there)
              IF (season.subMax[s] NE -9999) THEN BEGIN
                h6 = PLOT([u.dateJd[season.subMax[s]],u.dateJd[season.subMax[s]]], h7.yrange, COLOR = 'black', LINESTYLE='--', NAME='SmoothPeak',/OVERPLOT)
                lgndTargets = [lgndTargets, h6]
              ENDIF
            ENDIF
            ;treat locate season retrieval
            phenoRes.LocateSeason_code[s] = season.code[s]
            phenoRes.LocateSeason_msg[s] = season.msg[s]
            IF (season.retr[s] EQ 1) THEN BEGIN
              dateRange = [u.dateJd[season.subLeft[s]],u.dateJd[season.subRight[s]]]      ; window range for finding phenology
              baseDate = JULDAY(01,01,yearOfInterst)                                  ; base date for writing info (so 1 = 1 January x)
              pheno = phenoFitOverKnownInterval(u.dateJd, u.mean, dateRange, u.dateJd[season.subMax[s]], $
                                                season.ySmoothAtSubLeft[s], season.ySmoothAtSubRight[s], season.ySmoothAtSubMax[s], season.y_obs_smooth, $
                                                season.xjdLimP2[s], season.xjdLimP5[s], season.xjdFgP2[s], season.xjdFgP5[s], baseDate, index2retrieve, crop2process, uniqIds[j])
              IF (pheno.retr EQ 1) THEN BEGIN ;plot and save only if it was successful
                dateTmp = INDGEN(pheno.jdInterval[1]-pheno.jdInterval[0])+pheno.jdInterval[0]     ; array with all days (JD) in the modelled year (automatically adapts for shorter years in case year started with decay)
                fitNDVIseries = TANH_DOUBLE(dateTmp,pheno.PARAM_X)
                h2 = PLOT(dateTmp, fitNDVIseries, COLOR = 'r', NAME='DHT fit',/OVERPLOT)
                lgndTargets = [lgndTargets, h2]
                IF (FINITE(pheno.SOS50) EQ 1) THEN BEGIN
                  h3 = PLOT(baseDate+[pheno.SOS20,pheno.SOS20], [h2.yrange,h2.yrange], COLOR = 'green', NAME='SOS20',/OVERPLOT)
                  lgndTargets = [lgndTargets, h3]
                END
                IF (FINITE(pheno.PS90) EQ 1) THEN BEGIN
                  h4 = PLOT(baseDate+[pheno.PS90,pheno.PS90], [h2.yrange,h2.yrange], COLOR = 'yellow', NAME='PS90',/OVERPLOT)
                  lgndTargets = [lgndTargets, h4]
                ENDIF  
                IF FINITE(pheno.EOS20) THEN BEGIN
                  h5 = PLOT(baseDate+[pheno.EOS20,pheno.EOS20], [h2.yrange,h2.yrange], COLOR = 'red', NAME='EOS20',/OVERPLOT)
                  lgndTargets = [lgndTargets, h5]
                  ;hl = LEGEND(TARGET=[h1,h2,h3,h4,h5,h6,h7], POSITION=pos_legend_fit, LINESTYLE='', TRANSPARENCY = 75, ORIENTATION = 1, HORIZONTAL_ALIGNMENT=0.5)
                ENDIF
                IF (legend_added EQ 0) THEN BEGIN
                  hl = LEGEND(TARGET=lgndTargets, POSITION=pos_legend_fit, LINESTYLE='', TRANSPARENCY = 75, ORIENTATION = 1, HORIZONTAL_ALIGNMENT=0.5)
                  legend_added = 1
                ENDIF
                indInSeas = WHERE((u.dateJd GE pheno.jdInterval[0]) AND (u.dateJd LE pheno.jdInterval[1]), countInSeas)
                IF (phenoRes.Index EQ 'NDVI') THEN BEGIN
                  hTmp = ERRORPLOT(u.dateJd[indInSeas], u.mean[indInSeas], u.sd[indInSeas], LINESTYLE='',SYMBOL='o', ERRORBAR_COLOR= 'black', COLOR = 'black', /OVERPLOT)
                  ;Compute the index of parcel heterogeneity for NDVI 
                  indCV = indInSeas[WHERE(u.mean[indInSeas] GT 0.0, countCV)] ;to vaoid zero division
                  phenoRes.mean_cv_varrat_inSeason[s] =  MEAN(u.sd[indCV]/u.mean[indCV])
                  indHet = WHERE(u.sd[indCV]/u.mean[indCV] GT heteroNdviMaxCv, countHet)
                  phenoRes.prctHeterogObs_inSeason[s] = countHet / FLOAT(countCV) * 100.0
                  indSnowIS = WHERE(u.fractSnowCovered[indInSeas] GT maxFractOfS2SnowPixels, countSnowIS)
                  phenoRes.prctSnow_inSeason[s] = countSnowIS/FLOAT(countInSeas)
                ENDIF ELSE BEGIN
                  IF (countHetObs GT 0) THEN indHetObsIS = WHERE(u.vh_var_ratio[indInSeas] GT heteroVHMinVarRatio, countHetObsIS,  COMPLEMENT=indHomObsIS,  NCOMPLEMENT=countHomObsIS)  ;refine indexing on the current season
                  IF (countHomObsIS GT 0) THEN hTmp = PLOT(u.dateJd[indInSeas[indHomObsIS]], u.mean[indInSeas[indHomObsIS]], LINESTYLE='',SYMBOL='o', ERRORBAR_COLOR= 'black', COLOR = 'black', /OVERPLOT)
                  IF (countHetObsIS GT 0) THEN hTmp = PLOT(u.dateJd[indInSeas[indHetObsIS]], u.mean[indInSeas[indHetObsIS]], LINESTYLE='',SYMBOL='tu', ERRORBAR_COLOR= 'black', COLOR = 'black', /OVERPLOT)
                  ;Compute the index of parcel heterogeneity for NDVI
                  ;indCV = indInSeas[WHERE(u.mean[indInSeas] GT 0.0, countCV)]
                  phenoRes.mean_cv_varrat_inSeason[s] =  MEAN(u.vh_var_ratio[indInSeas])
                  indHet = WHERE(u.vh_var_ratio[indInSeas] GT heteroVHMinVarRatio , countHet)
                  phenoRes.prctHeterogObs_inSeason[s] = countHet / FLOAT(countInSeas) * 100.0
                ENDELSE
                phenoRes.Pval_fit[s] = pheno.pval
                phenoRes.SOSunrel[s] = pheno.SOSunrel
                phenoRes.EOSunrel[s] = pheno.EOSunrel
                phenoRes.SOSfractUnrel[s] = pheno.SOSfractUnrel
                phenoRes.EOSfractUnrel[s] = pheno.EOSfractUnrel
                phenoRes.PS90[s] = pheno.PS90
                ;PRINT,pheno.pval
                phenoRes.SD_in_season[s] = MEAN(u.sd[indInSeas])
                phenoRes.SOS20[s] = pheno.SOS20 &  phenoRes.EOS20[s] = pheno.EOS20 &  phenoRes.LGS20[s] = pheno.LGS20
                phenoRes.SOS50[s] = pheno.SOS50 &  phenoRes.EOS50[s] = pheno.EOS50 &  phenoRes.LGS50[s] = pheno.LGS50 
                phenoRes.maxFit[s] = pheno.maxNDVI & phenoRes.Slope_growth[s] = pheno.PARAM_x[3] & phenoRes.SLOPE_DECAY[s] = pheno.PARAM_x[6]
                phenoRes.r_fit[s] = pheno.PearsonCC & phenoRes.RMSD_fit[s] = pheno.RMSD & phenoRes.nImages_fit[s] = pheno.nImages & phenoRes.maxGap[s] = pheno.maxGap
                phenoRes.p0[s] = pheno.PARAM_X[0] & phenoRes.p1[s] = pheno.PARAM_X[1] & phenoRes.p2[s] = pheno.PARAM_X[2] & phenoRes.p3[s] = pheno.PARAM_X[3]  
                phenoRes.p4[s] = pheno.PARAM_X[4] & phenoRes.p5[s] = pheno.PARAM_X[5] & phenoRes.p6[s] = pheno.PARAM_X[6]
                phenoRes.p0_err[s] = pheno.PCERROR[0] & phenoRes.p1_err[s] = pheno.PCERROR[1] & phenoRes.p2_err[s] = pheno.PCERROR[2] & phenoRes.p3_err[s] = pheno.PCERROR[3]
                phenoRes.p4_err[s] = pheno.PCERROR[4] & phenoRes.p5_err[s] = pheno.PCERROR[5] & phenoRes.p6_err[s] = pheno.PCERROR[6]           
              ENDIF ELSE BEGIN ;pheno failed
                phenoRes.PhenoFit_msg[s] = pheno.message
                pheno = 0
              ENDELSE
            ENDIF 
          ENDFOR  ;FOR s = 0, season.n_seasons-1 DO BEGIN 
          ;Now write the csv lines
          ;pheno results
          tmp = !NULL
          FOR t = 0, 6 DO tmp = [tmp, STRTRIM(phenoRes.(t),2)] 
          FOR t = 7, N_TAGS(phenoRes)-1 DO tmp = [tmp, STRTRIM(phenoRes.(t)[0],2)]
          FOR t = 7, N_TAGS(phenoRes)-1 DO tmp = [tmp, STRTRIM(phenoRes.(t)[1],2)]
          tmp = STRJOIN(tmp+dlmtr)
          tmp = tmp.Replace('NaN','-9999')
          PRINTF, lun, tmp
          ;failures
          IF (TOTAL(phenoRes.PhenoFit_msg NE '') NE 0) THEN BEGIN ;there was a pheno_fit failure
            tmp = STRJOIN([phenoRes.Index, phenoRes.CROP_TYPE, STRTRIM(phenoRes.ID,2), phenoRes.PhenoFit_msg]+dlmtr)
            PRINTF, lunFail, tmp
          ENDIF
          IF (TOTAL(phenoRes.LocateSeason_msg NE '') NE 0) THEN BEGIN ;there was a locate season failure
            tmp = STRJOIN([phenoRes.Index, phenoRes.CROP_TYPE, STRTRIM(phenoRes.ID,2), phenoRes.LocateSeason_msg]+dlmtr)
            PRINTF, lunFail, tmp
          ENDIF
          h1.save, dir_out_graphs + '\fit_' + crop2process + '_ID_' + STRTRIM(uniqIds[j],2)+'_'+phenoRes.Index+'.png', RESOLUTION = dpi_fig
          h1.close
          pheno = 0
        ENDELSE ; it had more than 10 obs
      ENDIF ;extract_pheno
    ENDFOR  ;k
  ENDFOR ;j
  FOR k = 0, k_sub_max DO BEGIN
    handlerInputDataGraphs[k].Select
    IF (k EQ 0) THEN handlerInputDataGraphs[k].YRANGE = [-0.2,1]
    cbar = COLORBAR(RGB_TABLE=ctable, ORIENTATION = 1, RANGE=[minLat,maxLat], TITLE='Latitude N (deg)',FONT_SIZE=fs,TAPER=1,POSITION = [0.97, 0.2, 0.99, 0.8])
    handlerInputDataGraphs[k].save, dir_out_graphs + '\All_input_data_'+crop2process + label_splitted + '_'+selected_indices[k]+'.png', RESOLUTION = dpi_fig
    handlerInputDataGraphs[k].close
  ENDFOR
  IF (extract_pheno EQ 1) THEN BEGIN
    ;SAVE, /ALL, FILENAME = dir_out + '\pheno_run_'+crop2process+'_'+STRTRIM(c,2)+'.sav'
    FREE_LUN, lun
    FREE_LUN, lunFail
    FREE_LUN, lunMiss
  ENDIF
  PRINT, 'ENDED: '+crop2process
END
