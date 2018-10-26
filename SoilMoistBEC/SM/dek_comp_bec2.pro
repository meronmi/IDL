PRO dek_comp_BEC2
  ;makes 10-day composite of daily BEC SM data (note that 2 image per day may be available (asc and desc orbit)
  dir_in = '\\ies\d5\asap\BEC_soil_moisture\data'
  dir_out = '\\ies\d5\asap\BEC_soil_moisture\dataENVIformat'
  
  ;First day of first composite , IT MUST be day 01, 11 or 21
  firstDate = '20100121'
  ;Last day of the last composite , IT MUST be day 10, 20 or x 
  lastDate = '20170910' 
  ;name of the variable to read from netcdf
  varName = 'SM'
  ;*************************************************************************************************************
  
  fns = FILE_BASENAME(FILE_SEARCH(dir_in,'*.nc'))
  ;retrive dims and other info from first file
  ncdfObj = Obj_New('NCDF_DATA', dir_in + '\' + fns[0], /NO_READ_ON_PARSE)
  IF Obj_Valid(ncdfObj) THEN BEGIN
;    ncdfObj -> Browse, NO_NEW_FILE=Keyword_Set(no_new_file)
    GlobAttr = ncdfObj-> ReadGlobalAttr()
    x = ncdfObj -> ReadVariableWithAttr(varName)
    sz = SIZE(x.data)
    noDataVal = x.MISSING_VALUE
    description = x.DESCRIPTION
    units = x.UNITS
    geoInfo = x.GRID_MAPPING
    ;PRINT, GlobAttr
    ;x = ncdfObj -> ReadVariable('SM')
  ENDIF
  Obj_Destroy, ncdfObj
  
  ;generate all starting ending days for the required period
  
  IF (FIX(STRMID(firstDate,6,2)) NE 1) AND (FIX(STRMID(firstDate,6,2)) NE 11) AND (FIX(STRMID(firstDate,6,2)) NE 21) THEN STOP
  jdF = JULDAY(FIX(STRMID(firstDate,4,2)),FIX(STRMID(firstDate,6,2)),FIX(STRMID(firstDate,0,4)))
  IF (FIX(STRMID(lastDate,6,2)) NE 10) AND (FIX(STRMID(lastDate,6,2)) NE 21) AND (FIX(STRMID(lastDate,7,2)) NE 28) AND $
     (FIX(STRMID(lastDate,6,2)) NE 30) AND (FIX(STRMID(lastDate,6,2)) NE 31) THEN STOP
  jdL = JULDAY(FIX(STRMID(lastDate,4,2)),FIX(STRMID(lastDate,6,2)),FIX(STRMID(lastDate,0,4)))
  jdPeriod = LINDGEN(jdL-jdF+1) + jdF
  ;from this derive the staring and endding dates of each dekads
  jdDekStart = !NULL ;stores the jd of the first day of the dek
  jdDekStop = !NULL  ;stores the jd of the last day of the dek\

  
  FOR j = 0, N_ELEMENTS(jdPeriod)-1 DO BEGIN
    CALDAT, jdPeriod[j], MM, DD, YYYY
    CALDAT, jdPeriod[j]+1, MM1, DD1, YYYY1
    ;if this is the last day of the month, it is for sure a stop
    IF (MM NE MM1) THEN BEGIN
      ;there is a start already? if not stop (there cannot be end without start)
      IF (N_ELEMENTS(jdDekStart) NE N_ELEMENTS(jdDekStop)+1) THEN STOP
      jdDekStop = [jdDekStop, jdPeriod[j]]
    ENDIF ELSE BEGIN
      ;it is not the end of the month, proceed normally, it can be a start (1,11,21) or a stop (10,20). The endo of the month case is considered above
      IF ((DD EQ 1) OR (DD EQ 11) OR (DD EQ 21)) THEN jdDekStart = [jdDekStart, jdPeriod[j]]
      IF ((DD EQ 10) OR (DD EQ 20)) THEN BEGIN
        ;check that there is a start already
        IF (N_ELEMENTS(jdDekStart) NE N_ELEMENTS(jdDekStop)+1) THEN STOP
        jdDekStop = [jdDekStop, jdPeriod[j]]
      ENDIF
    ENDELSE
  ENDFOR
  
  yyyymmdd = INTARR(3, N_ELEMENTS(fns))
  yyyymmdd[0,*] = FIX(STRMID(fns, 13, 4))
  yyyymmdd[1,*] = FIX(STRMID(fns, 17, 2))
  yyyymmdd[2,*] = FIX(STRMID(fns, 19, 2))
  jd = REFORM(JULDAY( yyyymmdd[1,*], yyyymmdd[2,*], yyyymmdd[0,*]))
  indSorted = SORT(jd)
  yyyymmdd = yyyymmdd[*,indSorted]
  ;here I store js and names of all files
  jd = jd[indSorted]
  fns = fns[indSorted]
  
 
  ;now prepare all the required dek composties (n = N_ELEMENTS(jdDekStart))
  FOR i = 0, N_ELEMENTS(jdDekStart)-1 DO BEGIN
    ;find all files between jdDekStart[i] and jdDekStop[i]
    ind = WHERE((jd GE jdDekStart[i]) AND (jd LE jdDekStop[i]), nimages)
    IF (nimages NE ((jdDekStop[i] - jdDekStart[i] + 1)*2)) THEN BEGIN
      CALDAT, jdDekStart[i], mm1, dd1, yyyy1
      CALDAT, jdDekStop[i], mm2, dd2, yyyy2
      PRINT, 'WARNING: less than 2 obs per day over the period'
      PRINT, STRJOIN(STRTRIM([dd1,mm1,yyyy1],2), '-') + ' -> ' + STRJOIN(STRTRIM([dd2,mm2,yyyy2],2), '-') + ' n = ' + STRING(nimages)
      PRINT, fns[ind]
    ENDIF
     ; make the mattrix to store data
    x = MAKE_ARRAY(sz[1],sz[2], nimages, TYPE=sz[-2])
    ; loop on the files to be loaded
    FOR k = 0, nimages- 1 DO BEGIN
      fn = fns[ind[k]]
      ncdfObj = Obj_New('NCDF_DATA', dir_in + '\' + fn, /NO_READ_ON_PARSE)
      IF Obj_Valid(ncdfObj) THEN  x[*,*,k] = ncdfObj -> ReadVariable(varName) ELSE STOP
      Obj_Destroy, ncdfObj
    ENDFOR
    ;make the composite
    indNAN = WHERE(x EQ noDataVal, count)
    IF (count GT 0) THEN x[indNAN] = !VALUES.F_NAN
    ;avoid floating errors when it is all nan
    !except=0
    meanX = MEAN(x, DIMENSION=3, /NAN)
    clear = CHECK_MATH() ;set back to normal
    !except=1
    ;save the composite
    CALDAT, jdDekStart[i], mm, dd, yyyy
    base_fn_out = dir_out + '\sm_' + STRTRIM(yyyy,2) + STRING(mm, FORMAT='(I02)') + STRING(dd, FORMAT='(I02)')
    ;scale it to byte
    indFin = WHERE(FINITE(meanX), COMPLEMENT = indNan, count)
    meanX[indFin] = ROUND(TEMPORARY(meanX[indFin])/0.005)
    meanX[indNan] = 255
    IF ((MIN(meanX) LT 0) OR (MAX(meanX) GT 255)) THEN STOP ELSE meanX = BYTE(TEMPORARY(meanX)) 
    OPENW, lun, base_fn_out + '.img', /GET_LUN
    WRITEU, lun, REVERSE(meanX,2) ; flip it
    FREE_LUN, lun
    res = write_envi_hdr(base_fn_out + '.hdr', sz[1], sz[2], 1, NBANDS=1, INTERLEAVE='bsq',$
      ;MAPINFO='{EASE-Grid Global, 1.5000, 1.5000, -17321659.7750, 7332251.0625, 2.5067525000e+04, 2.5067525000e+04, WGS-84, units=Meters}', PROJECTIONINFO='{99, 6371228.0, 6371228.0, 0.000000, 0.000000, 0.0, 0.0, 30.000000, EASE-Grid Global, WGS-84, User Proj Cylind Equal Area, units=Meters}', $
      ;FREE_TEXT = 'BEC ' + varName + ', IDL dekadal domposite') ;, MAPINFO=geoInfo
      MAPINFO=' {EASE-Grid Global, 1,1, -17363604.8790229, 7307299.43225504, 2.5025e+04, 2.5025e+04, WGS-84, units=Meters}', $
      PROJECTIONINFO='{99, 6371228.0, 6371228.0, 0.000000, 0.000000, 0.0, 0.0, 30.000000, EASE-Grid Global, WGS-84, User Proj Cylind Equal Area, units=Meters}', $
      FREE_TEXT = 'values = {BEC Soil moisture, m3/m3, 0, 200, 0, 200, 0, 0.005}', $;'BEC ' + varName + ', IDL dekadal domposite') ;, MAPINFO=geoInfo
      FLAGS = '{255=missing}')
    ;reproject and resample to MODIS
    ;here I have to set the path befor sensing the gdal command because the spawn window have a different path
    str1spawn = 'set path=C:\Program Files\EMC NetWorker\nsr\bin;C:\Python278\Scripts;C:\Python278\Lib\site-packages;C:\Python278;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\OSGeo4W64\bin;C:\Program Files\TortoiseGit\bin;C:\Program Files\Exelis\IDL82\bin\bin.x86_64;C:\Program Files (x86)\Skype\Phone\;C:\Program Files\MATLAB\R2016a\bin;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3;d:\Users\meronmi\AppData\Local\Continuum\Anaconda3\Scripts;X:\dev\gdal\2.2.1\bin;X:\dev\gdal\2.2.1\bin\gdal\apps;c:\MRT\bin;d:\Users\meronmi\AppData\Local\Microsoft\WindowsApps'
    str2spawn = 'gdalwarp -t_srs EPSG:4326 -te -180 -56.00892859 180 75 -s_srs EPSG:6933 -tr 0.00892857143 0.00892857143 -of ENVI ' + base_fn_out + '.img ' + base_fn_out + 'BOKUgrid.img'
    SPAWN, str1spawn + ' & ' +str2spawn, /HIDE, /NOWAIT
  ENDFOR
  
  
  
  
;  firstYyyymmdd = [FIX(STRMID(firstDate, 0, 4)), FIX(STRMID(firstDate, 4, 2)), FIX(STRMID(firstDate, 6, 2))]
;  lastYyyymmdd = [FIX(STRMID(lastDate, 0, 4)), FIX(STRMID(lastDate, 4, 2)), FIX(STRMID(lastDate, 6, 2))]
;  
;  ;find the first
;  ;always avoid assuming that all data are present, so define the period of interest dfirst to dlast, turn it to JD and see waht is available
;  indFirst = WHERE((firstYyyymmdd[0] EQ yyyymmdd[0,*]) AND $
;                   (firstYyyymmdd[1] EQ yyyymmdd[1,*]) AND $
;                   (firstYyyymmdd[2] EQ yyyymmdd[2,*]))
;  indFirst = indFirst[0] ; in case more than two files are present make sure we use both
;  eos = 0 ;end of series condition
;  WHILE eos EQ 0 DO BEGIN
;    ;indFirst is the index to the first image of the ten day
;    ;now find the subscript of the last 
;    ;if firs is is 1 or 11, the last is 10 or 20
;    ;if it is 21, the last is the last of the month
;    CASE yyyymmdd[2,indFirst] OF
;      1: indLast = WHERE((yyyymmdd[0,indFirst] EQ yyyymmdd[0,*]) AND $  ;yyyy
;                        (yyyymmdd[1,indFirst] EQ yyyymmdd[1,*]) AND $  ;mm
;                        (10 EQ yyyymmdd[2,*]), count)                         ;dd
;      11:indLast = WHERE((yyyymmdd[0,indFirst] EQ yyyymmdd[0,*]) AND $
;                        (yyyymmdd[1,indFirst] EQ yyyymmdd[1,*]) AND $
;                        (20 EQ yyyymmdd[2,*]), count)
;      21: BEGIN
;        indLast = WHERE((yyyymmdd[0,indFirst] EQ yyyymmdd[0,*]) AND $
;                       (yyyymmdd[1,indFirst] EQ yyyymmdd[1,*])) ;here I have all of that month
;        
;        indLast = indLast[WHERE(MAX(yyyymmdd[2,indLast]) EQ yyyymmdd[2,indLast], count)]
;        END
;    ENDCASE
;    indLast = indLast[-1] ;take the last one i case there are more than one file per day 
;    IF (count GT 0) THEN BEGIN
;      ; do the 10-day comp
;      ; determine how many images are to be used
;      nimages = N_ELEMENTS(fns[indFirst:indLast])
;      PRINT, STRTRIM(nimages,2) + ' images for ' + STRTRIM(yyyymmdd[0,indFirst],2) + ' '  + STRTRIM(yyyymmdd[1,indFirst],2) + ' ' + STRTRIM(yyyymmdd[2,indFirst],2)
;      
;       
;      ; update first
;      indFirst = indLast + 1
;      ;check that it is at the biginning of a 10-day period and that data are present
;    ENDIF ELSE BEGIN
;      ; no last was found, do not attempt making the composite 
;      eos = 1
;    ENDELSE
;  ENDWHILE

  PRINT, 'Here
END