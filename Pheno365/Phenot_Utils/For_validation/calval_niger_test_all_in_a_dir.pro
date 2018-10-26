PRO calval_niger_test_all_in_a_dir
;Using gerorefenced measurements of biomass (Y), this function compute, for a given RS indicator (X) 
;specified in an input text file, the following statistics:
; - R2 of Y vs X
; - RMSE
; - BIAS
;
;Statistics are compute in fitting and cross validation leaving one year out.
;Therefore the cross val stats represents the performances of the linear model empirically calibrated on the 
;available dataset
;
;Statistics are compute for the overall dataset and by site
;
;INPUT
; info_fn: full path of the text file containg the following info:
; - ground_data_fn: full path file name of the csv containing the biomass observations
; - rs_data_fn:  full path file name of the bil file containg the rs INDICATOR OF INTEREST (E.G., cfapar, sos, ..)
;OUTPUT
; csv file with input specification and resulting statistics named XX and located in the directory of
; the Rs data file
;
;EXAMPLE of previous calval niger

;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_acc_Niger.txt')


;Program settings
boxdim = 3        ;dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
valnan2 = -999   ;value for no data in the Rs file (other than NaN). Typically -9999 for pheno variables

;Retrieve info for the required job
ground_data_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\NIGER_IDL_FORMAT_2014-03-05.csv'
rs_data_dir = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\all_acc_results'
rs_data_fn = FILE_BASENAME(FILE_SEARCH(rs_data_dir+'\*.hdr'), $
  '.hdr', /FOLD_CASE)

;rs_data_fn = read_info('rs_data_fn', info_fn)
first_year_in_rs  = 1997
doPlot = 0


;extract output path
path_out = rs_data_dir + '\results'
FILE_MKDIR, path_out
;extract RS indicator used without path
rs_data_fn_no_path = rs_data_fn
rs_data_fn = rs_data_dir + '\' + rs_data_fn


;Read the table of ground data
tmp =  READ_CSV(ground_data_fn, $
  HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
;load data into a strcture with the following fields:
;ID, SiteID, Location, Longitude, Latitude, Biomass, Year, Month, Day 
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7','FIELD8','FIELD9'], $ ; if you have more than 9 fields, name as follows: FIELD01, FIELD02, ...
                        ['ID','SiteID','Location','Longitude','Latitude','Biomass','Year','Month','Day'])
; replace -9999 as missing value by NaN for biomass column
FOR i=3,5 DO BEGIN
  ind = WHERE(data.(i) EQ -9999, count)
  IF (count GT 0) THEN data.(i)[ind] = !VALUES.F_NAN
ENDFOR

;add a field for RS indicator and standard deviation of RS indicator
data = CREATE_STRUCT(data, 'RSvar', DBLARR(N_ELEMENTS(data.ID)))        
data = CREATE_STRUCT(data, 'RSvarSD', DBLARR(N_ELEMENTS(data.ID)))
;Make sure that ground data are sorted by ID
indSorted = FSORT(data.ID)
FOR i = 0, N_TAGS(tmp)-1 DO data.(i) = data.(i)[indSorted]

;open file for output
;Print results into csv
CALDAT,SYSTIME(/JULIAN), Month, Day, Year
tmp = STRTRIM(Day,2) + '_' +STRTRIM(Month,2) + '_' +STRTRIM(Year,2)
outFile = path_out + '\' + 'calval_test_all_' + tmp + '.csv'
OPENW, W1, outFile, /GET_LUN        
PRINTF, W1, ground_data_fn
PRINTF, W1, 'first_year_in_rs ' + STRTRIM(first_year_in_rs,2)
PRINTF, W1, 'rs_data_fn,Global_R2,Global_gain,Global_offset,Global_P,Global_r'

FOR ff = 0, N_ELEMENTS(rs_data_fn)-1 DO BEGIN 
  PRINT, ff, '  ', rs_data_fn[ff]
  ;Match and add the RS indicator 
  data.RSvar[*] = !VALUES.F_NAN
  data.RSvarSD[*] = !VALUES.F_NAN 
  ;Loop on all sites and all years
  lastID = -999
  FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
    ;Extract the Rs indicator whenever a new ID is processed
    IF (data.ID[i] NE lastID) THEN BEGIN
      lastID = data.ID[i]
      ; create an empty array LonLat and fill it with longitude and latitude coordinates
      LonLat = DBLARR(2)
      LonLat(0)=data.Longitude(i)
      LonLat(1)=data.Latitude(i)
      ;get the Rs data
      IF N_ELEMENTS(LonLat) NE 2 THEN STOP
      z_val = zprofileFromLonLat(LonLat[0], LonLat[1], rs_data_fn[ff], boxdim, valnan2)
      IF ((N_ELEMENTS(z_val) EQ 1) AND (z_val[0] EQ -1)) THEN STOP
      z_years = INDGEN(N_ELEMENTS(z_val[0,*])) + first_year_in_rs
    ENDIF
    ind = WHERE(z_years EQ data.Year[i], count)
    IF (data.Year[i] LT first_year_in_rs) OR (data.Year[i] EQ 2013) THEN BEGIN
      data.RSvar[i] = !VALUES.F_NAN
      data.RSvarSD[i] = !VALUES.F_NAN
    ENDIF ELSE BEGIN
      IF ((count EQ 0) OR (count GT 1)) THEN STOP
      data.RSvar[i] = z_val[0,ind]
      data.RSvarSD[i] = z_val[1,ind]
    ENDELSE    
  ENDFOR
  
  ;Analysis
  ;######################################
  ;Fitting stats
  ;Global
  r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass', colorInd = data.Year)
  ;test without worst sites (update ID of sites)
  ;ind = WHERE((data.ID NE 1) AND (data.ID NE 4) AND (data.ID NE 5) AND (data.ID NE 15) AND $
    ;(data.ID NE 50) AND (data.ID NE 51) AND (data.ID NE 60) AND (data.ID NE 78) AND (data.ID NE 79) $
   ; AND (data.ID NE 82) AND (data.ID NE 85) AND (data.ID NE 88) AND (data.ID NE 92) AND (data.ID NE 95) $ 
    ;AND (data.ID NE 2) AND (data.ID NE 10) AND (data.ID NE 12) AND (data.ID NE 16) AND (data.ID NE 24) $
    ;AND (data.ID NE 66) AND (data.ID NE 86) $
    ;AND (data.ID NE 17) AND (data.ID NE 22) AND (data.ID NE 32) AND (data.ID NE 81) AND (data.ID NE 87) AND (data.ID NE 89))
  ;r2GainOff_b = LinRegMulti(data.RSvar[ind], data.Biomass[ind], doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass - worst sites removed', colorInd = data.Year[ind])
  ;By site
;  wh1 = WINDOW(WINDOW_TITLE='Regression by site')
;  siteList = UNIQLIST(data.ID)
;  r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
;  FOR i = 0, 35 DO BEGIN
;    ind = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
;      'ID' + STRTRIM(data.ID[ind[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDFOR
;  
;  wh2 = WINDOW(WINDOW_TITLE='Regression by site')
;  siteList = UNIQLIST(data.ID)
;  r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
;  FOR i = 36, 71 DO BEGIN
;    ind = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
;      'ID' + STRTRIM(data.ID[ind[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDFOR
;  
;  wh3 = WINDOW(WINDOW_TITLE='Regression by site')
;  siteList = UNIQLIST(data.ID)
;  r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
;  FOR i = 72, N_ELEMENTS(siteList)-1 DO BEGIN
;    ind = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
;      'ID' + STRTRIM(data.ID[ind[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDFOR
;  
;  wh = WINDOW(WINDOW_TITLE='Regression by site')
;  siteList = UNIQLIST(data.ID)
;  r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
;  FOR i = 0, N_ELEMENTS(siteList)-1 DO BEGIN
;    ind = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
;      'ID' + STRTRIM(data.ID[ind[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDFOR
   
  str_r2GainOff_b = STRTRIM(r2GainOff_b,2)    
  PRINTF, W1, rs_data_fn[ff] + ',' + str_r2GainOff_b[0] + ',' + str_r2GainOff_b[1] + ',' + str_r2GainOff_b[2] + $
                               ',' + str_r2GainOff_b[3]+ ',' + str_r2GainOff_b[4]
ENDFOR  ;ff


FREE_LUN, W1
END 