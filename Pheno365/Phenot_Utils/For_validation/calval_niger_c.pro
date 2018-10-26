Function calval_niger_c, info_fn

; cleared version of calval_niger
; 
; Using gerorefenced measurements of biomass (Y), this function compute, for a given RS indicator (X) 
; specified in an input text file, the following statistics:
; - R2 of Y vs X
; - RMSE
; - BIAS
;
; Statistics are compute in fitting and cross validation leaving one year out.
; Therefore the cross val stats represents the performances of the linear model empirically calibrated on the 
; available dataset
;
; Statistics are compute for the overall dataset and by site
;
; INPUT
; info_fn: full path of the text file containg the following info:
; - ground_data_fn: full path file name of the csv containing the biomass observations
; - rs_data_fn:  full path file name of the bil file containg the rs INDICATOR OF INTEREST (E.G., cfapar, sos, ..)
; - sites: full path file name of the csv containing the lists of biomass sites that should be included in analysis (need to specify which column (= sitelist) should be used)
; 
; OUTPUT
; csv file with input specification (and resulting statistics named XX and) located in the directory of the RS data file
; csv file where remote sensing data is added to the biomass observations
; csv file with results of the regression (R2, gain, offset, pValGain, r) for single sites
; csv file where estimated biomass (based on district specific gain and offset) is added to observed biomass
; txt file with summary of all "global" regression results
;
; EXAMPLE
; PRINT, calval_niger_c('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_accb_Niger.txt')
; PRINT, calval_niger_c('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_acc_Niger.txt')
; PRINT, calval_niger_c('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_bestRS_Niger.txt')

;Program settings
boxdim = 3        ;dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
valnan2 = -999   ;value for no data in the RS file (other than NaN). Typically -9999 for pheno variables


;Retrieve info for the required job
ground_data_fn = read_info('ground_data_fn', info_fn)
rs_data_fn = read_info('rs_data_fn', info_fn)
first_year_in_rs  = FIX(read_info('first_year_in_rs', info_fn))
doPlot = FIX(read_info('doPlot', info_fn))
sites = read_info('sitelist', info_fn)

;Extract output path
path_out = STRMID(rs_data_fn, 0, STRPOS(rs_data_fn,'\',/REVERSE_SEARCH))
;Extract RS indicator used without path
rs_data_fn_no_path = STRMID(rs_data_fn, STRPOS(rs_data_fn,'\',/REVERSE_SEARCH)+1, STRLEN(rs_data_fn))

;Read the table of ground data
tmp =  READ_CSV(ground_data_fn, $
  HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
;Load data into a structure with the following fields:
;ID, SiteID, Location, Longitude, Latitude, Biomass, Year, Month, Day 
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7','FIELD8','FIELD9'], $ ; if you have more than 9 fields, name as follows: FIELD01, FIELD02, ...
                        ['ID','SiteID','Location','Longitude','Latitude','Biomass','Year','Month','Day'])
;Replace -9999 as missing value by NaN for biomass column
FOR i=3,5 DO BEGIN
  ind = WHERE(data.(i) EQ -9999, count)
  IF (count GT 0) THEN data.(i)[ind] = !VALUES.F_NAN
ENDFOR
        
;Match and add the RS indicator
;add a field for RS indicator and standard deviation of RS indicator
data = CREATE_STRUCT(data, 'RSvar', DBLARR(N_ELEMENTS(data.ID)))
data.RSvar[*] = !VALUES.F_NAN
data = CREATE_STRUCT(data, 'RSvarSD', DBLARR(N_ELEMENTS(data.ID)))
data.RSvarSD[*] = !VALUES.F_NAN
;Make sure that ground data are sorted by ID
;indSorted = SORT(data.ID)
indSorted = FSORT(data.ID)
FOR i = 0, N_TAGS(tmp)-1 DO data.(i) = data.(i)[indSorted]
;Loop on all sites and all years
lastID = -999
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  ;Extract the RS indicator whenever a new ID is processed
  IF (data.ID[i] NE lastID) THEN BEGIN
    lastID = data.ID[i]
    ; create an empty array LonLat and fill it with longitude and latitude coordinates
    LonLat = DBLARR(2)
    LonLat(0)=data.Longitude(i)
    LonLat(1)=data.Latitude(i)
    ;get the RS data
    IF N_ELEMENTS(LonLat) NE 2 THEN STOP
    z_val = zprofileFromLonLat(LonLat[0], LonLat[1], rs_data_fn, boxdim, valnan2)
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

;Save data with added RS data in csv file
;write_csv, 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\Niger_biomass_rs_accb.csv', data


;Analysis
;######################################

;;Box plot of the RS var
;idList = uniqlist(data.ID)
;yearList = uniqlist(data.Year)
;tmp = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
;tmpSD = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
;FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
;  ind = WHERE(data.ID EQ idList[i])
;  tmp[i,*] = data.RSvar[ind]
;  tmpSD[i,*] = data.RSvarSD[ind]
;ENDFOR
;;check where I don't have at least 5 elements for tmp (cannot compute boxplot)
;fin = TOTAL(FINITE(tmp), 2)
;indKeep = WHERE(fin GE 5)
;indRemove = WHERE(fin LT 5)
;IF (indRemove[0] NE -1) THEN BEGIN
;  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs'
;  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs are not boxplotted, sites:'
;  PRINT, idList[indRemove]
;  idList2 = idList[indKeep]
;  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
;  tmp2 = tmp[indKeep,*]
;  ;tmp2SD = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
;  ;tmp2SD = tmpSD[indKeep,*]
;ENDIF ELSE BEGIN
;  tmpTitle = ''
;  idList2 = idList
;  tmp2 = tmp
;  ;tmp2SD = tmpSD
;ENDELSE
;
;;create boxplots for RS variable
;res = CREATEBOXPLOTDATA(tmp2)
;gh1 = BOXPLOT(res, TITLE = 'Rs var ' + tmpTitle, YTITLE=rs_data_fn_no_path, XRANGE =[-1, N_ELEMENTS(idList2)])
;gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
;gh1.XTICKFONT_SIZE = 8
;gh1.XMINOR = 0
;gh1.XTICKLEN = 0
;
;;check where I don't have at least 5 elements for tmpSD (cannot compute boxplot)
;fin = TOTAL(FINITE(tmpSD), 2)
;indKeepSD = WHERE(fin GE 5)
;indRemoveSD = WHERE(fin LT 5)
;IF (indRemoveSD[0] NE -1) THEN BEGIN
;  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs for SD'
;  PRINT, STRTRIM(N_ELEMENTS(indRemoveSD),2)+' sites containing less than 5 obs for SD are not boxplotted, sites:'
;  PRINT, idList[indRemoveSD]
;  idList3 = idList[indKeepSD]
;  tmp2SD = FLTARR(N_ELEMENTS(indKeepSD),N_ELEMENTS(yearList))
;  tmp2SD = tmpSD[indKeepSD,*]
;ENDIF ELSE BEGIN
;  tmpTitle = ''
;  idList3 = idList
;  tmp2SD = tmpSD
;ENDELSE
;
;;create boxplots for SD of RS variable
;res = CREATEBOXPLOTDATA(tmp2SD)
;gh1 = BOXPLOT(res, TITLE = 'Rs var spatial SD' + tmpTitle, YTITLE=rs_data_fn_no_path, XRANGE =[-1, N_ELEMENTS(idList2)])
;gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
;gh1.XTICKFONT_SIZE = 8
;gh1.XMINOR = 0
;gh1.XTICKLEN = 0


;Fitting stats globally
;;######################################

;all sites

;r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass')
;r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass', colorInd = data.Year)
;print, 'global R2 between biomass and RS var' 
;print, r2GainOff_b

;Selected sites
;
;Select sites to use for regression based on list in csv-file
list =  READ_CSV(sites, HEADER=hdr, MISSING_VALUE=-9999)
;print, hdr
;Select column with sites, which should be used (column A: all sites, column ID = 0; $
  ;column CA: sites with min 4 measurements, no neg gain (in regression with RSvar, within pasture zone, column ID = 78)
siteList = list.(78)
;Give column name to variable so that it could be used in the output file name
column = hdr[78]
print, column
;Get ride of no entry elements, represented by 0 (for site selection there are still 103 entries (=original amount of sites), but not all are used
siteList = siteList(WHERE(siteList GT 0))

;Create and fill variable index with indices of all measurements that should be used for regression
index = MAKE_ARRAY(1)
FOR i = 0, (N_ELEMENTS(siteList)-1) DO index = [index, WHERE(data.ID EQ siteList[i])]
;Remove first entry of index (=0), which was just created to have a starting value to fill the variable
index = index[1:N_ELEMENTS(index)-1]
 
;Perform the regression on all data of the selected sites (global regression)
r2GainOff_b = LinRegMulti(data.RSvar[index], data.Biomass[index], doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass - selected sites', colorInd = data.Year[index])
print, 'Global regression between biomass and RS var for selected sites' 
print, 'number of sites:' 
print, N_ELEMENTS(siteList)
print, 'regression results [r^2, gain, offset, pValGain, r] ='
print, r2GainOff_b


;Calculate a cross validated r2 (leaving one year out) of all available sites (not just selected)
; 
;Remove all missing data
ind = WHERE(FINITE(data.Biomass) AND FINITE(data.RSvar), count)
;print, 'index where biomass available:'
;print, ind
crossval1 = cv_stats_lo_year_o(data.RSvar[ind], data.Biomass[ind], data.Year[ind])
print, 'Cross validated global regression between biomass and RS var (all available sites)'
;print, 'data.ID[ind]'
;print, data.ID[ind]
;print, 'UNIQLIST(data.ID[ind])'
;print, UNIQLIST(data.ID[ind])
print, 'number of sites:'
print, N_ELEMENTS(UNIQLIST(data.ID[ind]))
print, 'cross validation [R2cv,RMSEcv,mod_eff]'
print, crossval1

;Calculate a cross validated r2 (leaving one year out) for all selected sites
;
;Extract ID, RSvar, Biomass and Year from data for selected sites and store in array
dataSelect = CREATE_STRUCT('ID', data.ID[index], 'RSvar', data.RSvar[index], 'Biomass', data.Biomass[index], 'Year', data.Year[index])
;Remove all missing data
ind2 = WHERE(FINITE(dataSelect.Biomass) AND FINITE(dataSelect.RSvar), count)
;print, 'index where biomass available:'
;print, ind2
crossval2 = cv_stats_lo_year_o(dataSelect.RSvar[ind2], dataSelect.Biomass[ind2], dataSelect.Year[ind2])
print, 'Cross validated global regression between biomass and RS var (for selected sites)'
print, 'number of sites:'
print, N_ELEMENTS(UNIQLIST(dataSelect.ID[ind2]))
;print, UNIQLIST(dataSelect.ID[ind2])
print, 'cross validation [R2cv,RMSEcv,mod_eff]'
print, crossval2

;Fitting stats by site
;######################################

;create empty array, which should contain regression results for all used sites
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))

;noMoreWin = 0
;; create a window to display up to 36 sub-graphics of regression
;wh1 = WINDOW(WINDOW_TITLE='Regression by site')

;; calculate regression stats for single sites and plot biomass vs. RS var for each site (first 36 sites)
;FOR i = 0, 35 DO BEGIN
;  ; check, if there are 36 sites available, if not stop calculation
;  IF (i LE N_ELEMENTS(siteList)-1) THEN BEGIN 
;    ; tmp stores indices
;    tmp = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[tmp], (data.Biomass[tmp]), doPlot, $
;      'ID' + STRTRIM(data.ID[tmp[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDIF ELSE BEGIN
;    noMoreWin = 1
;  ENDELSE
;ENDFOR
;
;; create new window, if there are more than 36 sites
;IF (noMoreWin EQ 0) THEN wh2 = WINDOW(WINDOW_TITLE='Regression by site')
;; calculate regression stats for single sites and plot biomass vs. RS var for each site (site 36 to site 72)
;FOR i = 36, 71 DO BEGIN                               
;  IF (i LE N_ELEMENTS(siteList)-1) THEN BEGIN
;    ; tmp stores indices
;    tmp = WHERE(data.ID EQ siteList[i], count)
;    r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[tmp], (data.Biomass[tmp]), doPlot, $
;      'ID' + STRTRIM(data.ID[tmp[0]],2), $
;      rs_data_fn_no_path, 'Biomass', $
;      layoutVec = [6,6,i+1])
;  ENDIF ELSE BEGIN
;    noMoreWin = 1
;  ENDELSE
;ENDFOR
;
;; create new window, if there are more than 72 sites
;IF (noMoreWin EQ 0) THEN wh3 = WINDOW(WINDOW_TITLE='Regression by site')
;; calculate regression stats for single sites and plot biomass vs. RS var for each site (site 36 to site 72)
;FOR i = 72, N_ELEMENTS(siteList)-1 DO BEGIN          
;  ; tmp stores indices
;  tmp = WHERE(data.ID EQ siteList[i], count)
;  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[tmp], (data.Biomass[tmp]), doPlot, $
;    'ID' + STRTRIM(data.ID[tmp[0]],2), $
;    rs_data_fn_no_path, 'Biomass', $
;    layoutVec = [6,6,i+1])
;ENDFOR


;Calculate regression stats for all selected sites in one step
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
FOR i = 0, N_ELEMENTS(siteList)-1 DO BEGIN
  ; tmp stores indices
  tmp = WHERE(data.ID EQ siteList[i], count)
  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[tmp], (data.Biomass[tmp]), 0, $
    'ID' + STRTRIM(data.ID[tmp[0]],2), $
    rs_data_fn_no_path, 'Biomass', $
    layoutVec = [6,6,i+1])
ENDFOR

;Save regression details in csv by creating new variable that contains site ID and regression results
dims = SIZE(r2GainOff_by_site)
tmp = FLTARR(dims[1]+1, dims[2])
tmp[0,*] = siteList
tmp[1:*, *] = r2GainOff_by_site
write_csv, 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\Niger_biomass_accb_regr_' + column + '.csv', tmp, $
  HEADER = ['siteID', 'R2', 'Gain', 'Offset', 'pValGain', 'r']


;Perform a regression between measured biomass and estimated biomass (bases on district specific gain and offset)

;Add a field in the data file for estimated biomass
data = CREATE_STRUCT(data, 'Bestimated', DBLARR(N_ELEMENTS(data.ID)))
data.Bestimated[*] = !VALUES.F_NAN
;Open file which contains the district gains and offsets
districtInfo =  READ_CSV('S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\Niger_DistReg_69.csv', HEADER=hdr, MISSING_VALUE=-9999)
;print, hdr
districtSiteID = districtInfo.(0)
;print, districtSiteID
districtGain = districtInfo.(8)
;print, districtGain
districtOffset = districtInfo.(9)
;print, districtOffset
;Fill the field with estimated biomass values or NaN (if there is no biomass value)
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  ID = data.ID(i)
  index2 = WHERE(districtSiteID EQ ID, count)
  IF (data.RSvar[i] EQ 'NaN') THEN BEGIN 
    data.Bestimated[i] = NaN 
  ENDIF ELSE BEGIN
    data.Bestimated[i] = districtGain[index2] * data.RSvar[i] + DistrictOffset[index2]
  ENDELSE
ENDFOR
;save data with added estimated biomass in csv file
write_csv, 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\Niger_biomass_rs_accb_Best.csv', data

;;create and fill variable in with indices of all measurements that should be used for regression
;in = MAKE_ARRAY(1)
;FOR i = 0, (N_ELEMENTS(siteList)-1) DO in = [in, WHERE(data.ID EQ siteList[i])]
;;remove first entry of in (=0), which was just created to have a starting value to fill the variable
;in = in[1:N_ELEMENTS(in)-1]
;;print, 'in ='
;;print, in

;Do regression between biomass and estimated biomass
r2GainOff_globalDistr = LinRegMulti(data.Biomass[index], data.Bestimated[index], doPlot, 'Biomass vs. estimated biomass (sel. sites)', $
  'Biomass [kg ha-1] of selected sites', 'Estimated biomass [kg ha-1]')
print, 'Global regression between biomass and estimated biomass (based on district specific gain and offset)'
print, 'number of sites:'
print, N_ELEMENTS(siteList) ; !!!!!!!!!!!!!!!!!!!!!!!
print, 'r2GainOff [r^2, gain, offset, pValGain, r] ='
print, r2GainOff_globalDistr


;District specific regression plus crossvalidation (selected sites, n = 69)
;#################################################

tab =  READ_CSV('S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\Niger_Sites_District.csv', HEADER=hdr, MISSING_VALUE=-9999)
;print, hdr
table = Make_array(2,69, /FLOAT)
table[0,*] = tab.(0)
table[1,*] = tab.(1)

;Get unique list of districts
deps = UNIQLIST(table[1,*])
;print, 'IDs of districts;'
;print, deps
;Get amount of districts
n_deps = N_ELEMENTS(deps)

;Create array in which observerd biomass (yobs) and estimated biomass (ypred) will be stored
yobs = make_array(1)
ypred = make_array(1)

FOR k = 0, n_deps-1 DO BEGIN 
  i = deps[k]
  inde = WHERE(table[1,*] EQ i, count)
  IDs = table[0, inde]
  
  ;Create and fill variable inde2 with indices of all measurements that should be used for regression (index for siteIDs)
  inde2 = MAKE_ARRAY(1)
  FOR j = 0, (N_ELEMENTS(IDs)-1) DO inde2 = [inde2, WHERE(data.ID EQ IDs[j])]
  ;Remove first entry of inde (=0), which was just created to have a starting value to fill the variable
  inde2 = inde2[1:N_ELEMENTS(inde2)-1]
;  print, 'indices of all measurements for districts'
;  print, i
;  print, inde2

  ;Perform the regression on all sites of districts i
  r2GainOff_d = LinRegMulti(data.RSvar[inde2], data.Biomass[inde2], 0, 'Biomass vs. RSvar' , 'RSvar', 'Biomass')
  print, 'Regression between biomass and RS var for district no. '
  print, i
  print, 'number of sites:'
  print, N_ELEMENTS(IDs)
  print, 'Regression results [r^2, gain, offset, pValGain, r] ='
  print, r2GainOff_d


  ;Perform a cross validated regression for districts i (leaving one year out)
  
  ;Create a structure with RSvar, biomass and year data of all sites in districts i
  depData = CREATE_Struct('RSvar', data.RSvar[inde2], 'Biomass', data.Biomass[inde2], 'Year', data.Year[inde2])
  ;print, depData
  ;Create variable with indices of all depData where RSvar and Biomass available (remove all missing data)
  inde3 = where(FINITE(depData.Biomass) AND FINITE(depData.RSvar), count)
  ;print, 'Index where biomass and RSvar available:'
  ;print, inde3
  crossval2 = cv_stats_lo_year2_o(depData.RSvar[inde3], depData.Biomass[inde3], depData.Year[inde3])
  print, 'Cross validated regression between biomass and RS var for district no.'
  print, i
  print, 'Number of sites:'
  print, N_ELEMENTS(IDs)
  print, 'Results of crossvalidation of district no.'
  print, i
  print, 'R2cv,  RMSEcv,  mod_eff' 
  print, crossval2.R2CV, crossval2.RMSECV, crossval2.MOD_EFF
  ;store yobs and ypred from cross validation in variables
  yobs = [yobs,crossval2.YOBS]
  ypred = [ypred,crossval2.YPRE]
  
ENDFOR
  
;Remove first entry of yobs and ypred, which were just created to have a starting value to fill the variables
yobs = yobs[1:N_ELEMENTS(yobs)-1]
ypred = ypred[1:N_ELEMENTS(ypred)-1]

;Make a regression between district specific observed biomass (yobs) and estimated biomass (ypred) (cross validated)
r2GainOff_dist = LinRegMulti(yobs, ypred, doPlot, 'Yobs vs. Ypred (cv, district specific)', 'Biomass [kg ha-1] of selected sites', $
  'Estimated biomass, cv [kg ha-1]')
print, 'Crossvalidated regression between biomass and estimated biomass (district specific)'
print, 'number of sites:'
print, N_ELEMENTS(table[0,*])
print, 'Results of regression (cv) [r^2, gain, offset, pValGain, r] ='
print, r2GainOff_dist


; Print all results (regression performances) in a file

caldat, SYSTIME(/JULIAN), mm, dd, yy
fn = 'Results_calval_niger_' + column + STRTRIM(yy,2) + '_' + STRTRIM(mm,2) + '_'  + STRTRIM(dd,2) + '.txt'

OPENW, W1, 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\' + fn, /GET_LUN
PRINTF, W1, 'Global regression between biomass and RS var (using all selected sites)'
PRINTF, W1, 'Number of sites:'
PRINTF, W1, N_ELEMENTS(siteList)
PRINTF, W1, 'Regression results [r^2, gain, offset, pValGain, r] ='
PRINTF, W1, r2GainOff_b
PRINTF, W1, ''
PRINTF, W1, 'Cross validated global regression between biomass and RS var (all available sites)'
PRINTF, W1, 'Number of sites:'
PRINTF, W1, N_ELEMENTS(UNIQLIST(data.ID[ind]))
PRINTF, W1, 'Cross validated regression results [R2cv,RMSEcv,mod_eff]'
PRINTF, W1, crossval1
PRINTF, W1, ''
PRINTF, W1, 'Global regression between measured biomass and estimated biomass (bases on district specific gain and offset)'
PRINTF, W1, 'Number of sites:'
PRINTF, W1, N_ELEMENTS(siteList)
PRINTF, W1, 'Regression results [r^2, gain, offset, pValGain, r] ='
PRINTF, W1, r2GainOff_globalDistr
PRINTF, W1, ''
PRINTF, W1, 'Regression between measured biomass and estimated biomass (bases on district specific gain and offset), cross validated'
PRINTF, W1, 'Number of sites:'
PRINTF, W1, N_ELEMENTS(table[0,*])
PRINTF, W1, 'Cross validated regression results [r^2, gain, offset, pValGain, r] ='
PRINTF, W1, r2GainOff_dist

FREE_LUN, W1

;Print results into csv

CALDAT,SYSTIME(/JULIAN), Month, Day, Year
tmp = STRTRIM(Day,2) + '_' +STRTRIM(Month,2) + '_' +STRTRIM(Year,2)
outFile = path_out + '\' + 'calval_' + rs_data_fn_no_path + '_' + tmp + '.csv'
OPENW, W1, outFile, /GET_LUN
PRINTF, W1, ground_data_fn
PRINTF, W1, rs_data_fn
PRINTF, W1, 'first_year_in_rs ' + STRTRIM(first_year_in_rs,2)
FREE_LUN, W1
END 