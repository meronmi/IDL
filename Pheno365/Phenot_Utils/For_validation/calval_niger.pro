Function calval_niger, info_fn
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
;EXAMPLE
;PRINT, calval_niger('E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\all_acc_results\info_validation_test_Niger.txt')
;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_accb_Niger.txt')
;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_acc_Niger.txt')
;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_sos_Niger.txt')
;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_eos_Niger.txt')
;PRINT, calval_niger('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\REALIGN_ON_SOS\info_validation_maxv_Niger.txt')


;COMPILE_OPT IDL2

;Program settings
boxdim = 3        ;dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
valnan2 = -999   ;value for no data in the Rs file (other than NaN). Typically -9999 for pheno variables

;Retrieve info for the required job
ground_data_fn = read_info('ground_data_fn', info_fn)
rs_data_fn = read_info('rs_data_fn', info_fn)
first_year_in_rs  = FIX(read_info('first_year_in_rs', info_fn))
doPlot = FIX(read_info('doPlot', info_fn))
;extract output path
path_out = STRMID(rs_data_fn, 0, STRPOS(rs_data_fn,'\',/REVERSE_SEARCH))
;extract RS indicator used without path
rs_data_fn_no_path = STRMID(rs_data_fn, STRPOS(rs_data_fn,'\',/REVERSE_SEARCH)+1, STRLEN(rs_data_fn))


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
  ;Extract the Rs indicator whenever a new ID is processed
  IF (data.ID[i] NE lastID) THEN BEGIN
    lastID = data.ID[i]
    ; create an empty array LonLat and fill it with longitude and latitude coordinates
    LonLat = DBLARR(2)
    LonLat(0)=data.Longitude(i)
    LonLat(1)=data.Latitude(i)
    ;get the Rs data
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




;Analysis
;######################################
;Box plot of the Rs var
idList = uniqlist(data.ID)
yearList = uniqlist(data.Year)
tmp = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
tmpSD = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  ind = WHERE(data.ID EQ idList[i])
  tmp[i,*] = data.RSvar[ind]
  tmpSD[i,*] = data.RSvarSD[ind]
ENDFOR
;check where I don't have at least 5 elements for tmp (cannot compute boxplot)
fin = TOTAL(FINITE(tmp), 2)
indKeep = WHERE(fin GE 5)
indRemove = WHERE(fin LT 5)
IF (indRemove[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs'
  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs are not boxplotted, sites:'
  PRINT, idList[indRemove]
  idList2 = idList[indKeep]
  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
  tmp2 = tmp[indKeep,*]
  ;tmp2SD = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
  ;tmp2SD = tmpSD[indKeep,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  idList2 = idList
  tmp2 = tmp
  ;tmp2SD = tmpSD
ENDELSE

;create boxplots for RS variable
res = CREATEBOXPLOTDATA(tmp2)
gh1 = BOXPLOT(res, TITLE = 'Rs var ' + tmpTitle, YTITLE=rs_data_fn_no_path, XRANGE =[-1, N_ELEMENTS(idList2)])
gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh1.XTICKFONT_SIZE = 8
gh1.XMINOR = 0
gh1.XTICKLEN = 0

;check where I don't have at least 5 elements for tmpSD (cannot compute boxplot)
fin = TOTAL(FINITE(tmpSD), 2)
indKeepSD = WHERE(fin GE 5)
indRemoveSD = WHERE(fin LT 5)
IF (indRemoveSD[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs for SD'
  PRINT, STRTRIM(N_ELEMENTS(indRemoveSD),2)+' sites containing less than 5 obs for SD are not boxplotted, sites:'
  PRINT, idList[indRemoveSD]
  idList3 = idList[indKeepSD]
  tmp2SD = FLTARR(N_ELEMENTS(indKeepSD),N_ELEMENTS(yearList))
  tmp2SD = tmpSD[indKeepSD,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  idList3 = idList
  tmp2SD = tmpSD
ENDELSE

;create boxplots for SD of RS variable
res = CREATEBOXPLOTDATA(tmp2SD)
gh1 = BOXPLOT(res, TITLE = 'Rs var spatial SD' + tmpTitle, YTITLE=rs_data_fn_no_path, XRANGE =[-1, N_ELEMENTS(idList2)])
gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh1.XTICKFONT_SIZE = 8
gh1.XMINOR = 0
gh1.XTICKLEN = 0

;Fitting stats
;;######################################
;Global
;r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass')
r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass', colorInd = data.Year)
;test without worst sites (update ID of sites)
;ind = WHERE((data.ID NE 1) AND (data.ID NE 4) AND (data.ID NE 5) AND (data.ID NE 15) AND $
  ;(data.ID NE 50) AND (data.ID NE 51) AND (data.ID NE 60) AND (data.ID NE 78) AND (data.ID NE 79) $
 ; AND (data.ID NE 82) AND (data.ID NE 85) AND (data.ID NE 88) AND (data.ID NE 92) AND (data.ID NE 95) $ 
  ;AND (data.ID NE 2) AND (data.ID NE 10) AND (data.ID NE 12) AND (data.ID NE 16) AND (data.ID NE 24) $
  ;AND (data.ID NE 66) AND (data.ID NE 86) $
  ;AND (data.ID NE 17) AND (data.ID NE 22) AND (data.ID NE 32) AND (data.ID NE 81) AND (data.ID NE 87) AND (data.ID NE 89))
;r2GainOff_b = LinRegMulti(data.RSvar[ind], data.Biomass[ind], doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass - worst sites removed', colorInd = data.Year[ind])
;######################################
;By site
wh1 = WINDOW(WINDOW_TITLE='Regression by site')
siteList = UNIQLIST(data.ID)
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
FOR i = 0, 35 DO BEGIN
  ind = WHERE(data.ID EQ siteList[i], count)
  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
    'ID' + STRTRIM(data.ID[ind[0]],2), $
    rs_data_fn_no_path, 'Biomass', $
    layoutVec = [6,6,i+1])
ENDFOR

wh2 = WINDOW(WINDOW_TITLE='Regression by site')
siteList = UNIQLIST(data.ID)
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
FOR i = 36, 71 DO BEGIN
  ind = WHERE(data.ID EQ siteList[i], count)
  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
    'ID' + STRTRIM(data.ID[ind[0]],2), $
    rs_data_fn_no_path, 'Biomass', $
    layoutVec = [6,6,i+1])
ENDFOR

wh3 = WINDOW(WINDOW_TITLE='Regression by site')
siteList = UNIQLIST(data.ID)
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
FOR i = 72, N_ELEMENTS(siteList)-1 DO BEGIN
  ind = WHERE(data.ID EQ siteList[i], count)
  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
    'ID' + STRTRIM(data.ID[ind[0]],2), $
    rs_data_fn_no_path, 'Biomass', $
    layoutVec = [6,6,i+1])
ENDFOR

wh = WINDOW(WINDOW_TITLE='Regression by site')
siteList = UNIQLIST(data.ID)
r2GainOff_by_site = FLTARR(5, N_ELEMENTS(siteList))
FOR i = 0, N_ELEMENTS(siteList)-1 DO BEGIN
  ind = WHERE(data.ID EQ siteList[i], count)
  r2GainOff_by_site[*,i] = LinRegMulti(data.RSvar[ind], (data.Biomass[ind]), doPlot, $
    'ID' + STRTRIM(data.ID[ind[0]],2), $
    rs_data_fn_no_path, 'Biomass', $
    layoutVec = [6,6,i+1])
ENDFOR

;Summary plot (3 plots), R2, gain an offset by site
gh = PLOT(siteList, r2GainOff_by_site[0,*], XTITLE='Site ID', YTITLE='R2', $
  LINESTYLE="--", SYMBOL="o", $
  XMINOR = 0, XRANGE=[0,MAX(siteList)+1], LAYOUT=[1,3,1])
gh.SYM_SIZE = 0.75
gh.SYM_FILLED=1
gh = PLOT(siteList, r2GainOff_by_site[1,*], XTITLE='Site ID', YTITLE='Gain', $
  LINESTYLE="--", SYMBOL="o", $
  XMINOR = 0, XRANGE=[0,MAX(siteList)+1], LAYOUT=[1,3,2], /CURRENT)
gh.SYM_SIZE = 0.75
gh.SYM_FILLED=1
gh = PLOT(siteList, r2GainOff_by_site[2,*]/1000.0, XTITLE='Site ID', YTITLE='Offset/1000', $
  LINESTYLE="--", SYMBOL="o", $
  XMINOR = 0, XRANGE=[0,MAX(siteList)+1], LAYOUT=[1,3,3], /CURRENT)
gh.SYM_SIZE = 0.75
gh.SYM_FILLED=1
;only r
meanR = STRING(FORMAT='(F4.2)',MEAN(r2GainOff_by_site[4,*], /NAN, /DOUBLE))
gh0 = PLOT(siteList,r2GainOff_by_site[4,*], "g",  MARGIN = [0.15, 0.15, 0.20, 0.15], XRANGE=[0,MAX(siteList)+1], $
  XTITLE='Site ID', YTITLE='r', AXIS_STYLE = 1, NAME='r', TITLE=rs_data_fn_no_path+', AVG(r) = ' + meanR)
gh1 = PLOT(siteList,r2GainOff_by_site[3,*], "b",  MARGIN = [0.15, 0.15, 0.20, 0.15], XRANGE=[0,MAX(idList)+1], $
  XTITLE='Site ID', /CURRENT, AXIS_STYLE = 0, NAME='P-val')
gh2 = PLOT(siteList,siteList*0+0.05, "b",  MARGIN = [0.15, 0.15, 0.20, 0.15], $
  /OVERPLOT, AXIS_STYLE = 0, NAME='P=0.05', LINESTYLE=2)
ah1 =    AXIS('y',  TARGET = gh1, LOCATION = [max(gh1.xrange),0,0], TEXTPOS = 1, TITLE = 'P-val')
!null = LEGEND(target=[gh0, gh1], SHADOW=0, THICK=0)
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