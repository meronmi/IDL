PRO get_ini_info

;ini file file containing processing details 
fn_ini = 'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\GGW_Senegal_East\GEE_download\GGW_SE_ini.txt'
;project file containg id of the project and year/years of implementation
fn_prj = 'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\GGW_Senegal_East\GEE_download\GGW_SE_prj.txt'

verbose = read_info('verbose', fn_ini) ;0 suppress some info..
sensor = read_info('sensor', fn_ini) ;'modis' 'ls'
;max rmse difference use to search for a control (rmse on class similarity and slc)
max_rmse_dif = FLOAT(read_info('max_rmse_dif', fn_ini))
;k: number of non-overlapping controls to be selected, minimum 1
k = FIX(read_info('k', fn_ini))
;id of the project and start and end of project implementation
tmp = READ_ASCII(fn_prj, DATA_START = 1, DELIMETER = ',') 
target_id_yr = REFORM(tmp.FIELD1)
;SECTION ON CRITERIA TO DEFINE THE CONTROLS
;The selction is based on:
;1. distance to the project, that is only taken into account by looking in a given area;
;2. similar class composition;
;
; 1.
; the maximum distance is automatically computed as follows:
; Ap is the area of the project
; As is the circular area used to look for controls
; we set that As = X Ap (here the idea is that about X non overlapping candidates are present, this is
; not fully true because of square shape of controls, they are less but hopefully not too much)
; then we compute tha radius search as
; max_dist = (As / pi)^1/2
; X is defined here
area_search_ratio  = FLOAT(read_info('area_search_ratio', fn_ini))
;images before and after
fnames = read_info('fnames', fn_ini)
fnames = STRTRIM(STRSPLIT(fnames, ',', /EXTRACT),2)
;In and OUT directories
in_dir = read_info('in_dir', fn_ini)
out_dir = read_info('out_dir', fn_ini)
;file storing the project mask (0 where no project, id of the project otherways)
;note that in the mask the value +999 is reserved to area to be excluded
;(it may be a project area we do notwant to analyze or an area we belive subjected to changes that we do not want to select as control)
fn_project_mask = read_info('fn_project_mask', fn_ini)
FILE_COPY, fn_ini, out_dir + '\ini_file_used.txt',  /OVERWRITE
FILE_COPY, fn_prj, out_dir + '\prj_file_used.txt',  /OVERWRITE

;file storing LULC classification; leave it 0 for doing classification automatically.
fn_usr_classification  = read_info('fn_usr_classification', fn_ini)
IF (fn_usr_classification EQ 0) THEN fn_usr_classification = ''
;max number of years to consider before and after
nMax = FIX(read_info('nMax', fn_ini))
CASE sensor OF
  'ls': BEGIN
    ;If landsat is selected fill the following lines
    slc_present  = FIX(read_info('slc_present', fn_ini)) ;set it to 1 if using LS7 with slc problem, and fill the following line
    slc_class   = FIX(read_info('slc_class', fn_ini)) ;;class value reserved for LS7 slc missing data, it will be set to 200 (0 creates problem with the search)
    bands2use  = read_info('bands2use', fn_ini) ;band used to build the slc mask and then to classify. SWIR may not be included as it has more SLC affected area
    bands2use = FIX(STRTRIM(STRSPLIT(bands2use, ',', /EXTRACT),2))
    ;2.
    ;Besides requiring similar class composition, as we want to keep the sample size of project
    ;and control balanced, we give more weight to difference in the class "SLC". That is to say,
    ;we want the control has a number of valid pixels similar to the project. So we consider class
    ;composition and SLC seprately:
    ;this for class composition:
    ;first = SQRT(TOTAL((fract_countMat[i,j,0:-2] - fract_comp[0:-2])^2)/FLOAT(npv-1))
    ;this is for SLC:
    ;second = SQRT((fract_countMat[i,j,-1] - fract_comp[-1])^2)
    ;and the overall RMSE is calulated as:
    ;rmse_total[i,j] = first * (1.0 - fract_w_of_slc_fraction) + second * fract_w_of_slc_fraction
    ;The user variable fract_w_of_slc_fraction defines this weight.
    ;fract_w_of_slc_fraction is between 1.0 and 0.0, default is 0.25
    fract_w_of_slc_fraction   = FLOAT(read_info('fract_w_of_slc_fraction', fn_ini))
  END
  'modis':BEGIN
    ;note that is has to be a BYTE with valid values 0-200 (original sMODIS data)
    fn_eMODIS_ts4class = read_info('fn_eMODIS_ts4class', fn_ini)
    ;specificy dates of fn_eMODIS_ts4class
    MODISts_firstYear = FIX(read_info('MODISts_firstYear', fn_ini))
    valNoDataModis = FIX(read_info('valNoDataModis', fn_ini))
    timesatValNan = DOUBLE(read_info('timesatValNan', fn_ini)) ;in fn_eMODIS_ts4class, values GE than this are no data
    timesatScaling = DOUBLE(read_info('timesatScaling', fn_ini)) ;value used for Nan in MODIS images analysed with timesat/R
  END
  ELSE: STOP
ENDCASE
PRINT, 'finito'
END