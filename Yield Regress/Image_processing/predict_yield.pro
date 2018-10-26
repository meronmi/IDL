;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    io_info_file
;    options_info_file
;    RS_fname
;    rep_yield_fname
;    RS_product_name
;    level
;
;
;
; :Author: meronmi
;-
FUNCTION predict_yield, io_info_file, options_info_file, RS_fname, RS_product_name, $ 
                        rep_yield_fname, level
; This function apply the desired linear regression to estimate the yield
; from the desired RS indicators.
; Since it's extremely important to use the datasets and options used 
; when calibrating the regression, this function uses the IO and option settings
; used in the calibration.
 
;INPUT parameters:
; io_info_file: IO file used in regression calibration (regress_handler) and copied in the output dir
; options_info_file: options file used in regression calibration (regress_handler) and copied in the output dir
; RS_fname: absolute path of the file containing one band floating RS indicator used for prediction 
;           the file must be float
; RS_product_name: the name of the RS product as in the rep_yield_file (used to load the regression coefficients)
; rep_yield_fname: the full path filename of the yield report  that has to be used
; level: regression level (can be "RSI-glob_Coeff-glob" or "RSI-glob_Coeff-dep")

;all other parameters are loaded from the rep yield or io_info_file or options_info_file 


;OUTPUT:
; report file containg the RS indicator processed and avaraged at dep level,
; yield estimation for department. This report is placed in the output dir 

;EXAMPLE 
; PRINT, predict_yield('K:\Tunisia\IO_info.txt', 'K:\Tunisia\OPTIONS_info.txt','K:\Tunisia\27_debug_Mar\dNDVI12_2009','dNDVI12','K:\Tunisia\27_debug_Mar\durum_wheat\rep_yield_durum_wheat_4-3.csv','RSI-glob_Coeff-glob')
; PRINT, predict_yield('K:\Tunisia\TEST_NDVI\IO_info_marsNDVI.txt', 'K:\Tunisia\TEST_NDVI\OPTIONS_info.txt','K:\Tunisia\TEST_NDVI\prediction\MarsNDVI11_2009','MarsNDVI11','K:\Tunisia\TEST_NDVI\Analysis\durum_wheat\rep_yield_durum_wheat_6-27.csv','RSI-glob_Coeff-dep')
; 
; PRINT, predict_yield('K:\Tunisia\4JOSH\on sowed\MarsFAPAR\durum_wheat\IO_info.txt', 'K:\Tunisia\4JOSH\on sowed\MarsFAPAR\durum_wheat\OPTIONS_info.txt','K:\Tunisia\VGT_data\2013 extraction\vt1311a.img_sc','MarsFAPAR11','K:\Tunisia\4JOSH\on sowed\MarsFAPAR\durum_wheat\rep_yield_durum_wheat_6-17.csv','RSI-glob_Coeff-dep')
; PRINT, predict_yield('K:\Tunisia\4JOSH\on sowed\MarsFAPAR\soft_wheat\IO_info.txt', 'K:\Tunisia\4JOSH\on sowed\MarsFAPAR\soft_wheat\OPTIONS_info.txt','K:\Tunisia\VGT_data\2013 extraction\vt1311a.img_sc','MarsFAPAR11','K:\Tunisia\4JOSH\on sowed\MarsFAPAR\soft_wheat\rep_yield_soft_wheat_6-17.csv','RSI-glob_Coeff-dep')
; PRINT, predict_yield('K:\Tunisia\4JOSH\on sowed\MarsFAPAR\barley\IO_info.txt', 'K:\Tunisia\4JOSH\on sowed\MarsFAPAR\barley\OPTIONS_info.txt','K:\Tunisia\VGT_data\2013 extraction\vt1311a.img_sc','MarsFAPAR11','K:\Tunisia\4JOSH\on sowed\MarsFAPAR\barley\rep_yield_barley_6-17.csv','RSI-glob_Coeff-dep')

;2014 run
; DW
; PRINT, predict_yield('E:\Tunisia\4JOSH\on sowed\MarsNDVI\durum_wheat\IO_info.txt', 'E:\Tunisia\4JOSH\on sowed\MarsNDVI\durum_wheat\OPTIONS_info.txt','E:\Tunisia\VGT_data\NDVI 2014\vt1413i.img_sc','MarsNDVI13','E:\Tunisia\4JOSH\on sowed\MarsNDVI\durum_wheat\rep_yield_durum_wheat_6-19-2014.csv','RSI-glob_Coeff-dep')
; SW
; PRINT, predict_yield('E:\Tunisia\4JOSH\on sowed\MarsNDVI\soft_wheat\IO_info.txt', 'E:\Tunisia\4JOSH\on sowed\MarsNDVI\soft_wheat\OPTIONS_info.txt','E:\Tunisia\VGT_data\NDVI 2014\vt1413i.img_sc','MarsNDVI13','E:\Tunisia\4JOSH\on sowed\MarsNDVI\soft_wheat\rep_yield_soft_wheat_6-19-2014.csv','RSI-glob_Coeff-dep')
; BA
; PRINT, predict_yield('E:\Tunisia\4JOSH\on sowed\MarsNDVI\barley\IO_info.txt', 'E:\Tunisia\4JOSH\on sowed\MarsNDVI\barley\OPTIONS_info.txt','E:\Tunisia\VGT_data\NDVI 2014\vt1413i.img_sc','MarsNDVI13','E:\Tunisia\4JOSH\on sowed\MarsNDVI\barley\rep_yield_barley_6-19-2014.csv','RSI-glob_Coeff-dep')
dlmtr=','

;RETRIEVE THE RELEVANT IO INFO
base_path = read_info('base_path', io_info_file)
layer_path=base_path+'\lc_dp_layer'
;department map
dept_fn = read_info('dept_fn', io_info_file)  
;area fraction crop mask file name
AFCM_fn = read_info('AFCM_fn', io_info_file)  
;maximum fAPAR file name (it is optionally used to mask pixels having it lower than a threshold). It's a bil file with a number of band equivalent to that of RS products
maxFapar_fn = read_info('maxFapar_fn', io_info_file) 
;output path
out_dir = read_info('out_dir', io_info_file)
;Crop base name 
crop_base_name = read_info('crop_base_name', io_info_file)                ;'durum_wheat' ;'soft_wheat';'barley';'durum_wheat'
out_path=base_path+out_dir+'\'+crop_base_name
;define absolute path for input files
cm_fn=layer_path+'\'+AFCM_fn 
;relative path to directory storing Remote Sensing data
rs_product_dir = read_info('rs_product_dir', io_info_file)   
pheno_path=base_path + rs_product_dir    
maxFapar_fn=pheno_path+'\'+maxFapar_fn
dept_fn=layer_path+'\'+dept_fn

;define RS product chracteristics (from VGT data at 1000 m resolution)
ns = read_info('ns', io_info_file)                      ; number of samples
nl = read_info('nl', io_info_file)                      ; number of lines

;Aggregation options
UseCropMask=FIX(read_info('UseCropMask', options_info_file))             ;1: use Area Fraction Crop Mask and compute a weighted average at department level
                        ;0: don't use any mask, compute the average of all pixels falling in the department
MinCropFract=FLOAT(read_info('MinCropFract', options_info_file))         ;(0-1), used only when UseCropMask EQ 1. 
                        ;Pixels having a fractional crop cover LT MinCropFract are 
                        ;neglected (not considered in the department average)                        
UseMaxFaparThreshold=FIX(read_info('UseMaxFaparThreshold', options_info_file))   ;1: use max fapar threshold. It filters out pixels having 
                        ;the average yearly maximum LT MaxFaparThreshold). The stack file 
                        ;containing all the yeraly maxima must be provided.
                        ;0: consider all pixels
MaxFaparThreshold=FLOAT(read_info('MaxFaparThreshold', options_info_file))  ;(fAPAR or NDVI units). used only when UseMaxFaparThreshold EQ 1
                        ;only pixels having a maxFapar value GT than threshold are
                        ;considered. maxFapar is usually extracted from the corresponding stack file (must be provided)
MinPixPerDept=FIX(read_info('MinPixPerDept', options_info_file))         ;minimum acceptable number of pixels per department. 
                        ;if the retained number of pixels per department is lower,
                        ;the program issues a warning

;List of the departments to be considered (department map pixel value) 
dept_ids=read_info('dept_ids', options_info_file)   
dept_ids = FIX(STRSPLIT(dept_ids, ',', /EXTRACT))
;Exclude data (set to 0 to consider all or list the data to be escluded)
excludeDepts=read_info('excludeDepts', options_info_file)   
excludeDepts = FIX(STRSPLIT(excludeDepts, ',', /EXTRACT))

;READ HOW TO TREAT -999
;read RS product to be investigated
pheno_products  = read_info('pheno_products', io_info_file)  
pheno_products = STRSPLIT(pheno_products, ',', /EXTRACT)
pheno_val999   = read_info('pheno_val999', io_info_file) 
IF (STRTRIM(pheno_val999[0],2) EQ 'all_NaN') THEN BEGIN
  pheno_val999 = FLTARR(N_ELEMENTS(pheno_products)) * !VALUES.F_NAN
ENDIF ELSE BEGIN
  pheno_val999 = STRSPLIT(pheno_val999, ',', /EXTRACT)
  ind0 = WHERE(pheno_val999 EQ '0', count0)
  indNaN = WHERE(pheno_val999 NE '0', countNaN)
  pheno_val999 = FLTARR(N_ELEMENTS(pheno_products))
  pheno_val999[ind0]=0.0
  IF (countNaN GT 0) THEN pheno_val999[indNaN]=!VALUES.F_NAN
ENDELSE
;Retrive the selected RS indicator
ind = WHERE(STRMATCH(pheno_products, RS_product_name), count)
IF (count EQ 0) THEN STOP
val999 = pheno_val999[ind]


;RETRIEVE DATA
;1 REGRESSION INFO
read_reg_coeff, rep_yield_fname, RS_product_name, level, depId, b0, b1

;2 Fractional crop map
fcm=BYTARR(ns,nl)                  ;for storing fractional crop mask (originally 0-200 = 0-100%)
IF (UseCropMask EQ 1) THEN BEGIN
  OPENR, R1, cm_fn, /GET_LUN        
  READU, R1, fcm & CLOSE, R1
  fcm=FLOAT(fcm)/200.0                             ; now 0-1
ENDIF ELSE BEGIN                                   ; do not use crop mask
  PRINT, 'Warning: crop mask will not be used as requested, MinCropFract set to 0'
  MinCropFract=0
  fcm=fcm*0+1.0
ENDELSE
; 3 Department map
dept=BYTARR(ns,nl)                               ;for storing the department map     
OPENR, R1, dept_fn, /GET_LUN  ;read the file
READU, R1, dept & CLOSE, R1

; 4 LOAD MAXFAPAR IF USED
IF (UseMaxFaparThreshold EQ 1) THEN avg_max_fapar = get_avg_max_fapar(maxFapar_fn)

;Open the RS product and set -999 to correct value (the same used in the regression analysis)
data = FLTARR(ns,nl)
OPENR, R1, RS_fname, /GET_LUN        
READU, R1, data & CLOSE, R1
ind = WHERE(data EQ -999, count)
IF (count GT 0) THEN BEGIN
   PRINT, '-999s found and replaced with '+STRTRIM(val999,2)
   data[ind] = val999
END

;Now make the prediction and save it in the report
CALDAT,SYSTIME(/JULIAN),mm, dd
strdata=strtrim(mm,2)+'-'+strtrim(dd,2)
report_prediction = 'prediction' + '_' + crop_base_name + '_' +strdata + '.csv'
OPENW, urep, out_path+'\'+report_prediction, /GET_LUN
PRINTF, urep, 'Model_type,RS_product,RS_data_used,Dep_id,b0,b1,Estimated_yield'

;find the depts on which the regression was tuned (indipendently by the model level
used_depts = SETDIFFERENCE(dept_ids, excludeDepts)
IF STRMATCH(level, 'RSI-glob_Coeff-glob') THEN BEGIN
  offset = FLTARR(N_ELEMENTS(used_depts))
  gain = offset
  offset[*] = b0
  gain[*] = b1
  depId = used_depts
ENDIF ELSE BEGIN
  IF ~STRMATCH(level, 'RSI-glob_Coeff-dep') THEN STOP
  offset = b0
  gain = b1
ENDELSE
FOR i = 0, N_ELEMENTS(used_depts)-1 DO BEGIN
  ind = WHERE(depId EQ used_depts[i], count)
  IF (count EQ 0) THEN STOP
  out = extract_RS_avg_val(data, dept, depId[i], $
                             UseMaxFaparThreshold, avg_max_fapar, MaxFaparThreshold, $
                             UseCropMask, fcm, MinCropFract, $
                             MinPixPerDept)
  estimate = offset[i] +  gain[i] * DOUBLE(out.wRSindex)
  PRINTF, urep, level + dlmtr + RS_product_name + dlmtr + RS_fname + dlmtr + STRTRIM(depId[i],2) + dlmtr + $
                STRTRIM(offset[i],2) + dlmtr + STRTRIM(gain[i],2) + dlmtr + $
                STRTRIM(estimate,2) 
ENDFOR
FREE_LUN, urep
RETURN, 0
END