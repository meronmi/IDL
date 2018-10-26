PRO regress_handler, io_info_file, options_info_file
; RUN the regression based on the IO settings and OPTION setting given in io_info_file and options_info_file
; io_info_file: full path of the txt file containing the IO settings
; options_info_file: full path of the txt file containing the OPTIONS
; 
; EXAMPLE regress_handler, 'K:\Tunisia\IO_info.txt', 'K:\Tunisia\OPTIONS_info.txt'
; 

@cb_io.comm 
@cb_options.comm  
;##################################################################################################
;READ IO SPECIFICATIONS
;root directory where input and output data are stored
base_path = read_info('base_path', io_info_file)
;relative path to directory storing Remote Sensing data
rs_product_dir = read_info('rs_product_dir', io_info_file)   
pheno_path=base_path + rs_product_dir
layer_path=base_path+'\lc_dp_layer'
yield_path=base_path+'\yield_data'
;output path
out_dir = read_info('out_dir', io_info_file)


                    ;\VGT_data\bil\DIR_RECOMPOSED_UppEnv_17_feb\REALIGN_ON_eos' ;'\BP_data\BIL\test';'\VGT_data\bil\DIR_RECOMPOSED_UppEnv_17_feb\REALIGN_ON_eos'
;administrative boundaries and yield data has to be placede in the folders: ;'\lc_dp_layer' and '\yield_data'
;area fraction crop mask file name (bsq, byte, crop mask pixel value= 0-200 DM (for % coverage ranging from 0 to 100%))
AFCM_fn = read_info('AFCM_fn', io_info_file)                              ;'AFI_CNCT_ceral_mask_Feb_2012';'CNCT_binary_cereals_mask'; 'Tunisia_glcovCRO'
;maximum fAPAR file name (it is optionally used to mask pixels having it lower than a threshold). It's a bil file with a number of band equivalent to that of RS products
maxFapar_fn = read_info('maxFapar_fn', io_info_file)   
;department raster map file name
dept_fn = read_info('dept_fn', io_info_file)   
;Crop base name (yield file and surface names are formed with it, yield_' + crop_base_name, 'surface_' + crop_base_name 
crop_base_name = read_info('crop_base_name', io_info_file)                ;'durum_wheat' ;'soft_wheat';'barley';'durum_wheat'
;Varibale Y for the regression (28/06/2012)
varY = read_info('varY', io_info_file) 
;OUTPUT: the output path is done with: out_path=path+'\yield_reg_out\'+crop_base_name
out_path=base_path+out_dir+'\'+crop_base_name

;define RS product chracteristics (from VGT data at 1000 m resolution)
ns = read_info('ns', io_info_file)                      ; number of samples
nl = read_info('nl', io_info_file)                      ; number of lines
nb = read_info('nb', io_info_file)                       ; number of years  from 1997-1998

; Set the yield year to be associated with the first band of the RS product. This is used to match RS and with the yield. 
; e.g. for tunisia the first band goes from dek 20 of 97 to dek 20 of 98, the frst_RS_yr for the match will be 1998 (because they are related to yield in 1998)
; if I use the the Aligned with 1 according to EOS, il primo è 97 (97 e 98 dovrebbero essere vuoti)
yield_Year_4_frst_RS_band = read_info('yield_Year_4_frst_RS_band', io_info_file) ;1997;1998

;define absolute path for input files
cm_fn=layer_path+'\'+AFCM_fn     
maxFapar_fn=pheno_path+'\'+maxFapar_fn
dept_fn=layer_path+'\'+dept_fn
dept_names_fn=layer_path+'\'+read_info('dept_names_fn', io_info_file)                      
;yield_data
yield_base_name = varY + '_' +  crop_base_name;'yield_' + crop_base_name
surface_base_name = 'surface_' + crop_base_name
yield_fn = yield_path + '\' + varY + '_' + crop_base_name + '.csv'
surface_fn = yield_path + '\' + 'surface_' + crop_base_name + '.csv'

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
  IF (count0 NE 0) THEN pheno_val999[ind0]=0.0
  IF (countNaN NE 0) THEN pheno_val999[indNaN]=!VALUES.F_NAN
ENDELSE


;copy the info file into the output path for future reference
FILE_MKDIR, out_path
FILE_COPY, io_info_file, out_path+'\IO_info.txt', /OVERWRITE

;##################################################################################################
;READ OPTION SPECIFICATIONS
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
excludeYears = read_info('excludeYears', options_info_file)
excludeYears = FIX(STRSPLIT(excludeYears, ',', /EXTRACT))
;Surface from crop mask or from file 
ComputeSurface = read_info('ComputeSurface', options_info_file)     ;1: the surface for production calculation is computed from the crop mask
                        ;0: the surface is read from file (to be set in set_io)

;Plotting and warning settings
doplot = read_info('doplot', options_info_file);1              ;0 suppress plots
deleteWindows = read_info('deleteWindows', options_info_file)         ;to preserve memory
verbose = read_info('verbose', options_info_file);1             ;0 suppress all warnings

;copy the option file into the output path for future reference
FILE_COPY, options_info_file, out_path+'\OPTIONS_info.txt', /OVERWRITE

ret=rs_yield_regress()
PRINT, 'Processing completed'
END
