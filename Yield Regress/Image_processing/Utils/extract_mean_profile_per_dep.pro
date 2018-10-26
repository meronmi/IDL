FUNCTION extract_mean_profile_per_dep, io_info_file, depid
;extract the weighted average profile (under the afi mask) for a given dep (needs 
;the dep map). Files name are given in the fname
;EXAMPLES:

;print, extract_mean_profile_per_dep('K:\Tunisia\IO_info_profile_extraction.txt',9)  
dlmtr=','   ;for csv output
;##################################################################################################
;READ IO SPECIFICATIONS
MinCropFract = read_info('MinCropFract', io_info_file)
;root directory where input and output data are stored
base_path = read_info('base_path', io_info_file)
;relative path to directory storing Remote Sensing data
rs_product_dir = read_info('rs_product_dir', io_info_file)   
rs_path=base_path + rs_product_dir
rs_fn = read_info('rs_fn', io_info_file) 
layer_path=base_path+'\lc_dp_layer'
;output path
out_dir = read_info('out_dir', io_info_file)
;administrative boundaries and yield data has to be placede in the folders: ;'\lc_dp_layer' and '\yield_data'
;area fraction crop mask file name (bsq, byte, crop mask pixel value= 0-200 DM (for % coverage ranging from 0 to 100%))
AFCM_fn = read_info('AFCM_fn', io_info_file)                              ;'AFI_CNCT_ceral_mask_Feb_2012';'CNCT_binary_cereals_mask'; 'Tunisia_glcovCRO'
;maximum fAPAR file name (it is optionally used to mask pixels having it lower than a threshold). It's a bil file with a number of band equivalent to that of RS products
;department raster map file name
dept_fn = read_info('dept_fn', io_info_file)   
;Crop base name (yield file and surface names are formed with it, yield_' + crop_base_name, 'surface_' + crop_base_name 
;OUTPUT: the output path is done with: out_path=path+'\yield_reg_out\'+crop_base_name
out_path=base_path+out_dir
FILE_MKDIR, out_path

;define RS product chracteristics (from VGT data at 1000 m resolution)
ns = read_info('ns', io_info_file)                      ; number of samples
nl = read_info('nl', io_info_file)                      ; number of lines
nb = read_info('nb', io_info_file)                       ; number of years  from 1997-1998

frstdek = read_info('frstdek', io_info_file)                       ; number of years  from 1997-1998

;define absolute path for input files
cm_fn = layer_path+'\'+AFCM_fn     
dept_fn = layer_path+'\'+dept_fn
rs_fn = rs_path +'\'+rs_fn 
;##################################################################################################

;open csv
OPENW, uprof, out_path+'\profile_of_dep_'+STRTRIM(depid,2)+'.csv', /GET_LUN

;READ the stack
rsmat=FLTARR(ns,nl,nb)
OPENR, R1, rs_fn, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
FOR line=0, nl-1, 1L DO rsmat[*, line, *]=float(line_ass_data[line])
CLOSE, R1

; Open the fractional crop map
fcm=BYTARR(ns,nl)                  ;for storing fractional crop mask (originally 0-200 = 0-100%)
OPENR, R1, cm_fn, /GET_LUN        
READU, R1, fcm & CLOSE, R1
fcm=FLOAT(fcm)/200.0                             ; now 0-1

dept=BYTARR(ns,nl)                               ;for storing the department map     
; Open the  department map
OPENR, R1, dept_fn, /GET_LUN  ;read the file
READU, R1, dept & CLOSE, R1
CLOSE, R1

;A) compute the wighted average for RsLayer

;A1) mask for that dept, and store the number of pixel of that dept
ind_dep = WHERE(dept EQ depid, tmp) 
  
;A2) refine mask for those pixels with a Fractional Cover of Crop GT 0
ind_cm = WHERE(fcm[ind_dep] GT 0, tmp)
ind_cm = ind_dep[ind_cm] 


;A4) refine mask for those pixels having at least MinCropFract (Above Crop Mask Threshold), if required
ind_cm_AFT_ACMT = WHERE(fcm[ind_cm] GT MinCropFract, tmp) 
ind_cm_AFT_ACMT = ind_cm[ind_cm_AFT_ACMT]
;debug
openw, uim, out_path+'\mask',/get_lun
qqq = bytarr(ns,nl)
qqq[ind_cm_AFT_ACMT]=1
writeu, uim, qqq
free_lun, uim



PRINTF, uprof, 'Dek'+dlmtr+'WeightedAvg'+dlmtr+'N='+STRTRIM(N_ELEMENTS(ind_cm_AFT_ACMT),2)
                
FOR i = 0, nb-1 DO BEGIN
  ;IF (i+frstdek EQ 64) THEN STOP
  tmp=rsmat[*,*,i]
  indf = WHERE(FINITE(tmp[ind_cm_AFT_ACMT]), countf)
  idnf = ind_cm_AFT_ACMT[indf]
  totalw=TOTAL(fcm[idnf])
  avg = TOTAL((tmp[idnf] * fcm[idnf]))/totalw
  PRINTF, uprof, STRTRIM(i+frstdek,2)+dlmtr+STRTRIM(avg,2)
ENDFOR
FREE_LUN, uprof
END