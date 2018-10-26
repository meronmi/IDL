FUNCTION a1_anomaly_analysis_JD, info_file
COMMON Share_image_production, path_out, fname, fNameX, aoper, ns, nl, nb, transformJD2DOC, s, ngspy_image, doavg
;  Purpose:
;     For an array of pheno product and given user parameters (set in the info file), this function
;     - [optional] transforms the pheno indicator from Julian Day to relative DOC (Day Of Cycle) using JD2DOC.propheno_dekoc. 
;       This option should be used for PE (Phenological Events) expressed in JD (so not with len, acc, maxV, etc.)
;     - compute the required anomaly operation (dev, devY, min, ..) for all the pheno bands
;     - then, for a given band of pheno (boi, band of interest in the phenol product)
;       produces department level averaged maps separately for those regions having 1 
;       growing season per year and 2 ngspy 
;     - for a correct interpretation, input must be realigned.
;     
;     be care: if the total average is rewuired run dlt or dev, if you use dltY or 
;     devY you get the avg excluding Y
;     
;  Outcome:
;     - *_dekoc files if required
;     - *_XXX anomaly files (where XXX is anomaly opertaion required)
;     - 1ngspy_?boi_XXX_depAVG  dep avg anomaly files for areas with 1 gs per year
;     - 2ngspy_?boi_XXX_depAVG  dep avg anomaly files for areas with 2 gs per year


;  Usage:
;     PRINT, ..

;  Input parameters: 
;     - info_file full path 
;  Return value: 
;     0:  normal completion 
;    10:  error has occurred
;    

;########### USER SETTINGS #########################################################
;**average at dep level or not
doavg = FIX(read_info('doavg', info_file))
;**how to consider -999 /at the moment pheno set acc to 0 when sos -999
val999 = (read_info('val999', info_file)) ; can be 'NaN or 0'
CASE val999 OF
  'NaN': val999 = !VALUES.F_NAN   ;when computing anomalies of timing
  '0':   val999 = 0.0             ;when computing acc (even if it should be already 0)
  ELSE: STOP
ENDCASE

;**First DOY of the Cycle to which the predocts have been aligned (frstDOYofCycle in A0_TZP_realign_pheno_products_JD.pro)
frstDOYofCycle = FIX(read_info('frstDOYofCycle', info_file))

;**Define here the band to be analyzed in detail (for which to compute the anomalies)
boi = FIX(read_info('boi', info_file)) ;Band Of Interest (starts from 1)
boi = boi -1

;**Anomaly operation to be performed
aoper = (read_info('aoper', info_file)) ;'dlt' ;'dltY';'devY';'devY'       ;see pheno_anmomalies for options

;**Pheno products to be analyzed, analyze separately pdekad (progressive dekad as sos and eos)
;that must be dekoc, and TZP data (len, acc) that don't have to be dekoc
fnames = (read_info('fnames', info_file))
fnames=STRTRIM(STRSPLIT(fnames, ',', /EXTRACT),2) ;'A19-1997_sos','A19-1997_eos','A19-1997_maxt';['A19-1997_maxt'];['A19-1997_eos'];['A20-1997_len']         ;fnames=['sos','len']

;** request to transform data from JD to DOC (Day Of Cycle)
transformJD2DOC = FIX(read_info('transformJD2DOC', info_file)) ;1 to perform dekoc transformation, 0 otherway

;**work path 
;Base dir (where globstat is)
basePath = read_info('basePath', info_file) ;'K:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv'  ;basePath='K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv\REALIGN\debug';;'X:\HoA\VGT data\pheno_products' ;'K:\HoA\Pheno_products'
;Dir of realigned data
realigned_dir = read_info('realigned_dir', info_file)
path=basePath + '\' + STRTRIM(realigned_dir,2)
;Dir of output
path_out=path + '\' + 'ANOMALIES'
;**completeness map (provide '' if no relaibility check has to be performed'
;if a correct file name is provided only those seasons having reliability GT rel_threshold are used
fname_rel = read_info('fname_rel', info_file) ;'A19-1997_comp';'A20-1997_comp'; 'rel'
;relative threshold
rel_threshold = read_info('rel_threshold', info_file) ;80.0 

;**globstat map
fname_globstat = read_info('fname_globstat', info_file) ;'globstat'
;**absolute path of the raster department map, if required
IF (doavg EQ 1 ) THEN $
  dep_fname = read_info('dep_fname', info_file); 'D:\Users\meronmi\Documents\JRC\JRC Documents\HoA 2011\GIS\HoA_Gaul2_raster'
;dep_fname='K:\Tunisia\crop_mask\department_of_int_CROP_AFI_GT50%'

;**file specitification
ns = FIX(read_info('ns', info_file))
nl = FIX(read_info('nl', info_file))
nb = FIX(read_info('nb', info_file))
data_type = FIX(read_info('data_type', info_file))

;###################################################################################

FILE_MKDIR, path_out

;Open globstat to retrieve the areas with 1/2 GS
ngspy_image=read_1_BIL_band(basePath+'\'+fname_globstat, 2, ns, nl, 2, 1)
IF (N_ELEMENTS(ngspy_image) EQ 1) THEN STOP
FOR i=0, N_ELEMENTS(fnames)-1 DO BEGIN
  ;Divide the analysis into two anomaly anlysis: first and second seasons, respectively
  FOR s=1, 2 DO BEGIN
    fname=fnames[i]
    fNameX=fname+strtrim(s,2)
    IF (fname_rel NE '') THEN BEGIN
      rel_fNameX=fname_rel+strtrim(s,2)
      ; assign full path at the reliability map (just because if '' in TZPpheno_anomalies.pro is not used
      rel_fNameX_FullPath = path+'\'+rel_fNameX
    ENDIF ELSE BEGIN
      rel_fNameX_FullPath = ''
    ENDELSE 
    
    ;compute Day Of Cycle it if required (all bands)
    IF (transformJD2DOC EQ 1) THEN BEGIN 
      PRINT, 'Warning (a1_anomaly_analysis_JD.pro): transformation from Julian Days to DOC was asked on: '+fNameX
      ret = TZP_imageJD_2_imageDOC(frstDOYofCycle, path+'\'+fNameX, path_out+'\'+fNameX, data_type, ns, nl, nb)
      IF (ret NE 0) THEN STOP
      fNameX=fNameX+'_DOC'
    ENDIF ELSE BEGIN
      FILE_COPY, path + '\' + fNameX, path_out + '\' + fNameX, /OVERWRITE
      FILE_COPY, path + '\' + fNameX + '.hdr', path_out + '\' + fNameX + '.hdr', /OVERWRITE
    ENDELSE
        
    ;compute the anomalies (always for all bands)
    ret = TZP_pheno_anomalies_JD(aoper, path_out+'\'+fNameX, path_out+'\'+fNameX, $
                                 data_type, ns, nl, nb, rel_fNameX_FullPath, rel_threshold, val999, transformJD2DOC)
                                 
    IF (ret NE 0) THEN STOP   ;stop if a problem arises here
    IF (aoper NE 'maxval') THEN ret = anom_image_production(boi)    ;the result of this function is a bsq in any case
  
  ENDFOR  ;s
ENDFOR  ;i

                       
PRINT, 'Task anomaly_analysis completed'
RETURN, 0
END