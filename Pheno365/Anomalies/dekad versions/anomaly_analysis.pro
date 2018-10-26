FUNCTION anomaly_analysis, info_file
;  Purpose:
;     For an array of pheno product and given user parameters (set in the code), this function
;     - if required, transforms the pheno indicator from absolute decade (0 is the first dekad of the
;       fAPAR time series) to relative dekad of the cycle using pheno_dekoc. This
;       option should be used for dekadal pheno product (so not with len, acc, maxV, etc.
;     - compute the require anomaly operation (dev, devY, min, ..) for all the pheno bands
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
;     PRINT, nomaly_analysis('FULL_PATH_OF_info_file')

;  Input parameters: 
;     - info_file full path 
;  Return value: 
;     0:  normal completion 
;    10:  error has occurred
;    
;###################################################################################
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
;** read the ordinal dekad representing the value range [0.1-1] in realigned fAPAR time series, 
;   normally it is the first (1)
ord_dek_of_input_01_range = FIX(read_info('ord_dek_of_input_01_range', info_file))
offset_dek_in = ord_dek_of_input_01_range - 1  ;dekad offsetting the first dekad in the realigned fAPAR time series, normally 0
;**ordinal dekad number of the first dekad in the realigned cycle
ord_dek_of_output_01_range = FIX(read_info('ord_dek_of_output_01_range', info_file))
offset_dek_out = ord_dek_of_output_01_range - 1 ;dekad number of the first dekad in the realigned cycle

;**Define here the band to be analyzed in detail
boi = FIX(read_info('boi', info_file)) ;Band Of Interest (starts from 0)

;**Anomaly operation to be performed
aoper = (read_info('aoper', info_file)) ;'dlt' ;'dltY';'devY';'devY'       ;see pheno_anmomalies for options

;**Pheno products to be analyzed, analyze separately pdekad (progressive dekad as sos and eos)
;that must be dekoc, and TZP data (len, acc) that don't have to be dekoc
fnames = (read_info('fnames', info_file))
fnames=STRTRIM(STRSPLIT(fnames, ',', /EXTRACT),2) ;'A19-1997_sos','A19-1997_eos','A19-1997_maxt';['A19-1997_maxt'];['A19-1997_eos'];['A20-1997_len']         ;fnames=['sos','len']

dekoc_oper = FIX(read_info('dekoc_oper', info_file)) ;1 to perform dekoc transformation, 0 otherway

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




FILE_MKDIR, path_out

;Open globstat to retrieve the areas with 1/2 GS
ngspy_image=read_1_BIL_band(basePath+'\'+fname_globstat, 2, ns, nl, 2, 1)
IF (N_ELEMENTS(ngspy_image) EQ 1) THEN STOP
FOR i=0, N_ELEMENTS(fnames)-1 DO BEGIN
  ;analyse sos1 and sos2, the analys of the pixels having a second season 
  ;(sos2 is present) will be divided into two anomaly anlysis: first and second
  ;seasons, respectively
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
    
    ;dekoy it if required (all bands)
    IF (dekoc_oper EQ 1) THEN BEGIN 
      PRINT, 'Warning (anomaly_analysis.pro): dekoc operation asked on: '+fNameX
      ret= TZPpheno_dekoc(ord_dek_of_input_01_range, ord_dek_of_output_01_range, path+'\'+fNameX, path_out+'\'+fNameX, data_type, ns, nl, nb)
      IF (ret NE 0) THEN STOP
      fNameX=fNameX+'_dekoc'
    ENDIF ELSE BEGIN
      FILE_COPY, path + '\' + fNameX, path_out + '\' + fNameX, /OVERWRITE
      FILE_COPY, path + '\' + fNameX, path_out + '\' + fNameX + '.hdr', /OVERWRITE
    ENDELSE
        
    ;compute the anomalies (always for all bands)
    ret = TZPpheno_anomalies(aoper, path_out+'\'+fNameX, path_out+'\'+fNameX, $
                              data_type, ns, nl, nb, rel_fNameX_FullPath, rel_threshold, val999, dekoc_oper)
    IF (ret NE 0) THEN STOP   ;stop if a problem arises here
    
    fNameX_Aoper=fNameX+'_'+aoper
                         
    ;Start the analysis for a given boi and areas with 1 GS and those with 2 GS
    pheno_boi=    read_1_BIL_band(path_out+'\'+fNameX, 4, ns, nl, nb, boi)
    IF (N_ELEMENTS(pheno_boi) EQ 1) THEN STOP ;we have a problem..
    ;if dekoc operation was not required, fNameX is just a copy, delete it
    IF (dekoc_oper EQ 0) THEN FILE_DELETE, path_out + '\' + fNameX  
    
    pheno_dev_boi=read_1_BIL_band(path_out+'\'+fNameX_Aoper, 4, ns, nl, nb, boi)
    IF (N_ELEMENTS(pheno_dev_boi) EQ 1) THEN STOP
    
    ;if the rquired operation is not min or max, also an avg file is saved.
    ;So, if it exists (it was not a min max), include it in the analysis
    pheno_avg=0
    IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPavg') EQ 1 THEN BEGIN
      ;check if it is one band (when dev is issued or multiband, when devY is issued)
      temp_nb=read_info('bands', path_out+'\'+fNameX_Aoper+'_TZPavg'+'.hdr')
      IF (temp_nb EQ 1) THEN bb = 0 ELSE bb=boi
      pheno_avg = read_1_BIL_band(path_out+'\'+fNameX_Aoper+'_TZPavg', 4, ns, nl, temp_nb, bb) 
    ENDIF
    ;same for the SD (actually redundant, if AVG exists, also SD exists)
    pheno_sd=0
    IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPsd') EQ 1 THEN BEGIN
      ;check if it is one band (when dev is issued or multiband, when devY is issued)
      temp_nb=read_info('bands', path_out+'\'+fNameX_Aoper+'_TZPsd'+'.hdr')
      IF (temp_nb EQ 1) THEN bb = 0 ELSE bb=boi
      pheno_sd = read_1_BIL_band(path_out+'\'+fNameX_Aoper+'_TZPsd', 4, ns, nl, temp_nb, bb) 
    ENDIF
    
    FOR ngspy=s, 2 DO BEGIN 
      ;e.g. if SOS1, divide in ngspy=1 and 2, is SOS2 compute only
      ;ngpsy=2 (it has to have a second season)
      ;work only on pixels having ngspy GS per year
      ind=WHERE(ngspy_image EQ ngspy, count)
      IF (count EQ 0) THEN STOP
      tmp_pheno = pheno_boi * !VALUES.F_NAN
      tmp_pheno_dev = pheno_dev_boi * !VALUES.F_NAN
      IF (N_ELEMENTS(pheno_avg) NE 0) THEN BEGIN  ;if it exists, use it..
        tmp_pheno_avg = pheno_avg * !VALUES.F_NAN
        tmp_pheno_avg[ind] = pheno_avg[ind]
      END
      IF (N_ELEMENTS(pheno_sd) NE 0) THEN BEGIN  ;if it exists, use it..
        tmp_pheno_sd = pheno_sd * !VALUES.F_NAN
        tmp_pheno_sd[ind] = pheno_sd[ind]
      END
      tmp_pheno[ind] = pheno_boi[ind]
      tmp_pheno_dev[ind] = pheno_dev_boi[ind]
      
      ;treat seson failures
      ind999=WHERE(tmp_pheno EQ -999, count999)
      indnf= WHERE((tmp_pheno NE -999) AND (ngspy_image EQ ngspy), countnf)
      
      ;Compute and print some basic stat
      PRINT, '**********************************************************'
      PRINT, 'Basic stat of band of interest number = '+STRTRIM(boi+1,2)+' (boi = '+STRTRIM(boi,2)+')'
      PRINT, 'Analysing the area with ngspy = ' + STRTRIM(ngspy,2)
      PRINT, 'Number of pixels =              ' + STRTRIM(count,2)
      PRINT, 'Number of seson failures =      ' + STRTRIM(count999,2)
      PRINT, '% of season failures =          ' + STRTRIM(FIX(count999/double(count)*100.0d),2)
      PRINT, ''
      PRINT, 'Statistics derived on required (excluding season failures):'
      PRINT, fNameX + ' MIN-MAX =    ' + STRTRIM(MIN(tmp_pheno[indnf], /NAN))+'-'+STRTRIM(MAX(tmp_pheno[indnf], /NAN))
      PRINT, fNameX + ' AVG =    ' + STRTRIM(MEAN(tmp_pheno[indnf], /NAN, /DOUBLE))
      PRINT, fNameX + ' STDDEV = ' + STRTRIM(STDDEV(tmp_pheno[indnf], /NAN, /DOUBLE))
      
      PRINT, fNameX_Aoper + ' MIN-MAX =    ' + STRTRIM(MIN(tmp_pheno_dev[indnf], /NAN))+'-'+STRTRIM(MAX(tmp_pheno_dev[indnf], /NAN))
      PRINT, fNameX_Aoper + ' AVG =    ' + STRTRIM(MEAN(tmp_pheno_dev[indnf], /NAN, /DOUBLE))
      PRINT, fNameX_Aoper + ' STDDEV = ' + strtrim(STDDEV(tmp_pheno_dev[indnf], /NAN, /DOUBLE))
      
      
      ;Relevant bands are written to files because AVG at depdt level works with files
      tmp_pheno_full_path_name = path_out+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+'boi_'+fNameX
      tmp_pheno_dev_full_path_name = path_out+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+'boi_'+fNameX_Aoper
      IF (N_ELEMENTS(pheno_avg) NE 0) THEN $
        tmp_pheno_avg_full_path_name = path_out+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+fNameX+'_TZPavg'
      IF (N_ELEMENTS(pheno_sd) NE 0) THEN $
        tmp_pheno_sd_full_path_name = path_out+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+fNameX+'_TZPsd'
     
      OPENW, W1, tmp_pheno_full_path_name , /GET_LUN
      WRITEU, W1, tmp_pheno
      FREE_LUN, W1
      ; WRITE HEADER OF THE OUTPUT
      HEADER_OUT=tmp_pheno_full_path_name+'.hdr'
      OPENW, 3, HEADER_OUT
      printf,3,'ENVI'
      printf,3,'description = pix based'
      printf,3,'samples ='+STRCOMPRESS(ns)
      printf,3,'lines   ='+STRCOMPRESS(nl)
      printf,3,'bands   ='+STRCOMPRESS(1)
      printf,3,'header offset = 0'
      printf,3,'file type = ENVI Standard'
      printf,3,'data type = 4'
      printf,3,'interleave = bsq'
      printf,3,'byte order = 0'
      CLOSE, 3
      
      OPENW, W1, tmp_pheno_dev_full_path_name , /GET_LUN
      WRITEU, W1, tmp_pheno_dev
      FREE_LUN, W1
      HEADER_OUT=tmp_pheno_dev_full_path_name+'.hdr'
      OPENW, 3, HEADER_OUT
      printf,3,'ENVI'
      printf,3,'description = pix based'
      printf,3,'samples ='+STRCOMPRESS(ns)
      printf,3,'lines   ='+STRCOMPRESS(nl)
      printf,3,'bands   ='+STRCOMPRESS(1)
      printf,3,'header offset = 0'
      printf,3,'file type = ENVI Standard'
      printf,3,'data type = 4'
      printf,3,'interleave = bsq'
      printf,3,'byte order = 0'
      CLOSE, 3
      IF (N_ELEMENTS(pheno_avg) NE 0) THEN BEGIN
        OPENW, W1, tmp_pheno_avg_full_path_name , /GET_LUN
        WRITEU, W1, tmp_pheno_avg
        FREE_LUN, W1
        HEADER_OUT=tmp_pheno_avg_full_path_name+'.hdr'
        OPENW, 3, HEADER_OUT
        printf,3,'ENVI'
        printf,3,'description = pix based'
        printf,3,'samples ='+STRCOMPRESS(ns)
        printf,3,'lines   ='+STRCOMPRESS(nl)
        printf,3,'bands   ='+STRCOMPRESS(1)
        printf,3,'header offset = 0'
        printf,3,'file type = ENVI Standard'
        printf,3,'data type = 4'
        printf,3,'interleave = bsq'
        printf,3,'byte order = 0'
        CLOSE, 3
      ENDIF
      IF (N_ELEMENTS(pheno_sd) NE 0) THEN BEGIN
        OPENW, W1, tmp_pheno_sd_full_path_name , /GET_LUN
        WRITEU, W1, tmp_pheno_sd
        FREE_LUN, W1
        HEADER_OUT=tmp_pheno_sd_full_path_name+'.hdr'
        OPENW, 3, HEADER_OUT
        printf,3,'ENVI'
        printf,3,'description = pix based'
        printf,3,'samples ='+STRCOMPRESS(ns)
        printf,3,'lines   ='+STRCOMPRESS(nl)
        printf,3,'bands   ='+STRCOMPRESS(1)
        printf,3,'header offset = 0'
        printf,3,'file type = ENVI Standard'
        printf,3,'data type = 4'
        printf,3,'interleave = bsq'
        printf,3,'byte order = 0'
        CLOSE, 3
      ENDIF
      ;dep_AVG_of_pheno is called on such bands
      
      ;res = dep_AVG_of_pheno(pheno_fname, pheno_dtype, dep_fname, dep_dtype, ns, nl, nb)
      IF (doavg EQ 1) THEN BEGIN
        dt=read_info('data type', dep_fname +'.hdr')
        res=dep_AVG_of_pheno(tmp_pheno_full_path_name, 4, dep_fname, dt, ns, nl, 1)
        IF res NE 0 THEN RETURN, 20 
        res=dep_AVG_of_pheno(tmp_pheno_dev_full_path_name, 4, dep_fname, dt, ns, nl, 1)
        IF res NE 0 THEN RETURN, 20 
        ;FILE_DELETE, tmp_pheno_full_path_name, tmp_pheno_dev_full_path_name
      ENDIF ELSE BEGIN
        PRINT, 'Files were not averaged at dep level'
      ENDELSE
      
      
      CLOSE, /ALL
      
    ENDFOR  ;ngspy
  ENDFOR  ;s
ENDFOR  ;i

                       
PRINT, 'Task anomaly_analysis completed'
RETURN, 0
END