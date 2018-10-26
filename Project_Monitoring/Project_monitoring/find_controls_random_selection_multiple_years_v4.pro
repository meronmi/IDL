PRO find_controls_random_selection_multiple_years_v4
  
  Compile_Opt idl2

  ;this function look at the class composition within the project and look for a square window of the same
  ;total area (or nearly) of the prject with similar composition.
  ;
  ;In order to select controls with similiar environmental condition and experiencing a similar
  ;climate, we restric the serach to a circular area having an extension that is a multiple of the project
  ;area. The multiple is giben by the variable "area_search_ratio".
  ;
  ;Similarity id described in terms of RMSE of the various fractional compostions, so a kind of "mean" difference in
  ;fractional composition. This includes the fraction of area affected by the SLC-off problem to which it is
  ;assigned a greater importance in the RMSE. The vairable "fract_w_of_slc_fraction" set the fraction of RMSE assigned
  ;SLC-off fractional area.
  ;
  ;We then assume that we can live with a maximum rmse in class composition (maxRMSE, e.g. 0.01, 1%).
  ;After that we randomly select the controls in this set.

 
  ;***************************************************
  ;USER PAR
  test_wfp = 1
  ;ini file file containing processing details
  fn_ini = '\\ies\d5\foodsec\Projects\GGW_monitoring\WFP\test_debug\niger_ini.txt';'T:\Projects\GGW_monitoring\WFP\WFP_test_Laura_ini.txt'
  ;project file containg id of the project and year/years of implementation
  fn_prj = '\\ies\d5\foodsec\Projects\GGW_monitoring\WFP\test_debug\niger_prj.txt';'T:\Projects\GGW_monitoring\WFP\WFP_test_Laura_prj.txt'
  ;***************************************************
  
  
  
  reuse_rnd_extr = 0    ;set it to 1 to resue last random extraction
  ;Read form ini and prj files
  ;############## Landsat
;  ;ini file file containing processing details
;  fn_ini = 'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\USGS\GGW_SE_ini9forPaper.txt'
;  ;project file containg id of the project and year/years of implementation
;  fn_prj = 'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\USGS\GGW_SE_prj9forPaper.txt'
  ;############## MODIS
  
  verbose = read_info('verbose', fn_ini) ;0 suppress some info..
  sensor = read_info('sensor', fn_ini) ;'modis' 'ls'
  ;max rmse difference use to search for a control (rmse on class similarity and slc)
  max_rmse_dif = FLOAT(read_info('max_rmse_dif', fn_ini))
  ;k: number of non-overlapping controls to be selected, minimum 1
  k = FIX(read_info('k', fn_ini))
  ;number of classes for ISO-data classification
  n_iso = FIX(read_info('n_iso', fn_ini))
  ;id of the project and start and end of project implementation
  tmp = READ_ASCII(fn_prj, DATA_START = 1, DELIMETER = ',')
  target_id_yr = FIX(REFORM(tmp.FIELD1))
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
  IF (FILE_SEARCH(out_dir) EQ '') THEN FILE_MKDIR, out_dir
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
      MODISts_firstDek = FIX(read_info('MODISts_firstDek', fn_ini))
      valNoDataModis = FIX(read_info('valNoDataModis', fn_ini))
      isTimeSatOutput  = FIX(read_info('isTimeSatOutput', fn_ini))
      IF (isTimeSatOutput EQ 1) THEN BEGIN
        timesatValNan = DOUBLE(read_info('timesatValNan', fn_ini)) ;in fn_eMODIS_ts4class, values GE than this are no data
        timesatScaling = DOUBLE(read_info('timesatScaling', fn_ini)) ;value used for Nan in MODIS images analysed with timesat/R
      ENDIF
      IF (isTimeSatOutput EQ 0) THEN BEGIN
        gain = DOUBLE(read_info('gain', fn_ini)) ;in fn_eMODIS_ts4class, values GE than this are no data
        offset = DOUBLE(read_info('offset', fn_ini)) ;value used for Nan in MODIS images analysed with timesat/R
      ENDIF
    END
    ELSE: STOP
  ENDCASE  
  ;***************************************************
  ;OPEN FILE TO STORE command line output
  OPENW, lun_cmd, out_dir + '\idl_output.txt', /GET_LUN
 
  ;Determine dates of the available ts
  acq_date = !NULL
  jd_acq_all = !NULL
  year_acq = !NULL
  CASE sensor OF
    'ls': BEGIN
      ;retrive date for GE metafle
      FOR i = 0, N_ELEMENTS(fnames)-1 DO BEGIN
;        ;This was for GEE:
;        tmp = get_date_tif_from_GEE(in_dir, fnames[i])
;        tmp = STRJOIN(STRSPLIT(tmp,'-', /EXTRACT), '/')
;        acq_date = [acq_date, tmp]
        ;This if for USGS files created with multi_tiff_from_USGS_sr.pro
        tmp=STRSPLIT(remove_ext_from_fn(fnames[i]),'_', /EXTRACT)
        ;temp solution for WFP
        IF (test_wfp EQ 1) THEN acq_date = [acq_date, STRJOIN([tmp[-1], tmp[-2], tmp[-3]], '/')] ELSE acq_date = [acq_date, STRJOIN([tmp[-1], tmp[-2], tmp[-3]], '/')]
      ENDFOR
    END
    'modis':BEGIN
      ;retrieve date from filename
      FOR i = 0, N_ELEMENTS(fnames)-1  DO BEGIN
        tmp = STRSPLIT(remove_ext_from_fn(fnames[i]),'_',/EXTRACT) ;'senegal_SI_yyyy'
        acq_date = [acq_date, tmp[2]+'/06/15']
      ENDFOR
    END
    ELSE: STOP
  ENDCASE
  ;order it
  FOR i = 0, N_ELEMENTS(fnames)-1 DO BEGIN
    tmp = STRSPLIT(acq_date[i], '/', /EXTRACT)
    year_acq = [year_acq, FIX(tmp[0])]
    jd_acq_all = [jd_acq_all, JULDAY(FIX(tmp[1]), FIX(tmp[2]), FIX(tmp[0]), 12, 0, 0)]
  ENDFOR
  ind = SORT(jd_acq_all)
  acq_date = acq_date[ind]
  fnames = fnames[ind]
  jd_acq_all = jd_acq_all[ind]
  year_acq = year_acq[ind]
  
  
  

  ;Open project mask and retrieve project size and misture
  ;here I assume I get the raster mask for the project (id where project 0 otherwise),
  ;id can be any integer and indicates the prject number
  ns_mask = FIX(read_info('samples', in_dir + '\' + remove_ext_from_fn(fn_project_mask) + '.hdr'))
  nl_mask = FIX(read_info('lines', in_dir + '\' + remove_ext_from_fn(fn_project_mask) + '.hdr'))
  dt_mask = FIX(read_info('data type', in_dir + '\' + remove_ext_from_fn(fn_project_mask) + '.hdr'))
  mask = MAKE_ARRAY(ns_mask, nl_mask, TYPE = dt_mask)
  OPENR, lun, in_dir + '\' + fn_project_mask, /GET_LUN
  READU, lun, mask &  FREE_LUN, lun

  project_ids = REFORM(target_id_yr[0,*])


  ;loop on the project if requested
  FOR p = 0, N_ELEMENTS(project_ids) -1 DO BEGIN
     IF (reuse_rnd_extr NE 1) THEN $ 
      OPENW, lun_pps, out_dir + '\idl_pps_prj'+ STRTRIM(project_ids[p],2)  +'.txt', /GET_LUN $
     ELSE OPENR, lun_pps, out_dir + '\idl_pps_prj'+ STRTRIM(project_ids[p],2)  +'.txt', /GET_LUN
      
    ;FOR GETTING 5 years max
;    ;Define what if before and what is after and do all calculations
;    ind_before = WHERE((year_acq LT target_id_yr[1,p])AND (target_id_yr[1,p]-year_acq LE nMax), count)
;    ;if more than n available, take the last n
;    IF (count GT nMax) THEN ind_before = ind_before[-nMax:-1]
;    ind_after = WHERE((year_acq GT target_id_yr[2,p]) AND (year_acq-target_id_yr[2,p] LE nMax), count)
;    ;if more than n available, take the first n
;    IF (count GT nMax) THEN ind_after = ind_after[0:nMax-1]
    ;FOR GETTING 5 images max
    ind_before = WHERE((year_acq LT target_id_yr[1,p]), count)  
    ;if more than n available, take the last n
    IF (count GT nMax) THEN ind_before = ind_before[-nMax:-1]
    ind_after = WHERE((year_acq GT target_id_yr[2,p]), count)
    ;if more than n available, take the first n
    IF (count GT nMax) THEN ind_after = ind_after[0:nMax-1]
    PRINT, 'Project_id  Implemented in' & PRINTF, lun_cmd, 'Project_id  Implemented in'
    PRINTF, lun_cmd, 'Project_id  Implemented in' & PRINTF, lun_cmd, 'Project_id  Implemented in'
    PRINT, target_id_yr[*,p] & PRINTF, lun_cmd,  target_id_yr[*,p]
    PRINTF, lun_cmd, target_id_yr[*,p] & PRINTF, lun_cmd,  target_id_yr[*,p]
    PRINT, 'Use years before: ', year_acq[ind_before] & PRINTF, lun_cmd, 'Use years before: ', year_acq[ind_before]
    ;PRINTF, lun_cmd, 'Use years before: ', year_acq[ind_before] & PRINTF, lun_cmd, 'Use years before: ', year_acq[ind_before]
    PRINT, 'Use years after: ', year_acq[ind_after] & PRINTF, lun_cmd, 'Use years after: ', year_acq[ind_after]
    ;PRINTF, lun_cmd, 'Use years before: ', year_acq[ind_before] & PRINTF, lun_cmd, 'Use years before: ', year_acq[ind_before]
    ;now define what is before and after
    fn_before = fnames[ind_before]
    fn_after = fnames[ind_after]
    nbef = N_ELEMENTS(fn_before)
    naft = N_ELEMENTS(fn_after)
    acq_before = acq_date[ind_before]
    acq_after  = acq_date[ind_after]
    jd_acq = [jd_acq_all[ind_before], jd_acq_all[ind_after]]
    ;load data
    CASE sensor OF
      'ls': BEGIN
        ;assume equal dimension of all, it will be checked few lines below
        res = QUERY_TIFF(in_dir + '\' +fn_before[0], infos)
        ndvi_before = FLTARR(infos.dimensions[0], infos.dimensions[1], nbef)
        ndvi_after = FLTARR(infos.dimensions[0], infos.dimensions[1], naft)
        ;calibrate and save Ls7 NDVI
        ;FOR i = 0, nbef-1 DO ndvi_before[*,*,i] = cal_and_save_NDVI_from_GEE_LS(in_dir, out_dir, fn_before[i])
        FOR i = 0, nbef-1 DO ndvi_before[*,*,i] = NDVI_from_USGS(in_dir, out_dir, fn_before[i])
        ;calibrate and save Ls8 NDVI
        ;FOR i = 0, naft-1 DO ndvi_after[*,*,i] = cal_and_save_NDVI_from_GEE_LS(in_dir, out_dir, fn_after[i])
        FOR i = 0, naft-1 DO ndvi_after[*,*,i] = NDVI_from_USGS(in_dir, out_dir, fn_after[i])
      END
      'modis':BEGIN
        ;assume equal dimension of all, it will be checked few lines below
        ns = FIX(read_info('samples', in_dir + '\' + remove_ext_from_fn(fn_before[0]) + '.hdr'))
        nl = FIX(read_info('lines', in_dir + '\' + remove_ext_from_fn(fn_before[0]) + '.hdr'))
        ndvi_before = FLTARR(ns, nl, nbef)
        ndvi_after = FLTARR(ns, nl, naft)
        IF (isTimeSatOutput EQ 1) THEN BEGIN
          FOR i = 0, nbef-1 DO ndvi_before[*,*,i] = clean_MODIS_TIMESAT(in_dir, fn_before[i], timesatValNan, timesatScaling)
          FOR i = 0, naft-1 DO ndvi_after[*,*,i] =clean_MODIS_TIMESAT(in_dir, fn_after[i], timesatValNan, timesatScaling)
        ENDIF
        IF (isTimeSatOutput EQ 0) THEN BEGIN
          FOR i = 0, nbef-1 DO ndvi_before[*,*,i] = scale_MODIS(in_dir, fn_before[i], valNoDataModis , gain, offset)
          FOR i = 0, naft-1 DO ndvi_after[*,*,i] =scale_MODIS(in_dir, fn_after[i], valNoDataModis , gain, offset)
        ENDIF
      END
      ELSE: STOP
    ENDCASE
    ;check consistency in the dimensions
    
    sz = SIZE(ndvi_before) & ns_b = sz[1] & nl_b = sz[2]
    sz = SIZE(ndvi_after) & ns_a = sz[1] & nl_a = sz[2]
    
    ;check consistency in the dimensions
    IF  (ns_mask NE ns_b) OR (nl_mask NE nl_b) $
      OR (ns_mask NE ns_a) OR (nl_mask NE nl_a) THEN STOP
    
    ;build slc mask (essentially only for Ls)
    CASE sensor OF
      'ls': BEGIN
;        ;For the GEE images
;        ;MM 15 June 2016. Here I have to understand which files are LS7 and can be potentially
;        ;affected by LSC problem (in all files, before and after.
;        fns4slc = !NULL
;        FOR ii = 0, N_ELEMENTS(fn_before)-1 DO $
;          IF (isLS7(in_dir, fn_before[ii])) THEN fns4slc = [fns4slc, fn_before[ii]]
;        FOR ii = 0, N_ELEMENTS(fn_after)-1 DO $
;          IF (isLS7(in_dir, fn_after[ii])) THEN fns4slc = [fns4slc, fn_after[ii]]
;        ;fn_LS_slc = fn_before
;        fn_classification_mask = build_slc_mask_funct_multiple_files(in_dir, out_dir, fns4slc, bands2use)
        
        ;For the USGS files created with multi_tiff_from_USGS_sr
        ;SLC and clouds info are reported in the mask file created when the computation of NDVI
        ;was called. Use that to create the mask that will apply to the following processing.
        ;Note that this procedure mask out also those pixels that are cloud free before but are
        ;cloudy after. This makes sense as there is no reason to process such pixels as they will not be used
        fn_classification_mask = build_USGS_mask(out_dir, fn_before, fn_after, project_ids[p])
      END
      'modis':BEGIN
        ;here I have to define the bands of the time series used to build the mask and perform classificatio, that
        ;is, the five years before project implementation
        ;locate the year before
        ybef = target_id_yr[1,p]-1
        ;last dekad (ordinal, starting from 1)
        ld = (ybef-MODISts_firstYear+1)*36 - (MODISts_firstDek-1) 
        ;first dekad to be considered, ordinal
        fd = ld-(36*nMax)+1
        IF (fd LE 0) THEN BEGIN
          fd = 1
          PRINT, 'WARNING: Project ' + STRTRIM(target_id_yr[0,p],2) + ' started in ' + STRTRIM(target_id_yr[1,p],2)
          PRINTF, lun_cmd, 'WARNING: Project ' + STRTRIM(target_id_yr[0,p],2) + ' started in ' + STRTRIM(target_id_yr[1,p],2)
          PRINT, 'A 5 years ts for the classification is not available, I will use ' + STRTRIM(ld-fd+1,2) + ' dekads'
          PRINTF, lun_cmd, 'A 5 years ts for the classification is not available, I will use ' + STRTRIM(ld-fd+1,2) + ' dekads'
        ENDIF
        MODIS_ts_bands2use = INDGEN(ld-fd+1, START=fd-1)
        fn_classification_mask = build_MODIS_no_data_mask(in_dir, out_dir, fn_eMODIS_ts4class, MODIS_ts_bands2use, 'MODIS_mask', valNoDataModis)
        ;fn_classification_mask =  'MODIS_mask.img'
        ;        tmp = BYTARR(ns_mask, nl_mask)*0B+1B
        ;        OPENW, lun, out_dir + '\' + fn_classification_mask, /GET_LUN
        ;        WRITEU, lun, tmp
        ;        FREE_LUN, lun
        ;        map_info = read_info('map info', remove_ext_from_fn(in_dir + '/' + fn_project_mask) + '.hdr')
        ;        coord_info = read_info('coordinate system string', remove_ext_from_fn(in_dir + '/' + fn_project_mask) + '.hdr')
        ;        res =write_envi_hdr(remove_ext_from_fn(fn_classification_mask), ns_mask, nl_mask, 1, DIR = out_dir, MAPINFO=map_info, COORDINFO=coord_info)
      END
      ELSE: STOP
    ENDCASE
    
    ;Define the dimension of the window used to serach for an appropriate control
    ;find out how big is my project (innumber of pixels)
    ind_trgt_project = WHERE(mask EQ project_ids[p], npix_trgt_project)
    PRINT, 'Project id ' + STRTRIM(project_ids[p],2) + ' has ' + STRTRIM(npix_trgt_project,2) + ' pixels'
    PRINTF, lun_cmd, 'Project id ' + STRTRIM(project_ids[p],2) + ' has ' + STRTRIM(npix_trgt_project,2) + ' pixels'
    winsize = FLOOR(SQRT(npix_trgt_project))
    IF (winsize MOD 2 EQ 0) THEN BEGIN
      PRINT, 'An even window size of ' + STRTRIM(winsize,2) + ' cannot be used, I will use'
      PRINTF, lun_cmd, 'An even window size of ' + STRTRIM(winsize,2) + ' cannot be used, I will use'
      winsize = winsize + 1
      PRINT,  STRTRIM(winsize,2) + ' instead'
      PRINTF, lun_cmd, STRTRIM(winsize,2) + ' instead'
    ENDIF
    PRINT, 'A square window of ' + STRTRIM(winsize,2) + ' by ' + STRTRIM(winsize,2) + ' will be used'
    PRINTF, lun_cmd, 'A square window of ' + STRTRIM(winsize,2) + ' by ' + STRTRIM(winsize,2) + ' will be used'
    ;Make some calculatio to allow computing the distance between any serached window and the project of interest
    ;here compute the centroid of the project in matrix coordinates
    eucl_dist = euclid_dist_from_pix(mask, ind_trgt_project) 
    posCR = ARRAY_INDICES(mask, ind_trgt_project) ;it is used later on
;    C_centroid = ROUND(MEAN(FLOAT(posCR[0,*])))
;    R_centroid = ROUND(MEAN(FLOAT(posCR[1,*])))
;    Cmat = mask*0
;    Rmat = Cmat
;    FOR c=0, nl_mask-1 DO Cmat[*,c] = INDGEN(ns_mask)
;    FOR c=0, ns_mask-1 DO Rmat[c,*] = INDGEN(nl_mask)
;    ;apply Pitagora
;    eucl_dist = SQRT((Cmat-C_centroid)^2+(Rmat-R_centroid)^2)
    ;limit the search to the search radius
    ; As is the circular area used to look for controls
    ; we set that As = 200 Ap
    ; then we compute tha radius search as
    ; max_dist = (As / pi)^1/2
    max_dist = SQRT((npix_trgt_project*area_search_ratio)/!PI)
    PRINT, 'Maximum search distance computed = ' + STRTRIM(max_dist,2)
    PRINTF, lun_cmd, 'Maximum search distance computed = ' + STRTRIM(max_dist,2)
    ;ind_dist_exclude = WHERE((eucl_dist*pixel_size_m) GT (max_dist_km * 1000), count_dist_exclude)
    ind_dist_exclude = WHERE((eucl_dist) GT (max_dist), count_dist_exclude)
    IF (count_dist_exclude GT 0) THEN eucl_dist[ind_dist_exclude] = !VALUES.F_INFINITY
    ;Save the distance to the project file
    fn_dist = 'dist_project_' + STRTRIM(project_ids[p],2)
    OPENW, lun, out_dir + '\' + fn_dist + '.img', /GET_LUN
    WRITEU, lun, eucl_dist & FREE_LUN, lun
    ;write hdr
    map_info = read_info('map info', remove_ext_from_fn(out_dir + '/' + fn_classification_mask) + '.hdr')
    coord_info = read_info('coordinate system string', remove_ext_from_fn(out_dir + '/' + fn_classification_mask) + '.hdr')
    dt = SIZE(eucl_dist)
    dt = dt[-2]
    res =write_envi_hdr(fn_dist, ns_mask, nl_mask, dt, DIR = out_dir, MAPINFO=map_info, COORDINFO=coord_info)


    ;get or make the classification
    IF (fn_usr_classification EQ '') THEN BEGIN
      ;update the mask with distance buffer
      fn_base = FILE_BASENAME(fn_before[0], '.tif')
      e = ENVI()
      mask_eraster = e.OpenRaster(out_dir + '\' + fn_classification_mask)
      IF (count_dist_exclude GT 0) THEN BEGIN
        data = BYTE(mask_eraster.GetData())
        data[ind_dist_exclude]=0
        res = FILE_SEARCH(out_dir + '\' + fn_base + '_mask_ISO_project_' + STRTRIM(project_ids[p],2))
        IF (res NE '') THEN BEGIN
          FILE_DELETE, out_dir + '\' + fn_base + '_mask_ISO_project_' + STRTRIM(project_ids[p],2)
          FILE_DELETE, out_dir + '\' + fn_base + '_mask_ISO_project_' + STRTRIM(project_ids[p],2) + '.hdr'       
        ENDIF
        mask2_eraster = ENVIRaster(data, URI=out_dir + '\' + fn_base + '_mask_ISO_project_' + STRTRIM(project_ids[p],2));, INHERITS_FROM=mask)
        mask2_eraster.Save
      ENDIF
      mask_eraster.Close
      mask2_eraster.Close
      ;iso classification using envi classic (not all parameters can be set with the new GUI of ENVI)
      fn_mask = out_dir + '\' + fn_base + '_mask_ISO_project_' + STRTRIM(project_ids[p],2)
      ENVI_OPEN_FILE, fn_mask, r_fid=m_fid
      CASE sensor OF
        'ls': BEGIN
          ;join the file if more than one, always use the selected bands
          base_4_classif = make_multi_file_tif(in_dir,out_dir,fn_before,bands2use,project_ids[p])
          fn_data = out_dir + '\' + base_4_classif
        END
        'modis':BEGIN
          base_4_classif = fn_eMODIS_ts4class 
          fn_data = in_dir + '\' + base_4_classif
        END
        ELSE: STOP
      ENDCASE

       ;get the data
      ENVI_OPEN_FILE, fn_data, r_fid=fid
      ENVI_FILE_QUERY, fid, ns=ns, nl=nl, nb=nb, bname=bname
      dims = [-1l, 0, ns-1, 0, nl-1]
      ;pos = [0,1,2,3,4,7]
      IF (sensor EQ 'modis') THEN pos = MODIS_ts_bands2use ELSE pos = INDGEN(nb);[0,1,2,3,4,7] 
      m_pos = [0l]
      fn_classification = out_dir + '\' + fn_base + '_ISO_project_' + STRTRIM(project_ids[p],2)
      ENVI_DOIT, 'class_doit', $
        fid=fid, pos=pos, dims=dims, $
        ;/check, o_min=0, o_max=255, $
        out_name=fn_classification, r_fid=r_fid, m_fid=m_fid, $
        method=4, change_thresh=0.05, iso_merge_dist=5, $ ; method 4 is isodata
        iso_min_pixels=10, min_classes=5, num_classes=n_iso, iterations=10, $
        iso_split_std=1.0, iso_merge_pairs=2, $
        value=0, m_pos=m_pos
      ;end of last alternative
      class_raster = e.OpenRaster(fn_classification)
      classif = class_raster.GetData()
      class_raster.Close
      ENVI_FILE_MNG , ID=m_fid, /REMOVE
      ENVI_FILE_MNG , ID=fid, /REMOVE
    ENDIF ELSE BEGIN
      fn_classification = out_dir + '\' + remove_ext_from_fn(fn_usr_classification)
      ns = FIX(read_info('samples', fn_classification + '.hdr'))
      nl = FIX(read_info('lines', fn_classification + '.hdr'))
      dt_class = FIX(read_info('data type', fn_classification + '.hdr'))
      ;here I get the classification results
      classif = MAKE_ARRAY(ns, nl, TYPE = dt_class)
      OPENR, lun, fn_classification, /GET_LUN
      READU, lun, classif & FREE_LUN, lu
    ENDELSE
    
    ;reset slc class or no classfied  to 200
    IF (sensor EQ 'modis') THEN slc_class = 0    
    ind_slc = WHERE(classif EQ slc_class, count_slc)
    unique_scl_class = 200
    IF (count_slc GT 0) THEN  BEGIN
      classif[ind_slc] = unique_scl_class
      PRINT, 'UNCLASSIFIED class with value 0 was reset to value 200 because 0 cannot be treated in search algorithm'
      PRINTF, lun_cmd, 'UNCLASSIFIED class with value 0 was reset to value 200 because 0 cannot be treated in search algorithm'
    ENDIF ELSE BEGIN
      PRINT, 'No Slc present'
      PRINTF, lun_cmd, 'No Slc present'
    ENDELSE
    
    

    ;Check what is assigned to SLC using classification of the images
    possible_values = classif[UNIQ(classif, SORT(classif))]
    PRINT, 'Classification possible values:'  & PRINTF, lun_cmd, 'Classification possible values:'
    PRINT, possible_values & PRINTF, lun_cmd, possible_values
    npv = N_ELEMENTS(possible_values)

    ;Count the occurrences (number) of various classes within each window
    ;results are stored in countMat, that has the same x and y dim of input image and for eah band,
    ;the count of possible_values[i] stored in the central pixel
    s = Size(classif, /DIMENSIONS)
    countMat = FLTARR(s[0], s[1], npv)
    C_centroid = ROUND(MEAN(FLOAT(posCR[0,*])))
    R_centroid = ROUND(MEAN(FLOAT(posCR[1,*])))
    ;coordinate_square_roi = [C_min, C_max, R_min, R_max]
    ;augment a bit max dist to avoid border effects
    coordinate_square_roi = [C_centroid - FIX(max_dist)*1.2 -1, C_centroid + FIX(max_dist)*1.2 + 1, $
      R_centroid - FIX(max_dist)*1.2 - 1, R_centroid + FIX(max_dist)*1.2 + 1]
    ;check that they are not negative or beyond the image
    IF (coordinate_square_roi[0] LT 0) THEN coordinate_square_roi[0] = 0
    IF (coordinate_square_roi[2] LT 0) THEN coordinate_square_roi[2] = 0
    IF (coordinate_square_roi[1] GT s[0]-1) THEN coordinate_square_roi[1] = s[0]-1
    IF (coordinate_square_roi[3] GT s[1]-1) THEN coordinate_square_roi[3] =  s[1]-1
    roi = classif[coordinate_square_roi[0]:coordinate_square_roi[1], coordinate_square_roi[2]:coordinate_square_roi[3]]
    FOR i = 0, npv-1 DO BEGIN
      valueToSearch = possible_values[i]
      kernel = Replicate(1.0/valueToSearch, winsize, winsize)
      ;countMat[*,*,i] = Small_Region_Values(classif, valueToSearch, KERNEL=kernel)
      countMat[coordinate_square_roi[0]:coordinate_square_roi[1], $
        coordinate_square_roi[2]:coordinate_square_roi[3],i] = $
        Small_Region_Values(roi, valueToSearch, KERNEL=kernel)
    ENDFOR

    ;compute the fractional composition of the project (n_pix in the various classification values)
    fract_comp = FLTARR(npv)  ;fraction having that value
    FOR i = 0, npv-1 DO BEGIN
      ind_tmp = WHERE(classif[ind_trgt_project] EQ possible_values[i], count_tmp)
      fract_comp[i] = count_tmp / FLOAT(npix_trgt_project)
    ENDFOR
    PRINT, 'Project composition' & PRINTF, lun_cmd, 'Project composition'
    PRINT, fract_comp & PRINTF, lun_cmd, fract_comp

    ;compute fractional composition of each of the candidate windows (based on countMat)
    fract_countMat = countMat / FLOAT(winsize^2)

    ;Compute the rmse between the fractional composition of any of the canidate windows and the project composition
    ;make the rmse half on slc and half on class composition
    IF (sensor EQ 'ls') THEN BEGIN
      PRINT, 'rmse_total made up by ' + STRTRIM(1.0 - fract_w_of_slc_fraction,2) + $
        ' RMSE class composition, and ' + STRTRIM(fract_w_of_slc_fraction,2) + ' of fraction of slc
      PRINTF, lun_cmd, 'rmse_total made up by ' + STRTRIM(1.0 - fract_w_of_slc_fraction,2) + $
        ' RMSE class composition, and ' + STRTRIM(fract_w_of_slc_fraction,2) + ' of fraction of slc
    ENDIF
    rmse_total = FLTARR(s[0], s[1])
    ;rmse_class = rmse_total
    FOR i = 0, s[0]-1 DO BEGIN ;columns
      FOR j = 0, s[1]-1 DO BEGIN ;raws
        ;if it is all no-data, set rmse to inf
        IF (fract_countMat[i,j,-1] EQ 1.0) OR (TOTAL(fract_countMat[i,j,0:-2]) EQ 0.0) THEN BEGIN
          rmse_total[i,j] = !VALUES.F_INFINITY
        ENDIF ELSE BEGIN
          first = SQRT(TOTAL((fract_countMat[i,j,0:-2] - fract_comp[0:-2])^2)/FLOAT(npv-1))
          IF (sensor EQ 'ls') THEN BEGIN
            second = SQRT((fract_countMat[i,j,-1] - fract_comp[-1])^2)
            rmse_total[i,j] = first * (1.0 - fract_w_of_slc_fraction) + second * fract_w_of_slc_fraction
          ENDIF ELSE rmse_total[i,j] = first
        ENDELSE
      ENDFOR
    ENDFOR


    ;EXCLUSIONS:
    ;set those outside the search radius to Infinity
    IF (count_dist_exclude GT 0) THEN rmse_total[ind_dist_exclude] = !VALUES.F_INFINITY
    ;set those in exluded areas (within the serach radius) to Infinity
    ind_to_exclude_in_control_srch = WHERE(mask EQ 999, count_to_exclude_in_control_srch)
    IF (count_to_exclude_in_control_srch GT 0) THEN rmse_total[ind_to_exclude_in_control_srch] = !VALUES.F_INFINITY
    ;Exclude additionally:
    ;-those window having border effect
    ;-all those touching a project (the reference itself or any other)
    ;exclude borders by loking at those cels that have all valid values (winsize*winsize values), these are kept
    n_validMat = TOTAL(countMat,3) ;countMat in band i counts the number of pixels having the calss value i 
    ;set those having less than winsize*winsize values to Inf
    tmp_ind = WHERE(n_validMat LT winsize^2, tmp_count)
    rmse_total[tmp_ind] = !VALUES.F_INFINITY
    ;exclude the project itself and those area belonging to other project
    tmp_ind = WHERE(mask NE 0, tmp_count)
    ;augment these excluded so that any moving window touching these area are excluded
    exclude = mask * 0
    exclude[tmp_ind] = 1
    kernel = Replicate(1.0, winsize, winsize)
    exclude2 = CONVOL(exclude, kernel, /EDGE_ZERO)
    tmp_ind = WHERE(exclude2 GT 0)
    rmse_total[tmp_ind] = !VALUES.F_INFINITY
    DELVAR, cmat, countmat, doy_dist, exclude, exclude2, n_validmat, rmat, tmp_ind
    
    ;save rmse before control extraction
    fn_rmse = 'before_extraction_rmse_project_' + STRTRIM(project_ids[p],2)
    OPENW, lun, out_dir + '\' + fn_rmse + '.img', /GET_LUN
    WRITEU, lun, rmse_total
    FREE_LUN, lun
    dt = SIZE(rmse_total)
    dt = dt[-2]
    res =write_envi_hdr(fn_rmse, ns, nl, dt, DIR = out_dir, MAPINFO=map_info, COORDINFO=coord_info)
    
    mask_control = LONG(mask * 0)
    ;assign the target prject
    mask_control[ind_trgt_project] = project_ids[p]
    ;posCR = ARRAY_INDICES(mask_control, pos_best)
    ;PRINT, 'position, fractional composition of the most similar, and rmse_total'
    ;PRINT, posCR
    ;PRINT, REFORM(fract_countMat[posCR[0],posCR[1],*])
    ;PRINT, REFORM(rmse_total[posCR[0],posCR[1]])


    ;Now randomly extract the required number of controls
    indFin = WHERE(FINITE(rmse_total))
    ind = WHERE(rmse_total[indFin] LT max_rmse_dif, n_candidates)
    ind = indFin[ind]
    PRINT, 'N of candidates based on total RMSE only = ' + STRTRIM(n_candidates,2)
    PRINTF, lun_cmd, 'N of candidates based on total RMSE only = ' + STRTRIM(n_candidates,2)
    n_selected = 0
    n_tested = 0
    rmse_total_of_controls = !NULL
    ;extract with probability proportional to size (PPS) of RMSE, the smallest should be selected, so work
    ; on 1.0 - RMSE
    ;test that ther is no RMSE bigger than 1.0
    indRMSE_GT_1 = WHERE(rmse_total[ind] GT 1.0, countRMSE_GT_1)
    IF (countRMSE_GT_1 GT 0) THEN STOP
    ;k to be selected
    stop_search = 0
    ;at least one candidate has to be present
    IF (n_candidates LT 1) THEN STOP
    REPEAT BEGIN
      ;re-extract the valid as they may have changed in the loop
      indFin = WHERE(FINITE(rmse_total))
      ind = WHERE(rmse_total[indFin] LT max_rmse_dif, n_candidates)
      ind = indFin[ind]
      IF (n_candidates GE 1) THEN BEGIN
        IF (n_selected LT k) THEN BEGIN
          IF (reuse_rnd_extr NE 1) THEN BEGIN
            pps_sub = PPS(1.0 - rmse_total[ind], 1)
            pps_sub = pps_sub[0]
            IF (pps_sub LT 0) THEN STOP
            PRINTF, lun_pps, pps_sub
          ENDIF ELSE BEGIN
            tmp = ''
            READF, lun_pps, tmp
            pps_sub = LONG(STRTRIM(tmp,2)) 
          ENDELSE          
          ;this should be selctable (check)
          IF (FINITE(rmse_total[ind[pps_sub]]) NE 1) THEN STOP
          n_selected = n_selected + 1
          posCRc = ARRAY_INDICES(mask_control, [ind[pps_sub]])
          ;print characteristics
          IF (verbose) THEN PRINT, 'C'+STRTRIM(project_ids[p]*1000 + 10 + n_selected,2) + $
            ' position, fractional composition of the PPS random extraction with max ' + $
            STRTRIM(max_rmse_dif,2) + ' rmse_total, and rmse_total, for control ' + STRTRIM(n_selected,2)
          ;PRINT, 'C'+STRTRIM(project_ids[p]*1000 + 10 + n_selected,2)+' position, fractional composition of the closest with max ' + STRTRIM(max_rmse_dif,2) + ' rmse_total, and rmse_total, for control ' + STRTRIM(n_selected,2)
          IF (verbose) THEN PRINT, posCRc
          IF (verbose) THEN PRINT, REFORM(fract_countMat[posCRc[0],posCRc[1],*])
          IF (verbose) THEN PRINT, REFORM(rmse_total[posCRc[0],posCRc[1]])
          rmse_total_of_controls = [rmse_total_of_controls, rmse_total[ind[pps_sub]]];set all area that cannot be anymore choosed to Inf
          ;set its and adjacent RMSE to inf
          rmse_total[ind[pps_sub]] = !VALUES.F_INFINITY
          exclude = mask * 0.0 ;make it float
          exclude[posCRc[0]-(winsize-1)/2:posCRc[0]+(winsize-1)/2, posCRc[1]-(winsize-1)/2:posCRc[1]+(winsize-1)/2] = 1
          ;make a smaller one to make the convol faster
          ;be sure that the subset does not go out of the image
          c0 = posCRc[0]-3*(winsize-1)/2
          c1 = posCRc[0]+3*(winsize-1)/2
          r0 = posCRc[1]-3*(winsize-1)/2
          r1 = posCRc[1]+3*(winsize-1)/2
          IF (c0 LT 0) THEN c0 = 0
          IF (c1 GT ns-1) THEN c1 = ns-1
          IF (r0 LT 0) THEN r0 = 0
          IF (r1 GT nl-1) THEN r1 = nl-1
          tmp = exclude[c0:c1, r0:r1]
          ;posCRc[1]-3*(winsize-1)/2:posCRc[1]+3*(winsize-1)/2]
          kernel = Replicate(1.0, winsize, winsize)
          tmp = CONVOL(tmp, kernel, /EDGE_ZERO)
          exclude[c0:c1, r0:r1] = tmp
          ;exclude2 = CONVOL(exclude, kernel, /EDGE_ZERO)
          tmp_ind = WHERE(exclude GT 0)
          rmse_total[tmp_ind] = !VALUES.F_INFINITY
          ;assign the value of n_project*1000 + 10+i to this i window (closest with max 1% diff)
          mask_control[posCRc[0]-(winsize-1)/2:posCRc[0]+(winsize-1)/2, posCRc[1]-(winsize-1)/2:posCRc[1]+(winsize-1)/2] = project_ids[p]*1000 + 10 + n_selected
        ENDIF ELSE BEGIN
          stop_search = 1
        ENDELSE
      ENDIF ELSE BEGIN
        ;no more candidates to serach before k was reached
        PRINT, 'Only ' + STRTRIM(n_selected, 2) + ' non overlapping controls were selected'
        PRINTF, lun_cmd, 'Only ' + STRTRIM(n_selected, 2) + ' non overlapping controls were selected'
        PRINT, 'instead of ' + STRTRIM(k, 2) + ' requested'
        PRINT, 'To get the required number you may:
        PRINT, '(1) try to decrease the required RMSE, now set to ', STRTRIM(max_rmse_dif,2)
        PRINT, '(2) try to increase the serach radius, now set to ', STRTRIM(max_dist,2) + ' pixels'
        PRINT, '(3) Reduce the number of classes in the ISOdata classification, now  ', STRTRIM(N_ELEMENTS(possible_values),2)
        PRINT, 'Be care with (1), it results in finding more, but with different composition, windows
        ;exit the search
        stop_search = 1
      ENDELSE
    ENDREP UNTIL (stop_search EQ 1)

    fn_control_mask = 'control_mask_project_' + STRTRIM(project_ids[p],2)
    OPENW, lun, out_dir + '\' + fn_control_mask + '.img', /GET_LUN
    WRITEU, lun, mask_control
    FREE_LUN, lun
    dt = SIZE(mask_control)
    dt = dt[-2]
    res =write_envi_hdr(fn_control_mask, ns, nl, dt, DIR = out_dir, MAPINFO=map_info, COORDINFO=coord_info)

    ;save  RMSE matrix
    ind = WHERE(rmse_total EQ 9.0)
    rmse_total[ind] = !VALUES.F_NAN
    fn_rmse = 'rmse_project_' + STRTRIM(project_ids[p],2)
    OPENW, lun, out_dir + '\' + fn_rmse + '.img', /GET_LUN
    WRITEU, lun, rmse_total
    FREE_LUN, lun
    ;    map_info = read_info('map info', remove_ext_from_fn(fn_classification_mask) + '.hdr')
    ;    coord_info = read_info('coordinate system string', remove_ext_from_fn(fn_classification_mask) + '.hdr')
    dt = SIZE(rmse_total)
    dt = dt[-2]
    res =write_envi_hdr(fn_rmse, ns, nl, dt, DIR = out_dir, MAPINFO=map_info, COORDINFO=coord_info)

    ;Now make the ndvi analysis

    ;Save, for the project and the control selected windows, a csv file with LS NDVI
    ;note that SLC pixels are not saved for LS7 (before) and LS8 (after)
    ;write also the same in BACI R format

    ;Save data in R BACI format
    dlmtr = ','
    OPENW, lunAnne, out_dir + '\ndvi_control_and_project' + STRTRIM(project_ids[p],2)+'_ANNE_FORMAT.csv', /GET_LUN, /APPEND
    PRINTF, lunAnne, 'SiteClass'+dlmtr+'Site'+dlmtr+'Period'+dlmtr+'SamplingDay'+dlmtr+'Pixel'+dlmtr+'RSvar'



    ;project
    ind = WHERE((mask EQ project_ids[p]) AND (classif NE unique_scl_class), count) ;the latter to exclude SLC pixels from both before an after
    ndvi_project_before = FLTARR(count,  nbef)
    stat_project = FLTARR(count, MAX([nbef,naft]),2) * !VALUES.F_NAN
    ;count is for the number of pixels, MAX([nbef,naft]) is for the different timings, 2 is before/after
    FOR i = 0, nbef-1 DO BEGIN
      tmp = REFORM(ndvi_before[*,*,i])
      ndvi_project_before[*,i] = tmp[ind]
      stat_project[*,i,0] = tmp[ind]
      ;some tests
      itest = where(finite(tmp[ind]), ctest)
      if ctest ne n_elements(tmp[ind]) then stop
      IF MIN(tmp[ind[itest]]) LT 0.0 THEN STOP
    ENDFOR
    ndvi_project_after = FLTARR(count,  naft)
    FOR i = 0, naft-1 DO BEGIN
      tmp = REFORM(ndvi_after[*,*,i])
      ndvi_project_after[*,i] = tmp[ind]
      stat_project[*,i,1] = tmp[ind]
      ;some tests
      itest = where(finite(tmp[ind]), ctest)
      if ctest ne n_elements(tmp[ind]) then stop
      IF MIN(tmp[ind[itest]]) LT 0.0 THEN STOP
    ENDFOR

    FOR t = 0, nbef-1 DO BEGIN
      FOR i = 0, N_ELEMENTS(ndvi_project_before[*,t])-1 DO BEGIN
        PRINTF, lunAnne, 'Impact'+dlmtr+'I'+STRTRIM(project_ids[p],2)+dlmtr+'before'+$
          dlmtr+STRTRIM(acq_before[t],2)+dlmtr+'P'+STRTRIM(i+1,2)+$
          dlmtr+STRTRIM(ndvi_project_before[i,t],2)
      ENDFOR  ;i
    ENDFOR ;t
    FOR t = 0, naft-1 DO BEGIN
      FOR i = 0, N_ELEMENTS(ndvi_project_after[*,t])-1 DO BEGIN
        PRINTF, lunAnne, 'Impact'+dlmtr+'I'+STRTRIM(project_ids[p],2)+dlmtr+'after'+$
          dlmtr+STRTRIM(acq_after[t],2)+dlmtr+'P'+STRTRIM(i+1,2)+$
          dlmtr+STRTRIM(ndvi_project_after[i,t],2)
      ENDFOR  ;i
    ENDFOR ;t

    ; controls
    stat_controls = FLTARR(winsize^2, MAX([nbef,naft]),2, n_selected) * !VALUES.F_NAN
    ;array set to maximum dimension a control may have
    FOR j = 1, n_selected DO BEGIN
      ind = WHERE((mask_control EQ project_ids[p]*1000+10+j) AND (classif NE unique_scl_class), count) ;the latter to exclude SLC pixels from both before an after
      ;n_selected is the number of controls
      ndvi_control_before = FLTARR(count,  nbef)
      FOR i = 0, nbef-1 DO BEGIN
        tmp = REFORM(ndvi_before[*,*,i])
        ndvi_control_before[*,i] = tmp[ind]
        stat_controls[0:N_ELEMENTS(ind)-1,i,0,j-1] = tmp[ind]
        ;some tests
        itest = where(finite(tmp[ind]), ctest)
        if ctest ne n_elements(tmp[ind]) then stop
        IF MIN(tmp[ind[itest]]) LT 0.0 THEN STOP
      ENDFOR
      ndvi_control_after = FLTARR(count,  naft)
      FOR i = 0, naft-1 DO BEGIN
        tmp = REFORM(ndvi_after[*,*,i])
        ndvi_control_after[*,i] = tmp[ind]
        stat_controls[0:N_ELEMENTS(ind)-1,i,1,j-1] = tmp[ind]
        ;some tests
        itest = where(finite(tmp[ind]), ctest)
        if ctest ne n_elements(tmp[ind]) then stop
        IF MIN(tmp[ind[itest]]) LT 0.0 THEN STOP
      ENDFOR

      FOR t = 0, nbef-1 DO BEGIN
        FOR i = 0, N_ELEMENTS(ndvi_control_before[*,t])-1 DO BEGIN
          PRINTF, lunAnne, 'Control'+dlmtr+'C'+STRTRIM(j,2)+dlmtr+'before'+$
            dlmtr+STRTRIM(acq_before[t],2)+dlmtr+'P'+STRTRIM(i+1,2)+$
            dlmtr+STRTRIM(ndvi_control_before[i,t],2)
        ENDFOR  ;i
      ENDFOR ;t
      FOR t = 0, naft-1 DO BEGIN
        FOR i = 0, N_ELEMENTS(ndvi_control_after[*,t])-1 DO BEGIN
          PRINTF, lunAnne, 'Control'+dlmtr+'C'+STRTRIM(j,2)+dlmtr+'after'+$
            dlmtr+STRTRIM(acq_after[t],2)+dlmtr+'P'+STRTRIM(i+1,2)+$
            dlmtr+STRTRIM(ndvi_control_after[i,t],2)
        ENDFOR  ;i
      ENDFOR ;t
    ENDFOR; k
    FREE_LUN, lunAnne

    IF (verbose EQ 1) THEN PRINT, 'RS_var Project before, ',  acq_before
    tmpStr = ''
    values = !NULL
    FOR i = 0, nbef-1 DO BEGIN
      tmpStr = tmpStr + STRTRIM(MEAN(stat_project[*,i,0], /NAN),2) + ' '
      values = [values, MEAN(stat_project[*,i,0], /NAN)]
    ENDFOR
    IF (verbose EQ 1) THEN PRINT, tmpStr
    IF (verbose EQ 1) THEN PRINT, 'RS_var Project after, ', + acq_after
    tmpStr = ''
    FOR i = 0, naft-1 DO BEGIN
      tmpStr = tmpStr + STRTRIM(MEAN(stat_project[*,i,1], /NAN),2) + ' '
      values = [values, MEAN(stat_project[*,i,1], /NAN)]
    ENDFOR
    IF (verbose EQ 1) THEN PRINT, tmpStr
    jd_impact_line1 = JULDAY(1,1,target_id_yr[1,p])
    jd_impact_line2 = JULDAY(1,1,target_id_yr[2,p])
    ;jd_impact_line = (jd_acq[N_ELEMENTS(acq_before)] + jd_acq[N_ELEMENTS(acq_before)-1])/2
    res = LABEL_DATE(DATE_FORMAT='%D/%N/%Z')
    hp = PLOT(jd_acq, values, SYMBOL='o', YTITLE='avgRS_var', $
      TITLE='Project = ' + STRTRIM(project_ids[p],2),  XTICKFORMAT = 'LABEL_DATE', XTICKFONT_SIZE= 9)


    values_before = FLTARR(nbef,n_selected)
    values_after = FLTARR(naft,n_selected)
    IF (verbose EQ 1) THEN PRINT, 'NDVI Controls'
    FOR j = 1, n_selected DO BEGIN
      IF (verbose EQ 1) THEN PRINT, 'Control ' + STRTRIM(project_ids[p]*1000 + 10 + j,2)  + ' before, ', acq_before
      tmpStr = ''
      FOR i = 0, nbef-1 DO BEGIN
        tmpStr = tmpStr + STRTRIM(MEAN(stat_controls [*,i,0,j-1], /NAN),2) + ' '
        values_before[i,j-1] = MEAN(stat_controls [*,i,0,j-1], /NAN)
      ENDFOR
      IF (verbose EQ 1) THEN PRINT, tmpStr
      IF (verbose EQ 1) THEN PRINT, 'Control '+ STRTRIM(project_ids[p]*1000 + 10 + j,2)  + ' after, ', acq_after
      tmpStr = ''
      FOR i = 0, naft-1 DO BEGIN
        tmpStr = tmpStr + STRTRIM(MEAN(stat_controls [*,i,1,j-1], /NAN),2) + ' '
        values_after[i,j-1] =MEAN(stat_controls [*,i,1,j-1], /NAN)
      ENDFOR
      IF (verbose EQ 1) THEN PRINT, tmpStr
    ENDFOR

    FOR j = 0, n_selected-1 DO BEGIN
      hc = PLOT(jd_acq, [values_before[*,j],values_after[*,j]], SYMBOL='+', $
        COLOR= 'red', OVERPLOT =1)
    ENDFOR
    ;plot again the project to have it on top
    hp = PLOT(jd_acq, values, SYMBOL='o', COLOR='black',OVERPLOT=1, THICK=2)
    hpl1 =  PLOT([jd_impact_line1,jd_impact_line1], hp.yrange, LINESTYLE= '--',OVERPLOT=1, COLOR='green')
    hpl2 =  PLOT([jd_impact_line2,jd_impact_line2], hp.yrange, LINESTYLE= '--',OVERPLOT=1, COLOR='green')
    fn_plot = out_dir + '\Plot_' + STRTRIM(project_ids[p],2)+'.png'
    hc.Save, fn_plot, BORDER=10, RESOLUTION=300
    PRINT, 'Project mean delta (avg After - avg Before)
    PRINTF, lun_cmd, 'Project mean delta (avg After - avg Before)
    PRINT, MEAN(stat_project[*,0:naft-1,1], /NAN) - MEAN(stat_project[*,0:nbef-1,0], /NAN)
    PRINTF, lun_cmd, MEAN(stat_project[*,0:naft-1,1], /NAN) - MEAN(stat_project[*,0:nbef-1,0], /NAN)
    PRINT, 'Controls After - Before deltas and similarity RMSE total'
    PRINTF, lun_cmd, 'Controls After - Before deltas and similarity RMSE total'
    FOR j = 0, n_selected-1 DO BEGIN
      PRINT, STRTRIM(project_ids[p]*1000 + 10 + j + 1,2) + ': ' + $
        STRTRIM(MEAN(stat_controls[*,0:naft-1,1,j], /NAN) - MEAN(stat_controls[*,0:nbef-1,0,j], /NAN),2) + $
        ' (' + STRTRIM(rmse_total_of_controls[j],2) + ')'
      PRINTF, lun_cmd, STRTRIM(project_ids[p]*1000 + 10 + j + 1,2) + ': ' + $
        STRTRIM(MEAN(stat_controls[*,0:naft-1,1,j], /NAN) - MEAN(stat_controls[*,0:nbef-1,0,j], /NAN),2) + $
        ' (' + STRTRIM(rmse_total_of_controls[j],2) + ')'
    ENDFOR
    PRINT, 'Finished project'
    PRINTF, lun_cmd, 'Finished project'
    FREE_LUN, lun_pps
  ENDFOR ; p
  FREE_LUN, lun_cmd
  
  
END