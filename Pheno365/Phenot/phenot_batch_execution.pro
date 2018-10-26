PRO phenot_batch_execution, info_file
  ;Can be called by a batch file prepared by decomposer or by command line
  ;EXAMPLE phenot: phenot_batch_execution, 'K:\MODIS_Niger\info.txt'
  ;or it can be used with IDL BRIDGE (better) as in Pheno365_IDL_bridge.pro

  @cb_job.comm
  @cb_pdhtf.comm
  @cb_options.comm
  @cb_in.comm
  @cb_stat.comm
  @cb_optimization.comm

  ;******************************************
  ;Part added to run on GRID unix
  ;  Identify the current OS family (IDL recognizes only two families, namely
  ;  'unix' and 'windows'):
  platform = !version.os_family

  ;  Set the screen device and the directory separator:
  IF (platform EQ 'unix') THEN BEGIN
    ;screen = 'X'
    dirsep = '/'
    info_file = STRJOIN(STRSPLIT(info_file, '\',/EXTRACT), '/')
  END
  ;******************************************

  ;#################################################################################################
  ;GENERAL SETTINGS: Image analysis or pixel analysis (doesn't write files and display graphics)
  remote = 1;1 ;1                      ;1 if running on a remote machine (set it to 1 to set up an error catcher)
  resume_from_save = 0            ;set to ne 0 if the work was interrupted and you want to start from last line (line is saved and will be restored)
  job='image'                     ;'pixel' or 'image'
  IF (remote EQ 1) THEN job = 'image'
  sample =2965; 497;
  line   = 2869;550;
  ;ONLY FOR LTDR analysis!!!!!!!
  ltdr_check = 0    ;implemented in coommon block job to skip the ltdr gap
  ;#################################################################################################




  ;READ INFO
  path = read_info('path', info_file)
  IF (platform EQ 'unix') THEN BEGIN
    path = STRJOIN(STRSPLIT(path, '\',/EXTRACT), '/')
  END

  faprangeminthresh = DOUBLE(read_info('faprangeminthresh', info_file))     ;expected minimum variability value moved to info file
  use_lomb = FIX(read_info('use_lomb', info_file))                          ;use_lomb makes lomb ratio to be calculated and saved , not used for the moment
  lombRatThreshold = DOUBLE(read_info('lombRatThreshold', info_file))       ;Above this ratio of peak1/peak2 we assume ngspy = 1, below it's ngspy = 2
  upenv = FIX(read_info('upenv', info_file))                                ;0 = Normal model fit, 1 = Upper envelope fit
  period = FIX(read_info('period', info_file))                              ;for decadal analysis, the only possible now

  ibel = DOUBLE(read_info('ibel', info_file))                               ;ignore below
  iabo = DOUBLE(read_info('iabo', info_file))                               ;ignore above
  fract_thresh = DOUBLE(read_info('fract_thresh', info_file))               ;fraction of amplitued use to compute start and stop in pdhtf (see White et al., 2009)
  fract_thresh2 = DOUBLE(read_info('fract_thresh2', info_file))             ;fraction of amplitued use to compute the second stop in pdhtf

  filein = read_info('filein', info_file)
  fileAcqJulDay =  read_info('acqJULDAY', info_file)
  mask =  read_info('mask', info_file)
  ns = read_info('ns', info_file)
  nl = read_info('nl', info_file)
  nb =  read_info('nb', info_file)
  fst_dek = read_info('fst_dek', info_file)
  fst_day = read_info('fst_day', info_file)
  fst_month = read_info('fst_month', info_file)
  first_year = read_info('first_year', info_file)
  last_year = read_info('last_year', info_file)
  julian1_f=float(floor(JULDAY(fst_month, fst_day, first_year, 13, 0, 0))) ;first dekad is 9810

  ;ret=pheno_options(job) ;pheno settings and parameters

  vrbs=0              ;no verbose in @ cb_options.comm, suppress all print and report file
  IF (job EQ 'image') THEN BEGIN
    pltres = 0             ;suppres all plots if 0
    wantplotlomb=0      ;suppress lomb plot
  ENDIF ELSE BEGIN              ;it's pixel analysis
    pltres = 1
    resume_from_save = 0
    wantplotlomb = 1
    vrbs=1
  END

  ret=phenot_handler(remote, job, path, filein, fileAcqJulDay, ns, nl, nb, fst_dek, first_year, last_year, julian1_f, mask, sample, line, faprangeminthresh)

END