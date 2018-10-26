PRO runHandler
  ;ini_list = ['E:\SimMod_data\RUN1.txt', 'E:\SimMod_data\RUN2.txt', 'E:\SimMod_data\RUN3.txt', 'E:\SimMod_data\RUN3b.txt', 'E:\SimMod_data\RUN4.txt', 'E:\SimMod_data\RUN5.txt']
  ;ini_list = ['F:\SimMod_data\RUN6_250CroBE-Lon_eps1-5P1_ll_gamma.txt'];['F:\SimMod_data\RUN6_250_C2DM.txt']
  
  ;ini_list = 'F:\SimMod_data\newRUN1_4sites_eps' + STRTRIM(INDGEN(16)+10,2) + '.txt'
  ;ini_list = ['D:\SimMod_data\newRUN1_Validation_sites.txt']
  ini_list = ['D:\SimMod_data\newRUN1_Validation_sites_exclude_avi.txt']
  ;remove and run on all when stable
  ;ini_list = ['D:\SimMod_data\RUN6_250.txt']
  clumping = [1.0, 0.78] ;[1.0];

  FOR i = 0, N_ELEMENTS(ini_list)-1 DO BEGIN
    FOR j = 0, N_ELEMENTS(clumping)-1 DO BEGIN
      ret = A_simInvMod_handler_water_lim(ini_list[i], clumping[j])
    ENDFOR
  ENDFOR
END

FUNCTION A_simInvMod_handler_water_lim, ini_fn, clumping
  !EXCEPT = 0

  buffer_site_year_plot = 1 ;with 1 no pop up windows, with 0 yes
  force_mono = 0 ;TEST FOR CROPS

  MPFIT_RESET_RECURSION

  ; Initialized required common blocks
  @cb_various.comm
  clump = clumping
  @cb_prospect_last_parameters.comm      ;last parameters
  lCab=-999 & lCar=-999 & lCbrown=-999 & lCw=-999 & lCm =-999 & lNs =-999
  @cb_prosail_data.comm                  ;skyl
  skyl = dataSkyle()
  data = dataSpec_P5B()
  @cb_sail_last_parameters.comm ; last parameters
  llai = -999 & lhspot = -999 & ltts =  -999 & ltto =  -99 & lpsi = -999
  @cb_ladgen.comm                        ;ladgen input/output
  llidf_a = -999 & llidf_b=-999 & lidf=-999
  ; Load (once and for all) specific tables repeatedly used to process PROSAIL outputs
  ; spectral weights to compute a single FAPAR value from spectral diffuse canopy absorption
  wpar = dataWpar()
  ; MODIS Terra relative spectral response functions for the first 7 bands (to get MODIS reflectances from spectral ref)
  rsrLR = dataRSR_MODIST_7b_fast2(); dataRSR_MODIST_7b()
  ; MODIS central wavelengths for the first 7 bands
  cwl = datacentwl_MODIST_7b()
  ; prosail simulated wavlengths
  wl = INDGEN(2101) + 400

  ;use MODIS 500 or 250
  modisRes = 250

  ;##########################################################################################
  ;USER PARAMETERS ARE DEFINED IN THE ini FILE
  ;THE ini FILE ALSO SPECIFIES A site_ini FILE THAT LISTS THE SITE SPECIFIC INFO (INPUT
  ;DATA, YEARS TO BE ANALYZED)
  ;ini_fn = 'E:\SimMod_data\v2_ini_RUN5wlimUpperenvNoWeight_PCAP200d0_with_P1logistic.txt';SimMod_all_ini.txt'
  ;##########################################################################################

  ;Read the info from ini file
  site_ini_fn = read_info('site_ini_fn', ini_fn)
  site_info = read_site_info_v2(site_ini_fn)
  ; sav file use to avoid the inversion and use last run results
  useSave = FIX(read_info('useSave', ini_fn))
  inverseResultsFile = read_info('inverseResultsFile', ini_fn)
  IF (useSave EQ 1) THEN BEGIN
    PRINT,'*************Warning: use SAVE**************'
    PRINTF, lunlog, '*************Warning: use SAVE**************'
  ENDIF
  ; Directy used to store the outputs (an copy ini and first guesses ect)
  out_dir  =  read_info('out_dir', ini_fn)
  out_dir = out_dir + '_clump' + STRING(clump, FORMAT = '(F4.2)');STRTRIM(clump,2)
  IF (FILE_SEARCH(out_dir,/TEST_DIRECTORY) EQ '') THEN FILE_MKDIR, out_dir
  ; Copy ini file there for future reference
  FILE_COPY, ini_fn, out_dir + '\' +  FILE_BASENAME(ini_fn), /OVERWRITE
  ;open a log file gathering all the info sent to command line
  OPENW, lunLog, out_dir + '\log.txt', /GET_LUN
  tmp = FILE_BASENAME(ini_fn)
  runReportFn =  out_dir + '\AAA_report_' + FILE_BASENAME(ini_fn,'.txt') + '.csv'
  site_run_reportFn = out_dir + '\AAA_site_report_' + FILE_BASENAME(ini_fn,'.txt') + '.csv'
  overall_run_reportFn =  out_dir + '\AAA_overall_report_' + FILE_BASENAME(ini_fn,'.txt') + '.csv'
  IF (FILE_SEARCH(runReportFn) NE '') THEN FILE_DELETE, runReportFn
  IF (FILE_SEARCH(site_run_reportFn) NE '') THEN FILE_DELETE, site_run_reportFn
  IF (FILE_SEARCH(overall_run_reportFn) NE '') THEN FILE_DELETE, overall_run_reportFn
  ; doPlotSpectra, set it to 1 to plot reflectances (sim and obs) for the second obs, @ max obs NDVI, second last
  doPlotSpectra = FIX(read_info('doPlotSpectra', ini_fn))

  ;Number of SOS and EOS Standard deviation use to enlarge SOS-EOS period an retrieve the temporal domain
  nSD = FIX(read_info('nSD', ini_fn))

  ;Type of modelization used for P1 and sen (“MaasLike” or “Logistic”)
  P1_sen_model  = read_info('P1_sen_model', ini_fn)

  ; use eddy or ecmwf to compute the water limitation
  ; useEddyMet, set it to 1 to use eddy meteo data (T, Globrad and Precipitation). 0 to use ECMWF
  useEddyMet = FIX(read_info('useEddyMet', ini_fn))
  IF (useEddyMet EQ 1) THEN BEGIN
    PRINT, '*************USING EDDY meteo***************'
    PRINTF, lunlog, '*************USING EDDY meteo***************'
    wlim_source = 'eddy'
  ENDIF ELSE wlim_source = 'ecmwf'

  inversion_algrtm = read_info('inversion_algrtm', ini_fn)

  ; uppEnvFit, set it to 1 to fit the upper envelope according to the TRUE method of Chen et 2004
  ; it is slower compared to normal fitting, NO EFFECT ON METROPOLIS FOR THE MOMENT
  uppEnvFit = FIX(read_info('uppEnvFit', ini_fn))
  ;water limitation (0/1)
  eps_wlimOnOff = FIX(read_info('eps_wlimOnOff', ini_fn))
  ;wlim_type (casa, casa_kc, bewm_opt)
  wlim_type = read_info('wlim_type', ini_fn)
  ;if casa_kc read the kcs
  IF (wlim_type EQ 'casa_kc') THEN kcs = read_kcs(ini_fn)
  ; weightingScheme cane be 'none' = no weigh, 'minmax' = weight based on (NDVI-min)/(max-min)
  ;this weighting scheme is used in the inversion of the model against NDVI data
  weightingScheme = read_info('weightingScheme', ini_fn)

  ;----------------------------------------------------------------
  ;START THE SITE SPECIFIC PART (and analyse year by year, season by season)
  run_c = 0 ;run counter
  ;variable used to store seasonal results, the following ara all resampled, SimGPP is zero outside the season
  multiSiteYearSimGpp = [!NULL]
  multiSiteYearEcGpp = [!NULL]
  multiSiteYearModisGPP = [!NULL]
  multiSiteYearSiteCode = [!NULL]
  multiSiteYearSiteId = [!NULL]
  multiSiteYearSiteIGBP = [!NULL]



  FOR t = 0, N_ELEMENTS(site_info.Site_code)-1 DO BEGIN
    ;if casa_kc is used, determin which kc to use for this site
    kc =0
    IF (wlim_type EQ 'casa_kc') THEN BEGIN
      CASE site_info.igbp[t] OF
        'CRO': BEGIN
          ind = WHERE(kcs[*].name EQ 'CRO')
          kc = kcs[ind]
        END
        ELSE: BEGIN
          ind = WHERE(kcs[*].name EQ 'GRA')
          kc = kcs[ind]
        END
      ENDCASE
      PRINT, site_info.site_code[t], ' ',site_info.igbp[t], ' using kc: '
      PRINTF, lunlog, site_info.site_code[t], ' ',site_info.igbp[t], ' using kc: '
      PRINT, kc
      PRINTF, lunlog, kc
    ENDIF
    ;MODIS data
    ;gpp
    IF ((site_info.rs_fn) NE '') THEN BEGIN
      RefGppAvailable = 1
      RefGpp_data = get_modis_gpp_profile_v2(site_info.gpp_fn, site_info.site_code[t])
    ENDIF ELSE BEGIN
      RefGppAvailable = 0
      RefGpp_data = 0
    ENDELSE
    ;Reflectances
    IF (modisRes EQ 500) THEN BEGIN
      modis_data = get_modis_profile_v2(site_info.rs_fn, site_info.site_code[t])
    ENDIF
    IF (modisRes EQ 250) THEN BEGIN
      modis_data = get_modis_VIprofile_v3(site_info.rs_fn, site_info.site_code[t])  ;MODIFIED FOR VI 250
    ENDIF
    ;ECMWF
    ecmwf_data = get_ecmwf_profile_v2(site_info.modmet_fn, site_info.site_code[t])
    ;EDDY data
    eddy_data = get_eddy_profile_v2(site_info.eddy_dir, site_info.site_code[t])
    ;Get the pheno info for this site
    ;pheno = bridge_SimMod2Phenot365(modis_data, force_mono)
    pheno = bridge_SimMod2Phenot365(modis_data, 0)
    IF (site_info.site_code[t]) EQ 'FR-Lam' THEN BEGIN
      ;pheno = CREATE_STRUCT('ngspy', 2, 'lombrat', 0.0, 'avg_sos', [1.0,220.0], 'sd_sos', [20.0,20],'avg_eos', [210.0,350], 'sd_eos', [20.0,20],'avg_tom', [140.0,290], 'sd_tom', [20,20], 'log_text', 'pheno imposed')
      pheno = CREATE_STRUCT('ngspy', 2, 'lombrat', 0.0, 'avg_sos', [330,180.0], 'sd_sos', [30.0,30],'avg_eos', [180.0,330], 'sd_eos', [40.0,30],'avg_tom', [130.0,280], 'sd_tom', [30,30], 'log_text', 'pheno imposed')
    ENDIF
    PRINTF, lunlog, pheno.log_text
    ;as we do analyze year after year s1 and s2, it is better to have s1 has rthe one occurring first during the year
    IF ((pheno.ngspy EQ 2) AND (pheno.AVG_SOS[0] GT pheno.AVG_SOS[1])) THEN BEGIN
      PRINT, 'S1 after S2, season order switched'
      PRINTF, lunlog, 'S1 after S2, season order switched'
      pheno.AVG_SOS = REVERSE(pheno.AVG_SOS)
      pheno.SD_SOS = REVERSE(pheno.SD_SOS)
      pheno.AVG_EOS = REVERSE(pheno.AVG_EOS)
      pheno.SD_EOS = REVERSE(pheno.SD_EOS)
      pheno.AVG_TOM = REVERSE(pheno.AVG_TOM)
      pheno.SD_TOM = REVERSE(pheno.SD_TOM)
    ENDIF

    ;v2: determine which years are available for the site, taking pheno into account
    ;These are all years tha must be covered by simulations
    avlblYears = MIN(eddy_data.year, /NAN) + INDGEN(MAX(eddy_data.year, /NAN)-MIN(eddy_data.year, /NAN)+1)
    ;so add one before beccause the season may cross the year (if no EC data are availble, does not matter?)
    avlblYears = [avlblYears[0]-1, avlblYears]
    ;if the season crosses the calendar year, the last year cannot be used? No it can be used, there will be less EC data..
    ;IF (pheno.AVG_EOS[0] LE pheno.AVG_SOS[0]) THEN avlblYears = avlblYears[0:-2]

    ;Retrieve for this site Rsoil at 1 nm resolution from the time series
    r_back = rback_from_MODIS_TS(modis_data.R1,modis_data.R2,modis_data.R3,modis_data.R4,$
      modis_data.R5,modis_data.R6,modis_data.R7, 2, doPlotSpectra)

    ;variable used to store seasonal results
    siteResmplSimGpp = [!NULL]
    siteResmplSimJD = [!NULL]
    siteSimNDVI = [!NULL]
    siteSimNDVIJD = [!NULL]

    skippedBecauseNoModis = 0

    IF CHECK_MATH() NE 0 THEN BEGIN
      PRINT, 'Math error occurred before here'
      STOP
    ENDIF

    FOR y = 0, N_ELEMENTS(avlblYears)-1 DO BEGIN
      ;----------------------------------------------------------------
      ;START THE YEAR SPECIFIC PART
      FOR s = 0, pheno.ngspy -1 DO BEGIN
        ;----------------------------------------------------------------
        ;START THE SEASON SPECIFIC PART
        FOR w = 0, 1 DO BEGIN
          ;----------------------------------------------------------------
          ;INVERT WITHOUT AND WITH WATER LIMITATION AND KEEP THE BEST FIT (added on 12/4/2017 for irrigated crop as ES-ES2)
          eps_wlimOnOff = w ;overwrite the the setting
          sYear = FIX(avlblYears[y])
          ;make a base name for output file names
          PRINT, ''
          PRINT, 'Analysing site ' + site_info.site_code[t] + ', year ' + STRTRIM(sYear,2) + ' and season ' + STRTRIM(s+1,2)
          PRINTF, lunlog, 'Analysing site ' + site_info.site_code[t] + ', year ' + STRTRIM(sYear,2) + ' and season ' + STRTRIM(s+1,2)
          base_name = site_info.igbp[t] + '_' + site_info.site_code[t] + '_Y' + STRTRIM(sYear,2) + '_S' + STRTRIM(s+1,2)

          ;case 1 eos<=sos, means sos is in year Y and eos in Y+1
          IF (pheno.AVG_EOS[s] LE pheno.AVG_SOS[s]) THEN eYear = sYear + 1 ELSE eYear = sYear
          IF (pheno.AVG_TOM[s] LE pheno.AVG_SOS[s]) THEN tYear = sYear + 1 ELSE tYear = sYear
          ;now move to Julian Days to avoid ambiguities
          ssJD = DOY_YEAR2JD(pheno.AVG_SOS[s], sYear)
          eeJD = DOY_YEAR2JD(pheno.AVG_EOS[s], eYear)
          tomJD = DOY_YEAR2JD(pheno.AVG_TOM[s], tYear)
          PRINT, 'Avg sDOY from pheno: ' + STRTRIM(pheno.AVG_SOS[s], 2)
          PRINTF, lunlog, 'Avg sDOY from pheno: ' + STRTRIM(pheno.AVG_SOS[s], 2)
          PRINT, 'Avg eDOY from pheno: ' + STRTRIM(pheno.AVG_EOS[s], 2)
          PRINTF, lunlog, 'Avg eDOY from pheno: ' + STRTRIM(pheno.AVG_EOS[s], 2)
          ;expand it by nSD standard deviations
          reducedN = nSD
          sJD = ROUND(ssJD - nSD * pheno.SD_SOS[s])
          eJD = ROUND(eeJD + nSD * pheno.SD_EOS[s])
          ;check that it is not longer than one year, reduce it if this is the case
          IF ((eJD - sJD) GT 365) THEN BEGIN
            WHILE ((eJD - sJD) GT 365) DO BEGIN
              reducedN = reducedN*0.9
              sJD = ROUND(ssJD - reducedN * pheno.SD_SOS[s])
              eJD = ROUND(eeJD + reducedN * pheno.SD_EOS[s])
            ENDWHILE
          ENDIF
          ; start year and DOY (a period larger than any possible start and end of season)
          sDOY = JD2DOY(sJD)
          sYearCurrentYear = sYear  ;keep a copy, SD enlargement may go in another year
          sYear = JD2YEAR(sJD)
          PRINT, 'sDOY -' + STRTRIM(reducedN,2) + 'SD and year from pheno: ' + STRTRIM(sDOY, 2) + ', ' + STRTRIM(sYear,2)
          PRINTF, lunlog, 'sDOY -' + STRTRIM(reducedN,2) + 'SD and year from pheno: ' + STRTRIM(sDOY, 2) + ', ' + STRTRIM(sYear,2)
          ; end year and DOY
          eDOY = JD2DOY(eJD)
          eYear = JD2YEAR(eJD)
          PRINT, 'eDOY +' + STRTRIM(reducedN,2) + 'SD and year from pheno: ' + STRTRIM(eDOY, 2) + ', ' + STRTRIM(eYear,2)
          PRINTF, lunlog, 'eDOY +' + STRTRIM(reducedN,2) + 'SD and year from pheno: ' + STRTRIM(eDOY, 2) + ', ' + STRTRIM(eYear,2)
          ;      ; express temporal settings as JD
          ;      sJD = DOY_YEAR2JD(sDOY, sYear)
          ;      eJD = DOY_YEAR2JD(eDOY, eYear)
          ;28/2/2017: at this point ssJD and eeJD are not used anymore, consider removing them
          PRINT, 'Temporal range (DD/MM/YYYY): ', [JD2DDMMYYYY(sJD), JD2DDMMYYYY(eJD)]
          PRINTF, lunlog,  'Temporal range (DD/MM/YYYY): ', [JD2DDMMYYYY(sJD), JD2DDMMYYYY(eJD)]
          PRINT, 'Temporal range (JD): ', LONG(sJD), LONG(eJD)
          PRINTF, lunlog, 'Temporal range (JD): ', LONG(sJD), LONG(eJD)

          ;FIXED PARAMETERS
          globrad2par=0.48    ;conversion factor from Global radiation to PAR, (McCree, 1972 and many others)

          ;SET FIRST guesses
          fcnargs = {SYEAR: sYear}
          fg = firstGuess_read_water_lim(ini_fn, site_info.igbp[t], _EXTRA=fcnargs)
          ;make some of the first guess available as single variable
          JD0 = fg[0]
          SLA = fg[2]
          Tb = fg[9]

          ;SET PARINFO
          fcnargs = {FG:fg, JD0:JD0, SJD:sJD, EJD:eJD}
          ;parinfo = parinfo_define_water_lim(4, _EXTRA=fcnargs)
          parinfo = parinfo_read_water_lim(ini_fn, _EXTRA=fcnargs)
          ;change parameters set using phenological analysis out come
          ;set first guess and parinfo
          fg[0] = DOY_YEAR2JD(pheno.AVG_SOS[s], sYearCurrentYear)
          ;fg[0] = DOY_YEAR2JD(pheno.AVG_SOS[s], sYear)
          JD0 = fg[0]
          str2search = parinfo[0].PARNAME + '_parinfo'
          values =  read_info(str2search, ini_fn)
          values = STRSPLIT(values, ',', /EXTRACT)
          parinfo[0].LIMITS  = [DOUBLE(MAX([JD0+FIX(values[3]), sJD])), DOUBLE(MIN([JD0+FIX(values[4]), eJD]))]
          PRINT, 'Limits for JD:', parinfo[0].LIMITS
          PRINTF, lunlog, 'Limits for JD:', parinfo[0].LIMITS
          PRINT, FORMAT='("Limits for JD (date): ",I2,"/",I2,"/",I4," - ",I2,"/",I2,"/",I4)',JD2DDMMYYYY(parinfo[0].LIMITS[0]), JD2DDMMYYYY(parinfo[0].LIMITS[1])
          PRINTF, lunlog, FORMAT='("Limits for JD (date): ",I2,"/",I2,"/",I4," - ",I2,"/",I2,"/",I4)',JD2DDMMYYYY(parinfo[0].LIMITS[0]), JD2DDMMYYYY(parinfo[0].LIMITS[1])
          ;if the logistic is used for partitioning and senescence compute here the first guesses
          ;a = GDD at which P1 and sen are is 0.05, a first guess this is the GDD over the
          ;period SOS(0.05) and TOM
          ;pheno cardinal points (extended by SD) are in ssJD, eeJD, tomJD.

          ;here use a and b for the P1 logistic
          ;b is the astmptotic value, fixed
          ;a = GDD when P1 is equal to cutoff value near 0, first guess is set to the GDD between start and tom
          a = extract_timerange(ecmwf_data.JD, sJD, tomJD, ecmwf_data.tav)
          a = computedGDD_abs(a, Tb)
          fg[7] = a
          ;set limits for parameter a
          mt = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.tav)
          mt = computedGDD_abs(mt, Tb) / FLOAT(N_ELEMENTS(mt))
          ;mt = MEAN(extract_timerange(ecmwf_data.JD, sJD, eJD, posTav))
          ll = mt * FIX(values[3])
          ;estimation of upper limit: GDD between start and end
          aUL = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.tav)
          aUL = computedGDD_abs(aUL, Tb)
          ;ul = mt * FIX(values[4])
          parinfo[7].LIMITS = [MAX([a + ll, mt*10]), aUL]

          IF (P1_sen_model EQ 'MaasLikeSen_P1logistic') THEN BEGIN
            PRINT, 'Logistic used for P1'
            PRINTF, lunlog, 'Logistic used for P1'
            PRINT, 'a fg = ' + STRTRIM(a,2)
            PRINTF, lunlog, 'a fg = ' + STRTRIM(a,2)
            ;make a first guess for c. GDD between sos and tom
            c = extract_timerange(ecmwf_data.JD, sJD , tomJD, ecmwf_data.tav)
            c = computedGDD_abs(c, Tb)              ;c = TOTAL(extract_timerange(ecmwf_data.JD, tomJD, eJD, posTav))
            fg[5] = c
            parinfo[5].LIMITS = [250, c*1.5]
          ENDIF
          ;Save fg and parinfo
          res = saveconfig(fg, parinfo, out_dir + '\' +  FILE_BASENAME(ini_fn, '.txt') + $
            '_' + base_name + '_cnfg.txt')


          ;PROSAIL PARAMETERS
          proSailVar = Set_Prosail_param(SLA)
          proSailVar.rsoil = r_back

          ;*******************************************************************
          ;Extract relevant data to be sent to forward model)
          ;ECMWF
          tJD = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.JD)
          ecmwf_globrad = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.rad)
          ecmwf_tair = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.tav)
          PRINT, 'Mean positive air T during the period (ecmwf): ' + strtrim(MEAN(ecmwf_tair[WHERE(ecmwf_tair GE 0.0)]),2)
          PRINTF, lunlog, 'Mean positive air T during the period (ecmwf): ' + strtrim(MEAN(ecmwf_tair[WHERE(ecmwf_tair GE 0.0)]),2)
          PRINT, 'Thus Phalf roughly corresponding to: ' + strtrim(ROUND(fg[11]/MEAN(ecmwf_tair[WHERE(ecmwf_tair GE 0.0)])),2) + ' days'
          PRINTF, lunlog, 'Thus Phalf roughly corresponding to: ' + strtrim(ROUND(fg[11]/MEAN(ecmwf_tair[WHERE(ecmwf_tair GE 0.0)])),2) + ' days'
          ecmwf_prec = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.rain)
          ; as this approach considers a "memory of precipitation", and how long depends on half_life parameter,
          ; I have to use some data before sJD. To be sure that I get enough, I use three months
          DOYoffset = - 90
          ;ecmwf always loaded
          wlim = { $
            eps_onoff: eps_wlimOnOff, $
            tJD: extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.JD), $
            tair: extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.tav), $
            rain: extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.rain), $
            DOYoffset: DOYoffset}
          IF (wlim_source EQ 'eddy') THEN BEGIN
            ;overwrite where eddy data ara available
            eddy_tJD= extract_timerange(eddy_data.JD, sJD + DOYoffset, eJD, eddy_data.JD)
            eddy_tair= extract_timerange(eddy_data.JD, sJD + DOYoffset, eJD, eddy_data.Ta)
            eddy_rain= extract_timerange(eddy_data.JD, sJD + DOYoffset, eJD, eddy_data.precip)
            ind0 = WHERE(wlim.tJD EQ eddy_tJD[0])
            indrain = WHERE(FINITE(eddy_rain))
            indtair = WHERE(FINITE(eddy_tair))
            ;ind1 = WHERE(wlim.tJD EQ eddy_tJD[N_ELEMENTS(eddy_tJD)-1])
            IF ((ind0[0] NE 0) OR (N_ELEMENTS(indrain) NE N_ELEMENTS(eddy_rain)) OR (N_ELEMENTS(indtair) NE N_ELEMENTS(eddy_tair))) THEN BEGIN
              PRINT, 'Warning: not enough eddy data or missing, filled with ecmwf'
              PRINTF, lunlog, 'Warning: not enough eddy data or missing, filled with ecmwf'
              PRINT, '(Only for the water limitation part)'
              PRINTF, lunlog, '(Only for the water limitation part)'
            ENDIF
            wlim.tair[ind0[0]+indtair] = eddy_tair[indtair]
            wlim.rain[ind0[0]+indrain] = eddy_rain[indrain]
          ENDIF
          ;MODIS
          satR1 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R1)
          satR2 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R2)
          satR3 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R3)
          satR4 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R4)
          satR5 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R5)
          satR6 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R6)
          satR7 = extract_timerange(modis_data.JD, sJD, eJD, modis_data.R7)
          satObsJD = extract_timerange(modis_data.JD, sJD, eJD, modis_data.JD)
          satObsRAA = extract_timerange(modis_data.JD, sJD, eJD, ABS(modis_data.RAA))
          satObsSZA = extract_timerange(modis_data.JD, sJD, eJD, modis_data.SZA)
          satObsVZA = extract_timerange(modis_data.JD, sJD, eJD, modis_data.VZA)

          ;Eddy data
          ec_prec = extract_timerange(eddy_data.JD, sJD, eJD, eddy_data.precip)
          ec_tair = extract_timerange(eddy_data.JD, sJD, eJD,eddy_data.Ta)
          ec_JD = extract_timerange(eddy_data.JD, sJD, eJD,eddy_data.JD)
          ec_gpp_mds = extract_timerange(eddy_data.JD, sJD, eJD,eddy_data.gpp_mds)
          ec_globrad= extract_timerange(eddy_data.JD, sJD, eJD,eddy_data.radg)
          ;*******************************************************************
          ;if no eddy data are available (more than 50% missing), skip the year 13/03/2017: no, analyse it anyhow, do not only if there is not one single data
          ;IF (TOTAL(FINITE(ec_gpp_mds))/FLOAT(N_ELEMENTS(ec_gpp_mds)) GT 0.5) THEN BEGIN
          If ((s EQ 0) AND (y EQ 0)) THEN firstJD2consider = tJD[0]
          ;update if previous was skipped because of modis ts not started yet
          IF (skippedBecauseNoModis EQ 1) THEN  firstJD2consider = tJD[0]
          IF (tJD[0] LT JULDAY(02, 02, 2001)) THEN skippedBecauseNoModis = 1 ELSE skippedBecauseNoModis = 0
          IF (TOTAL(FINITE(ec_gpp_mds)) GE 1) AND (tJD[0] GT JULDAY(02, 02, 2001)) THEN BEGIN ;JULDAY(02, 02, 2001) is the starting JD of Modis
            ;compute bewm once here and store in common block if bewm parameters are fixed
            IF (wLimParFixed EQ 1) THEN BEGIN
              bewm = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, wlim.rain, Tb, fg[11], fg[10])
              ;#################################################
              ;lta
              ;bewm_lta = compute_LTABewm(ecmwf_data.JD, ecmwf_data.rain, ecmwf_data.tav, sDOY, eDOY, DOYoffset, fg[11], fg[10])
              ;extract relavant period for graphics
              ind = WHERE(wlim.tJD EQ tJD[0])
              ;ind2 = WHERE(bewm_lta[ind:*] NE 0.0)
              ;ratio =  bewm[ind+ind2]/bewm_lta[ind+ind2]
              ;now for ET0
              et0 = extract_timerange(ecmwf_data.JD, sJD + DOYoffset, eJD, ecmwf_data.et0)
              bewm_et0 = BackwardExpWeightedMean_FAST(wlim.tJD, wlim.tair, et0, Tb, fg[11], 100.0)
              ;check for zeros
              ind0 = WHERE(bewm_et0 EQ 0.0, count0)
              IF (count0 GT 0) THEN bewm_et0[ind0] = 0.0001
              x = JD2DOY(wlim.tJD)
              dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
              xminor = 2        ;monor ticks between majors
              nmajor = 4

              ratio = bewm/bewm_et0
              IF (doPlotSpectra) THEN BEGIN
                xrange  = [MIN(wlim.tJD), MAX(wlim.tJD)]
                yr = [0,40]
                h_bewm = PLOT(wlim.tJD, bewm, XRANGE = xrange , NAME='Rain_bewm', MARGIN = [0.15, 0.15, 0.20, 0.15], $
                  YRANGE=yr, AXIS_STYLE = 1, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, WINDOW_TITLE = base_name + '_met')
                strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
                loc = -(yr[1]-yr[0])/10
                a_x =AXIS('X', TARGET = h_bewm, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)

                h_et0_bewm = PLOT(wlim.tJD, bewm_et0, XRANGE = xrange ,color='red', OVERPLOT=1, NAME='et0_bewm', $
                  YRANGE=yr,MARGIN = [0.15, 0.15, 0.20, 0.15])
                h_et0 = PLOT(wlim.tJD, et0, XRANGE = xrange ,color='purple', OVERPLOT=1, NAME='et0', $
                  YRANGE=yr,MARGIN = [0.15, 0.15, 0.20, 0.15])
                h_p = PLOT(wlim.tJD, wlim.rain, XRANGE = xrange , COLOR = 'blue', OVERPLOT=1,  NAME='rain', $
                  YRANGE=yr, AXIS_STYLE = 1, MARGIN = [0.15, 0.15, 0.20, 0.15])
                yr_ratio = [0, MAX(ratio)]
                h_ratio = PLOT(wlim.tJD, ratio, XRANGE = xrange ,color='green', /CURRENT, NAME='RATIO r_et0_bewms', MARGIN = [0.15, 0.15, 0.20, 0.15], THICK= 2, $
                  YRANGE=yr_ratio, AXIS_STYLE = 0)
                ah = AXIS('y', TARGET = h_ratio, LOCATION = 'right', TITLE = 'ratio')
                !null = LEGEND(target=[h_bewm, h_ratio, h_et0_bewm, h_et0, h_p], /AUTO_TEXT_COLOR, POSITION=[0.9,0.9], /NORMAL, $
                  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
              ENDIF
              ;#################################################
            ENDIF

            ;look for math errors
            IF CHECK_MATH() NE 0 THEN BEGIN
              PRINT, 'Math error occurred before here'
              STOP
            ENDIF


            IF (useSave EQ 0) THEN BEGIN
              ;RUN INV MODEL
              CASE useEddyMet OF
                0: ret = InvWrapper_water_lim(parinfo, fg, tJD, ecmwf_globrad, globrad2par, ecmwf_tair, wlim, proSailVar, $
                  satObsJD, satObsRAA, satObsSZA, satObsVZA, $
                  satR1, satR2, satR3, satR4, satR5, satR6, satR7, uppEnvFit, weightingScheme, inversion_algrtm)
                1: ret = InvWrapper_water_lim(parinfo, fg, ec_JD, ec_globrad, globrad2par, ec_tair, wlim, proSailVar, $
                  satObsJD, satObsRAA, satObsSZA, satObsVZA, $
                  satR1, satR2, satR3, satR4, satR5, satR6, satR7, uppEnvFit, weightingScheme, inversion_algrtm)
                ELSE: stop
              ENDCASE
              SAVE, /VARIABLES, FILENAME = inverseResultsFile
            ENDIF ELSE BEGIN
              RESTORE, FILENAME = inverseResultsFile
            ENDELSE

            IF (SIZE(ret, /DIMENSIONS) EQ 0) THEN BEGIN
              PRINT, ret
              STOP
            ENDIF

            IF (w EQ 0) THEN BEGIN
              ret0 = ret
              wlim0 = wlim
            ENDIF
          ENDIF
        ENDFOR ;with and without wlim
        ;now go on with analysis using the best wlim settings
        IF (TOTAL(FINITE(ec_gpp_mds)) GE 1) AND (tJD[0] GT JULDAY(02, 02, 2001)) THEN BEGIN
          ;now select, on the basis of the quality of NDVI fitting, which wlim setting to use
          ;current ret is for water limited, use ret0 (no water lim) only if better
          IF (ret0.info_mpfit.BestChi2[0] LT  ret.info_mpfit.BestChi2[0]) THEN BEGIN
            ret = ret0
            wlim = wlim0
          ENDIF

          ;RUN FWD MODEL with inverted parameters
          JD0=FLOOR(ret.parms[0]) & laiDOY0=ret.parms[1] & SLA=ret.parms[2] & eps_max=ret.parms[3] & gamma=ret.parms[4]
          c=ret.parms[5]  & d=ret.parms[6]       & a=ret.parms[7]   & b=ret.parms[8]  & tb=ret.parms[9]
          cap=ret.parms[10] & half_life=ret.parms[11] & bewm_opt=ret.parms[12]

          CASE useEddyMet OF
            0: sim = simForwMod_water_lim2(JD0, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, $
              tJD, ecmwf_globrad, globrad2par, ecmwf_tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)
            1: sim = simForwMod_water_lim2(JD0, laiDOY0, SLA, eps_max,  wlim, cap, half_life, bewm_opt, gamma, c, d, a, b, $
              ec_JD, ec_globrad, globrad2par, ec_tair, tb, proSailVar, satObsJD, satObsRAA, satObsSZA, satObsVZA)
            ELSE: stop
          ENDCASE

          ;Compute GDD of eddy data from first day of simulation
          indFin = WHERE(FINITE(ec_JD))
          indJD0_eddy = WHERE(ec_JD[indFin] EQ JD0, count)
          ;it may happen that the model begin before the data are collected
          IF (count EQ 0) THEN BEGIN
            ;take the first avialable
            IF (JD0 LT ec_JD[indFin[0]]) THEN BEGIN
              indJD0_eddy = 0
            ENDIF ELSE BEGIN
              ;the model begind when eddy data are not available anymore
              indJD0_eddy = -1
            ENDELSE
          ENDIF
          ;Compute GDD of ecmwf data from first day of simulation (may not be the one in sim if useEddyMet EQ1)
          indFin = WHERE(FINITE(tJD))
          indJD0_ecmwf = WHERE(tJD[indFin] EQ JD0)
          indJD0_ecmwf = indFin[indJD0_ecmwf]
          ta_ecmwf = ecmwf_tair[indJD0_ecmwf:*]
          ecmwf_GDD = computedGDD(ecmwf_tair[indJD0_ecmwf:*], Tb, /CUMULATIVE)
          ;now GDD can be shorter than the other vectors
          IF (JD0-tJD[0] NE 0 ) THEN ecmwf_GDD = [FLTARR(JD0-tJD[0]),ecmwf_GDD]
          indJD0_ecmwf = 0

          IF (indJD0_eddy NE -1) THEN BEGIN
            indJD0_eddy = indFin[indJD0_eddy]
            ta_eddy = ec_tair[indJD0_eddy:*]
            ec_GDD = computedGDD(ec_tair[indJD0_eddy:*], Tb, /CUMULATIVE)
            ;store it an array with ecJD dimensions
            IF (ec_JD[indJD0_eddy]-ec_JD[0] NE 0 ) THEN ec_GDD = [FLTARR(ec_JD[indJD0_eddy]-ec_JD[0])*!VALUES.F_NAN,ec_GDD]
            INDJD0_eddy = 0
          ENDIF ELSE BEGIN
            ec_GDD = FLTARR(N_ELEMENTS(sim.gdd))*!VALUES.F_NAN
            INDJD0_eddy = 0
            ;ther are no eddy data, make a vector of NaN
          ENDELSE
          ;plot_inversion_results, doPlotSpectra, wlim.eps_onoff, satR1, satR2, satR3, satR4, satR5, satR6, satR7, satObsJD, $
          ;        plot_inversion_results2, doPlotSpectra, satR1, satR2, satR3, satR4, satR5, satR6, satR7, satObsJD, $
          ;                                    tJD, indJD0_ecmwf, ec_JD, INDJD0_eddy, ecmwf_globrad, ec_globrad, ecmwf_tair, ec_tair, $
          ;                                    ecmwf_prec, ec_prec, sim, ec_GDD, ecmwf_GDD, ec_gpp_mds, ret, sYear, $
          ;                                    prosailvar, useEddyMet, wlim.tJD, ratio, out_dir, base_name, $
          ;                                    RefGppAvailable, RefGpp_data, globrad2par, eps_max, wlim.eps_onoff, runReportFn, ini_fn
          seasResmplSim = plot_inversion_results3(buffer_site_year_plot, doPlotSpectra, satR1, satR2, satR3, satR4, satR5, satR6, satR7, satObsJD, $
            tJD, indJD0_ecmwf, ec_JD, INDJD0_eddy, ecmwf_globrad, ec_globrad, ecmwf_tair, ec_tair, $
            ecmwf_prec, ec_prec, sim, ec_GDD, ecmwf_GDD, ec_gpp_mds, ret, sYear, $
            prosailvar, useEddyMet, wlim.tJD, ratio, out_dir, base_name, $
            RefGppAvailable, RefGpp_data, globrad2par, eps_max, wlim.eps_onoff, runReportFn, ini_fn)

          ;look for math errors
          IF CHECK_MATH() NE 0 THEN BEGIN
            PRINT, 'Math error occurred before here'
            STOP
          ENDIF
          siteResmplSimGpp = [siteResmplSimGpp, seasResmplSim.gpp]
          siteResmplSimJD = [siteResmplSimJD, seasResmplSim.tJD]
          siteSimNDVIJD = [siteSimNDVIJD, tJD]
          tmp = REFORM(((sim.ModRef[1,*]-sim.ModRef[0,*])/(sim.ModRef[1,*]+sim.ModRef[0,*])))
          siteSimNDVI = [siteSimNDVI, tmp]
          IF ((N_ELEMENTS(siteResmplSimGpp)) NE (N_ELEMENTS(siteResmplSimJD))) THEN STOP
          run_c = run_c + 1
        ENDIF
        ;it happens that the following rule
        ;IF (TOTAL(FINITE(ec_gpp_mds)) GE 1) AND (tJD[0] GT JULDAY(02, 02, 2001)) THEN BEGIN
        ;skipped the season because the simulation period starts befor MODIS
        ;in this case all the period tJD[0]- up to the next valid must be skipped
      ENDFOR ;s on season 1 and 2 (if any)
    ENDFOR ;y on number of years of a given site
    ; here we finished the site, look at a fair comparsion betwen EC data, MODIS and sim
    ; simulated data may be overlapping (the same tine period may be represented in two subsequent simulations. Take the mean for now
    ; it may happen that a site (fr-Mau as of Josh version in March 2017) has all NAN observations, manage the case
    IF ISA(siteResmplSimJD) THEN BEGIN
      dup = duplicateElementsInArray(siteResmplSimJD)
      ;for these duplucated elements take the avg
      IF (dup[0] NE -1) THEN BEGIN
        ;make arrays without duplications
        tmpJD = siteResmplSimJD[UNIQ(siteResmplSimJD, SORT(siteResmplSimJD))]
        tmpGpp = siteResmplSimGpp[UNIQ(siteResmplSimJD, SORT(siteResmplSimJD))]
        ;tmpNDVI = siteResmplSimNDVI[UNIQ(siteResmplSimJD, SORT(siteResmplSimJD))]
        FOR d = 0, N_ELEMENTS(dup)-1 DO BEGIN
          ;take the avg of gpp corresponding to value dup[d]
          ;locate the values
          ind = WHERE(siteResmplSimJD EQ dup[d], count) ;two elements must be found
          If (count NE 2) THEN STOP
          ;locate where the avg has to be placed in th enew array without duplications
          indTmp = WHERE(tmpJD EQ dup[d], count)
          If (count NE 1) THEN STOP
          tmpGpp[indTmp] = MEAN(siteResmplSimGpp[ind], /NAN)
          ;tmpNDVI[indTmp] = MEAN(siteResmplSimNDVI[ind], /NAN)
        ENDFOR
        siteResmplSimGpp = tmpGpp
        siteResmplSimJD = tmpJD
      ENDIF
      ; first make a 8 day composite of all eddy data
      resample2MODIS8days, eddy_data.gpp_mds, eddy_data.JD, siteEc_gpp, siteEc_JD
      ;here ignore data before firstJD2consider that it is the first goog period (with Eddy GPP and MODIS)
      indFirst = WHERE(siteEc_JD GE firstJD2consider, count)
      siteEc_gpp = siteEc_gpp[indFirst[0]:-1]
      siteEc_JD = siteEc_JD[indFirst[0]:-1]
      ;now set siteResmplSimGpp to 0 when Ec data exits but sim was not computed
      ;start with a all-zeros vector
      siteResmplSimGpp_offSeason = siteEc_gpp * 0.0
      ;fill it with sim values, where available
      match, siteEc_JD, siteResmplSimJD, sub_Ec_JD, subSimJD
      siteResmplSimGpp_offSeason[sub_Ec_JD] = siteResmplSimGpp[subSimJD]
      ;now get the available gpp estimates form MODIS
      siteModisGpp = siteEc_gpp * !VALUES.F_NAN
      match, siteEc_JD, RefGpp_data.JD, sub_Ec_JD, subModisJD
      siteModisGpp[sub_Ec_JD] = RefGpp_data.Gpp_daily[subModisJD]

      ;now for NDVI

      siteModisNDVI = (modis_data.R2 - modis_data.R1) / (modis_data.R2 + modis_data.R1)
      NDVIobsJD = modis_data.JD

      ;now some plots and than regression
      ret = plotSiteResults(pheno.lombrat, siteEc_JD, siteSimNDVIJD, NDVIobsJD, siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp, siteModisNDVI,  siteSimNDVI, site_info.igbp[t], site_info.site_code[t], site_run_reportFn, out_dir)
      ;store the data for the multi year multi site analysis
      multiSiteYearSimGpp = [multiSiteYearSimGpp, siteResmplSimGpp_offSeason]
      multiSiteYearEcGpp = [multiSiteYearEcGpp, siteEc_gpp]
      multiSiteYearModisGPP = [multiSiteYearModisGPP, siteModisGpp]
      multiSiteYearSiteCode = [multiSiteYearSiteCode, STRARR(N_ELEMENTS(siteEc_gpp)) + site_info.site_code[t]]
      multiSiteYearSiteId = [multiSiteYearSiteId, INTARR(N_ELEMENTS(siteEc_gpp)) + t]
      multiSiteYearSiteIGBP = [multiSiteYearSiteIGBP, STRARR(N_ELEMENTS(siteEc_gpp)) + site_info.igbp[t]]
      ;look for math errors
      IF CHECK_MATH() NE 0 THEN BEGIN
        PRINT, 'Math error occurred before here'
        STOP
      ENDIF
    ENDIF ;ISA(siteResmplSimJD)
  ENDFOR  ;t on n_sites
  ;here we finished all sites, plot scatterplots for all
  IF ISA(multiSiteYearSimGpp) THEN $
    ret = plotAll(multiSiteYearSimGpp, multiSiteYearEcGpp, multiSiteYearModisGPP, multiSiteYearSiteCode, multiSiteYearSiteId, multiSiteYearSiteIGBP, overall_run_reportFn, out_dir)
  FREE_LUN, lunLog
  CLOSE, /ALL
  PRINT, 'Finished'
END



;      IF (P1_sen_model EQ 'Logistic') THEN BEGIN
;        d = a ;set the beginning of senescence at the  same starting point
;        b = extract_timerange(ecmwf_data.JD, sJD, eJD, ecmwf_data.tav)
;        b = computedGDD (b, Tb)
;        ;c is the initial value for P1 which is not modulated here
;        fg[6] = d & fg[8] = b
;        ;limits compute with DOY0 limits and mean tempearture
;        ll = mt * FIX(values[3])
;        ul = mt * FIX(values[4])
;        parinfo[6].LIMITS = [MAX([a + ll, 0]), a + ul]
;        ;parinfo[7].LIMITS = [MAX([a + ll, 0]), a + ul]
;        parinfo[8].LIMITS = [b + ll, b + ul]
;        PRINT, 'Logistic used for P1 and sen'
;        PRINT, 'a fg = ' + STRTRIM(a,2) &  PRINT, 'a limits = ' + STRTRIM(parinfo[7].LIMITS,2)
;        PRINT, 'd fg = ' + STRTRIM(d,2) &  PRINT, 'd limits = ' + STRTRIM(parinfo[6].LIMITS,2)
;        PRINT, 'b fg = ' + STRTRIM(b,2) &  PRINT, 'b limits = ' + STRTRIM(parinfo[8].LIMITS,2)
;      ENDIF