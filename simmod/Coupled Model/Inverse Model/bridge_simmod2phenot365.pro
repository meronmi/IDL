FUNCTION bridge_SimMod2Phenot365, modis_data, force_mono
  @cb_job.comm
  @cb_in.comm
  @cb_stat.comm
  @cb_frst.comm
  @cb_pdhtf.comm
  @cb_options.comm
  @cb_optimization.comm

  ;variable to store the results
  pheno = CREATE_STRUCT('ngspy', 0, 'lombrat', 0.0, 'avg_sos', FLTARR(2), 'sd_sos', FLTARR(2),'avg_eos', FLTARR(2), $
    'sd_eos', FLTARR(2),'avg_tom', FLTARR(2), 'sd_tom', FLTARR(2), 'log_text', '')

  ;make the necessary adjstments to call phenot on MODIS NDVI used by the simple mod
  ;plot (0 to suppress)
  pltres  = 0  ;this is for pheno plot and to make the plot of original 8 day NDVI from MODIS and 10 day resampled for Pheno365
  newline = string([13B, 10B])
  ;declare some common block stuff and other housekeeping stuff
  ;platform = !version.os_family

  faprangeminthresh = 0.05      ;expected minimum variability value
  use_lomb = 1                ;use_lomb
  IF (force_mono EQ 1) THEN lombRatThreshold = 0.0 ELSE lombRatThreshold = 6.0     ;Above this ratio of peak1/peak2 we assume ngspy = 1, below it's ngspy = 2
  upenv = 1       ;0 = Normal model fit, 1 = Upper envelope fit
  period = 10             ;for decadal analysis, the only possible now
  ibel = 0.0      ;ignore below
  iabo = 1.0    ;ignore above
  fract_thresh = 0.05     ;fraction of amplitued use to compute start and stop in pdhtf (see White et al., 2009)
  fract_thresh2 = 0.99    ;fraction of amplitued use to compute the second stop in pdhtf

  ;Some adaptation
  ;v2 it may happen that the first or the last data point are missing (not that in thic case JD is NaN)
  indFin = WHERE(FINITE(modis_data.JD))

  ;first = JD2DDMMYYYY(modis_data.JD[0])
  first = JD2DDMMYYYY(modis_data.JD[indFin[0]])
  fst_dek = REFORM(ddmm2dek(first[0],first[1])) ;used by phenot
  fst_dek = fst_dek[0]
  fst_day = 1                           ;used by phenot
  fst_month = REFORM(first[1])                  ;used by phenot
  fst_month = fst_month[0]
  first_year =REFORM(first[2])                  ;used by phenot
  first_year = first_year[0]
  fst_year = first_year
  ;last = JD2DDMMYYYY(modis_data.JD[-1])
  last = JD2DDMMYYYY(modis_data.JD[indFin[-1]])
  lst_dek = REFORM(ddmm2dek(last[0],last[1]))
  last_year = REFORM(last[2])                   ;used by phenot
  last_year = last_year[0]
  lst_year = last_year
  julian1_f=float(floor(JULDAY(fst_month, fst_day, first_year, 13, 0, 0))) ;first dekad is 9810

  ;now build the vector of JD that will be used to interpolate
  exitCond = 0
  t = fst_dek
  y = first_year
  doy = DekYear2centralDOY(t[-1], y[-1])
  jd = DOY_YEAR2JD(doy[-1], y[-1])
  WHILE exitCond EQ 0 DO BEGIN
    IF (t[-1]+1) LE 36 THEN BEGIN
      t = [t, t[-1]+1]
      y = [y, y[-1]]
      doy = [doy, DekYear2centralDOY(t[-1], y[-1])]
      jd = [jd, DOY_YEAR2JD(doy[-1], y[-1])]
    ENDIF ELSE BEGIN
      t = [t, 1]
      y = [y, y[-1]+1]
      doy = [doy, DekYear2centralDOY(t[-1], y[-1])]
      jd = [jd, DOY_YEAR2JD(doy[-1], y[-1])]
    ENDELSE
    IF (t[-1] EQ lst_dek) AND (y[-1] EQ last_year) THEN exitCond = 1
  ENDWHILE
  ndvi = (modis_data.R2[indFin]-modis_data.R1[indFin])/(modis_data.R2[indFin]+modis_data.R1[indFin])
  ;it may happen that the last jd is > last  modis_data.JD[indFin], resulting in a math error
  ;;same for thr first
  ;check and fix
  indM = WHERE(jd GT MAX(modis_data.JD[indFin]), countM)
  IF (countM GT 0) THEN jd = jd[0:indM[0]-1]
  indM = WHERE(jd LT MIN(modis_data.JD[indFin]), countM)
  IF (countM GT 0) THEN jd = jd[indM[-1]+1:-1]
  ;in addition, two modis observation may have the same jd that is not treatable by INTERPOL
  jdModis = modis_data.JD[indFin]
  ;results of the following two lines depend on order, take the mean?
;  uniqueModisJd = jdModis[UNIQ(jdModis, SORT(jdModis))]
;  uniqueModisNdvi = ndvi[UNIQ(jdModis, SORT(jdModis))]

  ;find the list of the unique JDs 
  uniqueModisJd = jdModis[UNIQ(jdModis, SORT(jdModis))]
  ;for those JDs that have more than one record, averge ndvi
  uniqueModisNdvi = FLTARR(N_ELEMENTS(uniqueModisJd))
  FOR i = 0, N_ELEMENTS(uniqueModisJd)-1 DO BEGIN
    ind = WHERE(jdModis EQ uniqueModisJd[i], count)
;    IF (count GT 1) THEN BEGIN
;      print, ndvi[ind]
;    ENDIF
    uniqueModisNdvi[i] = MEAN(ndvi[ind])
  ENDFOR
  ;uniqueModisNdvi = ndvi[UNIQ(jdModis, SORT(jdModis))]
  ndvi_dek = INTERPOL(uniqueModisNdvi, uniqueModisJd, jd, /NAN)
  ;ndvi_dek = INTERPOL(ndvi, modis_data.JD[indFin], jd, /NAN)
  IF (pltres) THEN BEGIN
    dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
    h = PLOT(modis_data.JD[indFin], ndvi, SYMBOL = 1, LINESTYLE=' ', AXIS_STYLE = 1, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))')
    h = PLOT(jd, ndvi_dek, color = 'red', SYMBOL = 1, LINESTYLE=' ',OVERPLOT=1)
  ENDIF
  ;now copy from pheno_handler
  ;julian1_f=float(floor(JULDAY(fst_month, fst_day, first_year, 13, 0, 0))) ;first dekad is 9810
  npts = N_ELEMENTS(ndvi_dek)
  res=compute_j1_j2(julian1_f, npts)
  julian1=res.julian1
  julian2=res.julian2
  julianmid=(julian1+julian2)/2.0
  periods = indgen(npts)
  vrbs=0
  wantplotlomb=0
  fst_dec=fst_dek
  n_full_calendar_years=floor(npts/36.0)
  IF ((npts mod 36) ne 0.0) then $
    n_calendar_years=n_full_calendar_years+1 $
  ELSE n_calendar_years=n_full_calendar_years
  nyears=n_calendar_years
  ltdr_check = 0
  rc = phenot_funct(REFORM(ndvi_dek), REFORM(jd))
  tmp = '#################################################################'
  PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
  tmp = 'Info from pheno'
  PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
  IF (use_lomb EQ 1) THEN tmp = 'Lombrat cycle 1 / cycle 2 = ' + STRTRIM(lombrat, 2)
  PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
  pheno.lombrat = lombrat
  IF (rc EQ 0) OR (rc eq 1) THEN BEGIN
    tmp = 'Number of growing season per year: ' + STRTRIM(ngspy,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    cs=0  ;current season
    tmp = 'Season completeness, GS1:'
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
    PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    tmp = 'GSFLAG1:'
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = REFORM(gsflag[cs,*])
    PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    tmp = 'SOS:'
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = REFORM(JD2DOY(gsstrt[cs,*]))
    PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    tmp = 'EOS:'
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = REFORM(JD2DOY(gsstop[cs,*]))
    PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    tmp = 'TOM:'
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = REFORM(JD2DOY(gsmaxt[cs,*]))
    PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    IF (ngspy eq 2) then begin  ;GS2 may be present or not
      cs=1  ;current season
      tmp = 'Season completeness, GS2:'
      PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
      tmp = season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
      tmp = 'GSFLAG2:'
      PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
      tmp = REFORM(gsflag[cs,*])
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
      tmp = 'SOS:'
      PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
      tmp = REFORM(JD2DOY(gsstrt[cs,*]))
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
      tmp = 'EOS:'
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
      tmp =  REFORM(JD2DOY(gsstop[cs,*]))
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
      tmp =  'TOM:'
      PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
      tmp =  REFORM(JD2DOY(gsmaxt[cs,*]))
      PRINT, tmp & pheno.log_text = pheno.log_text +  STRJOIN(STRING(REFORM(tmp)),' ') + newline
    ENDIF
  ENDIF
  tmp = '#################################################################'
  PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
  ;compute the mean SOS, EOS and TOM
  pheno.ngspy = ngspy
  FOR i = 0, ngspy-1 DO BEGIN
    ;For the first season
    ;rescale the data 0 to 365 between 0 360
    iind = WHERE(gsstrt[i,*] EQ -999, countt)
    sos = REFORM(JD2DOY(gsstrt[i,*]))
    IF (countt GT 0) THEN sos[iind] = !VALUES.F_NAN
    ind = WHERE(FINITE(sos))
    sos = sos[ind]
    sos = (sos/365.0)*360.0
    ;compte avg only with full seasons
    seas_comp = season_completeness(i, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
    seas_comp = seas_comp[ind]
    indComp = WHERE(seas_comp GT 95.0)
    ;compute the mean angle and scale it back to 0-365
    avg_sos = ROUND((mean_vec(sos[indComp])/360.0)*365)
    sd_sos = (sd_vec(sos[indComp])/360.0)*365
    tmp =  'Average SOS of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(avg_sos,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = 'SD SOS of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(sd_sos,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    iind = WHERE(gsstop[i,*] EQ -999, countt)
    eos = REFORM(JD2DOY(gsstop[i,*]))
    IF (countt GT 0) THEN eos[iind] = !VALUES.F_NAN
    ind = WHERE(FINITE(eos))
    eos = eos[ind]
    eos = (eos/365.0)*360.0
    seas_comp = season_completeness(i, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
    seas_comp = seas_comp[ind]
    indComp = WHERE(seas_comp GT 95.0)
    ;compute the mean angle and scale it back to 0-365
    avg_eos = ROUND((mean_vec(eos[indComp])/360.0)*365)
    sd_eos = (sd_vec(eos[indComp])/360.0)*365
    tmp = 'Average EOS of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(avg_eos,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = 'SD EOS of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(sd_eos,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline

    iind = WHERE(gsmaxt[i,*] EQ -999, countt)
    tom = REFORM(JD2DOY(gsmaxt[i,*]))
    IF (countt GT 0) THEN tom[iind] = !VALUES.F_NAN
    ind = WHERE(FINITE(tom))
    tom = tom[ind]
    seas_comp = season_completeness(i, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
    seas_comp = seas_comp[ind]
    indComp = WHERE(seas_comp GT 95.0)
    tom = (tom/365.0)*360.0
    ;compute the mean angle and scale it back to 0-365
    avg_tom = ROUND((mean_vec(tom[indComp])/360.0)*365)
    sd_tom = (sd_vec(tom[indComp])/360.0)*365
    tmp = 'Average TOM of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(avg_tom,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline
    tmp = 'SD TOM of season ' + STRTRIM(i+1,2) + ': ' + STRTRIM(sd_tom,2)
    PRINT, tmp & pheno.log_text = pheno.log_text + tmp + newline

    pheno.avg_sos[i] = avg_sos
    pheno.sd_sos[i] = sd_sos
    pheno.avg_eos[i] = avg_eos
    pheno.sd_eos[i] = sd_eos
    pheno.avg_tom[i] = avg_tom
    pheno.sd_tom[i] = sd_tom
  ENDFOR
  RETURN, pheno
END