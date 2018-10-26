PRO plot_inversion_results, doPlotSpectra, $
  satR1, satR2, satR3, satR4, satR5, satR6, satR7, satObsJD, $
  tJDo, indJD0_ecmwf, ec_JD, INDJD0_eddy, $
  ecmwf_globrad, ec_globrad, ecmwf_tair, ec_tair, $
  ecmwf_prec, ec_prec, $
  simo, ec_GDD, ecmwf_GDD, ec_gpp_mds, ret, sYear, prosailvar, $
  useEddyMet, tJD_of_ratio_bewm_LTAbewm, ratio_bewm_LTAbewm, out_dir, base_name, $
  RefGppAvailable, RefGpp_data, globrad2par, eps_max, eps_wlimOnOff, runReportFn, ini_fn

  ;CATCH, Error_status
  ;
  ;;This statement begins the error handler:
  ;IF Error_status NE 0 THEN BEGIN
  ;  PRINT, 'Error index: ', Error_status
  ;  PRINT, 'Error message: ', !ERROR_STATE.MSG
  ;  ; Handle the error by extending A:
  ;  A=FLTARR(12)
  ;  ;STOP
  ;  CATCH, /CANCEL
  ;ENDIF

  ;align x axisi of ratio
  ind = WHERE(tJD_of_ratio_bewm_LTAbewm EQ tJDo[0])
  ratio_bewm_LTAbewm = ratio_bewm_LTAbewm[ind:ind+N_ELEMENTS(tJDo)-1]

  ;resample at frequency
  ;modis - > get the acquisition day
  ;globrad, gdd, lai, fapar, & Co -> mean over the priod
  ;precipitation -> mean, becaus with sum what with last 8 day comp that is not 8 day?
  resample = 1

  IF (resample) THEN BEGIN
    days = 8
    tComp = JD2ComIndex(tJDo, days)
    ;mean resampling
    tJD = meanResampleByComp(tJDo, tComp)
    n = N_ELEMENTS(tJD)
    sim = { $
      fa:             FLTARR(n)*0.0, $              ;fapar
      lai:            FLTARR(n)*0.0, $              ;lai
      dlai:           FLTARR(n)*0.0 , $             ;daily increment in LAI
      dlai_senescent: FLTARR(n)*0.0 , $             ;daily reduction in LAI
      gpp:            FLTARR(n)*0.0, $              ;daily GPP value
      P1:             FLTARR(n)*0.0, $              ;patitioning into leaves
      eps_s:          FLTARR(n)*0.0, $              ;eps stress
      ModRef:         FLTARR(7, N_ELEMENTS(tJDo))*!VALUES.F_NAN, $ ;simulated MODIS reflectances
      NRTtJDOfMax:    0.0}   
    ecmwf_globrado = ecmwf_globrad
    ecmwf_globrad = meanResampleByComp(ecmwf_globrad, tComp)
    ecmwf_prec = meanResampleByComp(ecmwf_prec, tComp)
    ecmwf_gdd = meanResampleByComp(ecmwf_gdd, tComp)
    ratio_bewm_LTAbewm = meanResampleByComp(ratio_bewm_LTAbewm, tComp)
    ecmwf_tair = meanResampleByComp(ecmwf_tair, tComp)
    ;sim
    sim.fa = meanResampleByComp(simo.fa, tComp)
    sim.lai = meanResampleByComp(simo.lai, tComp)
    sim.dlai = meanResampleByComp(simo.dlai, tComp)
    sim.dlai_senescent = meanResampleByComp(simo.dlai_senescent, tComp)
    sim.gpp = meanResampleByComp(simo.gpp, tComp)
    sim.P1 = meanResampleByComp(simo.P1, tComp)
    sim.eps_s = meanResampleByComp(simo.eps_s, tComp)
    ;simultaed reflectances are never resampled
    sim.ModRef = simo.ModRef
    sim.NRTtJDOfMax = simo.NRTtJDOfMax
    IF ((indJD0_ecmwf NE 0) OR (indJD0_eddy NE 0)) THEN STOP ;in this case we would have a problem with the corresponding graph
    tComp = JD2ComIndex(ec_JD, days)
    ec_JD = meanResampleByComp(ec_JD, tComp)
    ec_globrado = ec_globrad
    ec_globrad = meanResampleByComp(ec_globrad, tComp)
    ec_prec = meanResampleByComp(ec_prec, tComp)
    ec_tair = meanResampleByComp(ec_tair, tComp)
    ec_GDD = meanResampleByComp(ec_GDD, tComp)
    ec_gpp_mds = meanResampleByComp(ec_gpp_mds, tComp)
  ENDIF ELSE BEGIN
    sim = simo
  ENDELSE

  ;PLOTS
  ;compute ndvi to locate max used in the graphics
  val = MAX((satR2-satR1)/(satR2+satR1), ind_max_ndvi_obs, /NAN)
  ;
  ;common settings
  dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
  fs = 8 ;font size
  lp = [1.2, 1] ;legend position
  dm = [1000,900]  ;dimensions of the window
  xminor = 2        ;monor ticks between majors
  nmajor = 4
  ticlensec = 0.03
  ;base_layout = [3,3]
  xrange = [MIN(tJD), MAX(tJD)]

  ; y is divided in 0, 0.3, 0.6 so to leave some space above
  ; x is in 0, 0.33, .66
  ;       0.0  0.6   0.33  0.9
  ; Position of the upper left graph, all others graphs are positioned base on this
  ;       xmin  ymin  xmax  ymax

  xmin = 0.05
  ymin = 0.66 ;(0.605)
  graph_dim = 0.24

  pos0 = [xmin, ymin,xmin + graph_dim, ymin+graph_dim] ;0.24 in width and height

  ;Radiation, ECMWF
  pos = pos0
  yrange =[0,MAX([ecmwf_globrad, ec_globrad], /NAN)+2.0]
  gh_radg_ecmwf = PLOT (tJD, ecmwf_globrad, YTITLE='ecmwf_globrad (MJ m-2)', COLOR = 'black', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, XRANGE=xrange, Name='ECMWF', YRANGE=yrange, WINDOW_TITLE = base_name)
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  loc = -(yrange[1]-yrange[0])/10
  a_x =AXIS('X', TARGET = gh_radg_ecmwf, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;Radiation, Eddy flux data
  gh_radg_eddy = PLOT (ec_JD, ec_globrad, COLOR = 'red', OVERPLOT = 1, Name='Eddy',  YRANGE=yrange)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1, YRANGE=yrange)
  !null = LEGEND(target=[gh_radg_ecmwf, gh_radg_eddy], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;Precipitation, ECMWF
  pos = pos0
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange =[0,MAX([ecmwf_prec, ec_prec], /NAN)+2.0]
  gh_rain_ecmwf = PLOT (tJD, ecmwf_prec, YTITLE='Precipitation (mm)',  COLOR = 'black', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, XRANGE=xrange, Name='ECMWF', YRANGE=yrange);, THICK=2)
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  loc = -(yrange[1]-yrange[0])/10
  a_x =AXIS('X', TARGET = gh_rain_ecmwf, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;Precipitation, Eddy flux data
  gh_rain_eddy = PLOT (ec_JD, ec_prec, COLOR = 'red', OVERPLOT = 1, Name='Eddy', YRANGE=yrange)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1);, YRANGE=yrange)
  !null = LEGEND(target=[gh_rain_ecmwf, gh_rain_eddy], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;Temperature, ECMWF
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange =[0,MAX([ecmwf_gdd, ec_GDD], /NAN)+2.0]
  ;IF (eps_wlimOnOff EQ 1) THEN axSt = 1 ELSE axSt = 2
  gh_gdd_ecmwf = PLOT(tJD[indJD0_ecmwf:*], ecmwf_gdd, YTITLE='GDD (°C)',  COLOR = 'black', AXIS_STYLE = 1, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, XRANGE=xrange, YRANGE=yrange, Name='GDD ECMWF')
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_gdd_ecmwf, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;Temperature, Eddy flux data
  ;gh_gdd_eddy = PLOT (ec_JD[indJD0_eddy:*], ec_GDD, COLOR = 'red', OVERPLOT = 1, Name='GDD Eddy',  AXIS_STYLE = axSt)
  gh_gdd_eddy = PLOT (ec_JD[indJD0_eddy:*], ec_GDD, COLOR = 'red', OVERPLOT = 1, Name='GDD Eddy',  AXIS_STYLE = axSt)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1,  AXIS_STYLE = axSt);, YRANGE=yrange)
  a_y = AXIS('x', TARGET = gh_gdd_ecmwf, LOCATION = "top",GRIDSTYLE="none", MAJOR=0, MINOR=0)
  yrange =[MIN([ecmwf_tair, ec_tair], /NAN)-3.0,MAX([ecmwf_tair, ec_tair], /NAN)+3.0]
  gh_t_ecmwf  = PLOT(tJD[indJD0_ecmwf:*], ecmwf_tair,  COLOR = 'black', NAME = 'T ECMWF', AXIS_STYLE = 0, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE=':', $
    FONT_SIZE = fs, POSITION=pos, YRANGE = yrange, XRANGE=xrange, /CURRENT)
  gh_t_eddy  = PLOT(ec_JD[indJD0_eddy:*], ec_tair,  COLOR = 'red', NAME = 'T Eddy', AXIS_STYLE = 0, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', LINESTYLE=':', $
    FONT_SIZE = fs, POSITION=pos, YRANGE = yrange, XRANGE=xrange, /CURRENT)
  gh_tmp = PLOT(xrange, [ret.info_mpfit.parms[9], ret.info_mpfit.parms[9]], color = 'g', OVERPLOT=1)
  a_t = AXIS('y', TARGET = gh_t_ecmwf, LOCATION = [max(gh_t_ecmwf.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'T (°C)', YRANGE=yrange, TICKDIR=1)
  null = LEGEND(target=[gh_gdd_ecmwf, gh_gdd_eddy, gh_t_ecmwf, gh_t_eddy], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;LUE
  pos = pos0
  pos[[1,3]]= pos[[1,3]] - 0.3
  ;IF (eps_wlimOnOff EQ 1) THEN axSt = 1 ELSE axSt = 2
  ;AXIS_STYLE = 1, $
  ;first plot what the actual LUE would be using eddy data (and fapar simulation)
  apar = ec_globrad*globrad2par*sim.fa
  ;this can be inf when fapar is 0, set to NaN
  ind = WHERE(apar EQ 0, count)
  IF (count GT 0) THEN apar[ind]=!VALUES.F_NAN
  yrange = [0.0, MIN([MAX(apar, /NAN ) + 1.0, 4])]
  actEps_s_sim_fAPAR =  ec_gpp_mds/apar/eps_max
  gh_actLUE_ecmwf = PLOT (tJD, actEps_s_sim_fAPAR,  COLOR = 'red', $;AXIS_STYLE = 0, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, XRANGE=xrange, YRANGE=yrange, Name='actEps_s_sim_fAPAR', YTITLE='Eps_s')
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  x =AXIS('X', TARGET = gh_actLUE_ecmwf, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;check that I can recompute the used Eps_s
  ;use the daily resolution (small differences may be present after resampling)
  IF (useEddyMet) THEN BEGIN
    apar_s = ec_globrado*globrad2par*simo.fa
  ENDIF ELSE BEGIN
    apar_s = ecmwf_globrado*globrad2par*simo.fa
  ENDELSE
  ind = WHERE((apar_s EQ 0) OR (simo.gpp EQ 0), count)
  IF (count GT 0) THEN apar_s[ind]=!VALUES.F_NAN
  tmp = simo.gpp/apar_s/eps_max
  ind = WHERE(FINITE(tmp),count)
  maxdiff = MAX(ABS(tmp[ind]-simo.eps_s[ind])) ;ec_globrad
  IF (maxdiff GT 0.1) THEN STOP
  ;eps_s from wlim
  gh_bewm_ecmwf = PLOT (tJD, sim.eps_s,  COLOR = 'blue', OVERPLOT=1, Name='simEps_s')
  gh_ratio = PLOT (tJD, ratio_bewm_LTAbewm,  COLOR = 'violet', OVERPLOT=1, Name='ratio_bewm_bewm_et0')
  !null = LEGEND(target=[gh_actLUE_ecmwf, gh_bewm_ecmwf, gh_ratio], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [yrange[0],yrange[1]], OVERPLOT=1)

  ;LAI/FAPAR
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange =[0.0, MIN([MAX(sim.fa,/NAN) + (MAX(sim.fa,/NAN)-MIN(sim.fa,/NAN))/5.0, 1.0])]
  gh_fa  = PLOT (tJD, sim.fa,  YTITLE='fAPAR',  COLOR = 'black', AXIS_STYLE = 1,$
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, $
    NAME = 'FAPAR', XRANGE=xrange, YRANGE=yrange)
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_fa, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1, YRANGE=yrange, AXIS_STYLE = 1)
  ;plot also the nRT max used for kc computation
  gh_tmp = PLOT([sim.NRTtJDOfMax, sim.NRTtJDOfMax], yrange, COLOR='blue', OVERPLOT=1, YRANGE=yrange, AXIS_STYLE = 1)
  a_y = AXIS('x', TARGET = gh_fa, LOCATION = "top",GRIDSTYLE="none", MAJOR=0, MINOR=0)
  yrange =[0.0,MAX(sim.lai, /NAN)+(MAX(sim.lai, /NAN)-MIN(sim.lai, /NAN))/5.0]
  gh_lai  = PLOT (tJD, sim.lai,  COLOR = 'green', NAME = 'LAI', AXIS_STYLE = 0, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
    FONT_SIZE = fs, POSITION=pos, YRANGE = yrange, XRANGE=xrange, /CURRENT)
  a_lai = AXIS('y', TARGET = gh_lai, LOCATION = [max(gh_lai.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'LAI (m2 m-2)', YRANGE=yrange, TICKDIR=1)

  !null = LEGEND(target=[gh_fa, gh_lai], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;dLAI
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange =[0,MAX([sim.dlai,sim.dlai_senescent])+(MAX([sim.dlai,sim.dlai_senescent])-min([sim.dlai,sim.dlai_senescent]))/5.0]
  gh_dlai = PLOT (tJD, sim.dlai, YTITLE='dLAI, LAI_senescent (m2 m-2)',  COLOR = 'green', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 'newLAI', XRANGE=xrange, YRANGE=yrange, AXIS_STYLE = 1)
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_dlai, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  gh_senlai  = PLOT (tJD, sim.dlai_senescent,  COLOR = 'black', OVERPLOT=1, NAME = 'senLAI', AXIS_STYLE = 1)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1, $
    YRANGE=yrange, AXIS_STYLE = 1)
  a_y = AXIS('x', TARGET = gh_dlai, LOCATION = "top",GRIDSTYLE="none", MAJOR=0, MINOR=0)

  ;Partitioning
  gh_P1 = PLOT (tJD, sim.P1, COLOR = 'blue', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 'P1', YRANGE=[0,1.2], XRANGE=xrange, AXIS_STYLE = 0)
  a_P1 = AXIS('y', TARGET = gh_P1, LOCATION = [max(gh_dlai.xrange),0,0], TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'partitioning (into leaf)', YRANGE=[0,1.2])
  !null = LEGEND(target=[gh_dlai, gh_senlai, gh_P1], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;GPP
  ;GPP modeled
  pos = pos0
  pos[[1,3]]= pos[[1,3]] - 0.6
  IF (RefGppAvailable) THEN yrange =[-0.25,MAX([sim.gpp,ec_gpp_mds,RefGpp_data.Gpp_daily], /NAN)+0.1] $
  ELSE yrange =[-0.25,MAX([sim.gpp,ec_gpp_mds], /NAN)+0.1]
  gh_modgpp = PLOT (tJD, sim.gpp, YTITLE='GPP (gC m-2 d-1)',  COLOR = 'black', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 'simGPP', XRANGE=xrange, YRANGE=yrange)
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_modgpp, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;gh_cnpp  = PLOT (tJD, sim.cnpp,  COLOR = 'green', OVERPLOT=1, NAME = 'NPP')
  ;GPP meaured (Eddy data)
  gh_obsgpp = PLOT (ec_JD, ec_gpp_mds, OVERPLOT=1, COLOR = 'red', $
    NAME = 'obsGPP(mds)')
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1, YRANGE=yrange)
  ;plot GPP ref from MODIS if available
  IF (RefGppAvailable) THEN BEGIN
    gh_ref = PLOT(RefGpp_data.JD, RefGpp_data.Gpp_daily, OVERPLOT = 1, COLOR = 'green', $
      NAME = 'GPP_MOD15A2');, SYMBOL='+', SYM_SIZE = 0.5)
    !null = LEGEND(target=[gh_modgpp, gh_obsgpp, gh_ref], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION= [1, 1], /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)
  ENDIF ELSE BEGIN
    !null = LEGEND(target=[gh_modgpp, gh_obsgpp], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION= [1, 1], $ ;, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100, SAMPLE_WIDTH=0.1, VERTICAL_SPACING = 0.01)
  ENDELSE
  ;GPP scatterplot
  pos[[0,2]]= pos[[0,2]] + 0.33
  ec_gpp = ec_gpp_mds
  sim_gpp = sim.gpp[0:N_ELEMENTS(ec_gpp_mds)-1]
  ;set negative gpp to NaN
  indfin = WHERE(FINITE(ec_gpp_mds))
  indeg = WHERE(ec_gpp_mds[indfin] LT 0.0, nneg)
  IF (nneg GT 0) THEN ec_gpp_mds[indfin[indeg]] = !VALUES.F_NAN
  indfin = WHERE(FINITE(ec_gpp_mds))
  ;find where i smax NDVI
  indmax = WHERE(ec_JD GE satObsJD[ind_max_ndvi_obs])
  indmax = indmax[0]
  ;eddy data may be incomplete, e.g. no data after a give time
  yrange=[-0.5, MAX([ec_gpp_mds, sim_gpp], /NAN)+0.2]
  ;adjust case of eddy data ending before the max
  gh_scatterA = !NULL
  gh_scatterB = !NULL
  IF (indmax EQ -1) THEN BEGIN
    h_scatterB = PLOT(ec_gpp_mds, sim_gpp[0:N_ELEMENTS(ec_gpp_mds)-1], SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, $
      FONT_SIZE = fs, POSITION=pos, /CURRENT, XTITLE='EC GPP (>=0)', YTITLE='sim GPP')
  ENDIF ELSE BEGIN
    ;plot black until the ndvi max
    gh_scatterB = PLOT(ec_gpp_mds[0:indmax], sim_gpp[0:indmax], SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, $
      FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 't<=t_max_ndvi', XTITLE='EC GPP (>=0)', YTITLE='sim GPP')
    ;adust the case where ther are no eddy data after
    IF (indmax LT N_ELEMENTS(ec_gpp_mds)-1) THEN BEGIN
      ;and grey afterwards sim.gpp[0:N_ELEMENTS(ec_gpp_mds)-1]
      gh_scatterA = PLOT(ec_gpp_mds[indmax+1:*], sim_gpp[indmax+1:*], SYMBOL='+', LINESTYLE='none', $
        FONT_SIZE = fs, COLOR='grey', OVERPLOT=1, XRANGE=yrange, YRANGE=yrange, NAME = 't>t_max_ndvi')
    ENDIF
  ENDELSE


  gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
  GPP_simVSobs = {R2:0.0, slope:0.0, offset:0.0, percGPPdelta:0.0}
  b = REGRESS(ec_gpp_mds[indfin], sim_gpp[indfin], CONST = a, CORRELATION = r)
  GPP_simVSobs.slope = b & GPP_simVSobs.offset = a & GPP_simVSobs.R2 = r^2
  GPP_simVSobs.percGPPdelta = 100.0*(TOTAL(sim_gpp[indfin]) - TOTAL(ec_gpp_mds[indfin]))/TOTAL(ec_gpp_mds[indfin])

  ;trick to avoid plotting outside
  x0=(0.0-a)/b[0]
  x1=(yrange[1]-a)/b[0]
  IF (x0 LT 0) THEN x0=0.0
  yreg = a+b[0]*[x0,x1]
  ;gh_reg = PLOT(yrange, yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r^2,2), YRANGE=yrange)
  gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r^2,2), YRANGE=yrange)
  !null = LEGEND(target=[gh_scatterB, gh_scatterA], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=[0.92, 1.02], /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.03, TRANSPARENCY=100)
  tmp = STRING(r^2, FORMAT='(F4.2)', /PRINT)
  th1 = TEXT(pos[2]-0.1, pos[1]+0.03, '$R^2$='+tmp, COLOR='blue');, TARGET=gh_scatterB, /DATA)
  GPP_modisVSobs = {R2:0.0, slope:0.0, offset:0.0, percGPPdelta:0.0}
  IF (RefGppAvailable) THEN BEGIN
    ;here I have to extract the relevant data from the full MODIS time series
    modisGpp = extractModisVal(RefGpp_data.JD, RefGpp_data.Gpp_daily, tJD)
    b = REGRESS(ec_gpp_mds[indfin], modisGpp[indfin], CONST = a, CORRELATION = r)
    GPP_modisVSobs.slope = b & GPP_modisVSobs.offset = a & GPP_modisVSobs.R2 = r^2
    GPP_modisVSobs.percGPPdelta = 100.0*(TOTAL(modisGpp[indfin]) - TOTAL(ec_gpp_mds[indfin]))/TOTAL(ec_gpp_mds[indfin])
  ENDIF

  ;Add inversion info
  dlmtr = ', '
  resmp = ''
  IF (resample) THEN resmp = ', RESAMPLED TO 8 days'
  line0 = 'Status: ' + STRTRIM(FIX(ret.info_mpfit.Status),2) + dlmtr + 'BestChi2: ' + STRING(ret.info_mpfit.BestChi2, FORMAT ='(F9.7)') + $
    dlmtr + 'WgtSc: ' + ret.info_mpfit.WgtScheme + dlmtr + 'Fit2UpEnv: ' + STRTRIM(FIX(ret.info_mpfit.Fit2UpEnv), 2)
  ;if upperenv
  IF (FIX(ret.info_mpfit.Fit2UpEnv) EQ 1) THEN line0 = line0 + dlmtr + 'UpEnvItr: ' + STRTRIM(ret.info_mpfit.UpEnvItr, 2)
  IF (ret.info_mpfit.eps_wlimOnOff EQ 1) THEN line0 = line0 + dlmtr + 'W_Lim: ON' ELSE $
    line0 = line0 + dlmtr + 'W_Lim: OFF'
  line0 = line0 + dlmtr + 'nFree: ' + STRTRIM(N_ELEMENTS(ret.info_mpfit.pfree_index), 2)
  ;                        DOY0;JD0         lai0        SLA         e_max           gamma          c                 d          a            b           tb          cap        half      opt
  ;str_format =  '("------",A-13, "----",     A-7,"---", A-7, "-", A-6,"--",        A-7,"---",      A-7, "-------",  A-5, "----",A-8,"-----",A-7,"----", A-4, "---",A-6, "--", A-7,"-", A-7)'
  ;par_format_fg='("fg:--", I3,"--",F9.1,"--",F6.4,"--", F6.4,"--", F5.3,"----",     F4.2,"-------",F07.2,"----",   F5.3,"--", F07.2,"--",   F7.4,"--",  F4.2,"--",F05.1,"--", F06.1,"--", F04.2)'
  ;par_format_p ='("p :--", I3,"--",F9.1,"--",F6.4,"--", F6.4,"--", F5.3,"----",     F4.2,"-------",F07.2,"----",   F5.3,"--", F07.2,"--",   F7.4,"--",  F4.2,"--",F05.1,"--", F06.1,"--", F04.2)'
  str_format =  '("____",  A-13, "___",        A-7,"__",  A-7, "__", A-4,"___",         A-5,"___",    A-7, "___",    A-5, "___",  A-8,"___",        A-7,"__",  A-4, "___",A-6, "_", A-7,"__", A-7)'
  par_format_fg='("fg:__", I3,"__", F9.1,"__",F6.4,"__", F6.4,"___", F5.3,"____",     F4.2,"____",F07.2,"___",   F5.3,"__", F07.2,"___",   F7.4,"___",  F6.2,"__",  F05.1,"___", F06.1,"___", F05.2)'
  par_format_p ='("p :__", I3,"__", F9.1,"__",F6.4,"__", F6.4,"___", F5.3,"____",     F4.2,"____",F07.2,"___",   F5.3,"__", F07.2,"___",   F7.4,"___",  F6.2,"__",  F05.1,"___", F06.1,"___", F05.2)'
  tmp = ret.info_mpfit.parinfo[*].PARNAME
  ;tmp[0] = 'DOY0; '+tmp[0]
  tmp[0] = 'DOY0_'+tmp[0]
  tmp[ret.info_mpfit.pfree_index] = '['+ tmp[ret.info_mpfit.pfree_index] + ']'
  ;Check for free prameters pegged to limits
  IF (ret.info_mpfit.npegged GT 0) THEN BEGIN
    line0 = line0 +dlmtr + 'nFree peg to lmts (*L,*U): ' + STRTRIM(ret.info_mpfit.npegged,2)
    ll = ret.info_mpfit.parinfo[*].LIMITS[0]
    ul = ret.info_mpfit.parinfo[*].LIMITS[1]
    ;subscripts of parameters pegged to lower limit, add the info to the string
    ind_par_pegged_ll = WHERE((ret.info_mpfit.parms - ret.info_mpfit.parinfo[*].LIMITS[0]) EQ 0.0, count_par_pegged_ll)
    IF (count_par_pegged_ll GT 0) THEN  tmp[ind_par_pegged_ll] = tmp[ind_par_pegged_ll] + '*L'
    ind_par_pegged_ul = WHERE((ret.info_mpfit.parms - ret.info_mpfit.parinfo[*].LIMITS[1]) EQ 0.0, count_par_pegged_ul)
    IF (count_par_pegged_ul GT 0) THEN  tmp[ind_par_pegged_ul] = tmp[ind_par_pegged_ul] + '*U'
  ENDIF
  ;line1
  line1 = STRING(tmp, FORMAT=str_format, /PRINT)
  line1 = mg_streplace(line1, ' ', '_', /GLOBAL)
  ;remove last _ characthers
  p = STRPOS(line1, '_', /REVERSE_SEARCH)
  WHILE p EQ STRLEN(line1)-1 DO BEGIN
    line1 = STRMID(line1,0,p)
    p = STRPOS(line1, '_', /REVERSE_SEARCH)
  ENDWHILE
  ;line2 and line3
  tmp_fg  = ret.info_mpfit.fg
  tmp_parms  = ret.info_mpfit.parms
  IF (ret.info_mpfit.eps_wlimOnOff EQ 0) THEN BEGIN
    tmp_fg[10:12]=0.0
    tmp_parms[10:12]=0.0
  ENDIF
  line2 = STRING([JD2DOY(ret.info_mpfit.fg[0]),tmp_fg], FORMAT=par_format_fg, /PRINT)
  line3 = STRING([JD2DOY(ret.info_mpfit.parms[0]),tmp_parms], FORMAT=par_format_p, /PRINT)
  line0 = line0 + resmp
  IF (useEddyMet EQ 1) THEN tmp ='#use EC met#; ' ELSE tmp=''
  th0 = TEXT(0.01, 0.98, tmp + 'Year: '+ STRTRIM(sYear,2)+ '; '+ line0, FONT_SIZE = 10)
  th1 = TEXT(0.01, 0.96,line1)
  th2 = TEXT(0.01, 0.94, line2)
  th3 = TEXT(0.01, 0.92, line3)
  save_inversion_results, runReportFn, ini_fn, out_dir, base_name, sYear, useEddyMet, ret, GPP_simVSobs, GPP_modisVSobs, eps_wlimOnOff

  gh_radg_ecmwf.Save, out_dir + '\' + 'Model_' + base_name + '.png', BORDER=10, RESOLUTION=300

  ;*************************************************************************************
  ;*************************************************************************************
  ;*************************************************************************************
  ;add new windoe with reflectance graphs
  pos = pos0

  ;NDVI
  ;Simulated Modis NDVI
  b1_sim = REFORM(sim.ModRef[0,*])
  b2_sim = REFORM(sim.ModRef[1,*])
  ndvi_sim = ((b2_sim-b1_sim)/(b2_sim+b1_sim))
  yrange = [0.0,1.0]
  gh_sim_ndvi = PLOT (tJDo, ndvi_sim, YTITLE='MODIS NDVI',  COLOR = 'black', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, NAME = 'simNDVI', YRANGE=yrange, XRANGE=xrange, $
    DIMENSIONS=dm, WINDOW_TITLE = base_name + ' SPECTRAL')
  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_sim_ndvi, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  ;Observed MODIS NDVI
  ndvi_obs = ((satR2-satR1)/(satR2+satR1))
  gh_obs_ndvi = PLOT (satObsJD, ndvi_obs, COLOR = 'red', OVERPLOT = 1, $
    NAME = 'obsNDVI', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [0,1], OVERPLOT=1)
  legTarget = [gh_sim_ndvi, gh_obs_ndvi]
  !null = LEGEND(target=legTarget, /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

  ;Observed MODIS SIWSI (1 and 2) as in Fensholt & Sandholt 2003, 10.1016/j.rse.2003.07.002
  b6_sim = REFORM(sim.ModRef[5,*])
  b5_sim = REFORM(sim.ModRef[4,*])
  siswi_sim = -(b6_sim-b2_sim)/(b2_sim+b6_sim)
  ndwi_sim =  -(b5_sim-b2_sim)/(b2_sim+b5_sim)
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange = [-0.5,0.5]
  siswi_obs = -((satR6-satR2)/(satR6+satR2))
  gh_obs_siwsi = PLOT (satObsJD, siswi_obs, COLOR = 'blue', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    NAME = 'obs-SIWSI', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  POSITION=pos, $
    FONT_SIZE = fs, YRANGE=yrange, XRANGE=xrange, /CURRENT)
  loc = yrange[0] -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_obs_siwsi, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  gh_sim_siwsi = PLOT (tJDo, siswi_sim, COLOR = 'blue', OVERPLOT=1, $
    NAME = 'sim-SIWSI')
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [yrange[0],yrange[1]], OVERPLOT=1)
  ;NDVWI
  ndwi_obs = -((satR5-satR2)/(satR5+satR2))
  gh_obs_ndwi = PLOT (satObsJD, ndwi_obs, COLOR = 'green', $
    NAME = 'obs-NDWI', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  OVERPLOT=1)
  gh_sim_ndwi = PLOT (tJDo, ndwi_sim, COLOR = 'green', OVERPLOT=1, $
    NAME = 'sim-NDWI')

  legTarget = [gh_obs_siwsi, gh_sim_siwsi, gh_obs_ndwi, gh_sim_ndwi]
  !null = LEGEND(target=legTarget, /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, VERTICAL_SPACING = 0.01, TRANSPARENCY=100)


  ;Refletcances
  ;bands R1_645 R2_858.5  R3_469  R4_555  R5_1240 R6_1640 R7_2130
  ;pos[[0,2]]= pos[[0,2]] + 0.33
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange = [0, MAX(sim.ModRef[*,*]+0.1, /NAN)]
  sim_names =  's ' + STRTRIM(FLOOR(datacentwl_MODIST_7b()),2) ;['R1','R2','R3','R4','R5','R6','R7']
  obs_names =  'o ' + STRTRIM(FLOOR(datacentwl_MODIST_7b()),2)
  gh_sim1 = PLOT(tJDo, sim.ModRef[0,*], COLOR = 'red', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    FONT_SIZE = fs, POSITION=pos, MARGIN=margin, /CURRENT, NAME = sim_names[0], YRANGE=yrange, XRANGE=xrange, YSHOWTEXT=0)
  a_sim1 = AXIS('y', TARGET = gh_sim1, LOCATION = 'right', TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'R', YRANGE=yrange, TICKLAYOUT=1); [max(gh_sim1.xrange),0,0]




  loc = -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_sim1, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  gh_obs1 = PLOT (satObsJD, satR1, NAME= obs_names[0], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'red', OVERPLOT=1)

  gh_sim2 = PLOT(tJDo, sim.ModRef[1,*], COLOR = 'blue', OVERPLOT = 1, NAME = sim_names[1])
  gh_obs2 = PLOT (satObsJD, satR2, NAME= obs_names[1], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'blue', OVERPLOT=1)

  gh_sim3 = PLOT(tJDo, sim.ModRef[2,*], COLOR = 'black', OVERPLOT = 1, NAME = sim_names[2])
  gh_obs3 = PLOT (satObsJD, satR3, NAME= obs_names[2], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'black', OVERPLOT=1)

  gh_sim4 = PLOT(tJDo, sim.ModRef[3,*], COLOR = 'green', OVERPLOT = 1, NAME = sim_names[3])
  gh_obs4 = PLOT (satObsJD, satR4, NAME= obs_names[3], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'green', OVERPLOT=1)

  gh_sim5 = PLOT(tJDo, sim.ModRef[4,*], COLOR = 'grey', OVERPLOT = 1, NAME = sim_names[4])
  gh_obs5 = PLOT (satObsJD, satR5, NAME= obs_names[4], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'grey', OVERPLOT=1)

  gh_sim6 = PLOT(tJDo, sim.ModRef[5,*], COLOR = 'magenta', OVERPLOT = 1, NAME = sim_names[5])
  gh_obs6 = PLOT (satObsJD, satR6, NAME= obs_names[5], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'magenta', OVERPLOT=1)

  gh_sim7 = PLOT(tJDo, sim.ModRef[6,*], COLOR = 'orange', OVERPLOT = 1, NAME = sim_names[6])
  gh_obs7 = PLOT (satObsJD, satR7, NAME= obs_names[6], $
    LINESTYLE='none', SYMBOL='o', SYM_SIZE = 0.3, COLOR = 'orange', OVERPLOT=1)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], yrange, COLOR='grey', OVERPLOT=1, YRANGE=yrange)
  !null = LEGEND(target=[gh_sim1,gh_obs1,gh_sim2,gh_obs2,gh_sim3,gh_obs3,gh_sim4,gh_obs4,$
    gh_sim5,gh_obs5,gh_sim6,gh_obs6,gh_sim7,gh_obs7], /AUTO_TEXT_COLO, FONT_SIZE = fs, $
    POSITION=[0.30, 1.0], /RELATIVE, HORIZONTAL_SPACING = 0.01, VERTICAL_SPACING = 0.01, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.02, TRANSPARENCY=100)

  ;calculation of PRI (according to   Drolet, http://dx.doi.org/10.1016/j.rse.2005.07.006) is not straightforward
  ;and we know that resulting PRI is of doubtful usefullness, I skip it for the time being.
  ;Plot SWSI, NDWVI; all normalized by LAI
  ;plot residual of regression with NDVI and the others
  pos = pos0
  pos[[1,3]]= pos[[1,3]] - 0.3
  ;plot NDWI and SIWSI divided by LAI
  ;here I have to match satObsJD with sim JD
  ;note that if resample is required, all the vari except the reflectances are resample to 8-day
  ;the original simulations (daily step) are stored in simo
  satObsIndOfJDo = slow_directional_one2one_match(satObsJD, tJDo)
  ;some elements may be NaN, remove
  ind = WHERE(FINITE(satObsIndOfJDo))
  satObsIndOfJDo = satObsIndOfJDo[ind]
  ;avoid 0 division
  tmpLai = simo.lai[satObsIndOfJDo]
  ind = WHERE(tmpLai EQ 0, count)
  IF (count GT 0) THEN tmpLai[ind] = !VALUES.F_NAN
  tmp = siswi_obs/tmpLai
  yrange = [-3,2]
  gh_obs_siwsiLai = PLOT (satObsJD, tmp, COLOR = 'blue', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    NAME = 'obs-SIWSIonLAI_sim', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  POSITION=pos, $
    FONT_SIZE = fs, YRANGE=yrange, XRANGE=xrange, /CURRENT)
  loc = yrange[0] -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_obs_siwsiLai, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  tmp = siswi_sim[satObsIndOfJDo]/tmpLai
  gh_sim_siwsiLai = PLOT (satObsJD, tmp, COLOR = 'blue', OVERPLOT=1, $
    NAME = 'sim-SIWSIonLai_sim')
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [yrange[0],yrange[1]], OVERPLOT=1)
  ;NDVWI
  tmp = ndwi_obs/tmpLai
  gh_obs_ndwiLai = PLOT (satObsJD, tmp, COLOR = 'green', $
    NAME = 'obs-NDWIonLAI_sim', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  OVERPLOT=1)
  tmp = ndwi_sim[satObsIndOfJDo]/tmpLai
  gh_sim_ndwiLai = PLOT (satObsJD, tmp, COLOR = 'green', OVERPLOT=1, $
    NAME = 'sim-NDWIonLAI_sim')

  legTarget = [gh_obs_siwsiLai, gh_obs_ndwiLai];[gh_obs_siwsiLai, gh_sim_siwsiLai, gh_obs_ndwiLai, gh_sim_ndwiLai]
  !null = LEGEND(target=legTarget, /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, VERTICAL_SPACING = 0.01, TRANSPARENCY=100)

  ;now plot scatter plot and resildual of scatter plots
  ;SIWSI
  ;scatter with LAI
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange = [MIN(siswi_obs, /NAN),MAX(siswi_obs, /NAN)] & ofst = (yrange[1]-yrange[0])/10.0 & yrange = [yrange[0]-ofst,yrange[1]+ofst]
  xrange = [MIN(tmpLai, /NAN),MAX(tmpLai, /NAN)] & ofst = (xrange[1]-xrange[0])/10.0 & xrange = [xrange[0]-ofst,xrange[1]+ofst]
  IF (xrange[0] LT 0.0) THEN xrange[0] = 0.0
  ;plot black until the ndvi max
  gh_scatterB = PLOT(tmpLai[0:ind_max_ndvi_obs], siswi_obs[0:ind_max_ndvi_obs], SYMBOL='+', LINESTYLE='none', XRANGE=xrange, YRANGE=yrange, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 't<=t_max_ndvi', XTITLE='Sim LAI (excl 0)', YTITLE='Obs SIWSI')
  ;and grey afterwards
  gh_scatterA = PLOT(tmpLai[indmax+1:*], siswi_obs[indmax+1:*], SYMBOL='+', LINESTYLE='none', $
    FONT_SIZE = fs, COLOR='grey', OVERPLOT=1, XRANGE=xrange, YRANGE=yrange, NAME = 't>t_max_ndvi')
  both_finite = WHERE(FINITE(tmpLai) AND FINITE(siswi_obs))
  b = REGRESS(tmpLai[both_finite], siswi_obs[both_finite], CONST = a, CORRELATION = r, YFIT=yfit)
  ;trick to avoid plotting outside
  x0=(yrange[0]-a)/b[0] & x1=(yrange[1]-a)/b[0]
  IF (x0 LT xrange[0]) THEN x0=xrange[0] & IF (x1 GT xrange[1]) THEN x1=xrange[1]
  gh_reg = PLOT([x0,x1],  a+b[0]*[x0,x1], OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r^2,2), YRANGE=yrange)
  !null = LEGEND(target=[gh_scatterB, gh_scatterA], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=[0.92, 1.02], /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.03, TRANSPARENCY=100)
  th1 = TEXT(pos[2]-0.1, pos[1]+0.03, '$R^2$=' + STRING(r^2, FORMAT='(F4.2)', /PRINT), COLOR='blue');, TARGET=gh_scatterB, /DATA)

  ;residuals SIWSI with LAI
  pos[[0,2]]= pos[[0,2]] + 0.33
  residuals = siswi_obs[WHERE(FINITE(tmpLai))] - yfit
  yrange = [MIN(residuals, /NAN),MAX(residuals, /NAN)] & ofst = (yrange[1]-yrange[0])/10.0 & yrange = [yrange[0]-ofst,yrange[1]+ofst]
  xrange = [MIN(tJD), MAX(tJD)]
  gh_residuals = PLOT (satObsJD[WHERE(FINITE(tmpLai))], residuals, COLOR = 'blue', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    NAME = 'obs-SIWSIonLAI_sim', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  POSITION=pos, $
    FONT_SIZE = fs, YRANGE=yrange, XRANGE=xrange, /CURRENT, YTITLE = 'residuals obsSISWI-simLAI')
  loc = yrange[0] -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_residuals, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  gh_tmp = PLOT(xrange, [0,0], OVERPLOT=1)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [yrange[0],yrange[1]], OVERPLOT=1)

  ;scatter with NDVI
  pos = pos0
  pos[[1,3]]= pos[[1,3]] - 0.6
  yrange = [MIN(siswi_obs,/NAN),MAX(siswi_obs,/NAN)] & ofst = (yrange[1]-yrange[0])/10.0 & yrange = [yrange[0]-ofst,yrange[1]+ofst]
  xrange = [MIN(ndvi_obs,/NAN),MAX(ndvi_obs,/NAN)] & ofst = (xrange[1]-xrange[0])/10.0 & xrange = [xrange[0]-ofst,xrange[1]+ofst]
  IF (xrange[0] LT 0.0) THEN xrange[0] = 0.0
  ;plot black until the ndvi max
  gh_scatterB = PLOT(ndvi_obs[0:ind_max_ndvi_obs], siswi_obs[0:ind_max_ndvi_obs], SYMBOL='+', LINESTYLE='none', XRANGE=xrange, YRANGE=yrange, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 't<=t_max_ndvi', XTITLE='obs NDVI', YTITLE='Obs SIWSI')
  ;and grey afterwards
  gh_scatterA = PLOT(ndvi_obs[indmax+1:*], siswi_obs[indmax+1:*], SYMBOL='+', LINESTYLE='none', $
    FONT_SIZE = fs, COLOR='grey', OVERPLOT=1, XRANGE=xrange, YRANGE=yrange, NAME = 't>t_max_ndvi')
  b = REGRESS(ndvi_obs[WHERE(FINITE(ndvi_obs))], siswi_obs[WHERE(FINITE(siswi_obs))], CONST = a, CORRELATION = r, YFIT=yfit)
  ;trick to avoid plotting outside
  x0=(yrange[0]-a)/b[0] & x1=(yrange[1]-a)/b[0]
  IF (x0 LT xrange[0]) THEN x0=xrange[0] & IF (x1 GT xrange[1]) THEN x1=xrange[1]
  gh_reg = PLOT([x0,x1],  a+b[0]*[x0,x1], OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r^2,2), YRANGE=yrange)
  !null = LEGEND(target=[gh_scatterB, gh_scatterA], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=[0.92, 1.02], /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.03, TRANSPARENCY=100)
  th1 = TEXT(pos[2]-0.1, pos[1]+0.03, '$R^2$=' + STRING(r^2, FORMAT='(F4.2)', /PRINT), COLOR='blue');, TARGET=gh_scatterB, /DATA)

  ;residuals with NDVI
  pos[[0,2]]= pos[[0,2]] + 0.33
  residuals = siswi_obs[WHERE(FINITE(siswi_obs))] - yfit
  yrange = [MIN(residuals),MAX(residuals)] & ofst = (yrange[1]-yrange[0])/10.0 & yrange = [yrange[0]-ofst,yrange[1]+ofst]
  xrange = [MIN(tJD), MAX(tJD)]
  gh_residuals = PLOT (satObsJD[WHERE(FINITE(tmpLai))], residuals, COLOR = 'blue', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
    NAME = 'obs-SIWSIonNDVI', LINESTYLE='none', SYMBOL='+', SYM_SIZE = 0.7,  POSITION=pos, AXIS_STYLE = 1, $
    FONT_SIZE = fs, YRANGE=yrange, XRANGE=xrange, /CURRENT, YTITLE = 'residuals obsSISWI-obsNDVI')
  loc = yrange[0] -(yrange[1]-yrange[0])/10
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  a_x =AXIS('X', TARGET = gh_residuals, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-tJD[0]+JD2DOY(tJD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
  a_y = AXIS('x', TARGET = gh_residuals, LOCATION = "top",GRIDSTYLE="none", MAJOR=0, MINOR=0)
  gh_tmp = PLOT(xrange, [0,0], OVERPLOT=1)
  gh_tmp = PLOT([satObsJD[ind_max_ndvi_obs], satObsJD[ind_max_ndvi_obs]], COLOR='grey', [yrange[0],yrange[1]], OVERPLOT=1)
  yrange = [0, 2.5]
  gh_act  = PLOT (tJD, actEps_s_sim_fAPAR,  COLOR = 'green', NAME = 'LAI', AXIS_STYLE = 0, $
    XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
    FONT_SIZE = fs, POSITION=pos, YRANGE = yrange, XRANGE=xrange, /CURRENT)
  a_act = AXIS('y', TARGET = gh_act, LOCATION = 'right', TICKFONT_SIZE = fs, TEXTPOS = 1, $
    TITLE = 'actEps_s_sim_fAPAR', YRANGE=yrange, TICKDIR=1)

  ;Scatter Residuals vs Eps needed
  pos[[0,2]]= pos[[0,2]] + 0.33
  yrange = [MIN(actEps_s_sim_fAPAR, /NAN),MAX(actEps_s_sim_fAPAR, /NAN)] & ofst = (yrange[1]-yrange[0])/10.0 & yrange = [yrange[0]-ofst,yrange[1]+ofst]
  ;IF (yrange[1] GT 5) THEN yrange[1] = 5 & IF (yrange[0] LT -5) THEN yrange[0] = -5
  xrange = [MIN(residuals, /NAN),MAX(residuals, /NAN)] & ofst = (xrange[1]-xrange[0])/10.0 & xrange = [xrange[0]-ofst,xrange[1]+ofst]
  ylim = 3
  IF (yrange[1] GT ylim) THEN yrange[1] = ylim & IF (yrange[0] LT -ylim) THEN yrange[0] = -ylim
  ;plot black until the ndvi max
  gh_scatterB = PLOT(residuals[0:ind_max_ndvi_obs], actEps_s_sim_fAPAR[0:ind_max_ndvi_obs], SYMBOL='+', LINESTYLE='none', XRANGE=xrange, YRANGE=yrange, $
    FONT_SIZE = fs, POSITION=pos, /CURRENT, NAME = 't<=t_max_ndvi', XTITLE='resSISWI-NDVI)', YTITLE='actEps_s_sim_fAPAR<'+STRTRIM(ylim,2))
  ;and grey afterwards
  gh_scatterA = PLOT(residuals[indmax+1:*], actEps_s_sim_fAPAR[indmax+1:*], SYMBOL='+', LINESTYLE='none', $
    FONT_SIZE = fs, COLOR='grey', OVERPLOT=1, XRANGE=xrange, YRANGE=yrange, NAME = 't>t_max_ndvi')
  ; actEps_s_sim_fAPAR may contain NaN
  goodAct = BYTARR(N_ELEMENTS(actEps_s_sim_fAPAR)) * 0
  ind = WHERE(FINITE(actEps_s_sim_fAPAR))
  goodAct[ind] = actEps_s_sim_fAPAR[ind] LT ylim
  ind = WHERE(FINITE(residuals) AND FINITE(actEps_s_sim_fAPAR) AND (goodAct))
  IF (N_ELEMENTS(ind) GT 3) THEN BEGIN
    b = REGRESS(residuals[ind], actEps_s_sim_fAPAR[ind], CONST = a, CORRELATION = r, YFIT=yfit)
    ;trick to avoid plotting outside
    x0=(yrange[0]-a)/b[0] & x1=(yrange[1]-a)/b[0]
    IF (x0 LT xrange[0]) THEN x0=xrange[0] & IF (x1 GT xrange[1]) THEN x1=xrange[1]
    gh_reg = PLOT([x0,x1],  a+b[0]*[x0,x1], OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r^2,2), YRANGE=yrange)
  ENDIF
  !null = LEGEND(target=[gh_scatterB, gh_scatterA], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=[0.92, 1.02], /RELATIVE, $
    SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.03, TRANSPARENCY=100)
  th1 = TEXT(pos[2]-0.1, pos[1]+0.03, '$R^2$=' + STRING(r^2, FORMAT='(F4.2)', /PRINT), COLOR='blue');, TARGET=gh_scatterB, /DATA)


  gh_obs_ndvi.Save, out_dir + '\' + 'Spectral_' + base_name + '.png', BORDER=10, RESOLUTION=300
  ;*************************************************************************************
  ;*************************************************************************************
  ;*************************************************************************************
  IF (doPlotSpectra EQ 1) THEN BEGIN
    ;plot reflectances (sim and obs) for the second obs, @ max obs NDVI, second last
    lp = [0.05, 1] ;legend position
    dm = [900,900]  ;dimensions of the window
    xmin = 0.05
    ymin = 0.7 ;(0.605)
    sw = 0.02  ;sample width
    pos0 = [xmin, ymin,xmin+0.25, ymin+0.25] ;0.24 in width and height
    cwl = datacentwl_MODIST_7b()
    wlspec = FLOAT(INDGEN(2101) + 400)
    xtitle = 'Wl (nm)'
    ytitle = 'R (-)'
    sym_size = .7

    pos = pos0
    ind = 0
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='obs1', TITLE = 'Obs1', WINDOW_TITLE= 'Year: '+ STRTRIM(sYear,2), $
      WINDOW_TITLE = base_name + '_spectra')
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='sim1', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos = pos0
    pos[[0,2]]= pos[[0,2]] + 0.33
    ind = 1
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='obs2', TITLE = 'Obs2', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='sim2', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos[[0,2]]= pos[[0,2]] + 0.33
    ind = 2
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='obs3', TITLE = 'Obs3', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='sim3', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos = pos0
    pos[[1,3]]= pos[[1,3]] - 0.33
    indsim = WHERE(tJD EQ satObsJD[ind_max_ndvi_obs - 1])
    obs = [satR1[ind_max_ndvi_obs - 1],satR2[ind_max_ndvi_obs - 1],satR3[ind_max_ndvi_obs - 1],satR4[ind_max_ndvi_obs - 1],satR5[ind_max_ndvi_obs - 1],satR6[ind_max_ndvi_obs - 1], satR7[ind_max_ndvi_obs - 1]]
    sims = sim.ModRef[*,indsim]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='posMax-1 obs', TITLE = 'posMax-1', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='posMax-1 sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos[[0,2]]= pos[[0,2]] + 0.33

    indsim = WHERE(tJD EQ satObsJD[ind_max_ndvi_obs])

    obs = [satR1[ind_max_ndvi_obs],satR2[ind_max_ndvi_obs],satR3[ind_max_ndvi_obs],satR4[ind_max_ndvi_obs],satR5[ind_max_ndvi_obs],satR6[ind_max_ndvi_obs], satR7[ind_max_ndvi_obs]]
    sims = sim.ModRef[*,indsim]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='posMax obs', TITLE = 'posMax', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='posMax sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos[[0,2]]= pos[[0,2]] + 0.33
    indsim = WHERE(tJD EQ satObsJD[ind_max_ndvi_obs+1])
    obs = [satR1[ind_max_ndvi_obs + 1],satR2[ind_max_ndvi_obs + 1],satR3[ind_max_ndvi_obs + 1],satR4[ind_max_ndvi_obs + 1],satR5[ind_max_ndvi_obs + 1],satR6[ind_max_ndvi_obs + 1], satR7[ind_max_ndvi_obs + 1]]
    sims = sim.ModRef[*,indsim]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='posMax+1 obs', TITLE = 'posMax+1', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='posMax+1 sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')
    pos[[0,2]]= pos[[0,2]] + 0.33

    pos = pos0
    pos[[1,3]]= pos[[1,3]] - 0.66
    ind = N_ELEMENTS(satR1)-1
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='last obs', TITLE = 'last obs', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='last sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos[[0,2]]= pos[[0,2]] + 0.33
    ind = ind - 1
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='second last obs', TITLE = 'second last obs', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='second last sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')

    pos[[0,2]]= pos[[0,2]] + 0.33

    ind = ind - 2
    obs = [satR1[ind],satR2[ind],satR3[ind],satR4[ind],satR5[ind],satR6[ind], satR7[ind]]
    sims = sim.ModRef[*,ind]
    yrange = [0, MAX([obs, sims])+0.05]
    gh_obs = PLOT (cwl, obs, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, LINESTYLE='none', SYMBOL='o', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'red', Name='third last obs', TITLE = 'third last obs', /CURRENT)
    gh_sim = PLOT (cwl, sims, YTITLE=ytitle, XTITLE=xtitle, $
      FONT_SIZE = fs, LINESTYLE='none', SYMBOL='+', SYM_SIZE = sym_size, $
      YRANGE=yrange, COLOR = 'black', Name='third last sim', OVERPLOT=1)
    gh_rsoil = PLOT(wlspec,proSailVar.rsoil, COLOR='grey', OVERPLOT=1, NAME='rsoil')
    gh_tmp = PLOT([645,645], [0,1], YRANGE=yrange, COLOR='red', OVERPLOT=1)
    gh_tmp = PLOT([858,858], [0,1], YRANGE=yrange, COLOR='black', OVERPLOT=1)
    gh_tmp = PLOT([1640,1640], [0,1], YRANGE=yrange, COLOR='grey', OVERPLOT=1)
    !null = LEGEND(target=[gh_obs, gh_sim, gh_rsoil], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=sw, TRANSPARENCY=100, HORIZONTAL_ALIGNMENT = 'LEFT')
    gh_obs.Save, out_dir + '\' + base_name + 'spectra.png', BORDER=10, RESOLUTION=300
  ENDIF

  win = GetWindows(NAMES=winNames)
  FOR i = 0, N_ELEMENTS(win)-1 DO win[i].close

END