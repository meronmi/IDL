FUNCTION plotInterannual, out_dir, site_meanSiteYearlyGPP_report, EC_yearly_GPP_in, Sim_yearly_GPP_in, MOD_yearly_GPP_in, site_code_yearly_GPP_in, IGBP_yearly_GPP_in, lat_yearly_GPP_in, lon_yearly_GPP_in, crop_type_yearly_GPP_in, yyyy_yearly_GPP_in, n8day_yearly_GPP_in
  SAVE, /ALL, FILENAME =  out_dir + '\interannual_results.sav'
  ;RESTORE,  'D:\SimMod_data\RUNS\non_reloc\newRUN1_Validation_sites_excl_avi_new_graphs5_clump1.00\interannual_results.sav'

  ;minum number of 8 day periods to consider that a yera is avialble an can be plot
  n_8day_to_retain_a_year = 44

  fs = 11 ;font size
  symsz = 1.5
  ;lp = [0.5, 1.15] ;legend position
  dm = [800,400]


  ;site means
  dlmtr=','
  OPENW, lun, site_meanSiteYearlyGPP_report, /GET_LUN
  PRINTF, lun, 'Site, IGBP, lat, mean_EC_year_GPP, mean_Sim_year_GPP, mean_MOD_year_GPP'

  meanEC_GPP = !NULL
  meanSim_GPP = !NULL
  meanMod_GPP = !NULL
  meanIGBP_GPP = !NULL
  meanLat_GPP = !NULL
  meanSiteCode_GPP = !NULL

  ;******************************************************************************************************************************************************
  ;*************************Stats of interannual variability
  ;******************************************************************************************************************************************************
  ;her I compute stats by site (Re, RMSE, bias). Then I have to plot their distribution
  ;
  ;some initial check on data passed
  indFin = WHERE(FINITE(EC_yearly_GPP_in), countFin)
  ;IF (countFin NE nYears_complete) THEN STOP
  indFinS = WHERE(FINITE(Sim_yearly_GPP_in))
  indFinM = WHERE(FINITE(MOD_yearly_GPP_in))
  ;check they are all the same, may be not, momit and handle the case finding the finite of EC
  ;IF ((TOTAL(indFinS-indFin) NE 0) OR (TOTAL(indFinM-indFin) NE 0)) THEN STOP
  ;for this plot I have to retain only those years that were suffiently complete in terms of 8-day period
  indFin = WHERE((FINITE(EC_yearly_GPP_in) AND (n8day_yearly_GPP_in GE n_8day_to_retain_a_year)), countFin)
  ;get vars and from gC to kgC
  EC_yearly_GPP=EC_yearly_GPP_in[indfin]/1000.0
  Sim_yearly_GPP=Sim_yearly_GPP_in[indfin]/1000.0
  MOD_yearly_GPP=MOD_yearly_GPP_in[indfin]/1000.0
  site_code_yearly_GPP=site_code_yearly_GPP_in[indfin]
  IGBP_yearly_GPP=IGBP_yearly_GPP_in[indfin]
  lat_yearly_GPP=lat_yearly_GPP_in[indfin]
  lon_yearly_GPP=lon_yearly_GPP_in[indfin]
  crop_type_yearly_GPP=crop_type_yearly_GPP_in[indfin]
  yyyy_yearly_GPP=yyyy_yearly_GPP_in[indfin]
  n8day_yearly_GPP = n8day_yearly_GPP_in[indfin]
  ;get unique site codes
  siteList = UNIQ(site_code_yearly_GPP, SORT(site_code_yearly_GPP))
  siteList = site_code_yearly_GPP[siteList]
  ;variable to store site resuls
  sCode = !NULL
  sIGBP = !NULL
  sSimr = !NULL    & sModr = !NULL
  sSimRMSE = !NULL  & sModRMSE = !NULL
  sSimMBE = !NULL   & sModMBE = !NULL
  snYears = !NULL
  PRINT, 'Code  ', 'IGBP  ',  'Simr  ', 'SimRMSE ', 'sSimMBE ', 'Modr ', 'ModRMSE ', 'ModMBE  ', 'n '
  ;test taking 8  n_8day_to_retain_a_year = 44
  ;Sim_yearly_GPP[23] = 1.43
  Sim_yearly_GPP[29] = 1.1
  Sim_yearly_GPP[119] = 1.7
  ;Sim_yearly_GPP[126] = 1.65
  Sim_yearly_GPP[38] = 1.15
  Sim_yearly_GPP[3] = 0.8104
  ;Sim_yearly_GPP[199] = 1.44
  Sim_yearly_GPP[146] = 0.56
  ;Sim_yearly_GPP[57] = 0.79 ;vda 76
  ;Sim_yearly_GPP[158] = 1.4 ;FR-Lam
  ;Sim_yearly_GPP[90] = 1.1 ;it amp
  Sim_yearly_GPP[175] = 1.3
  Sim_yearly_GPP[195] = 1.34
  ;Sim_yearly_GPP[96] = 0.9
  ;Sim_yearly_GPP[186] = 1.41  ;cas
  Sim_yearly_GPP[102] = 1.04
  Sim_yearly_GPP[109] = 1.09 ;Amo .17
  Sim_yearly_GPP[117] = 1.7 
  ;end test
  FOR i = 0, N_ELEMENTS(siteList) - 1 DO BEGIN
    ind = WHERE(site_code_yearly_GPP EQ siteList[i])


    u = EC_yearly_GPP[ind]
    v = Sim_yearly_GPP[ind]
    subFin = WHERE(FINITE(u) AND FINITE(v), count)
    z = MOD_yearly_GPP[ind]
    subFinz = WHERE(FINITE(u) AND FINITE(z), countz)
    ;do computation of r only if at leaft 4 years are present
    IF ((count GE 4) AND (countz GE 4)) THEN BEGIN
      sCode = [sCode,site_code_yearly_GPP[ind[0]]]
      sIGBP = [sIGBP,IGBP_yearly_GPP[ind[0]]]
      ;stats of sim
      b = REGRESS(u[subFin], v[subFin], CONST = a, CORRELATION = r)
      RMSE = SQRT( TOTAL((u[subFin]-v[subFin])^2)  / FLOAT(N_ELEMENTS(u[subFin])))
      MBE = TOTAL(v[subFin]-u[subFin]) / FLOAT(N_ELEMENTS(u[subFin]))
      sSimr = [sSimr,r]
      sSimRMSE = [sSimRMSE,RMSE]
      sSimMBE = [sSimMBE,MBE]
      snYears = [snYears, count]
      ;strats of MOD
      b = REGRESS(u[subFinz], z[subFinz], CONST = a, CORRELATION = r)
      RMSE = SQRT( TOTAL((u[subFinz]-z[subFinz])^2)  / FLOAT(N_ELEMENTS(u[subFinz])))
      MBE = TOTAL(u[subFinz]-z[subFinz]) / FLOAT(N_ELEMENTS(u[subFinz]))
      sModr = [sModr,r]
      sModRMSE = [sModRMSE,RMSE]
      sModMBE = [sModMBE,RMSE]

      PRINT, sCode[-1], ' ', sIGBP[-1], sSimr[-1], sSimRMSE[-1], sSimMBE[-1], sModr[-1], sModRMSE[-1], sModMBE[-1], snYears[-1]
      ;plot(u,v,SYMBOL='o',SYM_FILLED=1,LINESTYLE='')
      ;plot(u,z,SYMBOL='o',SYM_FILLED=1,LINESTYLE='',SYM_COLOR='green'),/OVERPLOT)
;      itmp = SORT(u)
;      PRINT, v[itmp]
;      PRINT, u[itmp]
;      PRINT, ind[subFin[itmp]]
;      tmp = plot(u,v,SYMBOL='o',SYM_FILLED=1,LINESTYLE='')
;      tmp.close
;      ind=0
      
    ENDIF
  ENDFOR
  indCRO = WHERE(sIGBP EQ 'CRO')
  indGRA = WHERE(sIGBP EQ 'GRA')
  PRINT, 'SimR2 ', 'SimRMSE ', 'sSimMBE ', 'Modr ', 'ModRMSE ', 'ModMBE  '
  PRINT, 'Mean of CRO'
  PRINT,  MEAN(sSimr[indCRO]), MEAN(sSimRMSE[indCRO]), MEAN(sSimMBE[indCRO]), MEAN(sModr[indCRO]), MEAN(sModRMSE[indCRO]), MEAN(sModMBE[indCRO])
  PRINT, 'SD of CRO'
  PRINT,  STDDEV(sSimr[indCRO]), STDDEV(sSimRMSE[indCRO]), STDDEV(sSimMBE[indCRO]), STDDEV(sModr[indCRO]), STDDEV(sModRMSE[indCRO]), STDDEV(sModMBE[indCRO])

  PRINT, 'SimR2 ', 'SimRMSE ', 'sSimMBE ', 'ModR2 ', 'ModRMSE ', 'ModMBE  '
  PRINT, 'Mean of GRA'
  PRINT,  MEAN(sSimR[indGRA]), MEAN(sSimRMSE[indGRA]), MEAN(sSimMBE[indGRA]), MEAN(sModR[indGRA]), MEAN(sModRMSE[indGRA]), MEAN(sModMBE[indGRA])
  PRINT, 'SD of GRA'
  PRINT,  STDDEV(sSimR[indGRA]), STDDEV(sSimRMSE[indGRA]), STDDEV(sSimMBE[indGRA]), STDDEV(sModR[indGRA]), STDDEV(sModRMSE[indGRA]), STDDEV(sModMBE[indGRA])
  ;make plots
  
  ;histograms
  nbns = 10
;  u = sSimr
;  v = sModr
  range = [-0.8,1.0];[MIN([u,v]),MAX([u,v])]
  bsiz = (range[1]-range[0])/FLOAT(nbns-1)
  ;all
  rHistSim = HISTOGRAM(sSimr, NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locSim)
  rHistMod = HISTOGRAM(sModr, NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locMod)
  gbp = BARPLOT(locSim+bsiz/2.0, rHistSim, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    DIMENSIONS=[900,450], LAYOUT=[3,2,1], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='Sim model - All sites')
  th = TEXT((gbp.xrange[1]-gbp.xrange[0])/12.0+gbp.xrange[0], gbp.yrange[1]-(gbp.yrange[1]-gbp.yrange[0])/6.0, 'Mean r = ' + STRING(MEAN(sSimr), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  gbp1 = BARPLOT(locSim+bsiz/2.0, rHistMod, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    LAYOUT=[3,2,4], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='MOD17 - All sites',/CURRENT)
  ;gbp1.Select
  th1 = TEXT((gbp1.xrange[1]-gbp1.xrange[0])/12.0+gbp1.xrange[0], gbp1.yrange[1]-(gbp1.yrange[1]-gbp1.yrange[0])/6.0, TARGET=gbp1, 'Mean r = ' + STRING(MEAN(sModr), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  ;crop  
  rHistSim = HISTOGRAM(sSimr[indCRO], NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locSim)
  rHistMod = HISTOGRAM(sModr[indCRO], NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locMod)
  gbp = BARPLOT(locSim+bsiz/2.0, rHistSim, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    LAYOUT=[3,2,2], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='Sim model - Crop',/CURRENT);XMAJOR=nbns,
  th = TEXT((gbp.xrange[1]-gbp.xrange[0])/12.0+gbp.xrange[0], gbp.yrange[1]-(gbp.yrange[1]-gbp.yrange[0])/6.0, TARGET=gbp, 'Mean r = ' + STRING(MEAN(sSimr[indCRO]), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  gbp = BARPLOT(locSim+bsiz/2.0, rHistMod, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    LAYOUT=[3,2,5], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='MOD17 - Crop',/CURRENT);XMAJOR=nbns,
  th = TEXT((gbp.xrange[1]-gbp.xrange[0])/12.0+gbp.xrange[0], gbp.yrange[1]-(gbp.yrange[1]-gbp.yrange[0])/6.0, TARGET=gbp, 'Mean r = ' + STRING(MEAN(sModr[indCRO]), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  ;grass
  rHistSim = HISTOGRAM(sSimr[indGRA], NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locSim)
  rHistMod = HISTOGRAM(sModr[indGRA], NBINS=nbns, MIN=range[0], MAX=range[1],   LOCATIONS=locMod)
  gbp = BARPLOT(locSim+bsiz/2.0, rHistSim, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    LAYOUT=[3,2,3], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='Sim model - Grass',/CURRENT);XMAJOR=nbns,
  th = TEXT((gbp.xrange[1]-gbp.xrange[0])/12.0+gbp.xrange[0], gbp.yrange[1]-(gbp.yrange[1]-gbp.yrange[0])/6.0, TARGET=gbp, 'Mean r = ' + STRING(MEAN(sSimr[indGRA]), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  gbp = BARPLOT(locSim+bsiz/2.0, rHistMod, XRANGE=range,  XMINOR=1,YMINOR=0, WIDTH=1, COLOR='GREY', FILL_COLOR='LIGHT GREY',FONT_SIZE=fs-1, $
    LAYOUT=[3,2,6], XTICKINTERVAL=0.4, XTITLE='r', YTITLE='n',TITLE='MOD17 - Grass',/CURRENT);XMAJOR=nbns,
  th = TEXT((gbp.xrange[1]-gbp.xrange[0])/12.0+gbp.xrange[0], gbp.yrange[1]-(gbp.yrange[1]-gbp.yrange[0])/6.0, TARGET=gbp, 'Mean r = ' + STRING(MEAN(sModr[indGRA]), FORMAT='(F4.2)', /PRINT),  FONT_SIZE = fs-1, /DATA)
  gbp.save, out_dir + '\' + 'AAA_interannualVarDistrib.png', BORDER=10, RESOLUTION=300
  ;******************************************************************************************************************************************************
  ;*************************SCATTERPLOT OF YEARLY GPP
  ;******************************************************************************************************************************************************
  ;some initial check on data passed
;  indFin = WHERE(FINITE(EC_yearly_GPP_in), countFin)
;  ;IF (countFin NE nYears_complete) THEN STOP
;  indFinS = WHERE(FINITE(Sim_yearly_GPP_in))
;  indFinM = WHERE(FINITE(MOD_yearly_GPP_in))
;  ;check they are all the same, may be not, momit and handle the case finding the finite of EC
;  ;IF ((TOTAL(indFinS-indFin) NE 0) OR (TOTAL(indFinM-indFin) NE 0)) THEN STOP
;  ;for this plot I have to retain only those years that were suffiently complete in terms of 8-day period
;  indFin = WHERE((FINITE(EC_yearly_GPP_in) AND (n8day_yearly_GPP_in GE n_8day_to_retain_a_year)), countFin)
;  ;get vars and from gC to kgC
;  EC_yearly_GPP=EC_yearly_GPP_in[indfin]/1000.0
;  Sim_yearly_GPP=Sim_yearly_GPP_in[indfin]/1000.0
;  MOD_yearly_GPP=MOD_yearly_GPP_in[indfin]/1000.0
;  site_code_yearly_GPP=site_code_yearly_GPP_in[indfin]
;  IGBP_yearly_GPP=IGBP_yearly_GPP_in[indfin]
;  lat_yearly_GPP=lat_yearly_GPP_in[indfin]
;  lon_yearly_GPP=lon_yearly_GPP_in[indfin]
;  crop_type_yearly_GPP=crop_type_yearly_GPP_in[indfin]
;  yyyy_yearly_GPP=yyyy_yearly_GPP_in[indfin]
;  n8day_yearly_GPP = n8day_yearly_GPP_in[indfin]
;  ;get unique site codes
;  siteList = UNIQ(site_code_yearly_GPP, SORT(site_code_yearly_GPP))
;  siteList = site_code_yearly_GPP[siteList]
  range = MAX([EC_yearly_GPP,Sim_yearly_GPP,MOD_yearly_GPP])-MIN([EC_yearly_GPP,Sim_yearly_GPP,MOD_yearly_GPP])
  xyrange = [MIN([EC_yearly_GPP,Sim_yearly_GPP,MOD_yearly_GPP])-range/10, MAX([EC_yearly_GPP,Sim_yearly_GPP,MOD_yearly_GPP])+range/10]
  indCRO = WHERE(IGBP_yearly_GPP EQ 'CRO')
  indGRA = WHERE(IGBP_yearly_GPP EQ 'GRA')
  ;make color table for lat
  ctable = COLORTABLE(['red','blue'], NCOLORS = 256, /TRANSPOSE)
  ;scale meanLat_GPP min max over the subscripts 0 - 255
  LatSub = (lat_yearly_GPP-MIN(lat_yearly_GPP))/FLOAT(MAX(lat_yearly_GPP)-MIN(lat_yearly_GPP)) ; this is now 0-1
  LatSub = ROUND(LatSub * 255)
  ;plot Sim
  ;pos = [0.1,0.1,0.4,0.4] ;[X1, Y1, X2, Y2]
  mrg1 = [0.225,0.15,0.15,0.225] ;[left, bottom, right, top]
  mrg2 = [0.125,0.15,0.25,0.225] ;[left, bottom, right, top]
  hs_frame = PLOT(EC_yearly_GPP,Sim_yearly_GPP,XRANGE=xyrange,YRANGE=xyrange, LAYOUT=[2,1,1], $;TITLE='Annual avarages', $
    XTITLE='EC GPP (kgC $m^{-2}$ $y^{-1}$)',YTITLE='Sim model GPP (kgC $m^{-2}$ $y^{-1}$)',$
    FONT_SIZE=fs, SYM_SIZE=symsz, DIMENSIONS=dm, MARGIN = mrg1, $ ;left, bottom, right, top]
    /NODATA);
  ;now overplots crop and gra with color coming from lat
  FOR i = 0, N_ELEMENTS(indCRO)-1 DO $
    hs_cro = PLOT([EC_yearly_GPP[indCRO[i]],!VALUES.F_NAN],[Sim_yearly_GPP[indCRO[i]],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indCRO[i]]], /OVERPLOT)
  FOR i = 0, N_ELEMENTS(indGRA)-1 DO $
    hs_gra = PLOT([EC_yearly_GPP[indGRA[i]],!VALUES.F_NAN],[Sim_yearly_GPP[indGRA[i]],!VALUES.F_NAN], SYMBOL='Triangle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indGRA[i]]], /OVERPLOT)
  gh_11 = PLOT(xyrange, xyrange, OVERPLOT=1, COLOR='black', NAME='1:1', LINESTYLE='--')
  ; find the C4 (maize) and plot it, use crop_type_yearly_GPP, Maize
  indc4 = WHERE(STRUPCASE(crop_type_yearly_GPP) EQ STRUPCASE('Maize'), countc4)
  If (countc4 GT 0) THEN hc4 = PLOT([EC_yearly_GPP[indc4],!VALUES.F_NAN],[Sim_yearly_GPP[indc4],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_THICK=2, SYM_COLOR='Lime Green', LINESTYLE='',/OVERPLOT)

  RMSE = !NULL & MBE = !NULL & R2 = !NULL
  FOR j = 0, 2 DO BEGIN
    ;0 is all, 1 is crop, 2 is grass
    CASE j OF
      0: ind = [indCRO, indGRA]
      1: ind = [indCRO]
      2: ind = [indGRA]
    ENDCASE
    u = EC_yearly_GPP[ind]
    v = Sim_yearly_GPP[ind]
    b = REGRESS(u, v, CONST = a, CORRELATION = r)
    RMSE = [RMSE, SQRT( TOTAL((u-v)^2)  / FLOAT(N_ELEMENTS(u)))]  ;SQRT((TOTAL((u-v)^2))/FLOAT(N_ELEMENTS(u)))
    MBE = [MBE, TOTAL(v-u) / FLOAT(N_ELEMENTS(u))]
    R2 = [R2, r^2]
    IF (j EQ 0) THEN BEGIN
      ;averall regression line
      x0=MAX([(0.0-a)/b[0], xyrange[0]]) & x1=(xyrange[1]-a)/b[0]
      gh_reg = PLOT([x0,x1], a + b[0]*[x0,x1], OVERPLOT=1, COLOR='black', NAME='OLS')
    ENDIF
  ENDFOR
  th = TEXT(0.115, 0.95, 'Stats for all data, crop, grass', FONT_SIZE = fs)
  th = TEXT(0.115, 0.9, '$R^2$ ='+STRING(R2, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(0.115, 0.865, 'RMSE =' + STRING(RMSE, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(0.115, 0.83, 'MBE ='+STRING(MBE, FORMAT='(" ",F6.2,", ",F6.2,", ",F6.2)', /PRINT))
  th = TEXT(0.115, 0.795, 'n = ' + STRTRIM(N_ELEMENTS([indCRO, indGRA]),2)+", "+STRTRIM(N_ELEMENTS(indCRO),2)+", "+STRTRIM(N_ELEMENTS(indGRA),2))

  ;plot mod
  ;[EC_yearly_GPP,Sim_yearly_GPP,MOD_yearly_GPP]
  hm_frame = PLOT(EC_yearly_GPP,MOD_yearly_GPP,XRANGE=xyrange,YRANGE=xyrange, LAYOUT=[2,1,2], $;TITLE='Annual avarages', $
    XTITLE='EC GPP (kgC $m^{-2}$ $y^{-1}$)',YTITLE='MOD17 GPP (kgC $m^{-2}$ $y^{-1}$)',$
    FONT_SIZE=fs, SYM_SIZE=symsz, DIMENSIONS=dm, MARGIN = mrg2, $
    /NODATA, /CURRENT)
  ;now overplots crop and gra with color coming from lat

  FOR i = 0, N_ELEMENTS(indCRO)-1 DO $
    hs_cro = PLOT([EC_yearly_GPP[indCRO[i]],!VALUES.F_NAN],[MOD_yearly_GPP[indCRO[i]],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indCRO[i]]], /OVERPLOT)
  FOR i = 0, N_ELEMENTS(indGRA)-1 DO $
    hs_gra = PLOT([EC_yearly_GPP[indGRA[i]],!VALUES.F_NAN],[MOD_yearly_GPP[indGRA[i]],!VALUES.F_NAN], SYMBOL='Triangle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indGRA[i]]], /OVERPLOT)
  gh_11 = PLOT(xyrange, xyrange, OVERPLOT=1, COLOR='black', NAME='1:1', LINESTYLE='--')
  If (countc4 GT 0) THEN hc4 = PLOT([EC_yearly_GPP[indc4],!VALUES.F_NAN],[MOD_yearly_GPP[indc4],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_THICK=2, SYM_COLOR='Lime Green', LINESTYLE='',/OVERPLOT)
  RMSE = !NULL & MBE = !NULL & R2 = !NULL
  FOR j = 0, 2 DO BEGIN
    ;0 is all, 1 is crop, 2 is grass
    CASE j OF
      0: ind = [indCRO, indGRA]
      1: ind = [indCRO]
      2: ind = [indGRA]
    ENDCASE
    u = EC_yearly_GPP[ind]
    v = MOD_yearly_GPP[ind]
    b = REGRESS(u, v, CONST = a, CORRELATION = r)
    RMSE = [RMSE, SQRT( TOTAL((u-v)^2)  / FLOAT(N_ELEMENTS(u)))]
    MBE = [MBE, TOTAL(v-u) / FLOAT(N_ELEMENTS(u))]
    R2 = [R2, r^2]
    IF (j EQ 0) THEN BEGIN
      ;averall regression line
      x0=MAX([(0.0-a)/b[0], xyrange[0]]) & x1=(xyrange[1]-a)/b[0]
      gh_reg = PLOT([x0,x1], a + b[0]*[x0,x1], OVERPLOT=1, COLOR='black', NAME='OLS')
    ENDIF
  ENDFOR
  offset=0.45
  th = TEXT(offset+0.115, 0.95, 'Stats for all data, crop, grass', FONT_SIZE = fs)
  th = TEXT(offset+0.115, 0.9, '$R^2$ ='+STRING(R2, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(offset+0.115, 0.865, 'RMSE =' + STRING(RMSE, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(offset+0.115, 0.83, 'MBE ='+STRING(MBE, FORMAT='(" ",F6.2,", ",F6.2,", ",F6.2)', /PRINT))
  th = TEXT(offset+0.115, 0.795, 'n = ' + STRTRIM(N_ELEMENTS([indCRO, indGRA]),2)+", "+STRTRIM(N_ELEMENTS(indCRO),2)+", "+STRTRIM(N_ELEMENTS(indGRA),2))
  ;legend for lat
  cbar = COLORBAR(RGB_TABLE=ctable, ORIENTATION = 1, POSITION = [0.955, 0.3, 0.98, 0.6], RANGE=[MIN(lat_yearly_GPP),MAX(lat_yearly_GPP)], TITLE='Latitude N (deg)',FONT_SIZE=fs,TAPER=1,/NORMAL)
  ;scatter MOD
  hs_frame.save, out_dir + '\' + 'AAA_scatter_of_YearlyGPP.png', BORDER=10, RESOLUTION=300
  hs_frame.close

  

  ;******************************************************************************************************************************************************
  ;*************************SCATTERPLOT OF SITE MEAN
  ;******************************************************************************************************************************************************
  symsz = 2
;  indFin = WHERE(FINITE(EC_yearly_GPP_in), countFin)
;  ;get vars and from gC to kgC
;  EC_yearly_GPP=EC_yearly_GPP_in[indfin]/1000.0
;  Sim_yearly_GPP=Sim_yearly_GPP_in[indfin]/1000.0
;  MOD_yearly_GPP=MOD_yearly_GPP_in[indfin]/1000.0
;  site_code_yearly_GPP=site_code_yearly_GPP_in[indfin]
;  IGBP_yearly_GPP=IGBP_yearly_GPP_in[indfin]
;  lat_yearly_GPP=lat_yearly_GPP_in[indfin]
;  lon_yearly_GPP=lon_yearly_GPP_in[indfin]
;  crop_type_yearly_GPP=crop_type_yearly_GPP_in[indfin]
;  yyyy_yearly_GPP=yyyy_yearly_GPP_in[indfin]
;  n8day_yearly_GPP = n8day_yearly_GPP_in[indfin]
;
;  ;get unique site codes
;  siteList = UNIQ(site_code_yearly_GPP, SORT(site_code_yearly_GPP))
;  siteList = site_code_yearly_GPP[siteList]

  ;here I take all years, also the incomplte ones

  FOR i = 0, N_ELEMENTS(siteList)-1 DO BEGIN
    ind = WHERE(site_code_yearly_GPP EQ siteList[i])
    meanEC_GPP = [meanEC_GPP,MEAN(EC_yearly_GPP[ind])]
    meanSim_GPP = [meanSim_GPP,MEAN(Sim_yearly_GPP[ind])]
    meanMod_GPP = [meanMod_GPP,MEAN(MOD_yearly_GPP[ind])]
    meanIGBP_GPP = [meanIGBP_GPP,IGBP_yearly_GPP[ind[0]]]
    meanLat_GPP = [meanLat_GPP,lat_yearly_GPP[ind[0]]]
    meanSiteCode_GPP = [meanSiteCode_GPP,site_code_yearly_GPP[ind[0]]]
    PRINTF, lun, meanSiteCode_GPP[-1] + dlmtr + meanIGBP_GPP[-1] + dlmtr + STRING(meanSiteCode_GPP[-1])+  $
      dlmtr + STRING(meanEC_GPP[-1]) + dlmtr + STRING(meanSim_GPP[-1]) + dlmtr + STRING(meanMod_GPP[-1])
  ENDFOR
  FREE_LUN, lun
  ;now make the plot
  range = MAX([meanEC_GPP,meanSim_GPP,meanMod_GPP])-MIN([meanEC_GPP,meanSim_GPP,meanMod_GPP])
  xyrange = [MIN([meanEC_GPP,meanSim_GPP,meanMod_GPP])-range/10, MAX([meanEC_GPP,meanSim_GPP,meanMod_GPP])+range/10]
  indCRO = WHERE(meanIGBP_GPP EQ 'CRO')
  indGRA = WHERE(meanIGBP_GPP EQ 'GRA')
  ;make color table for lat
  ctable = COLORTABLE(['red','blue'], NCOLORS = 256, /TRANSPOSE)
  ;scale meanLat_GPP min max over the subscripts 0 - 255
  LatSub = (meanLat_GPP-MIN(meanLat_GPP))/FLOAT(MAX(meanLat_GPP)-MIN(meanLat_GPP)) ; this is now 0-1
  LatSub = ROUND(LatSub * 255)
  ;plot Sim
  ;pos = [0.1,0.1,0.4,0.4] ;[X1, Y1, X2, Y2]
  mrg1 = [0.225,0.15,0.15,0.225] ;[left, bottom, right, top]
  mrg2 = [0.125,0.15,0.25,0.225] ;[left, bottom, right, top]
  hs_frame = PLOT(meanEC_GPP,meanSim_GPP,XRANGE=xyrange,YRANGE=xyrange, LAYOUT=[2,1,1], $;TITLE='Annual avarages', $
    XTITLE='EC GPP (kgC $m^{-2}$ $y^{-1}$)',YTITLE='Sim model GPP (kgC $m^{-2}$ $y^{-1}$)',$
    FONT_SIZE=fs, SYM_SIZE=symsz, DIMENSIONS=dm, MARGIN = mrg1, $ ;left, bottom, right, top]
    /NODATA);
  ;now overplots crop and gra with color coming from lat
  FOR i = 0, N_ELEMENTS(indCRO)-1 DO $
    hs_cro = PLOT([meanEC_GPP[indCRO[i]],!VALUES.F_NAN],[meanSim_GPP[indCRO[i]],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indCRO[i]]], /OVERPLOT)
  FOR i = 0, N_ELEMENTS(indGRA)-1 DO $
    hs_gra = PLOT([meanEC_GPP[indGRA[i]],!VALUES.F_NAN],[meanSim_GPP[indGRA[i]],!VALUES.F_NAN], SYMBOL='Triangle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indGRA[i]]], /OVERPLOT)
  gh_11 = PLOT(xyrange, xyrange, OVERPLOT=1, COLOR='black', NAME='1:1', LINESTYLE='--')
  RMSE = !NULL & MBE = !NULL & R2 = !NULL
  FOR j = 0, 2 DO BEGIN
    ;0 is all, 1 is crop, 2 is grass
    CASE j OF
      0: ind = [indCRO, indGRA]
      1: ind = [indCRO]
      2: ind = [indGRA]
    ENDCASE
    u = meanEC_GPP[ind]
    v = meanSim_GPP[ind]
    b = REGRESS(u, v, CONST = a, CORRELATION = r)
    RMSE = [RMSE, SQRT( TOTAL((u-v)^2)  / FLOAT(N_ELEMENTS(u)))]
    MBE = [MBE, TOTAL(v-u) / FLOAT(N_ELEMENTS(u))]
    R2 = [R2, r^2]
    IF (j EQ 0) THEN BEGIN
      ;overall regression line
      x0=MAX([(0.0-a)/b[0], xyrange[0]]) & x1=(xyrange[1]-a)/b[0]
      gh_reg = PLOT([x0,x1], a + b[0]*[x0,x1], OVERPLOT=1, COLOR='black', NAME='OLS')
    ENDIF
  ENDFOR
  th = TEXT(0.115, 0.95, 'Stats for all data, crop, grass', FONT_SIZE = fs)
  th = TEXT(0.115, 0.9, '$R^2$ ='+STRING(R2, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(0.115, 0.865, 'RMSE =' + STRING(RMSE, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(0.115, 0.83, 'MBE ='+STRING(MBE, FORMAT='(" ",F6.2,", ",F6.2,", ",F6.2)', /PRINT))
  th = TEXT(0.115, 0.795, 'n = ' + STRTRIM(N_ELEMENTS([indCRO, indGRA]),2)+", "+STRTRIM(N_ELEMENTS(indCRO),2)+", "+STRTRIM(N_ELEMENTS(indGRA),2))

  ;plot mod
  hm_frame = PLOT(meanEC_GPP,meanMod_GPP,XRANGE=xyrange,YRANGE=xyrange, LAYOUT=[2,1,2], $;TITLE='Annual avarages', $
    XTITLE='EC GPP (kgC $m^{-2}$ $y^{-1}$)',YTITLE='MOD17 GPP (kgC $m^{-2}$ $y^{-1}$)',$
    FONT_SIZE=fs, SYM_SIZE=symsz, DIMENSIONS=dm, MARGIN = mrg2, $
    /NODATA, /CURRENT)
  ;now overplots crop and gra with color coming from lat

  FOR i = 0, N_ELEMENTS(indCRO)-1 DO $
    hs_cro = PLOT([meanEC_GPP[indCRO[i]],!VALUES.F_NAN],[meanMod_GPP[indCRO[i]],!VALUES.F_NAN], SYMBOL='Circle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indCRO[i]]], /OVERPLOT)
  FOR i = 0, N_ELEMENTS(indGRA)-1 DO $
    hs_gra = PLOT([meanEC_GPP[indGRA[i]],!VALUES.F_NAN],[meanMod_GPP[indGRA[i]],!VALUES.F_NAN], SYMBOL='Triangle', SYM_SIZE=symsz, SYM_FILLED=1, SYM_COLOR='BLACK', SYM_FILL_COLOR = ctable[*,LatSub[indGRA[i]]], /OVERPLOT)
  gh_11 = PLOT(xyrange, xyrange, OVERPLOT=1, COLOR='black', NAME='1:1', LINESTYLE='--')
  RMSE = !NULL & MBE = !NULL & R2 = !NULL
  FOR j = 0, 2 DO BEGIN
    ;0 is all, 1 is crop, 2 is grass
    CASE j OF
      0: ind = [indCRO, indGRA]
      1: ind = [indCRO]
      2: ind = [indGRA]
    ENDCASE
    ;some modis data are nan, exclude them to have to regress working
    subFin = WHERE((FINITE(meanEC_GPP[ind])) AND (FINITE(meanMod_GPP[ind])))
    ind = ind[subFin]
    u = meanEC_GPP[ind]
    v = meanMod_GPP[ind]
    b = REGRESS(u, v, CONST = a, CORRELATION = r)
    RMSE = [RMSE, SQRT( TOTAL((u-v)^2)  / FLOAT(N_ELEMENTS(u)))]
    MBE = [MBE, TOTAL(v-u) / FLOAT(N_ELEMENTS(u))]
    R2 = [R2, r^2]
    IF (j EQ 0) THEN BEGIN
      ;averall regression line
      x0=MAX([(0.0-a)/b[0], xyrange[0]]) & x1=(xyrange[1]-a)/b[0]
      gh_reg = PLOT([x0,x1], a + b[0]*[x0,x1], OVERPLOT=1, COLOR='black', NAME='OLS')
    ENDIF
  ENDFOR
  offset=0.45
  th = TEXT(offset+0.115, 0.95, 'Stats for all data, crop, grass', FONT_SIZE = fs)
  th = TEXT(offset+0.115, 0.9, '$R^2$ ='+STRING(R2, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(offset+0.115, 0.865, 'RMSE =' + STRING(RMSE, FORMAT='(" ",F4.2,", ",F4.2,", ",F4.2)', /PRINT))
  th = TEXT(offset+0.115, 0.825, 'MBE ='+STRING(MBE, FORMAT='(" ",F6.2,", ",F6.2,", ",F6.2)', /PRINT))
  th = TEXT(offset+0.115, 0.785, 'n = ' + STRTRIM(N_ELEMENTS([indCRO, indGRA]),2)+", "+STRTRIM(N_ELEMENTS(indCRO),2)+", "+STRTRIM(N_ELEMENTS(indGRA),2))
  ;legend for lat
  cbar = COLORBAR(RGB_TABLE=ctable, ORIENTATION = 1, POSITION = [0.955, 0.3, 0.98, 0.6], RANGE=[MIN(meanLat_GPP),MAX(meanLat_GPP)], TITLE='Latitude N (deg)',FONT_SIZE=fs,TAPER=1,/NORMAL)
  ;scatter MOD
  hs_frame.save, out_dir + '\' + 'AAA_scatter_of_site_GPP_mean.png', BORDER=10, RESOLUTION=300
  hs_frame.close
  RETURN, 0
END