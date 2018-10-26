PRO handler_multi
 ss = [1167,924,1009,1002,997,1006,1006,1203] ;
 ll = [389, 190,155, 319, 320,121, 122, 379]
 names = ['kenya_bad','spain','italy','sahel_good','sahel_bad','germany_bad','germany_good','somalia']
 FOR i = 0, N_ELEMENTS(ss)-1 DO BEGIN
  UL11coordinatesEN = [-180, 75]
  pixsize = 0.1875
  res = make_multi_stage_plot(ss[i],ll[i],'z',names[i],UL11coordinatesEN, pixsize)
  res = make_multi_stage_plot(ss[i],ll[i],'n',names[i],UL11coordinatesEN, pixsize)
  res = make_multi_stage_plot(ss[i],ll[i],'v',names[i],UL11coordinatesEN, pixsize)
 ENDFOR
END


FUNCTION make_multi_stage_plot, s, l, v, suffix, UL11coordinatesEN, pixsize
  ;designed for test_boku_o_stages
  ;for given sample and line (s, l), it plots temporal profile of ndvi (all stages) and z-score or other anomaly (v)
  reduced_range = 1 ; also make the same for the last 2 years
  ;example
  ;res = make_multi_stage_plot(1167,389,'z')
  ;res = make_multi_stage_plot(1167,389,'n')
  ;res = make_multi_stage_plot(1167,389,'v')
  CASE v OF
    'z': a_thresold = -1
    'n': a_thresold = 15.87
    'v': a_thresold = 35
    ELSE: STOP
  ENDCASE
  
  ;filenames
  outdir = 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\Results\Profiles\'
  root_dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\'
  root_dir_stat_img = 'X:\works\pubblicazioni\in preparazione\2017 Test Consolidation Stages\Results\all2\' 
  str_period = '2003-2016'
  ;ndvi
  stages = ['O0','O1','O2','O3','O4','OF']
  ndvi_fns = root_dir +  stages + str_period + '_bil_sc'
  ;anomaly
  anomaly_dir = root_dir + v + '2\'
  nxhx = TRANSPOSE('N' + STRTRIM(INDGEN(5),2) +'L' + STRTRIM(INDGEN(5),2))
  nxhf = TRANSPOSE('N' + STRTRIM(INDGEN(5),2) +'Lf')
  a_NxHx_fns = anomaly_dir + v + nxhx + '_' + str_period +'_bsq.img'
  a_NxHf_fns = anomaly_dir + v + nxhf + '_' + str_period +'_bsq.img'
  a_NfHf_fn = anomaly_dir + v + 'NfLf' + '_' + str_period +'_bsq.img'
  ;prepare profile variable
  npoints = 504
  ;ndvi structure
  x_zPro = CREATE_STRUCT('O0',FLTARR(npoints),'O1',FLTARR(npoints),'O2',FLTARR(npoints),'O3',FLTARR(npoints),'O4',FLTARR(npoints),'O5',FLTARR(npoints))
  a_NxHxPro =CREATE_STRUCT('O0',FLTARR(npoints),'O1',FLTARR(npoints),'O2',FLTARR(npoints),'O3',FLTARR(npoints),'O4',FLTARR(npoints))
  a_NxHfPro =CREATE_STRUCT('O0',FLTARR(npoints),'O1',FLTARR(npoints),'O2',FLTARR(npoints),'O3',FLTARR(npoints),'O4',FLTARR(npoints))
  a_NfHfPro =CREATE_STRUCT('Ofin',FLTARR(npoints))
  ;get data
  FOR i = 0, N_ELEMENTS(stages)-1 DO   x_zPro.(i) = zprofile_bil_bsq(ndvi_fns[i], s, l)
  FOR i = 0, N_ELEMENTS(stages)-2 DO BEGIN
    a_NxHxPro.(i) = zprofile_bil_bsq(a_NxHx_fns[i], s, l)
    a_NxHfPro.(i) = zprofile_bil_bsq(a_NxHf_fns[i], s, l)
  ENDFOR
  a_NfHfPro.Ofin = zprofile_bil_bsq(a_NfHf_fn, s, l)
  ;plot the data
  ;make time axis
  nyears = 14
  deks = !NULL
  yrs = !NULL
  FOR i = 0, nyears-1 DO deks = [deks,INDGEN(36)+1]
  FOR i = 0, nyears-1 DO yrs = [yrs, MAKE_ARRAY(36, VALUE=2003+i)]
  doys = !NULL
  FOR i = 0, N_ELEMENTS(deks)-1 DO doys = [doys, DekYear2centralDOY(deks[i], yrs[i])]
  jds = !NULL
  FOR i = 0, N_ELEMENTS(doys)-1 DO jds = [jds, DOY_YEAR2JD(doys[i], yrs[i])]


  ;now deterine when the pixel is active
  ;first make the avg year
  active36 = FLTARR(36)
  FOR i = 1, 36 DO BEGIN
    tt = STRING(i, FORMAT = '(I02)')
    tmp = FLOAT(ReadEnviWithHdr(root_dir+'pheno\active\active1962'+tt+'.img'))
    active36[i-1] = tmp[s,l]
  ENDFOR


  ;make the full time series:
  ;concatenate 14 years on z axis, in this way I have a full year (36 deks), repeated for 14 year
  active = !NULL
  FOR i = 1, 14 DO active =  [active,active36]
  ;turn 1 when not active, nan otherwise
  offseason = active
  indNA = WHERE(active NE 1)
  indA = WHERE(active EQ 1)
  offseason[indNA] = 1
  offseason[indA] = !VALUES.F_NAN


  ;test compute zNfHf here from x_zPro.O5
  testZNfHf = FLTARR(N_ELEMENTS(x_zPro.O5))
  xlta36 = FLTARR(36)
  xltsd36 = FLTARR(36)
  min36 = FLTARR(36)
  max36 = FLTARR(36)
  med36 = FLTARR(36)
  lwrQrt36  = FLTARR(36)
  uppQrt36  = FLTARR(36)
  FOR i = 0, 35 DO BEGIN
    ttIndex = INDGEN(14)*36+i
    xlta36[i] = MEAN(x_zPro.O5[ttIndex],/NAN)
    xltsd36[i] = STDDEV(x_zPro.O5[ttIndex],/NAN)
    indFin = WHERE(FINITE(x_zPro.O5[ttIndex]))
    tmp = CREATEBOXPLOTDATA(x_zPro.O5[ttIndex[indFin]])
    min36[i] = tmp[0]
    max36[i] = tmp[4]
    med36[i] = tmp[2]
    lwrQrt36[i] = tmp[1]
    uppQrt36[i] =  tmp[3]
  ENDFOR
  xlta = !NULL
  xltsd = !NULL
  xltmin = !NULL
  xltmax = !NULL
  xltmed = !NULL
  xltlwrQrt  = !NULL
  xltuppQrt  = !NULL
  FOR i = 1, 14 DO BEGIN
    xlta = [xlta, xlta36]
    xltsd = [xltsd, xltsd36]
    xltmin = [xltmin, min36]
    xltmax = [xltmax, max36]
    xltmed = [xltmed, med36]
    xltLwrQrt  = [xltlwrQrt, lwrQrt36]
    xltUppQrt  = [xltuppQrt, uppQrt36]
  ENDFOR
  testZNfHf = (x_zPro.O5-xlta)/xltsd
  IF (v EQ 'z') THEN IF (TOTAL(a_NfHfPro.Ofin - testZNfHf) GT 0.001) THEN STOP
  
  ;get the stats (detection and false alarm rate, hss) for the point
  statsN0H0 = CREATE_STRUCT('DR', 0.0, 'FAR', 0.0, 'HSS', 0.0, 'MAE', 0.0)
  statsN0Hf = CREATE_STRUCT('DR', 0.0, 'FAR', 0.0, 'HSS', 0.0, 'MAE', 0.0)
  statsN0H0.DR  = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0H0_veg_active_DETECTION_RATE.img', s, l)
  statsN0H0.FAR = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0H0_veg_active_FALSE_ALARM_RATE.img', s, l)
  statsN0H0.HSS = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0H0_veg_active_HSS.img', s, l)
  statsN0H0.MAE = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0H0_mean_delta_veg_active.img', s, l)
  
  statsN0Hf.DR  = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0Hf_veg_active_DETECTION_RATE.img', s, l)
  statsN0Hf.FAR = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0Hf_veg_active_FALSE_ALARM_RATE.img', s, l)
  statsN0Hf.HSS = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0Hf_veg_active_HSS.img', s, l)
  statsN0Hf.MAE = zprofile_bil_bsq(root_dir_stat_img  + v + '\' + v + 'N0Hf_mean_delta_veg_active.img', s, l)
  
  SAVE, /ALL, FILENAME='E:\buttami.sav'
  ;RESTORE, 'E:\buttami.sav'

  ;3 panels, ndvi aNxHX, aNxHf
  dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
  fs = 11 ;font size
  ;lp0 = [1.16, 1.05];]0.95] ;legend position
  lp = [1, 1.05];0.85] ;legend position
  dm = [900,900]  ;dimensions of the window
  xminor = 3        ;monor ticks between majors
  
  colors = ['green','orange','red','blue','purple','black']
  mrgin = [0.1,0.25,0.12,0.05]
  thick = 2
  colorBelowThresh = 'Dark Red'
  colorNotActive = 'Light Grey'
  filltras = 20
  FOR j = 0, reduced_range DO BEGIN
    xrange = [jds[0], jds[-1]]
    indOfFirst = 0
    IF (j EQ 1) THEN BEGIN
      xrange[0] = DOY_YEAR2JD(1, 2015)
      res = MIN(ABS(jds - xrange[0]), indOfFirst)
    ENDIF
    ;ndvi
    yrange =[MIN(x_zPro.O0[indOfFirst:-1], /NAN)- 0.05, MIN([1.0, MAX(x_zPro.O0[indOfFirst:-1], /NAN) + 0.1])]
    East = UL11coordinatesEN[0] + pixsize * s
    North = UL11coordinatesEN[1] - pixsize * l
    ;add LTA + / - sd 
    gh = PLOT(jds,xlta,COLOR='Plum',LAYOUT=[1,3,1],XRANGE=xrange,YRANGE=yrange,DIMENSIONS=dm,XMINOR=xminor,XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
      YTITLE='NDVI', XTITLE='Time', TITLE = 'Sample, line: ' + STRTRIM(s,2) + ', ' + STRTRIM(l,2) + $
      '. UL coordinates: ' + STRING(East, FORMAT = '(f7.3)') + ' East, ' + STRING(North, FORMAT = '(f7.3)') + ' North.', $
      MARGIN=mrgin + [0,0,0,0.1], FONT_SIZE = 12, XTICKDIR=1, YTICKDIR=1, AXIS_STYLE=1, /NODATA)
    gha = AXIS('X',LOCATION='top', TARGET=gh, SHOWTEXT=0, TICKLEN=0)
    gha = AXIS('Y',LOCATION='right', TARGET=gh, SHOWTEXT=0, TICKLEN=0)
    CASE v OF
      'z': BEGIN
        upper = xlta+xltsd
        bottom = xlta-xltsd
      END
      'n': BEGIN
        upper = xltLwrQrt  
        bottom = xltUppQrt  
      END
      'v': BEGIN
        upper = xltmin 
        bottom = xltmax 
      END
    ENDCASE
    poly = POLYGON([jds,reverse(jds)], [upper,reverse(bottom)], $  ;[REPLICATE(-0.01,504*2)], $   
      /DATA, /FILL_BACKGROUND, $
      COLOR = 'Plum', FILL_COLOR="Plum", PATTERN_ORIENTATION=45, $
      PATTERN_SPACING=4)
    CASE v OF
      'z': gh6 = PLOT(jds,xlta, NAME=   'Avg_SD',/OVERPLOT, COLOR='Plum', THICK = thick)
      'n': gh6 = PLOT(jds,xltmed , NAME='Med_Qrt',/OVERPLOT, COLOR='Plum', THICK = thick)
      'v': gh6 = PLOT(jds,upper , NAME= 'Min_Max',/OVERPLOT, COLOR='Plum', THICK = thick)
    ENDCASE
    
    gh0 = PLOT(jds,x_zPro.O0, NAME=stages[0],/OVERPLOT, COLOR=colors[0])
    gh1 = PLOT(jds,x_zPro.O1, NAME=stages[1],/OVERPLOT, COLOR=colors[1])
    gh2 = PLOT(jds,x_zPro.O2, NAME=stages[2],/OVERPLOT, COLOR=colors[2])
    gh3 = PLOT(jds,x_zPro.O3, NAME=stages[3],/OVERPLOT, COLOR=colors[3])
    gh4 = PLOT(jds,x_zPro.O4, NAME=stages[4],/OVERPLOT, COLOR=colors[4])
    gh5 = PLOT(jds,x_zPro.O5, NAME=stages[5],/OVERPLOT, COLOR=colors[5]);, THICK = thick)
    gh0b = PLOT(jds,yrange[1]*offseason, COLOR=colorNotActive, FILL_BACKGROUND=1, FILL_TRANSPARENCY = filltras,FILL_LEVEL = yrange[0], FILL_COLOR = colorNotActive, /OVERPLOT)
    !null = LEGEND(target=[gh0,gh1,gh2,gh3,gh4,gh5,gh6], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.025, HORIZONTAL_SPACING = 0.02, HORIZONTAL_ALIGNMENT = 'LEFT', TRANSPARENCY=100)
  
    ;aNxHf
    yrange =[MIN(a_NxHfPro.O0[indOfFirst:-1], /NAN) - 0.1, MAX(a_NxHfPro.O0[indOfFirst:-1], /NAN) + 0.1]
    gh0 = PLOT(jds,jds*0+a_thresold, COLOR=colorBelowThresh, FILL_BACKGROUND=0, FILL_LEVEL = yrange[0], FILL_COLOR = colorBelowThresh, LAYOUT=[1,3,2], XRANGE=xrange, YRANGE=yrange, DIMENSIONS=dm, XMINOR=xminor, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
      YTITLE=v+'NxHf', XTITLE='Time', MARGIN=mrgin, FONT_SIZE = 12, XTICKDIR=1, YTICKDIR=1, AXIS_STYLE=1, /CURRENT, $
      TITLE='N0Hf->DR ' + STRING(statsN0Hf.DR,FORMAT='(f6.2)') + ', FAR ' + STRING(statsN0Hf.FAR,FORMAT='(f6.2)') + $ 
      ', HSS ' + STRING(statsN0Hf.HSS,FORMAT='(f5.2)') + ', MAE ' + STRING(statsN0Hf.MAE,FORMAT='(f5.2)'))
    gha = AXIS('X',LOCATION='top', TARGET=gh0, SHOWTEXT=0, TICKLEN=0)
    gha = AXIS('Y',LOCATION='right', TARGET=gh0, SHOWTEXT=0, TICKLEN=0)
    gh0 = PLOT(jds,a_NxHfPro.O0, NAME=stages[0],/OVERPLOT, COLOR=colors[0])
    gh1 = PLOT(jds,a_NxHfPro.O1, NAME=stages[1],/OVERPLOT, COLOR=colors[1])
    gh2 = PLOT(jds,a_NxHfPro.O2, NAME=stages[2],/OVERPLOT, COLOR=colors[2])
    gh3 = PLOT(jds,a_NxHfPro.O3, NAME=stages[3],/OVERPLOT, COLOR=colors[3])
    gh4 = PLOT(jds,a_NxHfPro.O4, NAME=stages[4],/OVERPLOT, COLOR=colors[4])
    gh5 = PLOT(jds,a_NfHfPro.Ofin, NAME=stages[5],/OVERPLOT, COLOR=colors[5]);, SYMBOL='.');, THICK = thick)
    indSeas = WHERE((active EQ 1) AND (jds GE  xrange[0]))
    indEvent = WHERE(a_NfHfPro.Ofin[indSeas] LE a_thresold, countEvent)
    IF (countEvent GT 0) THEN $
      ghEvent = PLOT(jds[indSeas[indEvent]],jds[indSeas[indEvent]]*0+yrange[0]-(yrange[1]-yrange[0])/30.0, NAME='Event',/OVERPLOT, COLOR=colors[5], LINESTYLE=' ', SYMBOL = '|', CLIP = 0);, THICK = thick)
    indN0HfEvent = WHERE(a_NxHfPro.O0[indSeas] LE a_thresold, countEvent)
    IF (countEvent GT 0) THEN $
      ghEvent = PLOT(jds[indSeas[indN0HfEvent]],jds[indSeas[indN0HfEvent]]*0+yrange[0]+(yrange[1]-yrange[0])/30.0, NAME='N0HfEvent',/OVERPLOT, COLOR=colors[0], LINESTYLE=' ', SYMBOL = '|');, THICK = thick)
      ;gh6 = PLOT(jds,testZNfHf, NAME='TestZFF',/OVERPLOT, COLOR='Light grey')
    gh0b = PLOT(jds,yrange[1]*offseason, COLOR=colorNotActive, FILL_BACKGROUND=1, FILL_TRANSPARENCY = filltras, FILL_LEVEL = yrange[0], FILL_COLOR = colorNotActive, /OVERPLOT)
    !null = LEGEND(target=[gh0,gh1,gh2,gh3,gh4,gh5], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.025, HORIZONTAL_SPACING = 0.02, HORIZONTAL_ALIGNMENT = 'LEFT', TRANSPARENCY=100)
  
    ;aNxHx
    yrange =[MIN(a_NxHxPro.O0[indOfFirst:-1], /NAN) - 0.1, MAX(a_NxHxPro.O0[indOfFirst:-1], /NAN) + 0.1]
    gh0 = PLOT(jds,jds*0+a_thresold, COLOR=colorBelowThresh, FILL_BACKGROUND=0, FILL_LEVEL = yrange[0], FILL_COLOR = colorBelowThresh, LAYOUT=[1,3,3], XRANGE=xrange, YRANGE=yrange, DIMENSIONS=dm, XMINOR=xminor, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
      YTITLE=v+'NxHx', XTITLE='Time', MARGIN=mrgin, FONT_SIZE = 12, XTICKDIR=1, YTICKDIR=1, AXIS_STYLE=1, /CURRENT, $
      TITLE='N0H0->DR ' + STRING(statsN0H0.DR,FORMAT='(f6.2)') + ', FAR ' + STRING(statsN0H0.FAR,FORMAT='(f6.2)') + $
      ', HSS ' + STRING(statsN0H0.HSS,FORMAT='(f5.2)') + ', MAE ' + STRING(statsN0H0.MAE,FORMAT='(f5.2)'))
    gha = AXIS('X',LOCATION='top', TARGET=gh0, SHOWTEXT=0, TICKLEN=0)
    gha = AXIS('Y',LOCATION='right', TARGET=gh0, SHOWTEXT=0, TICKLEN=0)
    gh0 = PLOT(jds,a_NxHxPro.O0, NAME=stages[0],/OVERPLOT, COLOR=colors[0])
    gh1 = PLOT(jds,a_NxHxPro.O1, NAME=stages[1],/OVERPLOT, COLOR=colors[1])
    gh2 = PLOT(jds,a_NxHxPro.O2, NAME=stages[2],/OVERPLOT, COLOR=colors[2])
    gh3 = PLOT(jds,a_NxHxPro.O3, NAME=stages[3],/OVERPLOT, COLOR=colors[3])
    gh4 = PLOT(jds,a_NxHxPro.O4, NAME=stages[4],/OVERPLOT, COLOR=colors[4])
    gh5 = PLOT(jds,a_NfHfPro.Ofin, NAME=stages[5],/OVERPLOT, COLOR=colors[5]);, THICK = thick)
    indSeas = WHERE((active EQ 1) AND (jds GE  xrange[0]))
    indEvent = WHERE(a_NfHfPro.Ofin[indSeas] LE a_thresold, countEvent)
    IF (countEvent GT 0) THEN $
      ghEvent = PLOT(jds[indSeas[indEvent]],jds[indSeas[indEvent]]*0+yrange[0]-(yrange[1]-yrange[0])/30.0, NAME='Event',/OVERPLOT, colors[5], LINESTYLE=' ', SYMBOL = '|', CLIP=0);, THICK = thick)
    indN0H0Event = WHERE(a_NxHxPro.O0[indSeas] LE a_thresold, countEvent)
    IF (countEvent GT 0) THEN $
      ghEvent = PLOT(jds[indSeas[indN0H0Event]],jds[indSeas[indN0H0Event]]*0+yrange[0]+(yrange[1]-yrange[0])/30.0, NAME='N0H0Event',/OVERPLOT, COLOR=colors[0], LINESTYLE=' ', SYMBOL = '|');, THICK = thick)
    ;gh6 = PLOT(jds,testZNfHf, NAME='TestZFF',/OVERPLOT, COLOR='Light grey')
    gh0b = PLOT(jds,yrange[1]*offseason, COLOR=colorNotActive, FILL_BACKGROUND=1, FILL_TRANSPARENCY = filltras, FILL_LEVEL = yrange[0], FILL_COLOR = colorNotActive, /OVERPLOT)
    !null = LEGEND(target=[gh0,gh1,gh2,gh3,gh4,gh5], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
      SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.025, HORIZONTAL_SPACING = 0.02, HORIZONTAL_ALIGNMENT = 'LEFT', TRANSPARENCY=100)
    rangetxt = ['full', 'last2years']  
    gh.save, outdir + 's_' + STRTRIM(s,2) + 'l_'+STRTRIM(l,2)+'_'+ v + '_profile_'+suffix+'_'+rangetxt[j]+'.png'
    gh.close
  ENDFOR 
  CLOSE, /ALL
  RETURN, 0
END