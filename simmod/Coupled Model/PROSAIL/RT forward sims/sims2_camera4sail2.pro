PRO simS2_Camera4SAIL2

;#################### Some common and variable inizialization needed for PROSAIL
@cb_various.comm
@cb_prospect_last_parameters.comm      ;last parameters
lCab=-999 & lCar=-999 & lCbrown=-999 & lCw=-999 & lCm =-999 & lNs =-999
@cb_prosail_data.comm                  ;skyl and others
skyl = dataSkyle()
data = dataSpec_P5B()
@cb_sail_last_parameters.comm ; last parameters
llai = -999 & lhspot = -999 & ltts =  -999 & ltto =  -99 & lpsi = -999
@cb_ladgen.comm                        ;ladgen input/output
llidf_a = -999 & llidf_b=-999 & lidf=-999
wl = INDGEN(2500-400+1, /FLOAT) + 400.0
;#################### Load parameter from inversion data and set outpit dir
tmp = READ_CSV('X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\DATA4RT\All relevant data.csv', $
                HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN)
inv = rename_tags(tmp, TAG_NAMES(tmp), hdr)
outdir = 'X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\PROSAIL_sim'
;laiVal = [0, 0.1, 0.2, 0.3, 0.5, 1, 2, 2.5, 3, 3.5, 2, 1, 0.5, 0.1, 0]
;#################### load s2 spectral responses
sr = SR_S2A_red_nir400_2500()
subB = WHERE(sr.bSRnorm GT 0.0)
subG = WHERE(sr.gSRnorm GT 0.0)
subR = WHERE(sr.rSRnorm GT 0.0)
subNIR = WHERE(sr.nirSRnorm GT 0.0)
;#################### load camera spectral responses
sr_cam = SR_NIKON_coolpix()
subB_cam = WHERE(sr_cam.bSRnorm GT 0.0)
subG_cam = WHERE(sr_cam.gSRnorm GT 0.0)
subR_cam = WHERE(sr_cam.rSRnorm GT 0.0)
;#################### Temporary: define missing variable (not provided so far)
fix_var_but_lai = 1   ;0 to use rt results, 1 to use their mean (does not apply to lai)
smoothLai = 1         ;1 will smooth it
rsoil_fn = 'X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\envi_jhu_lib_dark_grayish_brown_sily_loam.csv'
tmp = READ_CSV(rsoil_fn, HEADER=hdr)
rsoil = INTERPOL(tmp.field2, tmp.field1, wl)
;Cab    = 29.0d  ; chlorophyll content (µg.cm-2) (0-100 ug cm-2) for grassland (Atzberger et al., Suitability and
Car    = 0.0d   ; 12.0d    ;; carotenoid content (µg.cm-2)  (0-25 ug cm-2)
Cbrown = 0.0d  ;0.5d     ;; brown pigment content (0-1 arbitrary units)
;Cw     = 0.015  ;0.02, 0.01;0.015d   ;; EWT (cm) - Equivalent water thickness (0-0.05 cm) Vohland and Jarmer 2008
;Cm     = 0.005  ;0.1d     ;; LMA (g cm-2) - leaf mass per unit leaf area
;Cm  (0.005-0.01 g cm-2 according to Darvishzadeh et al, 2008, RSE)
Ns     = 2.0; 2.0d    ;; structure coefficient (1-3 dimensionless) Vohland and Jarmer 2008
  ; % LIDFa LIDF parameter a, which controls the average leaf slope
  ; % LIDFb LIDF parameter b, which controls the distribution's bimodality
  ; % LIDF type     a      b
  ; % Planophile    1      0
  ; % Erectophile   -1     0
  ; % Plagiophile   0      -1
  ; % Extremophile  0      1
  ; % Spherical     -0.35  -0.15
  ; % Uniform       0      0
  ; %   requirement: |LIDFa| + |LIDFb| <= 1
lidf_a = -1.0; -0.35; -1      
lidf_b = 0.0;-0.15; 0  sperical corresponding to a ala = 60      
hspot  = 0.01; 0.05d      ; hot spot for grass (Atzberger et al., as above)
fb = 2/3.0;4/5.0;3.0/4.0
diss = 1.0
Cv = 1.0
zeta = 0.0
;#################### Overpass charcteristics
VZAsat = 0
VAAsat = 0
RAAsat = 0
VZAcam = 70; 70, 82 ;looking down 8 deg or 8+12.5 about 20
VAAcam = 0 ;facing North
satOverpassHoursGmt = 10
satOverpassMinutesGmt = 50
lat = 53.49208   ;mean of all camera locations
lon = 6.26986
;#################### RUN on all cameras
envi
;find unique fids
uniqueFids = uniqlist(inv.fid)


FOR f  = 0, N_ELEMENTS(uniqueFids)-1 DO BEGIN
  JD = !NULL
  GCC_sat = !NULL
  GCC_cam = !NULL
  GCC_cam_camSR = !NULL
  ndvi_sat = !NULL
  ndvi_cam = !NULL
  laiTs = !NULL
  ind = WHERE(inv.fid EQ uniqueFids[f], count)
  FOR i = 0, count-1 DO BEGIN
    DD = inv.day[ind[i]]
    MM = inv.month[ind[i]]
    YY = inv.year[ind[i]]
    JD = [JD, JULDAY(MM, DD, YY, satOverpassHoursGmt, satOverpassMinutesGmt)]
  ENDFOR
  IF (smoothLai EQ 1) THEN BEGIN
    ;make it regularly gridded (no data where ther was not)
    JD_regular = FINDGEN(JD[-1]-JD[0]+1)+JD[0]
    lai_interpol = INTERPOL(inv.lai[ind], JD, JD_regular, /SPLINE, /NAN)
    lai_interpol = lai_interpol > 0
    nodata = JD_regular * 0.0 +1.0
    indFinLai = WHERE(FINITE(inv.lai[ind]))
    JDwithFinLAI = JD[indFinLai]
    
    FOR j = 0, N_ELEMENTS(JDwithFinLAI)-1 DO BEGIN
      iind = WHERE(JD_regular EQ JDwithFinLAI[j])
      nodata[iind] = 0
    ENDFOR
    ;set start to zero for savgol
    lai_interpol[0] = 0 
    nodata[0] = 0 
    SAGO_INTERPOL_large_width,lai_interpol*100, nodata, iNDVI=lai1, smNDVI=lai2
;    h0 = plot(JD_regular, lai2/100.0)
;    h1 = plot(JD, inv.lai[ind], LINESTYLE='', SYMBOL='x', COLOR='r', OVERPLOT=1)
    storeiind = !NULL
    FOR j = 0, N_ELEMENTS(JD)-1 DO BEGIN
      iind = WHERE(JD_regular EQ JD[j])
      storeiind = [storeiind, iind]
    ENDFOR   
    lai_smooth = lai2[storeiind]/100.0
  ENDIF
  ;identify lai max
  res = MAX(lai_smooth, sub_lai_max)
  sub_lai_max = MIN(sub_lai_max)

  FOR i = 0, count-1 DO BEGIN
    GMTtime = FIX(STRTRIM(satOverpassHoursGmt,2) + STRTRIM(satOverpassMinutesGmt,2))
    DD = inv.day[ind[i]]
    MM = inv.month[ind[i]]
    YY = inv.year[ind[i]]
    SA = ENVI_COMPUTE_SUN_ANGLES(DD, MM, YY, GMTtime, lat, lon) ;solar elevation and solar azimuth angles
    SZA = 90.0 - SA[0]
    SAA = SA[1] 
    RAAcam = SAA - VAAcam ;alway positive, check that it is not 
    IF (smoothLai EQ 1) THEN BEGIN
      lai = lai_smooth[i]
      laiTs = [laiTs, lai]
    ENDIF ELSE BEGIN
      lai = inv.lai[ind[i]]
      laiTs = [laiTs, lai]
    ENDELSE
    IF (fix_var_but_lai EQ 1) THEN BEGIN
      Cab = MEAN(inv.chl[ind], /NAN)
      Cm = MEAN(inv.cm[ind], /NAN)
      Cw = MEAN(inv.cw[ind], /NAN)
      ;from that point till the end lineraly reduce chl
      ;CabTopAfterMax = 0;Cab/2.0
      t = 45 ;half life
      CabTopAfterMax = Cab/1.0 * EXP(-(JD[sub_lai_max:-1]-JD[sub_lai_max])/t)
      
      ;CabTopAfterMax = INTERPOL([Cab/2.0,0.0],[JD[sub_lai_max],JD[-1]],JD[sub_lai_max:-1])
    ENDIF ELSE BEGIN
      Cab = inv.chl[ind[i]]
      Cm = inv.cm[ind[i]]
      Cw = inv.cw[ind[i]]
    ENDELSE
    
    IF (FINITE(lai) EQ 1) THEN BEGIN
      ;satellite settings
      IF (i GT sub_lai_max) THEN BEGIN
        ;after max (senescent top layer)                                                          fb,diss,Cv,zeta
        IDL_PRO4SAIL2, CabTopAfterMax[i-sub_lai_max], 0.0 ,Cbrown,Cw,Cm,Ns,Cab,Car,Cbrown,Cw,Cm,Ns, lidf_a,lidf_b,lai,hspot,fb,diss,Cv,zeta, SZA,VZAsat,RAAsat,rsoil,resh,resv,absh,absa 
      ENDIF ELSE BEGIN 
        ;before max all equal
        IDL_PRO4SAIL2, Cab,Car,Cbrown,Cw,Cm,Ns,Cab,Car,Cbrown,Cw,Cm,Ns, lidf_a,lidf_b,lai,hspot,fb,diss,Cv,zeta, SZA,VZAsat,RAAsat,rsoil,resh,resv,absh,absa
      ENDELSE
      ;PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b,lai,hspot,SZA,VZAsat,RAAsat,rsoil,resh,resv,absh,absa
      ;resample to S2 spectral bands
      b_band = TOTAL(resv[subB] * sr.bSRnorm[subB])
      g_band = TOTAL(resv[subG] * sr.gSRnorm[subG])
      r_band = TOTAL(resv[subR] * sr.rSRnorm[subR])
      nir_band = TOTAL(resv[subNIR] * sr.nirSRnorm[subNIR])
      GCC_sat = [GCC_sat, g_band/(b_band+g_band+r_band)]
      ndvi_sat = [ndvi_sat, (nir_band - r_band)/(nir_band + r_band)]
      
      ;camera settings
      IF (i GT sub_lai_max) THEN $
        ;after max (senescent top layer)                                                          fb,diss,Cv,zeta
        IDL_PRO4SAIL2, CabTopAfterMax[i-sub_lai_max], 0.0 ,Cbrown,Cw,Cm,Ns,Cab,Car,Cbrown,Cw,Cm,Ns, lidf_a,lidf_b,lai,hspot,fb,diss,Cv,zeta, SZA,VZAcam,RAAcam,rsoil,resh,resv,absh,absa ELSE $
        ;before max all equal
        IDL_PRO4SAIL2, Cab,Car,Cbrown,Cw,Cm,Ns,Cab,Car,Cbrown,Cw,Cm,Ns, lidf_a,lidf_b,lai,hspot,fb,diss,Cv,zeta, SZA,VZAcam,RAAcam,rsoil,resh,resv,absh,absa
      ;PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b,lai,hspot,SZA,VZAcam,RAAcam,rsoil,resh,resv,absh,absa
      b_band = TOTAL(resv[subB] * sr.bSRnorm[subB])
      g_band = TOTAL(resv[subG] * sr.gSRnorm[subG])
      r_band = TOTAL(resv[subR] * sr.rSRnorm[subR])
      nir_band = TOTAL(resv[subNIR] * sr.nirSRnorm[subNIR])
      GCC_cam = [GCC_cam, g_band/(b_band+g_band+r_band)]
      ndvi_cam = [ndvi_cam, (nir_band - r_band)/(nir_band + r_band)]
      PRINT, lai,  GCC_sat[-1], GCC_cam[-1], SZA
      ;camera spectral res
      b_band = TOTAL(resv[subB_cam] * sr_cam.bSRnorm[subB_cam])
      g_band = TOTAL(resv[subG_cam] * sr_cam.gSRnorm[subG_cam])
      r_band = TOTAL(resv[subR_cam] * sr_cam.rSRnorm[subR_cam])
      GCC_cam_camSR  = [GCC_cam_camSR , g_band/(b_band+g_band+r_band)]
    ENDIF ELSE BEGIN
      GCC_sat = [GCC_sat, !VALUES.F_NAN]
      ndvi_sat = [ndvi_sat, !VALUES.F_NAN]
      GCC_cam = [GCC_cam, !VALUES.F_NAN]
      GCC_cam_camSR = [GCC_cam_camSR , !VALUES.F_NAN]
      ndvi_cam = [ndvi_cam, !VALUES.F_NAN]
    ENDELSE
  ENDFOR
  dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
  xminor = 2        ;monor ticks between majors
  nmajor = 4
  margin = [0.15, 0.15, 0.15, 0.25]
  xrange = [MIN(JD)-10, MAX(JD+10)]
  yrange = [0.0,1.0]
  
  ghs0 = PLOT(JD, ndvi_sat, YTITLE='NDVI, LAIx0.1', COLOR = 'black', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XRANGE=xrange, YRANGE=yrange, $ 
            TITLE = 'Fid '+ STRTRIM(uniqueFids[f],2), XMINOR=xminor, XMAJOR = nmajor, $
            MARGIN = margin, AXIS_STYLE = 1, /NODATA)

  ghs1 = PLOT(JD, ndvi_sat, Name='NDVIsat', LINESTYLE='--', SYMBOL ='o', OVERPLOT = 1, COLOR='PURPLE')
  ghlai = PLOT(JD, laiTs/10.0,  NAME='LAIx0.1', COLOR='BLUE', OVERPLOT = 1, LINESTYLE='-', SYMBOL ='D')
  strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
  loc = -(yrange[1]-yrange[0])/10
  a_x =AXIS('X', TARGET = ghs0, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-JD[0]+JD2DOY(JD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=0.03, TICKNAME=strlabel)
  
  ;ghlai = PLOT(JD, laiTs,  NAME='LAI', /CURRENT, COLOR='BLUE', YRANGE = [0,10], MARGIN = margin, AXIS_STYLE = 0, LINESTYLE='-', SYMBOL ='D', XRANGE=xrange)
  gccrange = [0.3,0.6]
  ghs2 = PLOT(JD, GCC_sat, NAME='GCC sat', LINESTYLE='--', SYMBOL ='o', COLOR='POWDER BLUE', /CURRENT, YRANGE = gccrange, MARGIN = margin, AXIS_STYLE = 0, XRANGE=xrange)
  ;ghc1 = PLOT(JD, NDVI_cam, NAME='NDVI cam', LINESTYLE='-', SYMBOL ='+', COLOR='RED', OVERPLOT = 1)
  ghc2 = PLOT(JD, GCC_cam, NAME='GCC cam S2SR', LINESTYLE='-', SYMBOL ='o', COLOR='DARK GREEN', /CURRENT, YRANGE = gccrange, MARGIN = margin, AXIS_STYLE = 0, XRANGE=xrange)
  ;ghc3 = PLOT(JD, GCC_cam_camSR, NAME='GCC cam camSR', LINESTYLE='-', SYMBOL ='*', COLOR='PURPLE', /CURRENT, YRANGE = gccrange, MARGIN = margin, AXIS_STYLE = 0, XRANGE=xrange)
  a_t = AXIS('y', TARGET = ghs2,  TITLE = 'GCC', YRANGE=gccrange, TICKDIR=1, TEXTPOS = 1, LOCATION = [max(ghs1.xrange),0,0])
  ;!null = LEGEND(target=[ghs1, ghs2, ghc1, ghc2, ghc3, ghlai], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.3, TRANSPARENCY=100)
  ;!null = LEGEND(target=[ghs1, ghs2, ghc2, ghc3, ghlai], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.3, TRANSPARENCY=100)
  !null = LEGEND(target=[ghs1, ghs2, ghc2, ghlai], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.3, TRANSPARENCY=100)
  IF (fix_var_but_lai EQ 1) THEN suffix = 'meanVarExceptLAI_smoothed_sail2layer' ELSE  suffix = ''
  ghs0.save, outdir + '\' + suffix + 'Fid '+ STRTRIM(uniqueFids[f],2) + '.bmp'
  PRINT, STRTRIM(uniqueFids[f],2) 
ENDFOR

PRINT, 'ok'

END