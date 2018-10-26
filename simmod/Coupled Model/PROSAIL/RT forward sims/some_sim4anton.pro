PRO some_sim4Anton
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

laiVal = [0, 0.1, 0.2, 0.3, 0.5, 1, 2, 2.5, 3, 3.5, 2, 1, 0.5, 0.1, 0]
rsoil_fn = 'X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\envi_jhu_lib_dark_grayish_brown_sily_loam.csv'
tmp = READ_CSV(rsoil_fn, HEADER=hdr)
wl = INDGEN(2500-400+1, /FLOAT) + 400.0
rsoil = INTERPOL(tmp.field2, tmp.field1, wl)
;gh = PLOT(tmp.field1, tmp.field2, XRANGE=[400,2500])
;gh1 = PLOT(wl, rsoil, color='r', OVERPLOT = 1)
;more or less from http://www-personal.acfr.usyd.edu.au/ddan1654/bongiorno2013spectral.pdf
sub_wl_b = WHERE((wl GE 440) AND (wl LE 538))
sub_wl_g = WHERE((wl GE 475) AND (wl LE 575))
sub_wl_r = WHERE((wl GE 590) AND (wl LE 650)) 
sub_wl_nir = WHERE((wl GE 780) AND (wl LE 850))

Cab    = 29.0d  ; chlorophyll content (µg.cm-2) (0-100 ug cm-2) for grassland (Atzberger et al., Suitability and
Car    = 0.0d   ; 12.0d    ;; carotenoid content (µg.cm-2)  (0-25 ug cm-2)
Cbrown = 0.0d  ;0.5d     ;; brown pigment content (0-1 arbitrary units)
Cw     = 0.015  ;0.02, 0.01;0.015d   ;; EWT (cm) - Equivalent water thickness (0-0.05 cm) Vohland and Jarmer 2008
Cm     = 0.005  ;0.1d     ;; LMA (g cm-2) - leaf mass per unit leaf area
;Cm  (0.005-0.01 g cm-2 according to Darvishzadeh et al, 2008, RSE)
Ns     = 1.6d    ;; structure coefficient (1-3 dimensionless) Vohland and Jarmer 2008
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
 lidf_a = -1      
 lidf_b = 0      
 hspot  = 0.05d      ; hot spot for grass (Atzberger et al., as above)


SZA = 0
VZAsat = 0
VZAcam = 80
RAA =  0; relative azimuth (°)
GCC_sat = !NULL
GCC_cam = !NULL
ndvi_sat = !NULL
ndvi_cam = !NULL

;load s2 spectral responses
sr = SR_S2A_red_nir400_2500()
subB = WHERE(sr.bSRnorm GT 0.0)
subG = WHERE(sr.gSRnorm GT 0.0)
subR = WHERE(sr.rSRnorm GT 0.0)
subNIR = WHERE(sr.nirSRnorm GT 0.0)

FOR i = 0, N_ELEMENTS(laiVal)-1 DO BEGIN
  lai = laiVal(i)
  PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b,lai,hspot,SZA,VZAsat,RAA,rsoil,resh,resv,absh,absa
  ;resample to S2 spectral bands
  b_band = TOTAL(resv[subB] * sr.bSRnorm[subB]) 
  g_band = TOTAL(resv[subG] * sr.gSRnorm[subG])
  r_band = TOTAL(resv[subR] * sr.rSRnorm[subR])
  nir_band = TOTAL(resv[subNIR] * sr.nirSRnorm[subNIR])
  GCC_sat = [GCC_sat, g_band/(g_band+g_band+r_band)]
  ndvi_sat = [ndvi_sat, (nir_band - r_band)/(nir_band + r_band)]
  ;GCC_sat = [GCC_sat, MEAN(resv_sat[sub_wl_g])/(MEAN(resv_sat[sub_wl_r]) + MEAN(resv_sat[sub_wl_g]) + MEAN(resv_sat[sub_wl_b]))]
  ;ndvi_sat = [ndvi_sat, (MEAN(resv_sat[sub_wl_nir])-MEAN(resv_sat[sub_wl_r]))/(MEAN(resv_sat[sub_wl_nir])+MEAN(resv_sat[sub_wl_r]))]
  PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b,lai,hspot,SZA,VZAcam,RAA,rsoil,resh,resv,absh,absa
  b_band = TOTAL(resv[subB] * sr.bSRnorm[subB])
  g_band = TOTAL(resv[subG] * sr.gSRnorm[subG])
  r_band = TOTAL(resv[subR] * sr.rSRnorm[subR])
  nir_band = TOTAL(resv[subNIR] * sr.nirSRnorm[subNIR])
  GCC_cam = [GCC_cam, g_band/(g_band+g_band+r_band)]
  ndvi_cam = [ndvi_cam, (nir_band - r_band)/(nir_band + r_band)]
  ;GCC_cam = [GCC_cam, MEAN(resv_cam[sub_wl_g])/(MEAN(resv_cam[sub_wl_r]) + MEAN(resv_cam[sub_wl_g]) + MEAN(resv_cam[sub_wl_b]))]
  ;ndvi_cam = [ndvi_cam, (MEAN(resv_cam[sub_wl_nir])-MEAN(resv_cam[sub_wl_r]))/(MEAN(resv_cam[sub_wl_nir])+MEAN(resv_cam[sub_wl_r]))]
ENDFOR

gh = PLOT(INDGEN(N_ELEMENTS(laiVal)), GCC_sat, NAME='GCC sat', XTITLE='Time', YTITLE= 'GCC, NDVI', AXIS_STYLE = 1, MARGIN = [0.15, 0.15, 0.20, 0.25], SYMBOL ='+')
gh1 = PLOT(INDGEN(N_ELEMENTS(laiVal)), GCC_cam, NAME='GCC cam', OVERPLOT=1, LINESTYLE='--', SYMBOL ='+')
ghb = PLOT(INDGEN(N_ELEMENTS(laiVal)), ndvi_sat, NAME='NDVI sat', OVERPLOT=1, LINESTYLE='-', SYMBOL ='+', color='r')
gh1b = PLOT(INDGEN(N_ELEMENTS(laiVal)), ndvi_cam, NAME='NDVI cam', OVERPLOT=1, LINESTYLE='--', SYMBOL ='+', color='r')

gh2 = PLOT(INDGEN(N_ELEMENTS(laiVal)),laiVal, NAME='LAI', /CURRENT, COLOR='g', YRANGE = [0,5], MARGIN = [0.15, 0.15, 0.20, 0.25], AXIS_STYLE = 0, LINESTYLE='-')
a_t = AXIS('y', TARGET = gh2,  TITLE = 'LAI', YRANGE=[0,5], TICKDIR=1, TEXTPOS = 1, LOCATION = [max(gh.xrange),0,0])
!null = LEGEND(target=[gh, gh1, ghb, gh1b, gh2], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.3, TRANSPARENCY=100)

PRINT, 'ok'

END