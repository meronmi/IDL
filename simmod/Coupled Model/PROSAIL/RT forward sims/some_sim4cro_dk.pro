PRO some_sim4CRO_DK
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
rsrLR = dataRSR_MODIST_7b_fast2()
envi


hh_loc_overpass = 10
mm_loc_overpass = 30
lat = 55.53027778
lon = 12.09722222
offsetGMT = -1

JDsolstice = JULDAY(12,22,2005)

JD = [JULDAY(10,1,2005), JULDAY(10,11,2005), JULDAY(10,21,2005), $ 
      JULDAY(11,01,2005), JULDAY(11,11,2005), JULDAY(11,21,2005), $
      JULDAY(12,01,2005), JULDAY(12,11,2005), JULDAY(12,21,2005), $
      JULDAY(1,1,2006), JULDAY(1,11,2006), JULDAY(1,21,2006), $
      JULDAY(2,1,2006), JULDAY(2,11,2006), JULDAY(2,21,2006), $
      JULDAY(3,1,2006), JULDAY(3,11,2006), JULDAY(3,21,2006), $
      JULDAY(4,1,2006), JULDAY(4,11,2006), JULDAY(4,21,2006), $
      JULDAY(5,1,2006), JULDAY(5,11,2006), JULDAY(5,21,2006), $
      JULDAY(6,1,2006), JULDAY(6,11,2006), JULDAY(6,21,2006), $
      JULDAY(7,1,2006), JULDAY(7,11,2006), JULDAY(7,21,2006)]
;for the site
ecmwf_data = get_ecmwf_profile_v2('E:\SimMod_data\ECMWF\JOSH_DATA\All_sites.csv','DK-Ris')
globrad=[!NULL]
;smooth it
savgolFilter = SAVGOL(30, 30, 0, 1)
ecmwf_data.rad = CONVOL(ecmwf_data.rad, savgolFilter, /EDGE_TRUNCATE)
FOR i = 0, N_ELEMENTS(JD)- 1 DO BEGIN
  ind = WHERE(ecmwf_data.JD eq JD[i], count)
  IF (count EQ 1) THEN globrad = [globrad, ecmwf_data.rad[ind]] ELSE globrad = [globrad, !VALUES.F_NAN]
ENDFOR
;***********************************************************************************
;LAI
;laiVal = (INDGEN(N_ELEMENTS(JD))+1)*0+5;+0.5;0.5
laiVal = (INDGEN(N_ELEMENTS(JD))+1)/FLOAT(MAX(INDGEN(N_ELEMENTS(JD))+1))*0.5+0.5;+0.5;0.5
;***********************************************************************************

rsoil_fn = 'X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\envi_jhu_lib_dark_grayish_brown_sily_loam.csv'
tmp = READ_CSV(rsoil_fn, HEADER=hdr)
wl = INDGEN(2500-400+1, /FLOAT) + 400.0
rsoil = INTERPOL(tmp.field2, tmp.field1, wl)


Cab    = 50.0d  ; chlorophyll content (µg.cm-2) (0-100 ug cm-2) for grassland (Atzberger et al., Suitability and
Car    = 12.0d   ; 12.0d    ;; carotenoid content (µg.cm-2)  (0-25 ug cm-2)
Cbrown = 0.5d  ;0.5d     ;; brown pigment content (0-1 arbitrary units)
Cw     = 0.015  ;0.02, 0.01;0.015d   ;; EWT (cm) - Equivalent water thickness (0-0.05 cm) Vohland and Jarmer 2008
Cm     = 0.1  ;0.1d     ;; LMA (g cm-2) - leaf mass per unit leaf area
lai    = 5.0d 
Ns     = 1.0d    ;; structure coefficient (1-3 dimensionless) Vohland and Jarmer 2008
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
lidf_a = -0.35   
lidf_b = -0.15      
hspot  = 0.2d      ; hot spot for grass (Atzberger et al., as above)
;;small test
;tts    = 65.0d    ;; solar zenith angle (°)
;tto    = 0.0d     ;; observer zenith angle (°)
;psi    = 0.0d     ;; azimuth (°)
;psoil  = 1.0d     ;; soil coefficient
;;skyl   = 70.0d    ;; diffuse/direct radiation
;data=dataSpec_P5B();;
;Es=data(7,*);;
;Ed=data(8,*);;
;sk = 70.0d
;PARdiro = (1.0-sk/100.0)*Es;;
;PARdifo = (sk/100.0)*Ed;;
;skyl = PARdifo / (PARdifo + PARdiro)
;Rsoil1=data(9,*);;
;Rsoil2=data(10,*);
;rsoil=psoil*Rsoil1+(1.0-psoil)*Rsoil2;; 
;PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b, $
;  lai,hspot, $
;  tts,tto,psi,rsoil,resh,resv,absh
;
;;end of small test, TO BE REMOVE BECAUSE OF SKYLE
SZA = [!NULL]
SAA = [!NULL]
FOR i=0, N_ELEMENTS(JD)-1 DO BEGIN
  CALDAT, JD[i], MM, DD, YY
  GMTtime = (hh_loc_overpass + offsetGMT) *100 + mm_loc_overpass 
  SA = ENVI_COMPUTE_SUN_ANGLES(DD, MM, YY, GMTtime, lat, lon)
  SZA = [SZA, 90.0-SA[0]]
  SAA = [SAA, SA[1]]
ENDFOR
VZA = 0
RAA =  0; relative azimuth (°)

NDVI = [!NULL]
Nr = [!NULL]
NIRv = [!NULL]
fapar = [!NULL]
fapara = [!NULL]
FOR i = 0, N_ELEMENTS(laiVal)-1 DO BEGIN
  lai = laiVal(i)
  PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b, $
    laiVal(i),hspot, $
    SZA[i],VZA,RAA,rsoil,resh,resv,absh, absa
    ModisBands = resample2MODIS7b(wl, resv, rsrLR) ;resample it
    R = REFORM(ModisBands[0])
    N = REFORM(ModisBands[1])
    NDVI = [NDVI, (N-R)/(N+R)]
    NIRv = [NIRv, NDVI * N]
    Nr = [Nr, N]
    fapar = [fapar,  MEAN(absh[0:300])]
    fapara = [fapara,  MEAN(absa[0:300])]
ENDFOR

dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
fs = 8
xrange = [MIN(JD), MAX(JD)]
xminor = 2        ;minor ticks between majors
nmajor = 4
ticlensec = 0.03
yrange =[0 , 1]

gh_ndvi = PLOT(JD, NDVI, NAME='NDVI',  YTITLE= 'NDVI, FAPAR, .01SZA', AXIS_STYLE = 1, MARGIN = [0.15, 0.15, 0.20, 0.25], XRANGE=xrange, YRANGE=yrange, $
          SYMBOL ='+', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, FONT_SIZE = fs)
gh_ndvicos = PLOT(JD, NDVI*COS(SZA*!DTOR), NAME='NDVIxcos(SZA)', AXIS_STYLE = 1, OVERPLOT = 1,LINESTYLE='-',  SYMBOL ='o', COLOR='black')
gh_nirv = PLOT(JD, NIRv*10, NAME='10xNIRv', AXIS_STYLE = 1, OVERPLOT = 1,LINESTYLE='-',  SYMBOL ='+', COLOR='green')
gh_n = PLOT(JD, Nr, NAME='Nr', AXIS_STYLE = 1, OVERPLOT = 1,LINESTYLE='-',  SYMBOL ='+', COLOR='purple')
gh_ndviXgr = PLOT(JD, NDVI*(globrad/(max(globrad)-min(globrad))), NAME='NDVIxGR', AXIS_STYLE = 1, OVERPLOT = 1,LINESTYLE='-',  SYMBOL ='+', color = 'blue')
;gh_fapar = PLOT(JD, fapar, NAME='FAPAR_dif', AXIS_STYLE = 1, OVERPLOT = 1,LINESTYLE='--', color='r')
gh_fapara = PLOT(JD, fapara, NAME='FAPAR_act', AXIS_STYLE = 1, OVERPLOT = 1, SYMBOL ='o', color='r')
gh_sza = PLOT(JD, SZA/100.0, NAME='.01SZA', AXIS_STYLE = 1, OVERPLOT = 1, color='grey', LINESTYLE ='--', THICK = 2)
gh_winter_sol = PLOT([JDsolstice, JDsolstice], [0,1], color='grey', OVERPLOT=1)
          ;SYMBOL ='+', XTICKFORMAT='(C(CDI,1x,CMoA))', XMINOR=xminor, FONT_SIZE = fs)
;gh_ndvi['axis0'].MAJOR = nmajor
strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
loc = -(yrange[1]-yrange[0])/10.0
a_x =AXIS('X', TARGET = gh_ndvi, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-JD[0]+JD2DOY(JD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
gh_lai = PLOT(JD, laiVal, NAME='LAI', /CURRENT, COLOR='g', YRANGE = [0,6], MARGIN = [0.15, 0.15, 0.20, 0.25], AXIS_STYLE = 0, LINESTYLE='-',  FONT_SIZE = fs,  XRANGE=xrange, THICK = 4)
a_t = AXIS('y', TARGET = gh_lai,  TITLE = 'LAI', YRANGE=[0,5], TICKDIR=1, TEXTPOS = 1, LOCATION = [max(gh_lai.xrange),0,0], color='g')
;!null = LEGEND(target=[gh_ndvi, gh_ndvicos, gh_ndviXgr, gh_fapar, gh_fapara, gh_sza, gh_lai], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, $
!null = LEGEND(target=[gh_ndvi, gh_n, gh_ndviXgr, gh_ndvicos], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, $
               TRANSPARENCY=100, POSITION=[0.5, 1],HORIZONTAL_ALIGNMENT='RIGHT', SAMPLE_WIDTH=0.1 )
!null = LEGEND(target=[gh_fapara, gh_sza, gh_lai, gh_nirv], /AUTO_TEXT_COLOR, SHADOW=0, LINESTYLE=6, $
               TRANSPARENCY=100, POSITION=[0.5, 1],HORIZONTAL_ALIGNMENT='LEFT', SAMPLE_WIDTH=0.1 )

PRINT, 'ok'

END