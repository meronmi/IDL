PRO sim4dailtAPAR
;CRO_dk
lat = 55.53027778
lon = 12.09722222
offsetGMT = -1
local_h_in_day = INDGEN(24)
DD = 1
MM = 11
YY = 2016
GMTtime = local_h_in_day + offsetGMT ;* 100
ind = WHERE(GMTtime LT 0, count)
IF (count) THEN  GMTtime[ind] = 24 + GMTtime[ind]



envi
SZA = [!NULL]
SAA = [!NULL]
FOR i=0, N_ELEMENTS(GMTtime)-1 DO BEGIN
  SA = ENVI_COMPUTE_SUN_ANGLES(DD, MM, YY, GMTtime[i]*100+30, lat, lon)
  SZA = [SZA, 90.0-SA[0]]
  SAA = [SAA, SA[1]]
ENDFOR
ind = WHERE(SZA LT 90, count_h_above_hor)
indSunFirst = ind[0]
indSunLast = ind[-1]
PRINT, 'SUN UP first: ',  local_h_in_day[indSunFirst]
PRINT, 'SUN UP last: ',  local_h_in_day[indSunLast]

;h = PLOT(local_h_in_day, SZA)
;h = PLOT([local_h_in_day[indSunFirst],local_h_in_day[indSunFirst]], h.yrange, OVERPLOT=1)
;h = PLOT([local_h_in_day[indSunLast],local_h_in_day[indSunLast]], h.yrange, OVERPLOT=1)


par_daily=10
n_hours = count_h_above_hor + 1
t_hours = INDGEN(n_hours+1)
par_max = par_daily/(TOTAL(SIN(!PI*t_hours/n_hours)))
par_max2 = par_daily/(TOTAL(SIN(!PI*t_hours/n_hours)^2))

;h = PLOT(local_h_in_day[indSunFirst]-1+t_hours, par_max * SIN(!PI*t_hours/n_hours), XRANGE=[0,24])
h2 = PLOT(local_h_in_day[indSunFirst]-1+t_hours, par_max2 * SIN(!PI*t_hours/n_hours)^2, COLOR='r', OVERPLOT=1)
part = local_h_in_day * 0.0
part[indSunFirst-1 : indSunFirst-1 + N_ELEMENTS(t_hours) -1] = par_max2 * SIN(!PI*t_hours/n_hours)^2
PRINT, TOTAL(par_max * SIN(!PI*t_hours/n_hours))
PRINT, TOTAL(par_max2 * SIN(!PI*t_hours/n_hours)^2)


rsoil_fn = 'X:\works\pubblicazioni\in preparazione\2017 Anton pheno s2\envi_jhu_lib_dark_grayish_brown_sily_loam.csv'
tmp = READ_CSV(rsoil_fn, HEADER=hdr)
wl = INDGEN(2500-400+1, /FLOAT) + 400.0
rsoil = INTERPOL(tmp.field2, tmp.field1, wl)


Cab    = 50.0d  ; chlorophyll content (µg.cm-2) (0-100 ug cm-2) for grassland (Atzberger et al., Suitability and
Car    = 12.0d   ; 12.0d    ;; carotenoid content (µg.cm-2)  (0-25 ug cm-2)
Cbrown = 0.5d  ;0.5d     ;; brown pigment content (0-1 arbitrary units)
Cw     = 0.015  ;0.02, 0.01;0.015d   ;; EWT (cm) - Equivalent water thickness (0-0.05 cm) Vohland and Jarmer 2008
Cm     = 0.1  ;0.1d     ;; LMA (g cm-2) - leaf mass per unit leaf area
lai    = 0.5d
Ns     = 1.0d
lidf_a = -0.35
lidf_b = -0.15
hspot  = 0.2d
VZA = 0
RAA =  0; relative azimuth (°)

fapara = [!NULL]
FOR i = 0, N_ELEMENTS(local_h_in_day)-1 DO BEGIN 
  IF (SZA[i] LE 90) THEN BEGIN
    PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b, $
    lai,hspot, $
    SZA[i],VZA,RAA,rsoil,resh,resv,absh, absa
    fapara = [fapara,  MEAN(absa[0:300])]
  ENDIF ELSE fapara = [fapara, 0]
ENDFOR
h = PLOT(local_h_in_day, fapara, OVERPLOT=1, COLOR='blue')
val = MIN(ABS(local_h_in_day-10.50), min_sub)
PRINT, 'Approx time of overpass: ',   local_h_in_day[min_sub]
PRINT, 'PARd*instantFAPAR = ', par_daily * fapara[min_sub]
PRINT, 'PARt*instantFAPARt = ', TOTAL(fapara * part)
PRINT, 'ok'
END