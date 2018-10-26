PRO readmo
path = 'd:\Users\meronmi\Documents\R_scripts\MODIS-FORGE\1x1SudanEddy'
fname = 'Lat13.28290Lon30.47830Start2000-01-01End2014-12-31_MOD09A1.asc'
res = readMODISprofile(path, fname)
END

FUNCTION readMODISprofile, path, fname
;read and process a time series name fname (must be an asc file) of product 
;MOD09A1
;downloaded with MODIStools 
;using the following R code:
;require(MODISTools) ##load it only if not loaded yet
;GetProducts()
;#GetBands(Product = "MOD13Q1")
;prod = "MOD09A1"
;bands = GetBands(Product = prod)
;latlon = c(13.2829, 30.4783)   ##SUDAN
;#show working directory
;getwd()
;
;modis.subset <- data.frame(lat = latlon[1], long = latlon[2])
;##years
;modis.subset$start.date <- rep(2000, nrow(modis.subset))
;modis.subset$end.date <- rep(2014, nrow(modis.subset))
;
;MODISSubsets(LoadDat = modis.subset, Products = prod, Bands = bb, Size = c(0,0), StartDate = TRUE)

;Note: I have checked that pixels are ordered from the upper left corner to the lower right, so if
;requiring a size of 1 km dey are:
; 1  2  3  4  5
; 6  7  8  9 10
;11 12 13 14 15
;16 17 18 19 20
;21 22 23 24 25

;However, having more than a pixel makes the thing complex as I cannnot make the simple
;average as I have may different day of acquisitio and angles.
;Therefore, for the moment I restrict to one single profile, using Size = c(0,0) im the R command

;*****************************
;PARAMETERS
bnames = ["sur_refl_day_of_year", "sur_refl_qc_500m", "sur_refl_raz", "sur_refl_state_500m", $
          "sur_refl_szen", "sur_refl_vzen", "sur_refl_b01", "sur_refl_b02", "sur_refl_b03", $
          "sur_refl_b04", "sur_refl_b05", "sur_refl_b06", "sur_refl_b07"]
fillval = [65535, 4294967295, 0, 65535, $
           0, 0, 0, -28672, -28672, -28672, $
           -28672, -28672, -28672, -28672]
scalefact = [1, 1, 0.01, 1, $
             0.01, 0.01, 0.0001, 0.0001, 0.0001, $
             0.0001, 0.0001, 0.0001, 0.0001]
;bands to be saved in output

bnames2output = ['sur_refl_day_of_year', 'sur_refl_raz', 'sur_refl_szen', 'sur_refl_vzen', $
                 'sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','sur_refl_b05','sur_refl_b06','sur_refl_b07'] 
fields2skip = 5       ;nummber of fields before data (data starts at fields2skip +1)
quality_threshold = 0 ;(0 = corrected product produced at ideal quality all bands)
;*****************************
fullPathFname = path + '\' + fname
res = QUERY_CSV(fullPathFname, Info)
IF (res NE 1) THEN STOP
nlines = info.lines
nfields = info.nfields
data = READ_CSV(fullPathFname, COUNT = n)
;in the first field (e.g., "MOD13Q1.A2000049.h18v07.005.2006269172719.250m_16_days_NDVI") keep only the band name
FOR i=0, nlines-1 DO BEGIN
  fields = STRSPLIT(data.(0)[i], '.', /EXTRACT)
  data.(0)[i] = fields[N_ELEMENTS(fields)-1]
ENDFOR
;now find the unique elements of the first field, i.e., the bands downloaded
;bandNames = UNIQLIST(data.(0))

;make array for the data
;columns: year + doy_composite + acq DOY + year.dayfract(acq DOY/366) + 7 bands + 3 angles 
;lines: number of obs nobs=WHERE(data.(0) EQ 'sur_refl_b01', nobs)
tmp=WHERE(data.(0) EQ 'sur_refl_b01', nobs)
cubehdr = ['Year', 'DOY_comp', 'Year.dayfract', 'Year_acq', 'DOY_acq', 'RAA', 'SZA', 'VZA', $
           'R1_645', 'R2_858.5', 'R3_469', 'R4_555', 'R5_1240', 'R6_1640', 'R7_2130']
cube = FLTARR(15, nobs)*!VALUES.F_NAN

;build the qc array
qc = FLTARR(nobs) * !VALUES.F_NAN
ind = WHERE(data.(0) EQ 'sur_refl_qc_500m', count)
FOR l = 0, count-1 DO BEGIN
  tmp = BINARY(data.(fields2skip)[ind[l]])
  qc[l] = 2^0*FIX(tmp[31])+2^1*FIX(tmp[30])
ENDFOR

;fill the cube with first band
;bnames2output = ['sur_refl_day_of_year', 'sur_refl_raz', 'sur_refl_szen', 'sur_refl_vzen', $
;  'sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','sur_refl_b05','sur_refl_b06','sur_refl_b07']
FOR b = 0, N_ELEMENTS(bnames2output)-1 DO BEGIN
  ;retrieve fill value and scale for that band
  pos = WHERE(bnames EQ bnames2output[b])
  fill = fillval[pos[0]]
  scale = scalefact[pos[0]]
  ind = WHERE(data.(0) EQ bnames2output[b], count)
  IF (count NE nobs) THEN STOP
  IF (b EQ 0) THEN BEGIN
    ;fill year, doy_comp and doy_acq, yar_acq once and for all
    tmp = data.(2)[ind]
    cube[0,*] = STRMID(tmp, 1, 4)       ;year
    cube[1,*] = STRMID(tmp, 5, 3)       ;doy_com
    tmp = FLOAT(data.(fields2skip)[ind])
    indFilled = WHERE(tmp EQ fill, countFilled)
    IF (countFilled GT 0) THEN tmp[indFilled] = !VALUES.F_NAN
    ;here I compute the year of acquistion, which is not trivial as the last 8-day composite
    ;period of the year may end in the next year, and the good observation may be picked up there.
    ;For instance it has been observed that the doy of acq for and obs with a doy of comp eq 361, was 3, 
    ;next year. Additionally it was found that the next 8-day comp was referring to same acq doy (3). All data
    ;(reflectance and angles were repeated).
    ;We want thus:
    ;- to get the correct year of acquisition unambiguously
    ;- if the two 8-day composites report same data (for year Y and Y+1), exclude the last 8-day comp
    ;- of year Y 
    ind2delete = -777
    FOR i = 0, nobs - 1 DO BEGIN
      ;find the correct year_of_acq checking all the last year composites, after 358 they may go to next year
      ;avoid testing a NAN tmp value giving "Floating illegal operand"
      IF FINITE(tmp[i]) THEN BEGIN
        IF (cube[1,i] GE 358) AND (tmp[i] LT cube[1,i]) THEN BEGIN
          cube[3,i] = cube[0,i] +1   ;acq year
          cube[2,i] = cube[3,i] + tmp[i]/367.0 ;year.dayfract, if DOY is NaN, this is NaN
          ;now check that the following comp (the first of Y+1 is not the same, in the case remove this one
          IF (tmp[i] = tmp[i+1]) THEN BEGIN
            cube[2:*,i] = !VALUES.F_NAN
            ind2delete = [ind2delete, i]
          ENDIF
        ENDIF ELSE BEGIN
          cube[3,i] = cube[0,i]
          cube[2,i] = cube[0,i] + tmp[i]/367.0 ;year.dayfract, if DOY is NaN, this is NaN
        ENDELSE
      ENDIF ELSE BEGIN
        cube[3,i] = cube[0,i]
        cube[2,i] = cube[0,i] + tmp[i]/367.0 ;year.dayfract, if DOY is NaN, this is NaN
      ENDELSE
    ENDFOR
  ENDIF
  tmp = FLOAT(data.(fields2skip)[ind])
  indFilled = WHERE(tmp EQ fill, countFilled)
  IF (countFilled GT 0) THEN BEGIN
    tmp[indFilled] = !VALUES.F_NAN
    PRINT, 'Band ' + bnames2output[b] + ', % of filled: ' +STRTRIM(countFilled/FLOAT(nobs)*100.0,2)
  ENDIF
  tmp = tmp * scale
  indBadQ = WHERE(qc GT quality_threshold, countBadQ)
  IF (countBadQ GT 0) THEN BEGIN
    tmp[indBadQ] = !VALUES.F_NAN
    PRINT, 'Band ' + bnames2output[b] + ', % of bad quality: ' +STRTRIM(countBadQ/FLOAT(nobs)*100.0,2)
  ENDIF
  cube[4+b,*] = tmp
ENDFOR
;set deleted records to NaN
IF (N_ELEMENTS(ind2delete) GT 1) THEN cube[2:*, ind2delete[1:*]]=!VALUES.F_NAN
ind = WHERE(FINITE(cube) EQ 0)
;set -9999 for NaN 
cube[ind]  = -9999
WRITE_CSV, path + '\' + data.(3)[0] + '.csv', cube, HEADER = cubehdr
RETURN, 0
END