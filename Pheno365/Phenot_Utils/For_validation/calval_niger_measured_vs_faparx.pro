PRO longint2float, fname
nc = FIX(read_info('samples', fname + '.hdr'))      ; number of columns
nl = FIX(read_info('lines', fname + '.hdr'))        ; number of lines
nb = FIX(read_info('bands',fname + '.hdr'))        ; number of bands
OPENR, R1, fname, /GET_LUN
OPENW, W1, fname+'_float', /GET_LUN
line_ass_data = ASSOC(R1, LONARR(nc,nb))
FOR i = 0, nl-1 DO BEGIN
  line=FLOAT(line_ass_data[i])
  WRITEU, W1, line
ENDFOR
FREE_LUN, R1
FREE_LUN, W1
;create and adjust the hdr maunually!
END

FUNCTION calval_niger_Measured_vs_FAPARx, info_fn
;this function retieve the FAPAR value for the dekad specified by date of measurement in
;the database of ground measurements

;EXAMPLE
;PRINT, calval_niger_Measured_vs_FAPARx('S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\calval_niger_Measured_vs_FAPARx.txt')

boxdim = 3        ;dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
valnan2 = -999   ;value for no data in the Rs file (other than NaN). Typically -9999 for pheno variables

ground_data_fn = read_info('ground_data_fn', info_fn)
rs_data_fn = read_info('rs_data_fn', info_fn)
g_data_fn = read_info('g_data_fn', info_fn)
first_year_in_rs  = FIX(read_info('first_year_in_rs', info_fn))
;first_dek_in_rs  = FIX(read_info('first_dek_in_rs', info_fn)) ; not used at the end
doPlot = FIX(read_info('doPlot', info_fn))
;extract RS indicator used without path
rs_data_fn_no_path = STRMID(rs_data_fn, STRPOS(rs_data_fn,'\',/REVERSE_SEARCH)+1, STRLEN(rs_data_fn))

;Read the table of ground data
tmp =  READ_CSV(ground_data_fn, $
  HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
;load data into a strcture with the following fields:
;ID, SiteID, Location, Longitude, Latitude, Biomass, Year, Month, Day
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7','FIELD8','FIELD9'], $ ; if you have more than 9 fields, name as follows: FIELD01, FIELD02, ...
  ['ID','SiteID','Location','Longitude','Latitude','Biomass','Year','Month','Day'])
; replace -9999 as missing value by NaN for biomass column
FOR i=3,5 DO BEGIN
  ind = WHERE(data.(i) EQ -9999, count)
  IF (count GT 0) THEN data.(i)[ind] = !VALUES.F_NAN
ENDFOR

;Match and add the RS indicator
;add a field for RS indicator and standard deviation of RS indicator
data = CREATE_STRUCT(data, 'RSvar', DBLARR(N_ELEMENTS(data.ID)))
data.RSvar[*] = !VALUES.F_NAN

debug = 1

;Make sure that ground data are sorted by ID
;indSorted = SORT(data.ID)
indSorted = FSORT(data.ID)
FOR i = 0, N_TAGS(tmp)-1 DO data.(i) = data.(i)[indSorted]
IF (debug EQ 1) THEN BEGIN
  inds = [0]
  bio = [0]
ENDIF
;Loop on all sites and all years
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN

  ;use:
;  data.Longitude(i)
;  data.Latitude(i)
;  data.Year[i]
;  data.Month[i]
;  data.Day[i]
  z_val = zprofileFromLonLat(data.Longitude[i], data.Latitude(i), rs_data_fn, boxdim, valnan2)
  z_years = INDGEN(N_ELEMENTS(z_val[0,*])) + first_year_in_rs
  ;extract g file (in julian date and find the closest to biomass measurement
  jd_val = zprofileFromLonLat(data.Longitude[i], data.Latitude(i), g_data_fn, boxdim, -999)
  ind = WHERE(z_years EQ data.Year[i], count)
  IF ((data.Day[i] NE -9999) AND (data.Month[i] NE -9999) AND (data.Year[i] NE -9999) AND FINITE(data.biomass[i])) THEN $
    data_exist = 1 ELSE data_exist = 0
  IF ((count EQ 1) AND (data_exist EQ 1)) THEN BEGIN
    ;convert date to julian day,
    jd_of_meas = DOY_YEAR2JD(DDMMYYYY2DOY(data.Day[i],data.Month[i],data.Year[i]), data.Year[i])
    res = MIN(jd_val[0,*]-jd_of_meas, min_ind, /ABSOLUTE)
    ;PRINT, res, min_ind
    ;controllare con envi che funzioni ->OK
    IF (debug EQ 1) THEN BEGIN
      inds = [inds, min_ind]
      bio = [bio, data.biomass[i]] 
    ENDIF
    data.RSvar[i] = z_val[0, min_ind]
  ENDIF ELSE BEGIN
    data.RSvar[i] = !VALUES.F_NAN
  ENDELSE
  IF (i NE N_ELEMENTS(data.ID)-1) THEN BEGIN
    IF (debug EQ 1) AND (data.ID[i+1] NE data.ID[i]) THEN BEGIN 
      inds = inds[1:*]
      bio = bio[1:*]
      x = INDGEN(N_ELEMENTS(z_val[0,*]))
      z =  REFORM(z_val[0,*])
      z_date = z[inds]
      
      
      yrange = [0.0,0.4]
      
      ;gh = PLOT(z_val[0,*],  symbol=".", SYM_SIZE=2, DIMENSIONS = [1200, 300], TITLE = 'ID ' + STRTRIM(data.ID[i],2), YRANGE = yrange)
      gh = PLOT(x, z,  symbol=".", SYM_SIZE=2, DIMENSIONS = [1200, 300], $
           TITLE = 'ID ' + STRTRIM(data.ID[i],2), AXIS_STYLE = 1, $
           MARGIN = [0.1, 0.15, 0.1, 0.1], XTITLE = 'dek', YTITLE = 'FAPAR')
      
      gh = PLOT(inds, z_date, OVERPLOT = 1, color='red', symbol="o", SYM_SIZE=1, AXIS_STYLE = 1)
      gh = PLOT(inds, z_date, OVERPLOT = 1, color='red', symbol="+", SYM_SIZE=1, AXIS_STYLE = 1)
      
      gh_bio = PLOT(inds, bio, /CURRENT, color='green', symbol="s", AXIS_STYLE = 0, $
               MARGIN = [0.1, 0.15, 0.1, 0.1], DIMENSIONS = [1200, 300], xrange=gh.xrange)
      gh_bio = PLOT(inds, bio, /CURRENT, color='green', symbol="+", AXIS_STYLE = 0, $
        MARGIN = [0.1, 0.15, 0.1, 0.1], DIMENSIONS = [1200, 300], xrange=gh.xrange)
      a_bio = AXIS('y', TARGET = gh_bio, LOCATION = [max(gh_bio.xrange),0,0], $
              TEXTPOS = 1, TITLE = 'Biomass', COLOR = "green")
     
      PRINT, 'debug'
      inds = [0]
      bio = [0]
    ENDIF
  ENDIF
ENDFOR ;i
;Global anlysis 
r2GainOff_b = LinRegMulti(data.RSvar, data.Biomass, doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass', colorInd = data.Year)
ind = WHERE(data.Year EQ 2010)
r2GainOff_b = LinRegMulti(data.RSvar[ind], data.Biomass[ind], doPlot, 'Biomass vs. ' + rs_data_fn_no_path, rs_data_fn_no_path, 'Biomass', colorInd = data.Year[ind])
RETURN, 0
END