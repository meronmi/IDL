PRO buildAccImageFromParam_only_selected_pixels_Niger
;this function read the DHTM parameter from files and compute the desired acc

;Differently from buildAccImageFromParam this code works only on selected pixels in Niger where a date
;for the measurement campaign is available. It compute the acc up to the timing of measurements for 9 pixels around the 
;given coordinates

;User settings
threshSOStype = '%'
threshSOS = 20 
threshEOStype = 'DOY' 
frstYear = 1997

fn_example_for_coord_ret = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\A1sos-1997_acc1'
pheno_path = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos'
base_par_fn = ['p0','p1','p2','p3','p4','p5','p6','t0','t1']
base_par_fn = 'A1sos-1997_'+base_par_fn
gs=1
base_par_fn = base_par_fn + STRTRIM(gs,2)

ns = 7841                      ; number of samples
nl = 1458                      ; number of lines
nb = 16                        ; number of years  from 1997-1998
frstyear = 1997

;here load the databse of measurements
ground_data_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\NIGER_IDL_FORMAT_2014-03-05.csv'
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
;add a field containing sample and lines
data = CREATE_STRUCT(data, 'CoordSL', FLTARR(N_ELEMENTS(data.ID),3,3,2))
data.CoordSL[*,*,*] = !VALUES.F_NAN

;Make sure that ground data are sorted by ID
indSorted = FSORT(data.ID)
FOR i = 0, N_TAGS(tmp)-1 DO data.(i) = data.(i)[indSorted]

FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  ;associate smaple and line to each record
  data.coordSL[i,*,*,*] = SampleLineFromLonLat(data.Longitude(i), data.Latitude(i), fn_example_for_coord_ret, 3)
ENDFOR


str = '_as_meas_date
fname_acc = pheno_path + '\' + 'A1sos-1997_' + 'RecomputedAcc' + STRTRIM(gs,2) + 'EOS' + STRTRIM(str,2)
fname_accb = pheno_path + '\' + 'A1sos-1997_' + 'RecomputedAccb' + STRTRIM(gs,2) + 'EOS' + STRTRIM(str,2)

;part to adjust for the time in dekads instead of JD (the new version of pheno correctly store JD so 
;this part should be removed)
JDacq_fn='E:\WA\all sahel\data\UpTo1301g_bil'
nbAcq = 532
OPENR, lunAcq, JDacq_fn, /GET_LUN
JDvar = ASSOC(lunAcq, LONARR(ns,nbAcq))
;see also in code for removing, seaarch for ACQ_CORRECTION

;Computation
;array used to store the luns
luna= LONARR(N_ELEMENTS(base_par_fn))
FOR i=0, N_ELEMENTS(luna)-1 DO BEGIN
  OPENR, lun, pheno_path+'\'+base_par_fn[i], /GET_LUN & luna[i]=lun
ENDFOR
p0assvar = ASSOC(luna[0], FLTARR(ns,nb)) & p1assvar = ASSOC(luna[1], FLTARR(ns,nb))
p2assvar = ASSOC(luna[2], FLTARR(ns,nb)) & p3assvar = ASSOC(luna[3], FLTARR(ns,nb))
p4assvar = ASSOC(luna[4], FLTARR(ns,nb)) & p5assvar = ASSOC(luna[5], FLTARR(ns,nb))
p6assvar = ASSOC(luna[6], FLTARR(ns,nb)) & t0assvar = ASSOC(luna[7], FLTARR(ns,nb))
t1assvar = ASSOC(luna[8], FLTARR(ns,nb))

;variable to store the line output
pheno_line_acc = FLTARR(ns,nl,nb) * !VALUES.F_NAN
pheno_line_accb = FLTARR(ns,nl,nb) * !VALUES.F_NAN
;loop on sites
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  ;check if the site, for that giben year, has both biomassa and date
  IF (FINITE(data.biomass[i])) THEN BEGIN
    IF (FINITE(data.Month[i]) AND FINITE(data.Day[i])) THEN BEGIN
      ;data are present, compute the acc for that year using that dates, for all nine pixels in the box
      FOR ll=0,2 DO BEGIN
        line = data.coordSL[i,0,ll,1] ;position to the UL corner at the first loop, then move to the next line
        p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
        p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
        p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
        p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
        t1line=float(t1assvar[line])
        ;ACQ_CORRECTION
        JDline=FLOAT(JDvar[line])
        FOR ss=0,2 DO BEGIN
          sample = data.coordSL[i,ss,ll,0] 
          year = data.year[i]
          doy = DDMMYYYY2DOY(data.Day[i], data.Month[i], year)
          y = year - frstYear
          IF ((y GT 0) AND (y LT 16))THEN BEGIN    ;the current year is GE 1998
            IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN
              IF (p0line[sample,y] NE -999) THEN BEGIN
                ;compute the fitted curve
                pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
                  p3line[sample,y], p4line[sample,y], p5line[sample,y], $
                  p6line[sample,y]]
                ;ACQ_CORRECTION
                t0 = JDline[sample,t0line[sample,y]]
                ;ACQ_CORRECTION
                t1 = JDline[sample,t1line[sample,y]] 
                threshEOS = doy
                tmp = accFromParam(pdhtf_pars, [t0, t1], threshSOStype, threshSOS, threshEOStype, threshEOS, frstyear+y)
                pheno_line_acc[sample,line,y] = tmp[0]
                pheno_line_accb[sample,line,y] = tmp[1]
              ENDIF ELSE BEGIN ;is -999
                pheno_line_acc[sample,line,y] = 0.0
                pheno_line_accb[sample,line,y] = 0.0
              ENDELSE
            ENDIF
          ENDIF
        ENDFOR
      ENDFOR
    ENDIF ELSE BEGIN
      PRINT, 'ID ' + STRTRIM(data-ID[i],2) + ' has biomass but no date for year ' + STRTRIM(data.Year[i],2)
    ENDELSE
  ENDIF 
ENDFOR
OPENW, W1, fname_acc, /GET_LUN
OPENW, W2, fname_accb, /GET_LUN
;F) write the output (ATTENZIONE now bsq)
WRITEU, W1, pheno_line_acc
WRITEU, W2, pheno_line_accb

FREE_LUN, W1
FREE_LUN, W2
FREE_LUN, lunAcq
CLOSE, /ALL
END