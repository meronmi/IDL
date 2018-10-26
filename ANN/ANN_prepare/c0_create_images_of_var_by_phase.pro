PRO c0_create_images_of_var_by_phase
!EXCEPT = 0
; for each pheno phase it creates the variables required by the ANN for that phase using actual data
; not nedded it's always 50% .. ->[it first create the variables required by the ANN  for each phase using LTA, then]

out_dir = 'E:\WA MODIS\VarByPheno'
fn_rain = 'E:\WA MODIS\WA ROI CHIRPS\chirps_bil'
nanRain = -9999
fn_rad = 'E:\WA MODIS\WA ERA INT\ECMWF_rad_bil'
nanRad = -9999
fn_tav = 'E:\WA MODIS\WA ERA INT\ECMWF_tav_bil' 
nanTav = -9999
fn_ndvi = 'E:\WA MODIS\WA ROI NDVI\bil files\BOKU_OF_WA_bil_sc'
;Nan is correct here
pheno_events = define_pheno_events()
phase_names = ['IniGrow','Grow','Sen','LateSen']
var_names = ['edf_cPrec','edf_lengthMaxDrySpell','edf_avgT', 'edf_avgRad','edf_cNDVI' ]     ;c stands for cumulative
var_name_first_phase = ['edf_cPrec1month']
;Build indicators with "NRT" data 
;-determine the maxmimun number of seasons

hdr_fn = FILE_DIRNAME(fn_ndvi, /MARK_DIRECTORY) + FILE_BASENAME(fn_ndvi, '.img') +'_sub_of_' + pheno_events[0] + '.hdr'
nSeas = LONG(read_info('bands', hdr_fn)) 

ns = LONG(read_info('samples', hdr_fn)) & nl = LONG(read_info('lines', hdr_fn))
map_info = read_info('map info', fn_ndvi + '.hdr')

hdr_fn = FILE_DIRNAME(fn_rain, /MARK_DIRECTORY) + FILE_BASENAME(fn_rain, '.img') + '.hdr'
nb_rain = LONG(read_info('bands', hdr_fn)) & dt_rain = read_info('data type', hdr_fn)

hdr_fn = FILE_DIRNAME(fn_rad, /MARK_DIRECTORY) + FILE_BASENAME(fn_rad, '.img') + '.hdr'
nb_rad = LONG(read_info('bands', hdr_fn)) & dt_rad = read_info('data type', hdr_fn)

hdr_fn = FILE_DIRNAME(fn_tav, /MARK_DIRECTORY) + FILE_BASENAME(fn_tav, '.img') + '.hdr'
nb_tav = LONG(read_info('bands', hdr_fn)) & dt_tav = read_info('data type', hdr_fn)

hdr_fn = FILE_DIRNAME(fn_ndvi, /MARK_DIRECTORY) + FILE_BASENAME(fn_ndvi, '.img') + '.hdr'
nb_ndvi = LONG(read_info('bands', hdr_fn)) & dt_ndvi = read_info('data type', hdr_fn)

;read once and for all the pheno sub files (bsq) into structures
rainSub = readSubInStruct(fn_rain, pheno_events, ns, nl)
radSub = readSubInStruct(fn_rad, pheno_events, ns, nl)
tavSub = readSubInStruct(fn_tav, pheno_events, ns, nl)
ndviSub = readSubInStruct(fn_ndvi, pheno_events, ns, nl)

;loop on the pheno phases
FOR p = 0, N_ELEMENTS(phase_names)-1 DO BEGIN
  ;make the output directory for the pheno phase
  out_dir_phase = out_dir + '\' + phase_names[p]
  FILE_MKDIR, out_dir_phase
  ;open the input bil files
  OPENR, lRain, fn_rain, /GET_LUN
  OPENR, lRad, fn_rad, /GET_LUN
  OPENR, lTav, fn_tav, /GET_LUN
  OPENR, lNdvi, fn_ndvi, /GET_LUN
  lineRain = ASSOC(lRain, MAKE_ARRAY(ns, nb_rain, TYPE=dt_rain))
  lineRad = ASSOC(lRad, MAKE_ARRAY(ns, nb_rad, TYPE=dt_rad))
  lineTav = ASSOC(lTav, MAKE_ARRAY(ns, nb_tav, TYPE=dt_tav))
  lineNdvi = ASSOC(lNdvi, MAKE_ARRAY(ns, nb_ndvi, TYPE=dt_ndvi))  
  ;open the output bil files for the phase p (if p is 0 add the prerain)
  OPENW, lPrct_cPrec, out_dir_phase + '\' + var_names[0] + '_' + phase_names[p] + '_bil', /GET_LUN
  OPENW, lPrct_lengthMaxDrySpell, out_dir_phase + '\' + var_names[1] + '_' + phase_names[p] + '_bil', /GET_LUN
  OPENW, lPrct_medT, out_dir_phase + '\' + var_names[2] + '_' + phase_names[p] + '_bil', /GET_LUN
  OPENW, lPrct_medRad, out_dir_phase + '\' + var_names[3] + '_' + phase_names[p] + '_bil', /GET_LUN
  OPENW, lPrct_cNDVI, out_dir_phase + '\' + var_names[4] + '_' + phase_names[p] + '_bil', /GET_LUN
  IF (p EQ 0) THEN BEGIN
    OPENW, lPrct_cPrec1month, out_dir_phase + '\' + var_name_first_phase + '_' + phase_names[p] + '_bil', /GET_LUN
    OPENW, lSeasPrct_cNDVI, out_dir + '\Full_seas_prct_cNDVI' + '_bil', /GET_LUN
    ;Climatology (median value of the variable during the season)
    OPENW, lSeasMean_NDVI, out_dir + '\Full_seas_mean_NDVI' + '_bsq', /GET_LUN
    OPENW, lSeasMean_Tav, out_dir + '\Full_seas_mean_Tav' + '_bsq', /GET_LUN
    OPENW, lSeasMean_Prec, out_dir + '\Full_seas_mean_Prec' + '_bsq', /GET_LUN
    OPENW, lSeasMean_Rad, out_dir + '\Full_seas_mean_Rad' + '_bsq', /GET_LUN
    OPENW, lSeasSD_NDVI, out_dir + '\Full_seas_SD_NDVI' + '_bsq', /GET_LUN
    OPENW, lSeasSD_Tav, out_dir + '\Full_seas_SD_Tav' + '_bsq', /GET_LUN
    OPENW, lSeasSD_Prec, out_dir + '\Full_seas_SD_Prec' + '_bsq', /GET_LUN
    OPENW, lSeasSD_Rad, out_dir + '\Full_seas_SD_Rad' + '_bsq', /GET_LUN
    ;input availability image (1/0, 0 when at least one of the meteo var is missing despite there is phenology
    OPENW, lInputAvailbl, out_dir + '\Boolean_input_availability' + '_bsq', /GET_LUN
  ENDIF
  ;proceed row by row
  
  FOR line = 0L, nl-1 DO BEGIN
    ;PRINT, line
    IF ((line/FLOAT(nl) MOD 0.2) EQ 0.0) THEN PRINT, 'Phase '  + phase_names[p] + ' at ' + STRTRIM(line/nl*100,2) + ' %'
    ;loop on the variables to be computed
    rain = FLOAT(lineRain[line])
    ind = WHERE(rain EQ nanRain, count)
    IF (count GE 0) THEN rain[ind] = !VALUES.F_NAN
    rad = FLOAT(lineRad[line])
    ind = WHERE(rad EQ nanRad, count)
    IF (count GE 0) THEN rad[ind] = !VALUES.F_NAN
    tav = FLOAT(lineTav[line])
    ind = WHERE(tav EQ nanTav, count)
    IF (count GE 0) THEN tav[ind] = !VALUES.F_NAN
    ndvi = FLOAT(lineNdvi[line]) 
    ;prepare/reset the line varables to store the results
    
    IF (p EQ 0) THEN BEGIN ;execute once only (the pre-season and the deviation from full season NDVI
      lineOutPrct_cPrec1month = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
      lineOutPrct_cPrec1month = ecdfBetweenSubs(rain, REFORM(rainSub.sos[*,line,*])-3, REFORM(rainSub.sos[*,line,*])-1, 'sum')
      lineOutSeasPrct_cNDVI = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
      lineOutSeasPrct_cNDVI = ecdfBetweenSubs(ndvi, REFORM(ndviSub.sos[*,line,*]), REFORM(ndviSub.eos[*,line,*]), 'sum')
      ;be care here I always have to take the last nSeas bands 
      WRITEU, lPrct_cPrec1month, lineOutPrct_cPrec1month[*, -nSeas:-1]
      WRITEU, lSeasPrct_cNDVI, lineOutSeasPrct_cNDVI[*, -nSeas:-1]
      ;Climatology here
      ;Keep track of where, despite having pheno, there is no meteo var (save it to a file)
      InputAvailbl = MAKE_ARRAY(ns, TYPE=1) * 0 + 1
      ;NDVI Mean
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = MeanBetweenSubs(ndvi, REFORM(ndviSub.sos[*,line,*]), REFORM(ndviSub.eos[*,line,*]))
      ind = WHERE(~FINITE(tmp), count)
      IF (count GT 0) THEN InputAvailbl[ind] = 0
      WRITEU, lSeasMean_NDVI, tmp
      ;SD
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = lineSDBetweenSubs(ndvi, REFORM(ndviSub.sos[*,line,*]), REFORM(ndviSub.eos[*,line,*]))
      WRITEU, lSeasSD_NDVI, tmp
      ;Tav Mean
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = MeanBetweenSubs(tav, REFORM(tavSub.sos[*,line,*]), REFORM(tavSub.eos[*,line,*]))
      ind = WHERE(~FINITE(tmp), count)
      IF (count GT 0) THEN InputAvailbl[ind] = 0
      WRITEU, lSeasMean_Tav, tmp
      ;SD
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = lineSDBetweenSubs(tav, REFORM(tavSub.sos[*,line,*]), REFORM(tavSub.eos[*,line,*]))
      WRITEU, lSeasSD_Tav, tmp
      ;rad Mean
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = MeanBetweenSubs(rad, REFORM(radSub.sos[*,line,*]), REFORM(radSub.eos[*,line,*]))
      ind = WHERE(~FINITE(tmp), count)
      IF (count GT 0) THEN InputAvailbl[ind] = 0
      WRITEU, lSeasMean_Rad, tmp
      ;SD
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = lineSDBetweenSubs(rad, REFORM(radSub.sos[*,line,*]), REFORM(radSub.eos[*,line,*]))
      WRITEU, lSeasSD_Rad, tmp
      ;Prec Mean
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = MeanBetweenSubs(rain, REFORM(rainSub.sos[*,line,*]), REFORM(rainSub.eos[*,line,*]))
      ind = WHERE(~FINITE(tmp), count)
      IF (count GT 0) THEN InputAvailbl[ind] = 0
      WRITEU, lSeasMean_Prec, tmp
      ;SD
      tmp = MAKE_ARRAY(ns, TYPE=4) * !VALUES.F_NAN & tmp = lineSDBetweenSubs(rain, REFORM(rainSub.sos[*,line,*]), REFORM(rainSub.eos[*,line,*]))
      WRITEU, lSeasSD_Prec, tmp
      ;write the input avialbility line
      WRITEU, lInputAvailbl,  BYTE(InputAvailbl)
      ;look for math errors
      IF CHECK_MATH() NE 0 THEN BEGIN
        PRINT, 'Math error occurred before here'
        STOP
      ENDIF
    ENDIF
    ;sum  up to one dek before the next phase with the excpetion of the last phase (eos) that is taken entirely  
    IF (p EQ N_ELEMENTS(phase_names)-1) THEN endOffSet = 0 ELSE endOffSet = 1
    lineOutPrct_cPrec = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
    lineOutPrct_cPrec =  ecdfBetweenSubs(rain, REFORM(rainSub.(p)[*,line,*]), REFORM(rainSub.(p+1)[*,line,*])-endOffSet, 'sum')
    ;definition (less than 10 mm) taken from https://gmao.gsfc.nasa.gov/research/subseasonal/atlas/Pindices-html/dryspell.html
    lineOutPrct_lengthMaxDrySpell = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
    lineOutPrct_lengthMaxDrySpell = ecdfBetweenSubs(rain, REFORM(rainSub.(p)[*,line,*]), REFORM(rainSub.(p+1)[*,line,*])-endOffSet, 'lengthDry')
    lineOutPrct_medT = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
    lineOutPrct_medT = ecdfBetweenSubs(tav, REFORM(tavSub.(p)[*,line,*]), REFORM(tavSub.(p+1)[*,line,*])-endOffSet, 'mean')
    lineOutPrct_medRad = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
    lineOutPrct_medRad =  ecdfBetweenSubs(rad, REFORM(radSub.(p)[*,line,*]), REFORM(radSub.(p+1)[*,line,*])-endOffSet, 'mean')
    lineOutPrct_cNDVI = MAKE_ARRAY(ns, nSeas, TYPE=4) * !VALUES.F_NAN
    lineOutPrct_cNDVI = ecdfBetweenSubs(ndvi, REFORM(ndviSub.(p)[*,line,*]), REFORM(ndviSub.(p+1)[*,line,*])-endOffSet, 'sum')
    ;write the line in the files 
    WRITEU, lPrct_cPrec, lineOutPrct_cPrec[*, -nSeas:-1]
    WRITEU, lPrct_lengthMaxDrySpell, lineOutPrct_lengthMaxDrySpell[*, -nSeas:-1]
    WRITEU, lPrct_medT, lineOutPrct_medT[*, -nSeas:-1]   
    WRITEU, lPrct_medRad, lineOutPrct_medRad[*, -nSeas:-1]    
    WRITEU, lPrct_cNDVI, lineOutPrct_cNDVI   
    ;look for math errors
    IF CHECK_MATH() NE 0 THEN BEGIN
      PRINT, 'Math error occurred before here'
      STOP
    ENDIF   
  ENDFOR  ;line
  FREE_LUN, lRain
  FREE_LUN, lRad
  FREE_LUN, lTav
  FREE_LUN, lNdvi
  FREE_LUN, lPrct_cPrec
  FREE_LUN, lPrct_lengthMaxDrySpell
  FREE_LUN, lPrct_medT
  FREE_LUN, lPrct_medRad
  FREE_LUN, lPrct_cNDVI
  IF (p EQ 0) THEN BEGIN
    FREE_LUN, lprct_cPrec1month
    FREE_LUN, lSeasPrct_cNDVI
    FREE_LUN, lSeasMean_NDVI 
    FREE_LUN, lSeasMean_Tav 
    FREE_LUN, lSeasMean_Prec 
    FREE_LUN, lSeasMean_Rad 
    FREE_LUN, lSeasSD_NDVI 
    FREE_LUN, lSeasSD_Tav 
    FREE_LUN, lSeasSD_Prec 
    FREE_LUN, lSeasSD_Rad 
    FREE_LUN, lInputAvailbl
  ENDIF
  ;write the various hdr
  res = write_envi_hdr(out_dir_phase + '\' + var_names[0] + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
  res = write_envi_hdr(out_dir_phase + '\' + var_names[1] + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
  res = write_envi_hdr(out_dir_phase + '\' + var_names[2] + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
  res = write_envi_hdr(out_dir_phase + '\' + var_names[3] + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
  res = write_envi_hdr(out_dir_phase + '\' + var_names[4] + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
  IF (p EQ 0) THEN BEGIN
    res = write_envi_hdr(out_dir_phase + '\' + var_name_first_phase + '_' + phase_names[p] + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_prct_cNDVI' + '_bil', ns, nl, 4, NBANDS=nSeas, INTERLEAVE='bil',  MAPINFO=map_info)  
    res = write_envi_hdr(out_dir + '\Full_seas_mean_NDVI' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_mean_Tav' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_mean_Prec' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_mean_Rad' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_SD_NDVI' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_SD_Tav' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_SD_Prec' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Full_seas_SD_Rad' + '_bsq', ns, nl, 4, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
    res = write_envi_hdr(out_dir + '\Boolean_input_availability' + '_bsq', ns, nl, 1, NBANDS=1, INTERLEAVE='bsq',  MAPINFO=map_info)
  ENDIF
ENDFOR  ;p
;look for math errors
IF CHECK_MATH() NE 0 THEN BEGIN
  PRINT, 'Math error occurred before here'
  STOP
ENDIF
;only once save a bil file with the length of each phase (in deks and in % of the season)
phase_length_dek = FLTARR(ns,nl,4)
phase_length_fract = FLTARR(ns,nl,4)
OPENW, lLengthDek, out_dir + '\phase_length_dek_bsq', /GET_LUN
OPENW, lLengthFract, out_dir + '\phase_length_fract_bsq', /GET_LUN
;compute total length
sos = REFORM(ndviSub.sos[*,*,0])
mg = REFORM(ndviSub.mg[*,*,0])
tom = REFORM(ndviSub.tom[*,*,0])
sen = REFORM(ndviSub.sen[*,*,0])
eos = REFORM(ndviSub.eos[*,*,0])
indFin = WHERE(sos GE 0, countFin)
length  = FLOAT(sos)*!VALUES.F_NAN 
length[indFin]=FLOAT(eos[indFin]-sos[indFin])
tmp  = FLOAT(sos)*!VALUES.F_NAN
;compute length of sos-mg
tmp[indFin]=FLOAT(mg[indFin]-sos[indFin])
phase_length_dek[*,*,0] = tmp
tmp[indFin] = tmp[indFin] / length[indFin]
phase_length_fract[*,*,0] = tmp
;compute length of mg-tom
tmp[indFin]=FLOAT(tom[indFin]-mg[indFin])
phase_length_dek[*,*,1] = tmp
tmp[indFin] = tmp[indFin] / length[indFin]
phase_length_fract[*,*,1] = tmp
;compute length of tom-sen
tmp[indFin]=FLOAT(sen[indFin]-tom[indFin])
phase_length_dek[*,*,2] = tmp
tmp[indFin] = tmp[indFin] / length[indFin]
phase_length_fract[*,*,2] = tmp
;compute length of sen-eos
tmp[indFin]=FLOAT(eos[indFin]-sen[indFin])
phase_length_dek[*,*,3] = tmp
tmp[indFin] = tmp[indFin] / length[indFin]
phase_length_fract[*,*,3] = tmp
WRITEU, lLengthDek, phase_length_dek
WRITEU, lLengthFract, phase_length_fract 
FREE_LUN, lLengthDek
FREE_LUN, lLengthFract

;write hdrs
res = write_envi_hdr(out_dir + '\phase_length_dek_bsq.hdr', ns, nl, 4, NBANDS=4, INTERLEAVE='bsq',  MAPINFO=map_info, BAND_NAMES=phase_names)
res = write_envi_hdr(out_dir + '\phase_length_fract_bsq.hdr', ns, nl, 4, NBANDS=4, INTERLEAVE='bsq',  MAPINFO=map_info, BAND_NAMES=phase_names)
;look for math errors
IF CHECK_MATH() NE 0 THEN BEGIN
  PRINT, 'Math error occurred before here'
  STOP
ENDIF
PRINT, 'ended..'

END
