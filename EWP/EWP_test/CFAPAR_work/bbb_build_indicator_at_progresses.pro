;PRO run_BBB_build_indicator_at_progresses
;;  hl = half_life_define_all()
;;**************************************************************************
;rfe_dataset = 'chirps';'tamsat' ;'chirps'
;CASE rfe_dataset OF
;  'tamsat': BEGIN
;    ;TAMSAT RESOLUTION
;    info.ns = 1867
;    info.nl = 348
;    spi_dir = 'E:\WA\EWP\SPI1 from Original Tamsat'
;    out_dir = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\Indicators_at_progress'
;    dir_prog = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution'
;    mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
;    coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;  END
;  'chirps': BEGIN
;    ;CHIRPS RESOLUTION
;    info.ns = 1402
;    info.nl = 262
;    spi_dir = 'E:\WA\EWP\SPIx from CHIRPS'
;    out_dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\Indicators_at_progress'
;    dir_prog = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution'
;    mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.05, 21.05, 0.05, 0.05, WGS-84, units=Degrees}'
;    coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;  END
;  ELSE: STOP
;ENDCASE
;;**************************************************************************  
;
;nb = 15
;nb_indicator = 540
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI3', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI6', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI9', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI12', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI15', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;build_indicator_at_progresses, spi_dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, 'BBB_', 'SPI18', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;
;
;END

PRO run_BBB_build_indicator_at_progresses
dir_out = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\Indicator_at_progress'
dir_prog = '\\ies\d5\asap\TEST_PREDICTORS\PROGRESS_SUBSCRIPTS'
base_fn_prog = '200701-201714'

;SPI
;calPar = CREATE_STRUCT('gain', 0.02, 'offset', -2.5, 'minVal', 0, 'maxVal', 250)
;1
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\SPI_1km_ROI_Africa\metaSPI1_200701-2017-14.mta'
;indicator_name_for_fn_out = 'SPI1'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out
;3
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\SPI_1km_ROI_Africa\metaSPI3_200701-2017-14.mta'
;indicator_name_for_fn_out = 'SPI3'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out



;SWI
;calPar = CREATE_STRUCT('gain', 0.02, 'offset', -2.5, 'minVal', 0, 'maxVal', 250)
;;015
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\zSWI_1km_ROI_Africa\metazSWI_015_200701-2017-14.mta'
;indicator_name_for_fn_out = 'zSWI015'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out
;;;040
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\zSWI_1km_ROI_Africa\metazSWI_040_200701-2017-14.mta'
;indicator_name_for_fn_out = 'zSWI040'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out
;;060
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\zSWI_1km_ROI_Africa\metazSWI_060_200701-2017-14.mta'
;indicator_name_for_fn_out = 'zSWI060'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out

;BEC sm 
;calPar = CREATE_STRUCT('gain', 0.02, 'offset', -2.5, 'minVal', 0, 'maxVal', 250)
;;015
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\zsm_BEC_1km_ROI_Africa\metazBECsm_200701-201714.mta'
;indicator_name_for_fn_out = 'zBECsm'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out

;CHIRPS spi3
;calPar = CREATE_STRUCT('gain', 0.02, 'offset', -2.5, 'minVal', 0, 'maxVal', 250)
;;015
;fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_1km_ROI_Africa\chirps_spi3_1km_200701-201714.mta'
;indicator_name_for_fn_out = 'chirpsSPI3'
;BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out

;CHIRPS spi1
calPar = CREATE_STRUCT('gain', 0.02, 'offset', -2.5, 'minVal', 0, 'maxVal', 250)
;015
fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_1km_ROI_Africa\chirps_spi1_1km_200701-201714.mta'
indicator_name_for_fn_out = 'chirpsSPI1'
BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out

END


;PRO BBB_build_indicator_at_progresses, dir, out_dir, dir_prog, info.ns, info.nl, nb, nb_indicator, prefix, base, suffix, mapinfo_string, coord_string
PRO BBB_build_indicator_at_progresses, fn_meta, dir_prog, base_fn_prog, calPar, indicator_name_for_fn_out, dir_out
info = READ_META(fn_meta)
nb_prog = FIX(read_info('bands', dir_prog + '\season1_sub_in_period_' + base_fn_prog + '_of_prog_0.hdr'))
PRINT, 'Working on: '+ fn_meta
FOR spy = 0, 1 DO BEGIN     ;loop on the two seasons
  sub_prog0 = INTARR(info.ns, info.nl, nb_prog) & sub_prog25 = sub_prog0 & sub_prog50 = sub_prog0 & sub_prog75 = sub_prog0 & sub_prog100 = sub_prog0
  ;dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_0', /GET_LUN
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_0', /GET_LUN
  READU, lun, sub_prog0 &   FREE_LUN, lun
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_25', /GET_LUN
  READU, lun, sub_prog25 &   FREE_LUN, lun
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_50', /GET_LUN
  READU, lun, sub_prog50 &   FREE_LUN, lun
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_75', /GET_LUN
  READU, lun, sub_prog75 &   FREE_LUN, lun
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_100', /GET_LUN
  READU, lun, sub_prog100 &   FREE_LUN, lun
  

;  indAT0 = FLTARR(info.ns, info.nl, nb) * !VALUES.F_NAN
;  indAT25 = FLTARR(info.ns, info.nl, nb) * !VALUES.F_NAN
;  indAT50 = FLTARR(info.ns, info.nl, nb) * !VALUES.F_NAN
;  indAT75 = FLTARR(info.ns, info.nl, nb) * !VALUES.F_NAN
;  indAT100 = FLTARR(info.ns, info.nl, nb) * !VALUES.F_NAN
  
  indAT0 = FLTARR(info.ns, nb_prog) * !VALUES.F_NAN
  indAT25 = FLTARR(info.ns, nb_prog) * !VALUES.F_NAN
  indAT50 = FLTARR(info.ns, nb_prog) * !VALUES.F_NAN
  indAT75 = FLTARR(info.ns, nb_prog) * !VALUES.F_NAN
  indAT100 = FLTARR(info.ns, nb_prog) * !VALUES.F_NAN
  
  FILE_MKDIR, dir_out
  fn_out = dir_out + '\season' + STRTRIM(spy+1,2) + indicator_name_for_fn_out 
  OPENW, lunW, fn_out + '_atProg0_bil', /GET_LUN
  OPENW, lunW1, fn_out + '_atProg25_bil', /GET_LUN
  OPENW, lunW2, fn_out + '_atProg50_bil'  , /GET_LUN    
  OPENW, lunW3, fn_out + '_atProg75_bil'  , /GET_LUN
  OPENW, lunW4, fn_out + '_atProg100_bil'  , /GET_LUN
  FOR l = 0, info.nl-1 DO BEGIN
    indAT0[*,*] = !VALUES.F_NAN
    indAT25[*,*] = !VALUES.F_NAN
    indAT50[*,*] = !VALUES.F_NAN
    indAT75[*,*] = !VALUES.F_NAN
    indAT100[*,*] = !VALUES.F_NAN
    IF ((l MOD 100) EQ 0) THEN PRINT, 'Season ' + STRTRIM(spy,2) + ' Reading Line: ' + STRTRIM(l)
    ;IF ((l MOD 100) EQ 0) THEN PRINT, 'Rading Line: ' + STRTRIM(l)
    tmp = FLOAT(READ_LINE_META(info, l))
    ; scale it accoring to calPar
    indNaN = WHERE((tmp LT calPar.minVal) OR (tmp GT calPar.maxVal), count)
    tmp = calPar.offset + tmp * calPar.gain
    tmp[indNaN] = !VALUES.F_NAN
    FOR s = 0, info.ns-1 DO BEGIN
      ;here I am looking at sinle pixels
      FOR y = 0, nb_prog-1 DO BEGIN
        IF (sub_prog0[s,l,y] NE -999)  THEN indAT0[s,y] = tmp[s,sub_prog0[s,l,y]] ;BEGIN
        IF (sub_prog25[s,l,y] NE -999) THEN indAT25[s,y] =tmp[s,sub_prog25[s,l,y]]  
;        IF ((sub_prog25[s,l,y] EQ -999) OR (sub_prog25[s,l,y] GT 539)) THEN BEGIN
;          indAT25[s,l,y] = !VALUES.F_NAN
;        ENDIF ELSE BEGIN
;          indAT25[s,l,y] =tmp[s,sub_prog25[s,l,y]]
;        ENDELSE
        IF (sub_prog50[s,l,y] NE -999) THEN indAT50[s,y] =tmp[s,sub_prog50[s,l,y]]
        IF (sub_prog75[s,l,y] NE -999) THEN indAT75[s,y] =tmp[s,sub_prog75[s,l,y]]
        IF (sub_prog100[s,l,y] NE -999) THEN indAT100[s,y] =tmp[s,sub_prog100[s,l,y]]       
      ENDFOR ;y 
    ENDFOR  ;s
    WRITEU, lunW, indAT0
    WRITEU, lunW1, indAT25
    WRITEU, lunW2, indAT50
    WRITEU, lunW3, indAT75
    WRITEU, lunW4, indAT100
  ENDFOR  ;l
  ;FREE_LUN, lunR
  ;now write the hdrs 
  FREE_LUN, lunW
  res = write_envi_hdr(fn_out + '_atProg0_bil.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bil', MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)  
  FREE_LUN, lunW1
  res = write_envi_hdr(fn_out + '_atProg25_bil.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bil', MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)  
  FREE_LUN, lunW2
  res = write_envi_hdr(fn_out + '_atProg50_bil.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bil', MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)
  FREE_LUN, lunW3
  res = write_envi_hdr(fn_out + '_atProg75_bil.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bil', MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)
  FREE_LUN, lunW4
  res = write_envi_hdr(fn_out + '_atProg100_bil.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bil', MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)
ENDFOR  ; spy loop on 1 and 2 growing seasons per year
END