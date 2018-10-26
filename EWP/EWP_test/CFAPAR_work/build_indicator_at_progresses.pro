PRO callerX
;  hl = half_life_define_all()
;**************************************************************************
rfe_dataset = 'chirps';'tamsat' ;'chirps'
CASE rfe_dataset OF
  'tamsat': BEGIN
    ;TAMSAT RESOLUTION
    ns = 1867
    nl = 348
    spi_dir = 'E:\WA\EWP\SPI1 from Original Tamsat'
    out_dir = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\Indicators_at_progress'
    dir_prog = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution'
    mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
    coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  'chirps': BEGIN
    ;CHIRPS RESOLUTION
    ns = 1402
    nl = 262
    spi_dir = 'E:\WA\EWP\SPIx from CHIRPS'
    out_dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\Indicators_at_progress'
    dir_prog = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution'
    mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.05, 21.05, 0.05, 0.05, WGS-84, units=Degrees}'
    coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  ELSE: STOP
ENDCASE
;**************************************************************************  

nb = 15
nb_indicator = 540
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI3', '_1-1999_36-2013_bil', mapinfo_string, coord_string
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI6', '_1-1999_36-2013_bil', mapinfo_string, coord_string
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI9', '_1-1999_36-2013_bil', mapinfo_string, coord_string
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI12', '_1-1999_36-2013_bil', mapinfo_string, coord_string
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI15', '_1-1999_36-2013_bil', mapinfo_string, coord_string
build_indicator_at_progresses, spi_dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, 'BBB_', 'SPI18', '_1-1999_36-2013_bil', mapinfo_string, coord_string
;  FOR i = 0, N_ELEMENTS(hl)- 1 DO $
;    build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl'+STRTRIM(hl[i],2), ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl250', ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl500', ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl1000', ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl2000', ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl3000', ''
;  build_indicator_at_progresses, 'E:\WA\EWP\EWP_images','', 'standardized_cwp_hl2000', ''
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI3', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI4', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI5', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI6', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI7', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI8', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI9', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI10', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI11', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI12', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI13', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI14', '_1-1999_36-2013_bil'
;  build_indicator_at_progresses, spi_dir, 'BBB_', 'SPI15', '_1-1999_36-2013_bil'

END

PRO build_indicator_at_progresses, dir, out_dir, dir_prog, ns, nl, nb, nb_indicator, prefix, base, suffix, mapinfo_string, coord_string


sub_prog0 = INTARR(ns, nl, nb)
sub_prog25 = sub_prog0
sub_prog50 = sub_prog0
sub_prog75 = sub_prog0
sub_prog100 = sub_prog0
OPENR, lun, dir_prog + '\in99-13sub_of_prog0', /GET_LUN
READU, lun, sub_prog0
FREE_LUN, lun
OPENR, lun, dir_prog + '\in99-13sub_of_prog25', /GET_LUN
READU, lun, sub_prog25
FREE_LUN, lun
OPENR, lun, dir_prog + '\in99-13sub_of_prog50', /GET_LUN
READU, lun, sub_prog50
FREE_LUN, lun
OPENR, lun, dir_prog + '\in99-13sub_of_prog75', /GET_LUN
READU, lun, sub_prog75
FREE_LUN, lun
OPENR, lun, dir_prog + '\in99-13sub_of_prog100', /GET_LUN
READU, lun, sub_prog100
FREE_LUN, lun
indATminus1 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indATminus2 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indATminus3 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indAT0 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indAT25 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indAT50 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indAT75 = FLTARR(ns, nl, nb) * !VALUES.F_NAN
indAT100 = FLTARR(ns, nl, nb) * !VALUES.F_NAN

;be care SPI had dt = 2, cwp dt 2
OPENR, lunR, dir + '\' + prefix + base + suffix, /GET_LUN
strVar = STRMID(base, 0, 3)
CASE strVar OF
  'sta': line_ass = ASSOC(lunR, FLTARR(ns,nb_indicator))
  'SPI': line_ass = ASSOC(lunR, INTARR(ns,nb_indicator))
ENDCASE
FOR l = 0, nl-1 DO BEGIN
  tmp = FLOAT(line_ass[l])
  ;if it's spi, scale it
  IF (strVar EQ 'SPI') THEN BEGIN
    indNaN = WHERE((tmp LT -10000) OR (tmp GT 10000), count)
    tmp = tmp * 0.001
    tmp[indNaN] = !VALUES.F_NAN
  ENDIF 
  FOR s = 0, ns-1 DO BEGIN
    ;here I am looking at sinle pixels
    FOR yy = 1999, 2013 DO BEGIN
      y = yy-1999
      IF ((sub_prog0[s,l,y] EQ -999) OR (sub_prog0[s,l,y] GT 539)) THEN BEGIN
        indATminus1[s,l,y] = !VALUES.F_NAN
        indATminus2[s,l,y] = !VALUES.F_NAN
        indATminus3[s,l,y] = !VALUES.F_NAN
        indAT0[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        indATminus1[s,l,y] = tmp[s,sub_prog0[s,l,y]-1]
        indATminus2[s,l,y] = tmp[s,sub_prog0[s,l,y]-2]
        indATminus3[s,l,y] = tmp[s,sub_prog0[s,l,y]-3]
        indAT0[s,l,y] = tmp[s,sub_prog0[s,l,y]]
      ENDELSE
      IF ((sub_prog25[s,l,y] EQ -999) OR (sub_prog25[s,l,y] GT 539)) THEN BEGIN
        indAT25[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        indAT25[s,l,y] =tmp[s,sub_prog25[s,l,y]]
      ENDELSE
      IF ((sub_prog50[s,l,y] EQ -999) OR (sub_prog50[s,l,y] GT 539)) THEN BEGIN
        indAT50[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        indAT50[s,l,y] =tmp[s,sub_prog50[s,l,y]]
      ENDELSE
      IF ((sub_prog75[s,l,y] EQ -999) OR (sub_prog75[s,l,y] GT 539)) THEN BEGIN
        indAT75[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        indAT75[s,l,y] =tmp[s,sub_prog75[s,l,y]]
      ENDELSE
      IF ((sub_prog100[s,l,y] EQ -999) OR (sub_prog100[s,l,y] GT 539)) THEN BEGIN
        indAT100[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        indAT100[s,l,y] =tmp[s,sub_prog100[s,l,y]]
      ENDELSE
    ENDFOR
  ENDFOR
ENDFOR
FREE_LUN, lunR
;now write the indicators
OPENW, lunW, out_dir + '\'+ base + '_atProg0-3'  , /GET_LUN
WRITEU, lunW, indATminus3
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg0-3.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg0-2'  , /GET_LUN
WRITEU, lunW, indATminus2
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg0-2.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg0-1'  , /GET_LUN
WRITEU, lunW, indATminus1
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg0-1.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg0'  , /GET_LUN
WRITEU, lunW, indAT0
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg0.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg25'  , /GET_LUN
WRITEU, lunW, indAT25
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg25.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg50'  , /GET_LUN
WRITEU, lunW, indAT50
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg50.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg75'  , /GET_LUN
WRITEU, lunW, indAT75
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg75.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

OPENW, lunW, out_dir + '\'+ base + '_atProg100'  , /GET_LUN
WRITEU, lunW, indAT100
FREE_LUN, lunW
OPENW, lunW, out_dir + '\'+ base + '_atProg100.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
PRINTF, lunW, mapinfo_string
PRINTF, lunW, coord_string
FREE_LUN, lunW

END