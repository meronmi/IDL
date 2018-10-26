PRO build_CFAPAR
  ;**************************************************************************
  rfe_dataset = 'chirps';'tamsat' ;'chirps'
  CASE rfe_dataset OF
    'tamsat': BEGIN
      ;TAMSAT RESOLUTION
      ns = 1867
      nl = 348
      fapar_fn = 'E:\WA\EWP\FAPAR at TAMSAT resolution\AAA_FAPAR_TAMSAT_res_9901-1336_bil'
      dir_prog = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution'
      dir_out = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\CFAPAR'
    END
    'chirps': BEGIN
      ;CHIRPS RESOLUTION
      ns = 1402
      nl = 262
      fapar_fn = 'E:\WA\EWP\FAPAR at CHIRPS resolution\AAA_FAPAR_CHIRPS_res_9901-1336_bil'
      dir_prog = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution'
      dir_out = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\CFAPAR'
    END
    ELSE: STOP
  ENDCASE
  ;**************************************************************************



nb_fapar = 540
nb_prog = 15
sub_prog0 = INTARR(ns, nl, nb_prog)
sub_prog100 = INTARR(ns, nl, nb_prog)
OPENR, lun, dir_prog + '\in99-13sub_of_prog0', /GET_LUN
READU, lun, sub_prog0
FREE_LUN, lun
OPENR, lun, dir_prog + '\in99-13sub_of_prog100', /GET_LUN
READU, lun, sub_prog100
FREE_LUN, lun

OPENW, lunW, dir_out + '\CFAPAR99-13', /GET_LUN
cfapar = FLTARR(ns, nl, nb_prog) * !VALUES.F_NAN
OPENR, lunR, fapar_fn, /GET_LUN
line_ass = ASSOC(lunR, BYTARR(ns,nb_fapar))
FOR l = 0, nl-1 DO BEGIN
  tmp = FLOAT(line_ass[l])
  indNaN = WHERE((tmp LT 0) OR (tmp GT 200), count)
  tmp = tmp * 0.005
  tmp[indNaN] = !VALUES.F_NAN
  FOR s = 0, ns-1 DO BEGIN
    ;here I am looking at sinle pixels  
    FOR yy = 1999, 2013 DO BEGIN
      y = yy-1999
      IF (sub_prog0[s,l,y] EQ -999) OR (sub_prog100[s,l,y] EQ -999) OR (sub_prog100[s,l,y] GT 539) THEN BEGIN
        cfapar[s,l,y] = !VALUES.F_NAN
      ENDIF ELSE BEGIN
        cfapar[s,l,y] = TOTAL(tmp[s,sub_prog0[s,l,y]:sub_prog100[s,l,y]], /NAN)
      ENDELSE
    ENDFOR
  ENDFOR
ENDFOR
FREE_LUN, lunR
WRITEU,lunW, cfapar
FREE_LUN, lunW 
OPENW, lunW, dir_out + '\CFAPAR99-13.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns, 2)
PRINTF, lunW, 'lines = ' + STRTRIM(nl, 2)
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
CASE rfe_dataset OF
  'tamsat': BEGIN
    ;TAMSAT RESOLUTION
    PRINTF, lunW, 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
    PRINTF, lunW, 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  'chirps': BEGIN
    ;CHIRPS RESOLUTION
    PRINTF, lunW, 'map info = {Geographic Lat/Lon, 1, 1, -18.05, 21.05, 0.05, 0.05, WGS-84, units=Degrees}'
    PRINTF, lunW, 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  ELSE: STOP
ENDCASE

FREE_LUN, lunW


;compute and save the Zcfapar
OPENW, lunW, dir_out + '\ZCFAPAR99-13', /GET_LUN
zcfapar = FLTARR(ns, nl, nb_prog) * !VALUES.F_NAN
FOR l = 0, nl-1 DO BEGIN
  FOR s = 0, ns-1 DO BEGIN
    IF (TOTAL(FINITE(cfapar[s,l,*])) GT 5) THEN BEGIN
      tmp = cfapar[s,l,*]
      indFin = WHERE(FINITE(tmp))
      zcfapar[s,l,indFin] = (tmp[indFin] - MEAN(tmp[indFin]))/ STDDEV(tmp[indFin])
    ENDIF 
  ENDFOR
ENDFOR
WRITEU,lunW, zcfapar
FREE_LUN, lunW
OPENW, lunW, dir_out + '\ZCFAPAR99-13.hdr', /GET_LUN
PRINTF, lunW, 'ENVI'
PRINTF, lunW, 'samples = ' + STRTRIM(ns, 2)
PRINTF, lunW, 'lines = ' + STRTRIM(nl, 2)
PRINTF, lunW, 'bands = 15'
PRINTF, lunW, 'header offset = 0'
PRINTF, lunW, 'file type = ENVI Standard'
PRINTF, lunW, 'data type = 4'
PRINTF, lunW, 'interleave = bsq'
PRINTF, lunW, 'byte order = 0'
CASE rfe_dataset OF
  'tamsat': BEGIN
    ;TAMSAT RESOLUTION
    PRINTF, lunW, 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
    PRINTF, lunW, 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  'chirps': BEGIN
    ;CHIRPS RESOLUTION
    PRINTF, lunW, 'map info = {Geographic Lat/Lon, 1, 1, -18.05, 21.05, 0.05, 0.05, WGS-84, units=Degrees}'
    PRINTF, lunW, 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  END
  ELSE: STOP
ENDCASE
FREE_LUN, lunW

END