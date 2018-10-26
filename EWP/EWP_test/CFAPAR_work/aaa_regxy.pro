PRO callerReg
  ;**************************************************************************
  rfe_dataset = 'tamsat';'tamsat' ;'chirps'
  CASE rfe_dataset OF
    'tamsat': BEGIN
      ;TAMSAT RESOLUTION
      ns = 1867
      nl = 348
      nb = 15
      dirx = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\Indicators_at_progress'
      diry = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\CFAPAR'
      out_dir = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\correlation'
      mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.01875, 21.01875, 0.0375, 0.0375, WGS-84, units=Degrees}'
      coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0
    END
    'chirps': BEGIN
      ;CHIRPS RESOLUTION
      ns = 1402
      nl = 262
      nb = 15
      dirx = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\Indicators_at_progress'
      diry = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\CFAPAR'
      out_dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps'
      mapinfo_string = 'map info = {Geographic Lat/Lon, 1, 1, -18.05, 21.05, 0.05, 0.05, WGS-84, units=Degrees}'
      coord_string = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
    END
    ELSE: STOP
  ENDCASE
  ;**************************************************************************
 prefixes = !NULL
 prefixes=[prefixes,'SPI3','SPI6','SPI9','SPI12','SPI15','SPI18']
 ;hl = half_life_define_all()
 ;FOR i = 0, N_ELEMENTS(hl)- 1 DO prefixes = [prefixes, 'standardized_cwp_hl'+STRTRIM(hl[i],2)]
 suffixes = ['_atProg0','_atProg25','_atProg50','_atProg75','_atProg100']
  
  FOR i= 0, N_ELEMENTS(suffixes)-1 DO BEGIN
    FOR j= 0, N_ELEMENTS(prefixes)-1 DO BEGIN
      fnX = prefixes[j] + suffixes[i]
      regXY, dirx, fnX, diry, 'ZCFAPAR99-13', ns, nl, nb, out_dir, mapinfo_string, coord_string
    ENDFOR
  ENDFOR
END


PRO regXY, dirX, fnX, dirY, fnY, ns, nl, nb, out_dir, mapinfo_string, coord_string
;compute per pixel r^2, b and significance of two stack images
X = FLTARR(ns,nl, nb)
Y = X
corr = FLTARR(ns,nl)*!VALUES.F_NAN
cod = FLTARR(ns,nl)*!VALUES.F_NAN ;coefficient of determination
b = FLTARR(ns,nl)*!VALUES.F_NAN ;slope
p =  FLTARR(ns,nl)*!VALUES.F_NAN
OPENR, lunX, dirX + '\' + fnX, /GET_LUN
READU, lunX, X
FREE_LUN, lunX
OPENR, lunY, dirY + '\' + fnY, /GET_LUN
READU, lunX, Y
FREE_LUN, lunY
FOR s = 0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    xp = X[s,l,*]
    yp = Y[s,l,*]
    indFin = WHERE((FINITE(xp)) AND (FINITE(yp)), countFin)
    IF (countFin GE 4) THEN BEGIN
      res = linregstat(xp[indFin], yp[indFin])
      ; RETURN, [offset, gain, corr, pgain]
      cod[s,l] = res[2]^2
      b[s,l] = res[1]
      corr[s,l] = res[2]
      p[s,l] = res[3]
    ENDIF
  ENDFOR
ENDFOR
suffixes = ['-corr_with-zCFAPAR', '-cod_with-zCFAPAR', '-gain_with-zCFAPAR', '-p_with-zCFAPAR']

OPENW, lun, out_dir + '\' + fnX + suffixes[0], /GET_LUN
WRITEU, lun, corr
FREE_LUN, lun

OPENW, lun, out_dir + '\' + fnX + suffixes[1], /GET_LUN
WRITEU, lun, cod
FREE_LUN, lun

OPENW, lun, out_dir + '\' + fnX + suffixes[2], /GET_LUN
WRITEU, lun, b
FREE_LUN, lun

OPENW, lun, out_dir + '\' + fnX + suffixes[3], /GET_LUN
WRITEU, lun, p
FREE_LUN, lun

FOR i = 0, N_ELEMENTS(suffixes)-1 DO BEGIN
  OPENW, lunW, out_dir + '\' + fnX + suffixes[i] + '.hdr', /GET_LUN
  PRINTF, lunW, 'ENVI'
  PRINTF, lunW, 'samples = ' + STRTRIM(ns,2);1867'
  PRINTF, lunW, 'lines = ' + STRTRIM(nl,2);348'
  PRINTF, lunW, 'bands = 1'
  PRINTF, lunW, 'header offset = 0'
  PRINTF, lunW, 'file type = ENVI Standard'
  PRINTF, lunW, 'data type = 4'
  PRINTF, lunW, 'interleave = bsq'
  PRINTF, lunW, 'byte order = 0'
  PRINTF, lunW, mapinfo_string
  PRINTF, lunW, coord_string
  FREE_LUN, lunW
  FREE_LUN, lun
ENDFOR
END