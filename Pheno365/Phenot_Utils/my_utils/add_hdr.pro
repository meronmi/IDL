PRO save_9999

list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES )
FOR i = 0, N_ELEMENTS(LIST) -1 DO begin
  ;add map info
  
  ns = read_info('samples', list[i])
  nl = read_info('lines', list[i])
  dt = read_info('data type', list[i])
  
  ;rename file
  fName = STRSPLIT(list[i], '.',/EXTRACT)
  CASE FIX(dt) of
    4: data = FLTARR(ns,nl)
    2: data = INTARR(ns,nl)
    ELSE: STOP
  ENDCASE
  
  fName[1]=fName[0]+'.img'
  OPENR, lun, fName[1], /GET_LUN
  READU, lun, data
  FREE_LUN, Lun 
  OPENW, lun, fName[0]+'_9999.img', /GET_LUN
  indN = WHERE(FINITE(data) NE 1, countN)
  IF (countN GT 0) THEN data[indN] = -9999
  WRITEU, lun, data
  FREE_LUN, lun 
   
ENDFOR
PRINT, 'Task add_hdr completed'
END


PRO add_hdr_and_save_9999
;add the geo info to hdr and add img to file
;line1 = 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, 7.29910715, 37.50446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
;TUN
;line1 = 'map info = {Geographic Lat/Lon, 1, 1, 19.9866072, 28.0133929, 0.00892857143, 0.00892857143}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;HoA
;line1 =  'map info = {Geographic Lat/Lon, 1.0000, 1.0000, 19.98660720, 28.01339290, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;WA
;line1 =  'map info = {Geographic Lat/Lon, 1, 1, -18.0044643, 20.0044643, 0.00892857143, 0.00892857143}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;WA Dominique
;map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}
;coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}

line1 =  'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'


list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES )
FOR i = 0, N_ELEMENTS(LIST) -1 DO begin
  ;add map info
  OPENW, lun, list[i], /GET_LUN, /APPEND
  PRINTF, lun, line1
  PRINTF, lun, line2
  FREE_LUN, lun
  ns = read_info('samples', list[i])
  nl = read_info('lines', list[i])
  dt = read_info('data type', list[i])
  
  ;rename file
  fName = STRSPLIT(list[i], '.',/EXTRACT)
  CASE FIX(dt) of
    4: data = FLTARR(ns,nl)
    2: data = INTARR(ns,nl)
    ELSE: STOP
  ENDCASE
  
  OPENR, lun, fName[0], /GET_LUN
  READU, lun, data
  FREE_LUN, Lun 
  OPENW, lun, fName[0]+'.img', /GET_LUN
  indN = WHERE(FINITE(data) NE 1, countN)
  IF (countN GT 0) THEN data[indN] = -9999
  WRITEU, lun, data
  FREE_LUN, Lun 
  ;FILE_MOVE, fName[0], fName[0]+'.img', /OVERWRITE 
ENDFOR
PRINT, 'Task add_hdr completed'
END

PRO add_hdr
;add the geo info to hdr and add img to file
;line1 = 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, 7.29910715, 37.50446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
;TUN
;line1 = 'map info = {Geographic Lat/Lon, 1, 1, 19.9866072, 28.0133929, 0.00892857143, 0.00892857143}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;HoA
;line1 =  'map info = {Geographic Lat/Lon, 1.0000, 1.0000, 19.98660720, 28.01339290, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
;WA
;line1 =  'map info = {Geographic Lat/Lon, 1, 1, -18.0044643, 20.0044643, 0.00892857143, 0.00892857143}'
;line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'

; sahel
;line1 = 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
;line2 =  'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'

line1 = 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
line2 =  'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'



list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES, GET_PATH=path)


FOR i = 0, N_ELEMENTS(LIST) -1 DO begin
  ;add map info
  OPENW, lun, list[i], /GET_LUN, /APPEND
  PRINTF, lun, line1
  PRINTF, lun, line2
  FREE_LUN, lun
  ;rename file
  fName = STRSPLIT(list[i], '.',/EXTRACT)
  FILE_MOVE, fName[0], fName[0]+'.img', /OVERWRITE 
ENDFOR
PRINT, 'Task add_hdr completed'
END