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
line1 =  'map info = {Geographic Lat/Lon, 1, 1, -18.0044643, 20.0044643, 0.00892857143, 0.00892857143}'
line2 = 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'

list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES )
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