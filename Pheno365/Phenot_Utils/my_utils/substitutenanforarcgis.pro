PRO SubstituteNaNforArcGIS
list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES )


FOR i = 0, N_ELEMENTS(LIST) -1 DO begin

  ns = FIX(read_info('samples', list[i]))
  nl = FIX(read_info('lines', list[i]))
  nb = FIX(read_info('bands', list[i]))
  dt = FIX(read_info('data type', list[i]))
  IF (dt NE 4) THEN STOP 
  IF (nb GT 1) THEN STOP
  fName = STRSPLIT(list[i], '.',/EXTRACT)
  res = FILE_INFO(fName[0])
  IF (res.exists EQ 0) THEN BEGIN
    res = FILE_INFO(fName[0]+'.img')
    IF (res.exists EQ 0) THEN STOP
    basename = fName[0]
    fname = fName[0]+'.img'
  END ELSE BEGIN
    basename = fname[0]
    fname = fname[0]
  ENDELSE
  mat = FLTARR(ns, nl)
  
  OPENR, lun, fname, /GET_LUN
  READU, lun, mat
  FREE_LUN, lun
  indNaN = WHERE(FINITE(mat) EQ 0, countNaN)
  IF (countNaN GT 0) THEN mat[indNaN]=-999
  
  OPENW, lun, basename + '_ARCGIS999.img', /GET_LUN
  WRITEU, lun, mat
  FREE_LUN, lun
  
  FILE_COPY, list[i], basename + '_ARCGIS999.hdr'

ENDFOR

PRINT, 'Task substitute completed'
END