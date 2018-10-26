PRO syria_save_9999

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
    FILE_COPY, fName[0] + '.hdr', fName[0]+'_9999.hdr'
  ENDFOR
  PRINT, 'Task add_hdr completed'
END