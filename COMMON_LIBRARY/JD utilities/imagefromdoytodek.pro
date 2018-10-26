PRO ImageFromDoyToDek, ns, nl, dt, im_type

;this is used to transform pheno results into dekadal units for ferdinando

;im_type can be "avg" or "sd". Avg is wisely treated, sd is just divided by 10
list =  DIALOG_PICKFILE(FILTER='*.hdr', /MULTIPLE_FILES )
;list all first doy for each dek, from 1 to 36
fdoy = [1, 11, 21, 32, 42, 52, 60, 70, 80, 91, 101, 111, 121, 131, 141, 152, 162, 172, 182, 192, 202, 213, 223, 233, 244, 254, 264, 274, 284, 294, 305, 315, 325, 335, 345, 355]
ldoy = [10, 20, 31, 41, 51, 59, 69, 79, 90, 100, 110, 120, 130, 140, 151, 161, 171, 181, 191, 201, 212, 222, 232, 243, 253, 263, 273, 283, 293, 304, 314, 324, 334, 344, 354, 365]
;list all last doy for each dek, from 1 to 36

; ImageFromDoyToDek, 4538, 4124, 4, 'avg'


FOR j = 0, N_ELEMENTS(LIST) -1 DO BEGIN
  CASE FIX(dt) of
    4: data = FLTARR(ns,nl)
    2: data = INTARR(ns,nl)
    ELSE: STOP
  ENDCASE
  fName = STRSPLIT(list[j], '.',/EXTRACT)
  OPENR, lun, fName[0], /GET_LUN
  READU, lun, data
  FREE_LUN, Lun 
  check = INTARR(ns,nl)
  OPENW, lun, fName[0]+'_DEK', /GET_LUN
  IF (im_type EQ 'sd') THEN BEGIN
    data_out = data /10.0
  ENDIF ELSE BEGIN
    IF (im_type NE 'avg') THEN STOP
    indf = WHERE(FINITE(data), countf)
    data[indf] = FLOAT(FLOOR(data[indf])+1)  ;it starts at 0 a and goes to 365.0 (che è la fine di 364)
    ind366 = WHERE(data[indf] EQ 366, count366)
    IF (count366 NE 0) THEN data[indf[ind366]]=365.0
    data_out = data
    FOR i = 0, N_ELEMENTS(fdoy)-1 DO BEGIN
      ;retrive problematic
      ind_neg = WHERE(data[indf] LT 0, count_neg)
      IF (count_neg NE 0) THEN STOP
      ind_gt365 = WHERE(data[indf] GE 366, count_gt365)
      IF (count_gt365 NE 0) THEN STOP
      ;find the doy of dek i and substitute
      IF i GT 35 THEN STOP
      ind = WHERE((data[indf] GE fdoy[i]) AND (data[indf] LE ldoy[i]), count)
      PRINT, i+1, fdoy[i], ldoy[i]
      IF (count GT 0) THEN BEGIN 
        data_out[indf[ind]] = FLOAT(i + 1)
        check[indf[ind]] = 1
      ENDIF    
    ENDFOR
    IF (TOTAL(check[indf]) NE countf) THEN STOP
;    a = where(check[indf] EQ 0)
;    print, data[indf[a]]
  ENDELSE
  WRITEU, lun, data_out
  FREE_LUN, Lun 
  FILE_COPY, fName[0]+'.hdr',  fName[0]+'_DEK'+'.hdr'
END
END
