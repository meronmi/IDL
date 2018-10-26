PRO test_Read_MTILE
storage_dir = 'Q:\MODIS_NIGER\MODIS LST\LST_day_1km\Single_Dates'
datainfo = {datatype: 12, $
            ins: 1904, $
            inl: 1456, $
            tns: 100, $
            tnl:100, $
            year0: 2000, $
            doy0: 65, $
            year1: 2010, $
            doy0: 65}
res = Read_MTILE(storage_dir, 'CCCCCCCYYYYDDDCCCCCCCCCCCC', datainfo, 100, 100, /WARN) 
END


FUNCTION Read_MTILE, storage_dir, fname_template, datainfo, uls, ull, warn = wn
;PURPOSE:
; Extract a 3D cube (X, Y, time) required dimension from a time series of single files
;CALLING SEQUENCE:
; Result = Read_MTILE( ARR1, ARR2 [, keywords])
;INPUTS:
; - storage_dir: (string) directory where the single file are stored
; - fname_template: (string) template for file name (C = character, YYYY = year, DDD = DOY), for 
;                   example CCCYYYYDDDCCC can be MOD2011032LST 
; - datainfo: (structure)
;             datainfo.datatype: input data type, it can be 1 (BYTE), 12 (UNSIGNINT)
;             datainfo.ins: number of sample in the input image
;             datainfo.inl: number of line in the input image
;             datainfo.tns: number of sample in the output cube
;             datainfo.tnl: number of line in the output cube
;             datainfo.year0: first year to be read
;             datainfo.doy0: first DOY to be read
;             datainfo.year1: last year to be read
;             datainfo.doy0: last DOY to be read  
; - uls: sample of Upper Left corner
; - ull: line of Upper Left corner             
;KEYWORD PARAMETERS:
;    /WARN
; Switch. If set, check the time series and issue a warning message if needed
;OUTPUTS:
; Returns the 3D data cube or an error flag
; -1: template does not contain YYYY or DDD

  ;FOR MODIS daily
  files = FILE_BASENAME(FILE_SEARCH(storage_dir + '\' + '*.img'))
  y_beg = STRPOS(fname_template, 'YYYY')
  d_beg = STRPOS(fname_template, 'DD')
  
  IF ((y_beg LT 0) OR (d-beg LT 0)) THEN RETURN, -1
  
  yyyy = FIX(STRMID(files, y_beg, y_beg+4))
  ddd = FIX(STRMID(files, d_beg, d_beg+3))
  tmp = yyyy + ddd/370.0
  ;reorder files and array chronologically
  files = files[SORT(tmp)]
  yyyy = yyyy[SORT(tmp)]
  ddd = ddd[SORT(tmp)]

  
  IF KEYWORD_SE(wn) THEN BEGIN
    ;Control the quality of the time series and issue a warning if there is a problem
    ;Check that the first and last day requested are present
    ind0 = WHERE((yyyy EQ datainfo.year0) AND (ddd EQ datainfo.doy0), count0)
    IF (count0 NE 1) THEN PRINT, 'No or more than one image present for the first date'
    ind0 = WHERE((yyyy EQ datainfo.year1) AND (ddd EQ datainfo.doy1), count1)
    IF (count1 NE 1) THEN PRINT, 'No or more than one image present for the lastt date'
    ;Check that there are not missing images in between
    IF ((count0 EQ 1) AND (count1 EQ 1)) THEN BEGIN
      yyyy0 = yyyy[ind0]
      ddd0 = ddd[ind0]
      FOR i = ind0+1, ind1 DO BEGIN
        CASE yyyt[i] OF
          yyyy0: IF ddd[i] NE ddd0+1 THEN PRINT, STRTRIM(yyyy[i],2) + ' ' + STRTRIM(ddd[i],2) + ', missing file before this date'
          yyyy0 + 1: BEGIN
            ;here checak if it is the 1st day of year and if the previous was 366 or 366 (use JD utilities to cmpute the right one)
            
            ;SONO QuI!!!!!
            
            
            END 
          ELSE:  PRINT, STRTRIM(yyyy[i],2) + ' ' + STRTRIM(ddd[i],2) + ', missing file before this date'
        ENDCASE
      
      ENDFOR
    ENDIF
  ENDIF
  ss = 3000
  ll = 3000
  ds = 1000
  dl = 1000
  PRINT, 'START'
  STARTTIME = SYSTIME(1)
  zprofile = BYTARR(ds, dl, N_ELEMENTS(files))
  FOR f = 0, N_ELEMENTS(files)-1 DO BEGIN
    zprofile[*,*,f] =  READ_BINARY (storage_dir + files[f], DATASTART = (ss * ll), DATA_TYPE = 1, DATA_DIMS=[ds,dl])
  ENDFOR
  ELAPSED_TIME = SYSTIME(1) - STARTTIME
  PRINT, 'Reading z-profile TOOK seconds: ' + STRTRIM(ELAPSED_TIME,2)
END