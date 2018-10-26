PRO change_no_data
res = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.img', GET_PATH = dir)
n = N_ELEMENTS(res)
;************************
;fill this:
val2src = 'NaN' ; can be 'NaN' or any numeric 
val2put = -9999
;NOT YET IMPLENTED:
;overwrite = 0 ;if to overwrite or not
;prefix = '4ArcGis_' ;if no overwrite use this prefix 
;************************
FOR i=0, n-1 DO BEGIN
  hdr_fn = STRSPLIT(res[i], '\', /EXTRACT)
  hdr_fn = STRSPLIT(hdr_fn[-1], '.', /EXTRACT)
  hdr_fn = dir + '\' + hdr_fn[0] + '.hdr'
  ns = LONG(read_info('samples', hdr_fn))
  nl = LONG(read_info('lines', hdr_fn))
  dt = FIX(read_info('data type', hdr_fn))
  IF (dt EQ 1) AND ((val2put LT 0) OR (val2put GT 255)) THEN STOP
  CASE dt OF
    1: mat = BYTARR(ns,nl)
    2: mat = INTARR(ns,nl)
    4: mat = FLTARR(ns,nl)
    5: mat = DBLARR(ns,nl)
    ELSE: STOP
  ENDCASE
  OPENR, lun, res[i], /GET_LUN
  READU, lun, mat
  FREE_LUN, lun
  sz = SIZE(val2src)
  IF sz[1] EQ 7 THEN ind = WHERE(~FINITE(mat), count) ELSE ind = WHERE(mat EQ val2src, count)
  IF count GT 0 THEN BEGIN
    mat[ind] = val2put
    OPENW, lun, res[i], /GET_LUN
    WRITEU, lun, mat
    FREE_LUN, lun
  ENDIF
  ;the hdr
  fn = STRSPLIT(res[i], '.', /EXTRACT)
  fn = fn[0] + '.hdr'
  tmp_fn = fn + '000'
  FILE_COPY, fn, tmp_fn
  OPENR, lunR, tmp_fn, /GET_LUN
  OPENW, lunW, fn, /GET_LUN
  what = 'flags'
  WHILE ~ EOF(lunR) DO BEGIN
    tmp=''
    found = 0
    READF, lunR, tmp
    ;remove comments 
    tmp0 = STRSPLIT(tmp, ';', /EXTRACT)
    tmp0 = tmp0[0]    
    tmp0 = STRSPLIT(tmp0, '=', /EXTRACT)   
    IF (STRTRIM(tmp0[0],2) EQ what) THEN BEGIN
      found = 1
      PRINTF, lunW, 'flags = {' + STRTRIM(val2put,2) + '=nodata}'
    ENDIF ELSE BEGIN
      PRINTF, lunW, tmp
    ENDELSE
  ENDWHILE
  IF (found EQ 0) THEN PRINTF, lunW, 'flags = {' + STRTRIM(val2put,2) + '=nodata}'
  FREE_LUN, lunR
  FREE_LUN, lunW
  CLOSE, /ALL
  FILE_DELETE, tmp_fn
ENDFOR

END