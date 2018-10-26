FUNCTION read_yield, file, dlmtr
  str=''
  nlines=FILE_LINES(file)
  mat=FLTARR(3, nlines) ;dept, year, yield
  OPENR, lun, file, /get_lun
  FOR i=0L, nlines-1 DO BEGIN
    READF, lun, str
    dataInTheRow=DOUBLE(STRSPLIT(str, dlmtr, count=n_row, /EXTRACT))
    mat[0,i]=dataInTheRow[0]
    mat[1,i]=dataInTheRow[1]
    mat[2,i]=dataInTheRow[2]
  ENDFOR
  ind_nan=WHERE(mat EQ -999, count_nan)
  IF (count_nan GT 0) THEN mat[ind_nan]=!VALUES.F_NAN
  FREE_LUN, lun
  return, mat
END
;
;
;+
; :Description:
;  
; :Params:
;    file: Input CSV file to be read by the program
;
; :OUTPUTS:
;  info_csv structure containing fields:
;    -  region
;    -  year
;    -  yield
;    -  ndvi
;    
; :REQUIRES:
;
; :NOTES:
;
; :Author: Mic
;
; :HISTORY:
;
; Created: 
;-

FUNCTION RDCSV,file,dlmtr
  str=''
  nlines=FILE_LINES(file)
  reg=DBLARR(nlines-1) & year=DBLARR(nlines-1) & yield=DBLARR(nlines-1) & nd=DBLARR(21, nlines-1)
  OPENR, lun, file, /get_lun
  READF, lun, str
  FOR i=0L, nlines[0]-2 DO BEGIN
    READF, lun, str
    dataInTheRow=DOUBLE(STRSPLIT(str, dlmtr, count=n_row, /EXTRACT))
    reg[i]=dataInTheRow[0]
    year[i]=dataInTheRow[1]
    yield[i]=dataInTheRow[2]
    nd[*,i]=dataInTheRow[3:*]
  
  ENDFOR
  FREE_LUN, lun
  info_csv = {reg:reg, year:year, yield:yield, nd:nd}
  return, info_csv
END