FUNCTION read_band_names, basename
;usage= bnames=read_band_names(full path basename, without '.hdr')
;return null string if no bnames found

;Load band names from HDR
IF (FILE_TEST(basename+'.hdr') NE 1) THEN BEGIN
  PRINT, 'Read_band_names Warning: hdr of input file was not found'
  RETURN, ''
ENDIF ELSE BEGIN
  nlines=FILE_LINES(basename+'.hdr')
  OPENR, R2, basename+'.hdr', /GET_LUN
  strarray=STRARR(1)
  str='' & res=0 & c=0 & finish=0
  FOR i=0L, nlines[0]-1 DO BEGIN
    READF, R2, str
    islastband=STRMATCH(str,'*}')
    IF res EQ 0 THEN res=STRMATCH(str,'*band names*')
    IF (res EQ 1) AND (finish NE 1) THEN BEGIN
      IF (c EQ 0) THEN strarray[c]=str ELSE strarray=[strarray, str]
      c = c + 1
      IF (islastband EQ 1) THEN finish=1
    ENDIF 
  ENDFOR
  FREE_LUN, R2
  IF (c GT 0) THEN RETURN, strarray ELSE RETURN, ''
ENDELSE
END