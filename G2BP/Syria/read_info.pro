;_______________________________________________________________________________________
; Read Info file _______________________________________________________
FUNCTION read_info, what, filename_path
  COMPILE_OPT idl2
  ;this funcion can be called with what=PCtime(timesaving/solar),PCtime-UTC, UTCLATITUDE, LONGITUDE, WR_CAL_filename
  ;old:this funcion can be called with what=TIME(timesaving/solar),UTCLATITUDE, LONGITUDE, WR_CAL_filename
  ;ALWAYS return a string
  found=0
  ;print, filename_path
  IF FILE_TEST(filename_path) NE 1 THEN BEGIN
    PRINT, 'File was not found, the program will stop'
    STOP
  ENDIF
  OPENR, lun, filename_path, /GET_LUN ;check the parameter of GEO_location.txt
  WHILE ~ EOF(lun) DO BEGIN
    tmp=''
    READF, lun, tmp
    ;remove comments
    tmp0 = STRSPLIT(tmp, ';', /EXTRACT)
    tmp0 = tmp0[0]
    tmp0 = STRSPLIT(tmp0, '=', /EXTRACT)
    IF (STRTRIM(tmp0[0],2) EQ what) THEN BEGIN
      res=strtrim(tmp0[1],2)
      found=1
    ENDIF
  ENDWHILE
  FREE_LUN, LUN
  IF (found EQ 0) THEN BEGIN
    tmp = 'Error at read_info, <'+STRTRIM(what,2)+'> not found'
    null=dialog_message(tmp,/information)
    ;PRINT, 'No match, the program will stop'
    STOP
  ENDIF
  RETURN, res
END