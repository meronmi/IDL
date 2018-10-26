;_______________________________________________________________________________________
; Read Info file _______________________________________________________
FUNCTION read_info, what, filename_path, show_dialog = show_dialog
COMPILE_OPT idl2
  ;this funcion can be called with what=PCtime(timesaving/solar),PCtime-UTC, UTCLATITUDE, LONGITUDE, WR_CAL_filename
  ;old:this funcion can be called with what=TIME(timesaving/solar),UTCLATITUDE, LONGITUDE, WR_CAL_filename
  ;ALWAYS return a string
  
  found=0
  ;print, filename_path
  IF FILE_TEST(filename_path) NE 1 THEN BEGIN
    PRINT, 'File *** ' +  filename_path + ' *** was not found, the program will stop'
    STOP
  ENDIF
  OPENR, lun, filename_path, /GET_LUN ;check the parameter of GEO_location.txt
  WHILE ~ EOF(lun) DO BEGIN
    tmp=''
    READF, lun, tmp
    ;be sure that all the lines is not a comment (starting with ;)
    tmp = STRTRIM(tmp, 2)
    IF (STRMID(tmp, 0, 1) EQ ';') THEN tmp = ''
    ;remove comments 
    tmp0 = STRSPLIT(tmp, ';', /EXTRACT)
    tmp0 = tmp0[0]    
    tmp0 = STRSPLIT(tmp0, '=', /EXTRACT)   
    IF (STRTRIM(tmp0[0],2) EQ what) THEN BEGIN
      IF (N_ELEMENTS(tmp0) GT 2) THEN BEGIN
        ;there must be an = somewhere after the firs one
        FOR i = 2, N_ELEMENTS(tmp0)-1 DO tmp0[1] = tmp0[1] + '=' + tmp0[i]
      ENDIF
      res=strtrim(tmp0[1],2)
      found=1
    ENDIF
  ENDWHILE
  FREE_LUN, LUN
  
  IF (found EQ 0) THEN BEGIN
    tmp = 'Warning at read_info, <'+STRTRIM(what,2)+'> not found'
    IF  KEYWORD_SET(show_dialog) THEN null=dialog_message(tmp,/information)
    ;PRINT, 'No match, the program will stop'
    RETURN, -1
  ENDIF
  RETURN, res
END