FUNCTION test_IDL_bridge
;Create the desired number of new IDL_IDLBridge object.
nPro = 1;3  ;number of process, up to a maximum number equal the number of logical processor cores (8 on my machine)
array_size = 5
oIDLBridgeArray = OBJARR(nPro) ;array of Bridge objects
res = FLTARR(array_size, nPro)             ;variable to store the results of the child process, specific to the process
status = BYTARR(nPro)
ErrorString=STRARR(nPro) 
parallelProName = 'idlbridge_parallelStatement'


tt=systime(/sec)
FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i] = OBJ_NEW("IDL_IDLBridge")
  IF (~OBJ_VALID(oIDLBridgeArray[i])) THEN BEGIN
    void=DIALOG_MESSAGE(/ERROR,'Unable to create an IDL_IDLBridge session')
    WIDGET_CONTROL, wWrapper, /DESTROY
    RETURN, 0l
  ENDIF
  PRINT, 'Child process ' + STRTRIM(i,2) + ' was initialized'
ENDFOR

FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i]->SetVar, 'x', FINDGEN(array_size)*(i+1)
  oIDLBridgeArray[i]->Execute, 'y=' + parallelProName + '(x)', /NOWAIT
ENDFOR

eob = 0
WHILE (eob EQ 0) DO BEGIN
  ;check who has finished or if I have errors
  FOR i = 0, nPro-1 DO BEGIN
    status[i]=oIDLBridgeArray[i]->Status(ERROR=err)
    IF (status[i] GE 3) THEN BEGIN
      PRINT, 'Problem with child ' + STRTRIM(i,2) + ', ' + err
    ENDIF
    ErrorString[i] = err
  ENDFOR
  ;get inedex of those that have finished
  indIdle = WHERE(status EQ 0, countIdle)
  IF (countIdle EQ nPro) THEN eob = 1
ENDWHILE

FOR i = 0, nPro-1 DO BEGIN
  res[*,i] = oIDLBridgeArray[i]->GetVar('y')
ENDFOR
PRINT, res
PRINT, 'FIRST USE time', systime(/sec)-tt



OBJ_DESTROY, oIDLBridgeArray
RETURN, 10
END

