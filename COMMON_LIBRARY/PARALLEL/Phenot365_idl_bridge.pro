FUNCTION Pheno365_IDL_bridge
arguments= ['D:\LTDR\Pheno\DIR'+STRTRIM(INDGEN(20),2)+'\info.txt']
;Create the desired number of new IDL_IDLBridge object.
nPro = N_ELEMENTS(arguments)  ;number of process, up to a maximum number equal the number of logical processor cores (8 on my machine)

oIDLBridgeArray = OBJARR(nPro) ;array of Bridge objects
res = FLTARR(nPro)             ;variable to store the results of the child process, specific to the process
status = BYTARR(nPro)
ErrorString=STRARR(nPro) 
parallelProName = 'idlbridge_Pheno365'    ;ATTENZIONE QUESTA DEVE ESSERE NELLA PATH DI IDL
;arguments= ['D:\LTDR\Pheno\DIR4\info.txt']


tt=systime(/sec)
FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i] = OBJ_NEW("IDL_IDLBridge")
  IF (~OBJ_VALID(oIDLBridgeArray[i])) THEN BEGIN
    void=DIALOG_MESSAGE(/ERROR,'Unable to create an IDL_IDLBridge session')
    WIDGET_CONTROL, wWrapper, /DESTROY
    RETURN, 0l
  ENDIF
  oIDLBridgeArray[i]->SetVar, 'x', arguments[i]
  oIDLBridgeArray[i]->Execute, 'y=' + parallelProName + '(x)', /NOWAIT 
  PRINT, 'Child process ' + STRTRIM(i,2) + ' was initialized and launched'
ENDFOR
 
eob = 0
prevCountIdle = 0
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
  IF (countIdle GT prevCountIdle) THEN BEGIN
    PRINT, STRTRIM(countIdle,2) + ' childs are finished at ' + systime() + '. This are finished: ' + STRJOIN(STRTRIM(indIdle,2),', ')  
    prevCountIdle = countIdle
  ENDIF
  IF (countIdle EQ nPro) THEN eob = 1
ENDWHILE

FOR i = 0, nPro-1 DO BEGIN
  res[i] = oIDLBridgeArray[i]->GetVar('y')
ENDFOR
PRINT, res
PRINT, 'USE time', systime(/sec)-tt


OBJ_DESTROY, oIDLBridgeArray
RETURN, 10
END

