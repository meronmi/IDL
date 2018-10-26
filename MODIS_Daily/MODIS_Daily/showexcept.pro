;
; Copyright (c) 2002-2013, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
; Purpose: print exception information after an 'Exception thrown' 
;
; Usage:
;    IDL> showexcept
;
 
pro SHOWEXCEPT

  ; Grab the special IDLJavaBridgeSession object
  oBridgeSession = OBJ_NEW("IDLJavaObject$IDLJAVABRIDGESESSION")

  ; Use session object to get our Exception
  oExc = oBridgeSession->getException()

  IF (oExc NE OBJ_NEW()) THEN BEGIN

     ; Now we can access the members java.lang.Throwable
     print, 'Exception thrown : ', oExc->toString()
     print, 'Stack Trace : '
     oExc->printStackTrace
					print, ''

     ; clean up
     OBJ_DESTROY, oExc

  ENDIF ELSE BEGIN
     print, 'No exception to show.'
  ENDELSE

  ; clean up
  OBJ_DESTROY, oBridgeSession

end
