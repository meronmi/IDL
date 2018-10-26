Pro exampl
 firstYear = 1 
 lastYear = 2
 fileNames = STRARR((lastYear - firstYear + 1) *36)
 FOR year=firstYear,lastYear DO BEGIN
   FOR dekade=1,36 DO BEGIN
    fileNames[(year - firstYear) * 36 + dekade -1] = 'vt' + STRING(year, FORMAT='(I02)') + STRING(dekade, FORMAT='(I02)') + 'i.img' 
   ENDFOR
 ENDFOR
 print, fileNames


 ; MergeBip might throw an exception, so let's catch it
 catch, error_status
 IF error_status NE 0 THEN BEGIN

   catch, /cancel

   showexcept

 ENDIF

 width = 7841
 height = 8289
 wx = 100 ;UL
 wy = 200 ;UL
 ww = 50  ;width
 wh = 10  ;height

 mb = OBJ_NEW('IDLjavaObject$EU_EUROPA_EC_JRC_IES_MARS_MERGEBIP_MERGEBIP', 'eu.europa.ec.jrc.ies.mars.mergebip.MergeBip')
 print, 'MergeBip version ', mb->getVersion()
 mb->setInputDirPath, 'U:\data\cid-bulk15\TS\42002\AFR\ACT2\S10'
 mb->setFileNames,fileNames
 mb->setWindow, width, height, wx, wy, ww, wh
 
 ;How many files are opened?
 bands = mb->getBands()
 print, bands, " bands"
 
 
 ;InputStream
 is = mb->merge()
 ; At this point all the files are opened, ready to be read
 
 ;Create an array to hold the values for a single pixel
 PX = BYTARR(bands);
 length = 0
 t = SYSTIME(/SECONDS)
 ;Each block of bands bytes is a pixel
 FOR w=0,ww-1 DO BEGIN
 FOR h=0,wh-1 DO BEGIN
   read = is->read(PX, 0, bands)
   IF (read < 0) THEN message, "No more data"
   length = length + read
   ;print, PX
   ;sum = 0UL
   ;FOR x=0,bands-1 DO BEGIN
   ;  sum = sum + PX[x]
   ;ENDFOR
   ;avg = sum / bands
   ;print, avg
 ENDFOR
 ENDFOR
 is->close
 OBJ_DESTROY, is
 OBJ_DESTROY, mb
 print, length, ' bytes read in ',  SYSTIME(/SECONDS) -t, ' seconds'
end

;From the IDL command line, you can then use the session object to help debug the problem:
;IDL> oJSession = OBJ_NEW('IDLJavaObject$IDLJAVABRIDGESESSION')
;IDL> oJExc = oJSession->GetException()
;IDL> oJExc->PrintStackTrace