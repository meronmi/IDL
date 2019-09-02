

PRO Standard_Processing
fn = 'X:\test_bridge\BBB_m_adj_1_bil'; this is a multiband (14 bands) example file
IF (STRTRIM(read_info('interleave', remove_ext_from_fn(fn) + '.hdr'),2) NE 'bil') THEN STOP
dt = FIX(read_info('data type',  remove_ext_from_fn(fn) + '.hdr'))
ns = LONG(read_info('samples',  remove_ext_from_fn(fn) + '.hdr'))
nl = LONG(read_info('lines',  remove_ext_from_fn(fn) + '.hdr'))
nb = FIX(read_info('bands',  remove_ext_from_fn(fn) + '.hdr'))
;*********************************

OPENR, lunR, fn, /GET_LUN
assoR = ASSOC(lunR, MAKE_ARRAY(ns,nb, TYPE=dt))
OPENW, lunW, fn+'_out', /GET_LUN
tt=systime(/sec)
FOR line = 0, nl-1, 1L DO BEGIN
  ;IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
;  IF ((line MOD FIX(nl/20)) EQ 0) THEN BEGIN
;    PRINT, 'debug line ' + STRTRIM(line) + '. Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
;  ENDIF
  lineOut = lineProcess(FLOAT(assoR[line]), ns, nb, 100)
  WRITEU, lunW, lineOut
ENDFOR
FREE_LUN, lunR
FREE_LUN, lunW
res = write_envi_hdr(fn+'_out', ns, nl, 4, NBANDS=4, INTERLEAVE='bil')
PRINT, 'Standard processing time', systime(/sec)-tt
END


FUNCTION Parallel_bil_processing
; This function uses IDL Bridges to parallelize a process that is made on single lines of a bil file
; That is, it speed up the typical time series processing of a bil file (read the line - process it - write the line)
;########################################################################################################################
;Set up of IDL bridge
nPro = 8                           ;number of process, up to a maximum number equal the number of virtual processor cores (16 on my machine = 2 * logical if virtualization enabled)
parallelProName = 'lineProcess'     ;name of the function to be executed in parallel.
                                    ;IMPORTANT: this function MUST be positioned in the IDL path (Window-Preferences IDL-Paths)
;########################################################################################################################
;OUTPUT
;define the dimension of the output line (the number of samples and lines are dictated by the input file)
nb_out = 4      ;numer of band of output image
dt_out = 4      ;datatype of output image
;########################################################################################################################
;INPUT
fn = 'X:\test_bridge\BBB_m_adj_1_bil'    ;input file name. ; this is a multiband (14 bands) example file
;########################################################################################################################



;Check input and collect relavant data
IF (STRTRIM(read_info('interleave', remove_ext_from_fn(fn) + '.hdr'),2) NE 'bil') THEN STOP
dt = FIX(read_info('data type',  remove_ext_from_fn(fn) + '.hdr'))
ns = LONG(read_info('samples',  remove_ext_from_fn(fn) + '.hdr'))
nl = LONG(read_info('lines',  remove_ext_from_fn(fn) + '.hdr'))
nb = FIX(read_info('bands',  remove_ext_from_fn(fn) + '.hdr'))

OPENR, lunR, fn, /GET_LUN
assoR = ASSOC(lunR, MAKE_ARRAY(ns, nb, TYPE=dt))
OPENW, lunW, fn+'_out_parallel', /GET_LUN
assoW = ASSOC(lunW, MAKE_ARRAY(ns, nb_out, TYPE=dt_out))
line = 0 

;Some housekeeping for the bridges
IF (nl LT nPro) THEN BEGIN
  PRINT, 'Typically it does not make sense to parallelize on ' + STRTRIM(nl,2) + ' lines, Please check'
  STOP
ENDIF
oIDLBridgeArray = OBJARR(nPro)      ;array of Bridge objects, one object = one child process that can be executed
status = BYTARR(nPro)               ;array for storing the status of the childs
ErrorString=STRARR(nPro)            ;array for storing potential problems of the child
inactive = BYTARR(nPro)             ;array to store the lifecycle of each child, it is turned to 1 when no more results are expected from this child

;Initialize childs
FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i] = OBJ_NEW("IDL_IDLBridge")
  IF (~OBJ_VALID(oIDLBridgeArray[i])) THEN BEGIN
    void=DIALOG_MESSAGE(/ERROR,'Unable to create an IDL_IDLBridge session')
    WIDGET_CONTROL, wWrapper, /DESTROY
    RETURN, 0l
  ENDIF
  PRINT, 'Child process ' + STRTRIM(i,2) + ' was initialized'
ENDFOR

tt=systime(/sec)

;Start by assigning a line to each of them
FOR i = 0, nPro-1 DO BEGIN
  oIDLBridgeArray[i]->SetVar, 'line', assoR[line]
  oIDLBridgeArray[i]->SetVar, 'ns', ns
  oIDLBridgeArray[i]->SetVar, 'nb', nb
  oIDLBridgeArray[i]->SetVar, 'linenumber', line
  oIDLBridgeArray[i]->Execute, 'lineOut=' + parallelProName + '(line, ns, nb, linenumber)', /NOWAIT
  oIDLBridgeArray[i]->SetProperty, USERDATA=line
  line = line + 1 ;get reday for the next line
ENDFOR

eop = 0     ;1 if all lines were sent to of processing (when it turns 1 no more excecutions of the childs)
eor = 0     ;1 if all theresults were harvested (when it turns 1 nothing more to do, exit the loop

;now test who is idle and let him process a new line, in the test loop take care of eor and eop conditions
WHILE eor EQ 0 DO BEGIN
  ;get the status of the childs
  FOR i = 0, nPro-1 DO BEGIN
    status[i]=oIDLBridgeArray[i]->Status(ERROR=err)
    ErrorString[i] = err
  ENDFOR
  
  ;check if I have problems
  indFail = WHERE(status GE 3, countFail)
  IF (countFail GT 0) THEN BEGIN
    FOR k = 0, countFail-1 DO PRINT, 'Problem with child ' + STRTRIM(k,2) + ', ' + err
    STOP
  ENDIF
  
  ;get inedex of those that have finished
  indIdle = WHERE(status EQ 0, countIdle)
  ;if some of them has finished
  IF (countIdle GT 0) THEN BEGIN 
    ;get the results of them
    FOR j = 0, countIdle-1 DO BEGIN  
      IF (inactive[indIdle[j]] EQ 0) THEN BEGIN
        ;get the results if it is not inactive  
        oIDLBridgeArray[indIdle[j]]->GetProperty, USERDATA=childLine ;indIdle[j] is the index of the child that has processed childLine
        assoW[childLine] = oIDLBridgeArray[indIdle[j]]->GetVar('lineOut')
;        IF ((childLine MOD FIX(nl/10)) EQ 0) THEN BEGIN
;          PRINT, 'debug line ' + STRTRIM(childLine) + '. Processing, '+string(childLine/float(nl)*100)+'% at '+string(SYSTIME(0))
;        ENDIF 
      ENDIF
      ;in case we have finished the lines, set them to inactive
      IF (eop EQ 1) THEN BEGIN
        ;no more new executions, I jusrt have to harvest wat is finishing
        inactive[indIdle[j]] = 1
      ENDIF
      ;if all are inactive set eor to 1 to get out of the cycle
      IF (TOTAL(inactive) EQ nPro) THEN eor = 1
    ENDFOR
    ;if there are still ine to be processed launch a new ececution
    IF (eop EQ 0) THEN BEGIN
    ;run a line processing for each of the idle childs
      FOR j = 0, countIdle-1 DO BEGIN
        oIDLBridgeArray[indIdle[j]]->SetVar, 'line', assoR[line]
        oIDLBridgeArray[indIdle[j]]->SetVar, 'ns', ns
        oIDLBridgeArray[indIdle[j]]->SetVar, 'nb', nb
        oIDLBridgeArray[indIdle[j]]->SetVar, 'linenumber', line
        oIDLBridgeArray[indIdle[j]]->Execute, 'lineOut=' + parallelProName + '(line, ns, nb, linenumber)', /NOWAIT
        oIDLBridgeArray[indIdle[j]]->SetProperty, USERDATA=line 
        ;check if the last line, in case exit the loop
        IF (line EQ nl-1) THEN BEGIN
          eop = 1 
        ENDIF ELSE BEGIN
          line = line + 1 ;get reday for the next line
        ENDELSE
      ENDFOR
    ENDIF ;eop
  ENDIF
  
  ;IF (countIdle EQ nPro) THEN eob = 1
 
ENDWHILE
;wait for all to end and do the rest
OBJ_DESTROY, oIDLBridgeArray
FREE_LUN, lunR
FREE_LUN, lunW
res = write_envi_hdr(fn+'_out_parallel', ns, nl, dt_out, NBANDS=nb_out, INTERLEAVE='bil')
PRINT, 'Parallel processing time with '+STRTRIM(nPro,2) + ' processors:', systime(/sec)-tt
RETURN, 10
END
