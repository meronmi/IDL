FUNCTION TZP_realign_pheno_products, ord_dek_of_input_01_range, fstcalendaryrts, ord_dek_of_output_01_range, path, globstat_fname, $
         sos_fname, fnames, ns, nl, nb;, nbts
         
;  Terminology
;  'ordinal dekad'  Is the ordinal dekad, the first, the second, up to the 36th
;  'dekoy'   is the dekad of the year (0.0-36.0), it tells where in the year but not in which year
;            It can be interpreted as completion of dekad. The first dekad
;            goes from GT 0.0 to LE 1.0 and so on.
;            [0.0001-1.0, 1.0001-2.0, ..] 
;            first dekad = "dekad 1"   = cover the dekoy range 0.0001- 1
;            xth dekad = "dekad x"     = cover the dekoy range dekoy x-0.9999 - x
;            10th dekad (start of VGT) = cover the dekoy range dekoy 9.0001-10 and its
;            central point is 9.5
;  'pdekad'  is the progressive dekad counting from a start_dekoy 
;            (it is a dekoy that must be  given with the calendar year to interpret pdekad).
;            If start_dekoy = 0.5 (first dekad) and Year=Y then pdekad 
;            where pdekad=1.5 is the second dekad of Year Y and so on ..
;            
;            Always to get dekoy and calendar_year from pdekad, start_dekoy, start_caldendar_year:
;            dekoy=(pdekad+start_dekoy) MOD 36
;            IF dekoy EQ 0 THEN decoy = 36
;            calendar_year =  FLOOR((pdekad+start_dekoy)/36.0)+start_caldendar_year

;  add05     Date results of Pheno are expressed with 0 being associated to the first dekad of
;            the time series. According to the terminology above I will add 0.5 to date results of Pheno
;            to point to the central day of the dekad (so 0 will become 0.5)
;  

;  
;  Purpose:
;     This function align the pheno products bands according to 
;     a given year-long cycle of interest (alway 36 dekads, can be calendar but also user defined to match
;     the true local repeating cycle (e.g. in Somalia the period february to february 
;     includes normally the 2 GS).
;     *** The procudure align the seasons wisely 
;     The alignment to the arbitray year of interest is based on SOS (EOS can be also used)
;      if two seasons are present the first one is the one occurring
;     earlier in that year.
;  Input:
;     - ord_dek_of_input_01_range:  ordinal number of the dekad corresponding to pdekad = 0, dekad of 
;       first image of the time series (e.g. 10, if VG from 1998 are used)
;     - fstcalendaryrts: first calendar year in the RS time series (e.g. for VGT=1998)
;     - ord_dek_of_output_01_range: ordinal dekad of the start of the cycle of interest 
;       (e.g. 1 if calendar year is used)
;     - path: [compulsory] working path
;     - globstat_fname:[compulsory] used to check how many ngspy
;     - sos_fname: [compulsory] used to realing (only base name, not 1 or 2, e.g. 'sos')
;     - fname_list: vectors of files to be realigned, can be '*' meaning all
;     - ns, nl, nb of pheno product (SOS1 for example)
;     - (OBSOLETE) nbts: number of bands in the timeseries (
;     
;     obs: only bil files    
;  Outcome:
;     - realign and save all the files given as input 
;  Usage:
;     print, TZP_realign_pheno_products(1, 2008, 1, 'Q:\MODIS_Niger\new\DIR_RECOMPOSED_UppEnv', 'globstat', 'sos',['sos'], 7730, 3122, 4)
;     print, TZP_realign_pheno_products(10, 1998, 28, 'Q:\HoA\VGT data\raw\bil\Q:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv_2012_01_01', 'globstat', 'eos',['eos','sos','acc','len'], 3586, 3810, 15)
;     print, TZP_realign_pheno_products(10, 1998, 1, 'Q:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv_15_Mar', 'globstat', 'sos',['maxt'], 3586, 3810, 14)
;     print, TZP_realign_pheno_products(10, 1998, 1, 'K:\WA\raw\bil\DIR_RECOMPOSED_UppEnv_23_Jan', 'globstat', 'sos',['*'], 4716, 1233, 14)
;     print, TZP_realign_pheno_products(10, 1998, 1, 'K:\Tunisia\VGT_data\bil\pheno_productsFEB12', 'globstat', 'eos',['*'], 494, 842, 14)
;     print, TZP_realign_pheno_products(10, 1998, 1, 'K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv_17_feb', 'globstat', 'eos',['*'], 494, 842, 14)
;     print, TZP_realign_pheno_products(1, 1999, 1, 'K:\Tunisia\BP_data\BIL\DIR_RECOMPOSED_UppEnv', 'globstat', 'eos',['sos'], 494, 842, 13)
;  Return value: 
;     0:  normal completion 
;    10:  error has occurred

;BUILD FRAMEWOK FOR ALIGNMENT
;the goal is to build a general 2  images (array 'orderS' and 'orderB') coherent with the new aligment.
;First array (orderS):
;- 1/2 (0 indicate that the appropriate pheno product is in sos1 or 2
;Second band (orderB):
;- the cell value at band i (i is the index of arbitrary year i) is the index pointing
;to the correct orginal pheno band 

useSave = 0   ;set it to 1 if creation of matrix was already performed
            ;and you want to use SAVE, orderS, ordeB, filename=path+'\'+'order'

add05  = 1    ;set to 1 to add 0.5 to pheno results axpressed in pdekad

IF (add05 EQ 1) THEN BEGIN
  PRINT, '#########################################################
  PRINT, 'Warning: adding 0.5 to pheno results axpressed in pdekad'
  PRINT, 'to point to the central day of the dekad '
  PRINT, '#########################################################
ENDIF
  
;FIND THE number of band for output
;-Compute the number of bands of the output
; First and last dekad of time series according to 
; the time series calendar year time scale (1 is the 0101 of the first year for which the 
; time series is available, and can be actually available later.
; In case of VGT the first dekad (1) would be 9801 (while the first available data is dekad 10)
; The last dekad is the last dekad available in the time series, counted as progressive number from 
; first dekad 
; ---

;Make a directory to store the data
path_out=path + '\' + 'REALIGN_ON_' + STRTRIM(sos_fname,2)
FILE_MKDIR, path_out

;Treat fnames request (if *, realign all)
IF (fnames[0] EQ '*') THEN BEGIN
  CD, path
  phdrs=FILE_SEARCH('*1.hdr')             ;pheno hdr names
  fnames=phdrs                            ;pheno files base names
  FOR i = 0, N_ELEMENTS(phdrs)-1 DO BEGIN
    ;tmp=STRSPLIT(phdrs[i], '1.', /EXTRACT)
    fnames[i]=STRMID(phdrs[i],0, STREGEX(phdrs[i],'1.hdr'))
  ENDFOR  
ENDIF
; compute the array pdekad from dekoy 0 of the first calendar year of the time series
; note that from pdekad0 = 0 to ord_dek_of_input_01_range-1, RS are not available 

IF ord_dek_of_output_01_range EQ 1 THEN BEGIN
  ;the cycle of interest is the calendar cycle
  nbout = nb+1 ;??? it was: ceil(N_ELEMENTS(pdekad0)/36.0)
  fstcalendaryrout = fstcalendaryrts -1
ENDIF ELSE BEGIN
  ;the cycle of interest does not start the first dekad
  ;but somewhere in the first year. To avoid loosing the data that may be present between the first dekad
  ;of the first year and this somewhere, I to count the number of year from this somewhere,
  ;and add one year before
  nbout = nb+1
  fstcalendaryrout = fstcalendaryrts - 1
ENDELSE

;prefix for file out
prefix='A'+STRTRIM(ord_dek_of_output_01_range,2)+STRTRIM(sos_fname,2)+'-'+STRTRIM(fstcalendaryrout,2)+'_'

STARTTIME = SYSTIME(1)
IF (useSave EQ 0) THEN BEGIN
  ;OPEN SOS FILES FOR INPUT
  ;-SOS1
  fname=path+'\'+sos_fname+'1'
  IF FILE_TEST(fname) EQ 0 THEN return, 10
  OPENR, RSOS1, fname, /GET_LUN
  assoSOS1 = ASSOC(RSOS1, FLTARR(ns,nb))
  ;-SOS2
  fname=path+'\'+sos_fname+'2'
  IF FILE_TEST(fname) EQ 0 THEN return, 10
  OPENR, RSOS2, fname, /GET_LUN
  assoSOS2 = ASSOC(RSOS2, FLTARR(ns,nb))
  ;globstat
  fname=path+'\'+globstat_fname
  IF FILE_TEST(fname) EQ 0 THEN return, 10
  OPENR, RGlobStat, fname, /GET_LUN
  assoGlobStat = ASSOC(RGlobStat, INTARR(ns,2))
  
  
  

  ;CREATE the order matrix based on SOS dimensions
  ;for season1 and season2
  orderS=INTARR(ns,nl,nbout,2)-999
  orderB=INTARR(ns,nl,nbout,2)-999
  
  
  
  
  ;LOOP ON THE IMAGE AND BUILS orderS and orderB arrays
  FOR line=0, nl-1, 1L DO BEGIN
    IF ((line MOD (nl/10)) eq 0) then begin  ;every 10%
      print, 'Creating order matrix, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
    ENDIF
    globstat=float(assoGlobStat[line])
    ngspy=REFORM(globstat[*,1])
    sos1=float(assoSOS1[line])
    sos2=float(assoSOS2[line])
    ;add 0.5 to point the central day if required
    IF (add05 EQ 1) THEN BEGIN
      ind = WHERE((FINITE(sos1) EQ 1) AND (sos1 NE -999), count)
      IF (count NE 0) THEN sos1[ind] = sos1[ind] + 0.5
      ind = WHERE((FINITE(sos2) EQ 1) AND (sos2 NE -999), count)
      IF (count NE 0) THEN sos2[ind] = sos2[ind] + 0.5
    ENDIF
    
    
    FOR column=0, ns-1, 1L DO BEGIN      ; loop over all lines
      ;if column EQ 247 and line eq 216 then stop
      
      IF (ngspy[column] GT 0) THEN BEGIN  ;exclude from the analysis those with no data
        sosX=FLTARR(nb,2) ;first row is sos1, second is sos2
        sosX[*,0]=REFORM(sos1[column,*])
        sosX[*,1]=REFORM(sos2[column,*])
              
        ;assign order      
        IF  (ngspy[column]) EQ 1 THEN BEGIN
          ;one GS per Year
          orderS[column, line, *, 0] = 0  ;season2 (column, line, *, 2) was initialized to -999
          orderB[column, line, *, 0] = $
              align_cycleX_2_cycleY(sosX[*,0], ord_dek_of_input_01_range, fstcalendaryrts, $
              ord_dek_of_output_01_range, fstcalendaryrout)          
        ENDIF ELSE BEGIN;ngspy[column] is 2
          b = INTARR(nbout, 2)
          b[*, 0] = align_cycleX_2_cycleY(sosX[*,0], ord_dek_of_input_01_range, fstcalendaryrts, $
              ord_dek_of_output_01_range, fstcalendaryrout)
          b[*, 1] = align_cycleX_2_cycleY(sosX[*,1], ord_dek_of_input_01_range, fstcalendaryrts, $
              ord_dek_of_output_01_range, fstcalendaryrout)          
          ;ind0 = WHERE(b[*, 0] NE -999)
          ;ind1 = WHERE(b[*, 1] NE -999)
          ;identify the index of the season coming first
          ;frst_ind = find_first_season(sosX[ind0,0], sosX[ind1,1], $
          frst_ind = find_first_season(REFORM(sosX[*,0]), REFORM(sosX[*,1]), $
                        ord_dek_of_input_01_range, fstcalendaryrts, ord_dek_of_output_01_range, fstcalendaryrout)
          ;as the first season in output point to the first in input 
          orderS[column, line, *, 0] = frst_ind  
          orderS[column, line, *, 1] = 1 - frst_ind
          orderB[column, line, *, 0] = REFORM(b[*, frst_ind])
          orderB[column, line, *, 1] = REFORM(b[*, 1- frst_ind])          
        ENDELSE  
      ENDIF ;if not exclude because has 0 ngspy 
    ENDFOR  ;column
  ENDFOR  ;line
  SAVE, orderS, orderB, filename=path_out+'\'+'order'
  CLOSE, /ALL  
ENDIF ELSE BEGIN
  PRINT, 'ATTENTION: option restore save was used!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  RESTORE, path_out+'\'+'order'
ENDELSE     
;Now close the files and realign vectors
;*** NOT that when orderS or orderB is -999 the season is not the RS record,
;*** it's not missing
;globstat to use ngspy
ngspy=FLTARR(ns,nl)
fname=path+'\'+globstat_fname
IF FILE_TEST(fname) EQ 0 THEN return, 10
OPENR, RGlobStat, fname, /GET_LUN
assoGlobStat = ASSOC(RGlobStat, INTARR(ns,2))
FOR line=0, nl-1, 1L DO BEGIN
  globstat=float(assoGlobStat[line])
  ngspy[*,line]=globstat[*,1]
ENDFOR
FOR f=0, N_ELEMENTS(fnames)-1 DO BEGIN
  PRINT, 'Realigning file '+STRTRIM(path + '\' + fnames[f], 2)
  ;OPEN INPUT
  fname=path+'\'+fnames[f]+'1'
  IF FILE_TEST(fname) EQ 0 THEN return, 10
  OPENR, RIn1, fname, /GET_LUN
  assoIn1 = ASSOC(RIn1, FLTARR(ns,nb))
  
  fname=path+'\'+fnames[f]+'2'
  IF FILE_TEST(fname) EQ 0 THEN return, 10
  OPENR, RIn2, fname, /GET_LUN
  assoIn2 = ASSOC(RIn2, FLTARR(ns,nb))
  
   ;OPEN OUTPUT
  fname=path_out+'\'+prefix+fnames[f]+'1'
  IF FILE_TEST(fname) EQ 1 THEN FILE_DELETE, fname
  OPENW, ROut1, fname, /GET_LUN, /APPEND
  
  fname=path_out+'\'+prefix+fnames[f]+'2'
  IF FILE_TEST(fname) EQ 1 THEN FILE_DELETE, fname
  OPENW, ROut2, fname, /GET_LUN, /APPEND
  
  ;Check if it is variable expressed in progressive dekad (sos, eos, ..)
  IF (fnames[f] EQ 'sos') OR (fnames[f] EQ 'eos') $
  OR (fnames[f] EQ 'eose') OR (fnames[f] EQ 'maxt') THEN BEGIN
    PRINT, fnames[f] + ' is considered expressed in pdekad, values could be recalculated according to new year out'
    isinpdek=1
  ENDIF ELSE BEGIN
    PRINT, fnames[f] + ' is not considered expressed in pdekad, values are not changed'
    isinpdek=0
  ENDELSE
  ;ALIGN
  ;make in/out lines
  lineIn12=FLTARR(ns, nb, 2)  ;0 is for season1, 1 for season 2
  lineOut1=FLTARR(ns, N_ELEMENTS(orderS[0,0,*,0]))
  lineOut2=lineOut1
  FOR line=0, nl-1, 1L DO BEGIN
    lineIn12[*,*,0]=float(assoIn1[line])
    lineIn12[*,*,1]=float(assoIn2[line])
    lineOut1=lineOut1*!VALUES.F_NAN
    lineOut2=lineOut2*!VALUES.F_NAN
    FOR column=0, ns-1, 1L DO BEGIN
      ;debug
;      IF (line EQ 3016) and (column EQ 2592) THEN STOP
      IF ngspy[column,line] GT 0 THEN BEGIN
        IF (isinpdek EQ 1) THEN BEGIN
          ;add 0.5 to point the central day if required
          IF (add05 EQ 1) THEN BEGIN
            data=lineIn12[column,*,0]
            ind = WHERE((FINITE(data) EQ 1) AND (data NE -999), count)
            IF (count NE 0) THEN data[ind] = data[ind] + 0.5
            lineIn12[column,*,0] = data
            data=lineIn12[column,*,1]
            ind = WHERE((FINITE(data) EQ 1) AND (data NE -999), count)
            IF (count NE 0) THEN data[ind] = data[ind] + 0.5
            lineIn12[column,*,1] = data
          ENDIF
        ENDIF
        ;bands tha will have to be in season1
        bands4Out1=REFORM(orderB[column,line,*,0])
        ;remove -999, they will be NaN as initialized
        ind=WHERE(bands4Out1 NE -999, count)
        IF (count GT 0) THEN bands4Out1=bands4Out1[ind] ELSE stop
        IF (isinpdek EQ 1) THEN $
          lineOut1[column,ind]=pdekad_decX_yearX_2_pdekad_dec1_yearY($
          lineIn12[column,bands4Out1,orderS[column,line,0,0]], ord_dek_of_input_01_range, fstcalendaryrts, fstcalendaryrout) $
        ELSE $
            lineOut1[column,ind]=lineIn12[column,bands4Out1,orderS[column,line,0,0]]
          
        ;there's the second season
        IF ngspy[column,line] GT 1 THEN BEGIN 
          bands4Out2=REFORM(orderB[column,line,*,1])
          ind=WHERE(bands4Out2 NE -999, count)
          IF (count GT 0) THEN bands4Out2=bands4Out2[ind] ELSE stop
          IF (isinpdek EQ 1) THEN $
            lineOut2[column,ind]=pdekad_decX_yearX_2_pdekad_dec1_yearY($
            lineIn12[column,bands4Out2,orderS[column,line,0,1]], ord_dek_of_input_01_range, fstcalendaryrts, fstcalendaryrout) $
          ELSE $
            lineOut2[column,ind]=lineIn12[column,bands4Out2,orderS[column,line,0,1]]
        ENDIF
        ;print, 'debug'
      ENDIF ;ngspy
    ENDFOR ;column
    ;WRITE LINES
    WRITEU, ROut1, lineOut1
    WRITEU, ROut2, lineOut2
  ENDFOR  ;line
   ;CLOSE OTHER FILES
  CLOSE, /ALL
ENDFOR  ;f

;WRITE HDR
FOR f=0, N_ELEMENTS(fnames)-1 DO BEGIN 
  FOR s=1, 2 DO BEGIN
    fname=path_out+'\'+prefix+fnames[f]+STRTRIM(s,2)+'.hdr'
    OPENW, lun, fname, /GET_LUN
    PRINTF,lun,'ENVI'
    PRINTF,lun,'description = {realigned pheno results}'
    PRINTF,lun,'samples ='+STRCOMPRESS(ns)
    PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
    PRINTF,lun,'bands   ='+STRCOMPRESS(nbout)
    PRINTF,lun,'header offset = 0'
    PRINTF,lun,'file type = ENVI Standard'
    PRINTF,lun,'data type = 4'
    PRINTF,lun,'interleave = bil'
    PRINTF,lun,'byte order = 0'
    PRINTF,lun,'values = {If dekads, it is number of dekad since 0, first dekad (0.1-1)'+ $
               ' of '+STRTRIM(fstcalendaryrout,2)+'}';fstcalendaryrout
    PRINTF,lun,'band names = {'
    FOR b=fstcalendaryrout, fstcalendaryrout+nbout-1 DO BEGIN
      lstd=ord_dek_of_output_01_range-1
      nxty=b+1
      IF (lstd EQ 0) THEN BEGIN
        lstd=36
        nxty=b
      ENDIF 
      IF (b EQ fstcalendaryrout+nbout-1) THEN $
      PRINTF,lun, STRTRIM(ord_dek_of_output_01_range,2)+'/'+STRTRIM(b,2)+ $
                  ' - '+STRTRIM(lstd,2)+'/'+STRTRIM(nxty,2)+ '}'$
      ELSE PRINTF,lun, STRTRIM(ord_dek_of_output_01_range,2)+'/'+STRTRIM(b,2)+ $
                  ' - '+STRTRIM(lstd,2)+'/'+STRTRIM(nxty,2)+',' 
    ENDFOR  ;b
    FREE_LUN, lun
  ENDFOR  ;s
ENDFOR  ;f

; Evaluation of processing time
ELAPSED_TIME = SYSTIME(1) - STARTTIME
HOURS =  FLOOR(ELAPSED_TIME / (60*60))
MINUTES = FLOOR((ELAPSED_TIME MOD (60*60))/ 60)
SECS = FLOOR((ELAPSED_TIME - HOURS*60*60-MINUTES*60))
PRINT, 'OVERALL PROCESSING TOOK :'+STRCOMPRESS(HOURS)+' HOURS, ' +STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
RETURN, 0
END