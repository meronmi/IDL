FUNCTION A0_TZP_realign_pheno_products_JD, frstYearOfTimeSeries, frstDOYofCycle, path, globstat_fname, $
         PE_fname, fnames, ns, nl, nb
 
;  Purpose:
;     This function align the pheno products bands according to 
;     a given year-long cycle of interest (alway 365 days, can be calendar but also user defined to match
;     the true local repeating cycle (e.g. in Somalia the period february to february includes normally the 2 GS).
;     *** The procudure align the seasons wisely 
;     The alignment to the arbitray year of interest is based on a PE (Penological Event) that 
;     can be SOS, EOS, ..
;     if two seasons are present the first one is the one occurring earlier in that output cycle.
;  Input:
;     - frstYearOfTimeSeries: first calendar year in the RS time series (e.g. for VGT=1998)
;     - frstDOYofCycle: Day Of Year (1-365) of the start of the cycle of interest (e.g. 1 if calendar year is used)
;     - path: working path
;     - globstat_fname: used to check how many ngspy
;     - PE_fname: the pheno event file base name used to realing (only base name, not 1 or 2, e.g. 'sos')
;     - fname_list: vectors of files to be realigned, can be '*' meaning all
;     - ns, nl, nb of pheno product (SOS1 for example)
;     
;  Restrictions: the routine works on bil files only   
;   
;  Outcome:
;     - realign and save all the files given as input. The aligned file is still expressed in Julian Days and
;       has alway one band more than the input file (because realignment may cause additional years) 
;  Usage:
;     print, A0_TZP_realign_pheno_products_JD(1998, 1, 'Q:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv', 'globstat', 'sos',['comp','eos','sos','acc','len','maxv','maxt'], 7841, 1458, 15)
;     print, A0_TZP_realign_pheno_products_JD(1998, 280, 'Q:\fromDek2DayDebug\pheno_products_day', 'globstat', 'eos',['comp','eos','sos','acc','len','maxv'], 3586, 3810, 15)
;     print, A0_TZP_realign_pheno_products_JD(1998, 1, 'Q:\AFAR region\pheno_products', 'globstat', 'sos',['comp','eos','sos','acc','len','maxv'], 4538, 4124, 15)
;     print, A0_TZP_realign_pheno_products_JD(1998, 1, 'Q:\Extended_HoA\DIR_RECOMPOSED_UppEnv', 'globstat', 'sos',['comp','eos','sos','acc','len','maxv'], 4538, 4124, 15)
;     print, A0_TZP_realign_pheno_products_JD(1998, 1, 'Q:\Extended_HoA\DIR_RECOMPOSED_UppEnv_LR6_iter_v1', 'globstat', 'sos',['comp','eos','sos'], 4538, 4124, 15)
;     print, A0_TZP_realign_pheno_products_JD(1998, 1, 'E:\Extended_HoA\DIR_RECOMPOSED_UppEnv_LR6_iter_v1', 'globstat', 'sos',['comp','eos','sos','len','acc'], 4538, 4124, 15)
;     print, A0_TZP_realign_pheno_products_JD(2001, 1, 'E:\ILRI\pheno\DIR_RECOMPOSED_UppEnv', 'globstat', 'sos',['comp','eos','sos','len','acc'], 3350, 4124, 13)
;     print, A0_TZP_realign_pheno_products_JD(2001, 1, 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\DIR_RECOMPOSED_UppEnv_22Feb2016', 'globstat', 'sos',['*'], 6155, 1721, 15)
;     print, A0_TZP_realign_pheno_products_JD(2001, 1, 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\DIR_RECOMPOSED_UppEnv_09Mar2016', 'globstat', 'sos',['*'], 6673, 3896, 15)
;     print, A0_TZP_realign_pheno_products_JD(1981, 1, 'Y:\users\meronmi\EL NINO Anton\LTDR\Screened data\Africa_subset\DIR_RECOMPOSED_UppEnv_21Mar2016', 'globstat', 'sos',['*'], 1572, 1697, 35)
;     ;A0_TZP_realign_pheno_products_JD(2002, 1, 'X:\FSNAU_pheno\DIR_RECOMPOSED', 'globstat', 'sos',['*'], 1344, 1624, 15)
;  Return value: 
;     0:  normal completion 
;    10:  error has occurred
;    20: input file not found

;BUILD FRAMEWOK FOR ALIGNMENT
;the goal is to build a two images (array 'orderS' and 'orderB') coherent with the new aligment.
;First array (orderS):
;- 1/2 (0 indicate that the appropriate pheno product is in sos1 or 2
;Second band (orderB):
;- the cell value at band i (i is the index of arbitrary year i) is the index pointing to the correct orginal pheno band 

useSave = 0   ;set it to 1 if creation of matrix was already performed
              ;and you want to use SAVE, orderS, ordeB, filename=path+'\'+'order'

  
;Make a directory to store the data
path_out=path + '\' + 'REALIGN_ON_' + STRTRIM(PE_fname,2)
FILE_MKDIR, path_out

;Treat fnames request (if *, realign all)
IF (fnames[0] EQ '*') THEN BEGIN
  CD, path
  phdrs=FILE_SEARCH('*1.hdr')             ;pheno hdr names
  fnames=phdrs                            ;pheno files base names
  FOR i = 0, N_ELEMENTS(phdrs)-1 DO fnames[i]=STRMID(phdrs[i],0, STREGEX(phdrs[i],'1.hdr'))
ENDIF

; To avoid loosing the increase the number of bands of the output
nbout = nb+1
frstYearOut = frstYearOfTimeSeries - 1

;prefix for file out
prefix='A'+STRTRIM(frstDOYofCycle,2)+STRTRIM(PE_fname,2)+'-'+STRTRIM(frstYearOut,2)+'_'

STARTTIME = SYSTIME(1)
IF (useSave EQ 0) THEN BEGIN
  ;Open PEs (Pheno Event, may be SOS or EOS) files for input
  ;-PE1
  fname=path+'\'+PE_fname+'1'
  IF FILE_TEST(fname) EQ 0 THEN RETURN, 20
  OPENR, RPE1, fname, /GET_LUN
  assoPE1 = ASSOC(RPE1, FLTARR(ns,nb))
  ;-PE2
  fname=path+'\'+PE_fname+'2'
  IF FILE_TEST(fname) EQ 0 THEN RETURN, 20
  OPENR, RPE2, fname, /GET_LUN
  assoPE2 = ASSOC(RPE2, FLTARR(ns,nb))
  ;globstat
  fname=path+'\'+globstat_fname
  IF FILE_TEST(fname) EQ 0 THEN RETURN, 20
  OPENR, RGlobStat, fname, /GET_LUN
  assoGlobStat = ASSOC(RGlobStat, INTARR(ns,2))
  
  ;Create the order matrix based on SOS dimensions for season1 and season2. Set it to -999.
  orderS=INTARR(ns,nl,nbout,2)-999
  orderB=INTARR(ns,nl,nbout,2)-999
    
  ;Loop on the image and build the orderS and orderB arrays
  FOR line=0, nl-1, 1L DO BEGIN
    IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Creating order matrix, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
    globstat=float(assoGlobStat[line])
    ngspy=REFORM(globstat[*,1])
    PE1=float(assoPE1[line])
    PE2=float(assoPE2[line])
    
    FOR column=0, ns-1, 1L DO BEGIN      ; loop over all lines
      ;IF (column EQ 3220) AND (line EQ 1128) THEN STOP
      IF (ngspy[column] GT 0) THEN BEGIN  ;exclude from the analysis those with no data
        PEx=FLTARR(nb,2) ;first row is sos1, second is sos2
        PEx[*,0]=REFORM(PE1[column,*])
        PEx[*,1]=REFORM(PE2[column,*])              
        ;assign order      
        IF  (ngspy[column]) EQ 1 THEN BEGIN
          ;one GS per Year
          orderS[column, line, *, 0] = 0  ;season2 (column, line, *, 2) was initialized to -999
          orderB[column, line, *, 0] = align_JD_2_Arbitrary_Cycle(PEx[*,0], $
                                       frstYearOfTimeSeries, frstDOYofCycle, frstYearOut) 
          
        ENDIF ELSE BEGIN ;ngspy[column] is 2
          b = INTARR(nbout, 2)
          ;IF (column EQ 2425) AND (line EQ 2751) THEN STOP
          b[*, 0] = align_JD_2_Arbitrary_Cycle(PEx[*,0], frstYearOfTimeSeries, frstDOYofCycle, frstYearOut) 
          b[*, 1] = align_JD_2_Arbitrary_Cycle(PEx[*,1], frstYearOfTimeSeries, frstDOYofCycle, frstYearOut)
          ;identify the index of the season coming first
          frst_ind = find_first_season_JD(REFORM(PEx[*,0]), REFORM(PEx[*,1]), frstDOYofCycle)
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

;*** NOTE that when orderS or orderB is -999 the season is not the RS record,
;*** it's not missing

;All files have been close, reopen the useful ones
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
      IF ngspy[column,line] GT 0 THEN BEGIN
        ;bands tha will have to be in season1
        bands4Out1=REFORM(orderB[column,line,*,0])
        ;remove -999, they will be NaN as initialized
        ind=WHERE(bands4Out1 NE -999, count)
        IF (count GT 0) THEN bands4Out1=bands4Out1[ind] ELSE stop
        ;assign the value
        lineOut1[column,ind]=lineIn12[column,bands4Out1,orderS[column,line,0,0]]          
        ;If there's the second season
        IF ngspy[column,line] GT 1 THEN BEGIN 
          bands4Out2=REFORM(orderB[column,line,*,1])
          ind=WHERE(bands4Out2 NE -999, count)
          IF (count GT 0) THEN bands4Out2=bands4Out2[ind] ELSE stop
          lineOut2[column,ind]=lineIn12[column,bands4Out2,orderS[column,line,0,1]]
        ENDIF
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
    PRINTF,lun,'values = {If time is Julian Day)'+ $
               ' of '+STRTRIM(frstYearOut,2)+'}';frstYearOut
    PRINTF,lun,'band names = {'
    FOR b=frstYearOut, frstYearOut+nbout-1 DO BEGIN
      lstd=frstDOYofCycle-1
      nxty=b+1
      IF (lstd EQ 0) THEN BEGIN
        lstd=365
        nxty=b
      ENDIF 
      IF (b EQ frstYearOut+nbout-1) THEN $
      PRINTF,lun, STRTRIM(PE_fname,2)+':'+STRTRIM(frstDOYofCycle,2)+'/'+STRTRIM(b,2)+ $
                  ' - '+STRTRIM(lstd,2)+'/'+STRTRIM(nxty,2)+ '}'$
      ELSE PRINTF,lun, STRTRIM(PE_fname,2)+':'+STRTRIM(frstDOYofCycle,2)+'/'+STRTRIM(b,2)+ $
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