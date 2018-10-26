PRO phenot_command_line
;This is the sav file and the command to be run from dos for parallel execution
 ;null=dialog_message('hallo',/information)
 cla=command_line_args(count=count) 
; null=dialog_message([string(count),cla],/information)
 phenot_batch_execution, STRING(cla)
 
END







FUNCTION phenot_handler, remote, job, path, filein, fileAcqJulDay, ns, nl, nb, fst_dek, first_year, last_year, julian1_f, mask, ss, ll, fminthresh
;  Purpose:
;     To manage phenot operations over an image
;     input: multiband fapar image (floating) 
;     no data has to be NaN
;  Input
; -mask: it is a one band bsq file, any pixel value NE 0 is processed, it has to be in the same path of the fAPAR file, ;        (so a department raster map is fine)
; -ss: sample for job 'pixel' execution
; -ll: line for job 'pixel' execution

@cb_job.comm
@cb_in.comm
@cb_stat.comm
@cb_frst.comm
@cb_pdhtf.comm
@cb_options.comm

STARTTIME = SYSTIME(1)


;DEBUG, strt the error handler
Error_status = 0
IF (remote EQ 1) THEN CATCH, Error_status
IF Error_status NE 0 THEN BEGIN
  Result = DIALOG_MESSAGE('ERROR: ' + STRTRIM(Error_status,2) + ', ' +!ERROR_STATE.MSG + ' At path '+ $
  path + ', sample = ' + STRTRIM(column,2) +', line = ' + STRTRIM(line,2))
  RETURN, 100
ENDIF  

faprangeminthresh = fminthresh

npts = nb
periods = indgen(npts)
fst_dec=fst_dek

CLOSE, /ALL
platform = !version.os_family
IF (platform EQ 'unix') THEN dirsep = '/' ELSE dirsep='\'


filein=path+dirsep+filein
fileAcqJulDay=path+dirsep+fileAcqJulDay

dir_products = 'pheno_products'
path_out = path + dirsep + dir_products
IF (job eq 'image') THEN FILE_MKDIR, path_out 
IF mask NE 'no_mask' THEN BEGIN
  mask=path+dirsep+mask
  IF FILE_TEST(mask) NE 1 THEN BEGIN
    print, 'The file ' + mask + ' could not be found, the program will stop.'
    stop
  END
  use_mask=1
ENDIF ELSE BEGIN
  use_mask=0
ENDELSE

;Phenot works with integer period (0,1,2) that can be days, dececades etc
;julian1 is just usd to make the graphic
;results from phenot are expressed in periods
res=compute_j1_j2(julian1_f, nb)
julian1=res.julian1
julian2=res.julian2
julianmid=(julian1+julian2)/2.0 ;this is used to build the correspondence between periods and julian day
;for VGT I'll have 6/1, 16/1, 26/1, 6/2, 16/2, 25/2 .. it's not always 10 days
;caldat, julianmid[0], mm, dd, yy

;;missing data
;ibel = -0.5  ;FOR NDVI to be putted in options!!!
;iabo = 1.0
fst_year = first_year
lst_year = last_year
;############### (END) USER DEF PARAMETERS #######################

IF (resume_from_save EQ 0) THEN BEGIN
  start_line=0
ENDIF ELSE BEGIN
  RESTORE, path_out+dirsep+'saveline'
  IF (saveline EQ -999) THEN BEGIN
    PRINT, 'Saveline EQ '+STRTRIM(saveline,2)
    PRINT, 'and cannot be resumed from save. The program will stop'
    CLOSE, /ALL
    STOP
  ENDIF
  start_line=saveline+1 ;start from the next one
ENDELSE
;############### OUTPUT FILES #######################
; Define the maximum number of solar year in the data.
; Solar years (repeating cycle of 36 decades starting at 
; an unknown decade) cannot exceed the number of calendar years (36 
; decades from the first of Jenuary) available in the time serie.
; First and last year may not be complete   
n_full_calendar_years=floor(npts/36.0)
IF ((npts mod 36) ne 0.0) then $
  n_calendar_years=n_full_calendar_years+1 $
ELSE n_calendar_years=n_full_calendar_years
nyears=n_calendar_years ; in each of this year we may have 1 or 2 GS


IF (job eq 'image') THEN BEGIN
  ;definition of the structures receiving the data
  ;since I am using bil processing, they are lines
  ;global statitiscs (one value per pixel, no time series)
  globstat_bands=['retcode', 'ngspy']
  
  nflag=N_ELEMENTS(globstat_bands)     
  globstat = {$
    fname:  'globstat', $
    bands: globstat_bands, $
    data:   INTARR(ns, nflag) }
    
  ;rectcode, error codes:
  ; -1 the pixel was not processed because out of mask
  ; 0  success
  ; 10 More than 40% of the values in the input record are invalid
  ; 20 [95th - 5th] percentile range is too low, less than faprangeminthresh
  ; 30 unrecognized data periodicity
  ; 40 anomalous number of gs was found (lt 1 or gt 2)
  
  ;definition of the structure for storing the lomb scarg ratio, if used
  IF (use_lomb EQ  1) THEN BEGIN  ;use_lomb makes lomb ratio to be calculated and saved , not used for the moment
    lomb = {$
      fname : 'lombrat', $
      bands : 'lombratio_c1/c2', $
      data : FLTARR(ns) }
  ENDIF
  
  ;varaible to store xoffst
  offset= {$
    fname: 'offset', $
    bands: ['xoffstGS1','xoffstGS2'], $
    data: INTARR(ns,2)}
  
  
  
  ;gsflag codes
  ;-2  : the opt was not performed (because there is not enough variability)
  ;-1  : too few obs (cuurently LE 7)
  ;0   : optization failed 
  ;10  : optimization success
  ;
  ;
  ;gsstat codes
  ;-100  : too few obs (cuurently LE 7)
  ;0-4 : mpfit success
  ;5 : mpfit failure
  ;gt 5  : mpfit success
  ;
  ;gsrel
  ;;-1: unkown (it was not investigated because flagX is <0)
  ;0 :ok
  ;+10 : 2/3 or more of the missing values are within the first half of the period of interest 
  ;+20 : 2/3 or more of the missing values are within the second half of the period of interest 
  ;+100  : the following conditions are not met
  ;    - first and last good values LE median of entire distrubution
  ;+500 : there is no local maximum above the median of entire distrubution
  ;+1000 : the algorithm was trying to extend the search period beyond the limits of fapar array
  
  ;gscomp Growing Season Completeness
  ;This is an indicator of the actual presence of data for a given season. It is computed for the all 
  ;the season for concistency with other outputs and it is usefull for the first one and the last (one or two).
  ;After full processing of the time series, for all season where rel<1000 (all needed data present)
  ;the avg dekad of SOS and EOS (5 % criterion) is computed and transformed into progressive dekad form 0.
  ;Such periods will contain avg_n observations. Each season will actually have current_n.
  ;The % = current_n/avg_n * 100 is then an indicator of how much the season is covered. A percent > 100
  ;indicates that also the tails are presents 
  
  ;phenological parameters
  pheno = pheno_struct_define(ns, n_calendar_years)
  
  ntags=n_tags(pheno)
ENDIF ;IF (job eq 'image') THEN

; start image analysis

lstLU=9         ;logical units are used from this numebr on (+1), ecah time a unit is used lstLU is updated
                ;so the first LU used is lstLU+1

; Input files are opened by performing ASSOC to one line
lstLU=lstLU+1 & R1=lstLU
OPENR, R1, filein;, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
lstLU=lstLU+1 & R2=lstLU
OPENR, R2, fileAcqJulDay
line_ass_acqJulDay = ASSOC(R2, LONARR(ns,nb))
IF (use_mask EQ 1) THEN BEGIN
  lstLU=lstLU+1 & umask=lstLU
  OPENR, umask, mask;, /GET_LUN
  line_ass_mask = ASSOC(umask, BYTARR(ns))
ENDIF


IF (job eq 'image') THEN BEGIN
  ; Create files for output
  uglob  = openw_bil(lstLU, resume_from_save, path_out, globstat.fname)
  uoffst = openw_bil(lstLU, resume_from_save, path_out, offset.fname)
  IF (use_lomb EQ  1) THEN ulomb  = openw_bil(lstLU, resume_from_save, path_out, lomb.fname)
  
  ;all pheno files 
  FOR t=0, ntags-1 DO $
    pheno.(t).lun=openw_bil(lstLU, resume_from_save, path_out, pheno.(t).label)
  ;keep record of open files, FLOATING FILES data type 4
  out_fnames_dt4=pheno.(0).label
  FOR t=1, ntags-1 DO out_fnames_dt4=[out_fnames_dt4, pheno.(t).label]
ENDIF ;IF (job eq 'image') THEN

;create line matrix to store the input
data_line=FLTARR(ns, nb) ;INPUT
;PERFORM THE PIXEL ANALYSIS OR START THE IMAGE LOOP
IF (job eq 'pixel') THEN BEGIN
  data_line=FLOAT(line_ass_data[ll])
  acq_line = line_ass_acqJulDay[ll]
  rc = phenot_funct (REFORM(data_line[ss, *]), REFORM(acq_line[ss, *])) 
  PRINT, 'job pixel for sample '+STRTRIM(ss,2)+' and line '+STRTRIM(ll,2)+ $
         ' has returned code = '+ STRTRIM(rc, 2)
  IF (use_lomb EQ 1) THEN PRINT, 'Lombrat cycle 1 / cycle 2 = ', STRTRIM(lombrat, 2)
  IF (rc EQ 0) OR (rc eq 1) THEN BEGIN
    cs=0  ;current season
    PRINT, 'Season completeness, GS1:'
    ;PRINT, season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDek, gsiniendDek)
    PRINT, season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
    PRINT, 'ACC1:'
    PRINT, REFORM(gsaccu[0,*])
    PRINT, 'GSFLAG1:'
    PRINT, REFORM(gsflag[0,*])
    IF (ngspy eq 2) then begin  ;GS2 may be present or not
     cs=1  ;current season
     PRINT, 'Season completeness, GS2:'
     PRINT, season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
     PRINT, 'ACC2:'
     PRINT, REFORM(gsaccu[1,*])
     PRINT, 'GSFLAG2:'
     PRINT, REFORM(gsflag[1,*])
    ENDIF
  ENDIF
  RETURN, 0       
ENDIF

FOR line=start_line, nl-1, 1L DO BEGIN       ; loop over all lines
  ;print, line
  ; Now create per line the stack of all dates
  data_line = FLOAT(line_ass_data[line])
  acq_line = line_ass_acqJulDay[line]
  IF (use_mask EQ 1) THEN mask_line=line_ass_mask[line]
;  IF ((line MOD (nl/10)) eq 0) then begin  ;every 10%
;    print, string(line/float(nl)*100)+'% at '+string(SYSTIME(0)) 
;  ENDIF
  ;clear pheno befor starting
  FOR t=0, ntags-1 DO pheno.(t).data[*,*]=!VALUES.F_NAN
  
  FOR column=0, ns-1, 1L DO BEGIN   ; loop over all columns 
    ;IF (column EQ 1208) AND (line EQ 58) THEN STOP
    ;IF (column MOD 100) EQ 0 THEN PRINT, 'Column, line:', column, line 
    ; Now create per pixel the profile of all dates an run phenot
    IF (use_mask EQ 1) THEN BEGIN
      go=mask_line[column] 
    ENDIF ELSE BEGIN
      go=1  ;always perform calculation if a mask is not used
    ENDELSE
    IF (go GT 0) THEN BEGIN
      rc = phenot_funct (REFORM(data_line[column, *]), REFORM(acq_line[column, *])) 
    ENDIF ELSE rc=-1
    ;debug lombart+
    ;if (rc eq 777) then begin
;      globstat.data[column, 0]=lombrat
;      globstat.data[column, 1]=ngspy
    ;endif
    ;debug lombart-
    ;now I have all the needed variables in the common blocks
    IF (rc EQ 0) OR (rc EQ 1) THEN BEGIN
      ;print, column, line
      ;IF column eq 4097 and line eq 2 then stop 
      globstat.data[column, 0] = rc
      globstat.data[column, 1] = ngspy
      offset.data[column, 0] = xoffst[0]
      offset.data[column, 1] = -999
      IF (use_lomb EQ 1) THEN lomb.data[column] = lombrat
      lombrat = !VALUES.F_NAN
      ;computation of completeness
      cs=0  ;current season
      pheno.comp1.data[column,*] =  season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
     
      
      ;different treatment of pheno output in the case of 1 or 2 GS
      ;GS 1 alsways present
      pheno.sos1.data[column,*] =   gsstrt[0,*]
      pheno.eos1.data[column,*] =   gsstop[0,*]
      pheno.eos1e.data[column,*] =  gsstop2[0,*]
      pheno.len1.data[column,*] =   gsleng[0,*]
      pheno.len1e.data[column,*] =  gsleng2[0,*]
      
      pheno.acc1.data[column,*] =   gsaccu[0,*]
      pheno.acc1e.data[column,*] =  gsaccue[0,*]
      pheno.acc1b.data[column,*] =  gsaccub[0,*]
      pheno.acc1eb.data[column,*] = gsaccueb[0,*]
      pheno.maxv1.data[column,*] = gsmaxv[0,*]
      pheno.maxt1.data[column,*] = gsmaxt[0,*]
      
      pheno.flag1.data[column,*] = gsflag[0,*]
      pheno.stat1.data[column,*] = gsstat[0,*]
      pheno.rel1.data[column,*] =  gsrel[0,*]
      
      pheno.p01.data[column,*] = pdhtf_base[0,*] 
      pheno.p11.data[column,*] = pdhtf_amp1[0,*]
      pheno.p21.data[column,*] = pdhtf_sft1[0,*]
      pheno.p31.data[column,*] = pdhtf_slo1[0,*]
      pheno.p41.data[column,*] = pdhtf_amp2[0,*]
      pheno.p51.data[column,*] = pdhtf_sft2[0,*]
      pheno.p61.data[column,*] = pdhtf_slo2[0,*]
      pheno.t01.data[column,*] = gsinistartDek[0,*]
      pheno.t11.data[column,*] = gsiniendDek[0,*]
      ;...
      if (ngspy eq 2) then begin  ;GS2 may be present or not
      
        offset.data[column, 1]=xoffst[1]
        ;computation of completeness
        cs=1  ;current season
        pheno.comp2.data[column,*] =  season_completeness(cs, gsrel, gsstrtTc, gsstopTc, gsinistartDekJD, gsiniendDekJD)
        
        pheno.sos2.data[column,*] =  gsstrt[1,*]
        pheno.eos2.data[column,*] =  gsstop[1,*]
        pheno.eos2e.data[column,*] = gsstop2[1,*]
        pheno.len2.data[column,*] =  gsleng[1,*]
        pheno.len2e.data[column,*] = gsleng2[1,*]
        
        pheno.acc2.data[column,*] =  gsaccu[1,*]
        pheno.acc2e.data[column,*] = gsaccue[1,*]
        pheno.acc2b.data[column,*] = gsaccub[1,*]
        pheno.acc2eb.data[column,*] =gsaccueb[1,*]
        pheno.maxv2.data[column,*] = gsmaxv[1,*]
        pheno.maxt2.data[column,*] = gsmaxt[1,*]
      
        pheno.flag2.data[column,*] = gsflag[1,*]
        pheno.stat2.data[column,*] = gsstat[1,*]
        pheno.rel2.data[column,*] =  gsrel[1,*]
        
        pheno.p02.data[column,*] = pdhtf_base[1,*] 
        pheno.p12.data[column,*] = pdhtf_amp1[1,*]
        pheno.p22.data[column,*] = pdhtf_sft1[1,*]
        pheno.p32.data[column,*] = pdhtf_slo1[1,*]
        pheno.p42.data[column,*] = pdhtf_amp2[1,*]
        pheno.p52.data[column,*] = pdhtf_sft2[1,*]
        pheno.p62.data[column,*] = pdhtf_slo2[1,*]
        ;Corrected on 21 Feb 2014, pdhtf parameters apply to a time axis in JD,
        ;and JD shoud be stored
        ;pheno.t02.data[column,*] = gsinistartDek[1,*]
        ;pheno.t12.data[column,*] = gsiniendDek[1,*]
        pheno.t02.data[column,*] = gsinistartDekJD[1,*]
        pheno.t12.data[column,*] = gsiniendDekJD[1,*]
      endif
    endif else begin
      globstat.data[column, 0]=rc
      globstat.data[column, 1]=0
      offset.data[column, 0]=-999 
      offset.data[column, 1]=-999 
      IF (use_lomb EQ  1) THEN lomb.data[column] =  !VALUES.F_NAN
      ;endif
      ;the optimizatio was not started because of missing obs (10) 
      ;or low variability (20), set all the variable to NaN
    endelse
    
  ENDFOR  ;column
  ;Now write the line equal to variable "line"
  ;set to -999 because if the computer crashes between here and the wrting of last file
  ;the various files will have / not have the same number of data and cannot be resumed easily
  saveline=-999
  SAVE, saveline, FILENAME = path_out+dirsep+'saveline'  
  WRITEU, uglob, globstat.data
  WRITEU, uoffst, offset.data
  IF (use_lomb EQ  1) THEN WRITEU, ulomb, lomb.data
  ;Write all files
  FOR t=0, ntags-1 DO WRITEU, pheno.(t).lun, pheno.(t).data

  ;all file are written here, save the last written line
  saveline=line
  SAVE, saveline, FILENAME = path_out+dirsep+'saveline'
ENDFOR  ;line

CLOSE, /ALL

; WRITE HEADER OF THE OUTPUTS
HEADER_OUT=path_out+dirsep+globstat.fname +'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {xoffset pheno results}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nflag)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'byte order = 0'
;the number in format is N_ELEMENTS(globstat.bands)-1
printf,3, globstat.bands, FORMAT='("band names = {", 1(a,","),(a),"}")'
CLOSE, 3

HEADER_OUT=path_out+dirsep+offset.fname +'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {offset pheno results}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(N_ELEMENTS(offset.bands))
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 2'
printf,3,'interleave = bil'
printf,3,'byte order = 0'
;the number in format is N_ELEMENTS(globstat.bands)-1
printf,3, offset.bands, FORMAT='("band names = {", 1(a,","),(a),"}")'
CLOSE, 3

IF (use_lomb EQ  1) THEN BEGIN
  HEADER_OUT=path_out+dirsep+lomb.fname +'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = {lombrat}'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(nl)
  printf,3,'bands   ='+STRCOMPRESS(N_ELEMENTS(lomb.bands))
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 4'
  printf,3,'interleave = bil'
  printf,3,'byte order = 0'
  ;the number in format is N_ELEMENTS(globstat.bands)-1
  printf,3, lomb.bands, FORMAT='("band names = {", 1(a,","),(a),"}")'
  CLOSE, 3
ENDIF
;FOR j=0, N_ELEMENTS(out_fnames_dt2)-1 DO BEGIN 
;  HEADER_OUT=path+dirsep+out_fnames_dt2[j] +'.hdr'
;  OPENW, 3, HEADER_OUT
;  printf,3,'ENVI'
;  printf,3,'description = {pheno results}'
;  printf,3,'samples ='+STRCOMPRESS(ns)
;  printf,3,'lines   ='+STRCOMPRESS(nl)
;  printf,3,'bands   ='+STRCOMPRESS(n_calendar_years)
;  printf,3,'header offset = 0'
;  printf,3,'file type = ENVI Standard'
;  printf,3,'data type = 2'
;  printf,3,'interleave = bil'
;  printf,3,'byte order = 0'
;  CLOSE, 3
;ENDFOR

FOR j=0, N_ELEMENTS(out_fnames_dt4)-1 DO BEGIN 
  HEADER_OUT=path_out+dirsep+out_fnames_dt4[j] +'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = {pheno results}'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(nl)
  printf,3,'bands   ='+STRCOMPRESS(n_calendar_years)
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 4'
  printf,3,'interleave = bil'
  printf,3,'byte order = 0'
  CLOSE, 3
ENDFOR

;FOR CREATING DIFFERENCE IMAGES (delta with respect to mean value) use pheno_anomalies

; Evaluation of processing time
ELAPSED_TIME = SYSTIME(1) - STARTTIME
HOURS =  FLOOR(ELAPSED_TIME / (60*60))
MINUTES = FLOOR((ELAPSED_TIME MOD (60*60))/ 60)
SECS = FLOOR((ELAPSED_TIME - HOURS*60*60-MINUTES*60))
tmp = path+' OVERALL PROCESSING TOOK :'+STRCOMPRESS(HOURS)+' HOURS, ' +STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
null=dialog_message(tmp,/information)
CLOSE, /ALL
RETURN, 0
End