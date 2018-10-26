FUNCTION write_info, info_name, path, faprangeminthresh, use_lomb, lombRatThreshold, upenv, period, ibel, iabo, fract_thresh, fract_thresh2, $
         filein, acqJULDAY, mask, ns, nl, nb, fst_dek, fst_day, fst_month, first_year,last_year
OPENW, lun, info_name, /GET_LUN
PRINTF, lun, 'faprangeminthresh='+STRTRIM(faprangeminthresh,2)
PRINTF, lun, 'use_lomb ='+STRTRIM(use_lomb,2) 
PRINTF, lun, 'lombRatThreshold ='+STRTRIM(lombRatThreshold,2) 
PRINTF, lun, 'upenv='+STRTRIM(upenv,2)
PRINTF, lun, 'period ='+STRTRIM(period,2) 
PRINTF, lun, 'ibel='+STRTRIM(ibel,2)
PRINTF, lun, 'iabo='+STRTRIM(iabo,2)
PRINTF, lun, 'fract_thresh ='+STRTRIM(fract_thresh,2) 
PRINTF, lun, 'fract_thresh2 ='+STRTRIM(fract_thresh2,2)

PRINTF, lun, 'path='+path
PRINTF, lun, 'filein='+filein
PRINTF, lun, 'acqJULDAY='+acqJULDAY
PRINTF, lun, 'mask='+STRTRIM(mask,2)
PRINTF, lun, 'ns='+STRTRIM(ns,2)
PRINTF, lun, 'nl='+STRTRIM(nl,2)
PRINTF, lun, 'nb='+STRTRIM(nb,2)
PRINTF, lun, 'fst_dek='+STRTRIM(fst_dek,2)
PRINTF, lun, 'fst_day='+STRTRIM(fst_day,2)
PRINTF, lun, 'fst_month='+STRTRIM(fst_month,2)
PRINTF, lun, 'first_year='+STRTRIM(first_year,2)
PRINTF, lun, 'last_year='+STRTRIM(last_year,2)
FREE_LUN, lun
RETURN, 0
END

FUNCTION decomposer, info_file
;This function is used to split the job into n smaller jobs that can be executed in
;parallel on a multiprocessor machine. Basically it a brute force approach to parallelize 
;the code.
;The RS time series (bil file of dimensions ns sample, nl , nb) is divided into 
;n segments of nl/n lines. idlrt -rt is then launched by a batch file on each single segment. 
;The function recomposer is used the rebuild the original spatial extent from the n segments

;Input parameters: 
; - the info_file containg the info about the RS time series.
 
 ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;FIRST BUILD THE PROJECT (AND SAVE A SAV) 
 
;Example
; PRINT, decomposer('Y:\projects\grid\io\info_day_sNE_eM_010101-151221_BIL_160301.txt') 
; PRINT, decomposer('S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\info_day_sNE_eM_010101-151221_BIL_160301.txt')
; PRINT, decomposer('S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\info_day_eMODIS_niger 05 Feb 2015.txt')
; PRINT, decomposer('K:\MODIS_Niger\info.txt')
; PRINT, decomposer('K:\Tunisia\VGT_data\bil\info2.txt')
; PRINT, decomposer('E:\GRACE\emodis\bil1km\info_day_eMODIS_iga.txt')
; PRINT, decomposer('E:\ILRI\pheno\info_day_eMODIS_Kenya.txt')
; PRINT, decomposer('D:\LTDR\info_parallel_LTDR.txt')

@cb_parallel.comm

;##############################################################################################
;USER SETTING
use_bridge = 1 ;1 to use idl bridge instead of batch file launching several IDL istances
ret = par_settings()
;##############################################################################################
dirsep = '\'
;determine if it has to run on Unix
runOnUnix = read_info('runOnUnix', info_file)
IF (runOnUnix EQ 1) THEN BEGIN
  dirsep = '/'
  ret = par_settings_GRID()
  rem_out_path = STRJOIN(STRSPLIT(rem_out_path, '\',/EXTRACT), '/')
ENDIF
FILE_MKDIR, out_path
;Copy executable sav file on the remote machine
IF (use_bridge EQ 0) THEN FILE_COPY, sav_path+'\'+sav_fn, out_path+'\'+sav_fn, /OVERWRITE
;Define the command for remote execution
IF (runOnUnix EQ 1) THEN $
  command='START idlrt -rt="' + rem_out_path + dirsep + sav_fn +'" -args' $
  ;ELSE command='exec idlrt -rt="' + rem_out_path + dirsep + sav_fn +'" -args' 
  ELSE command='START idlrt -rt="' + rem_out_path + dirsep + sav_fn +'" -args'


;Read information about the RS time series
path = read_info('path', info_file)
faprangeminthresh = DOUBLE(read_info('faprangeminthresh', info_file))
use_lomb = FIX(read_info('use_lomb', info_file))                          
lombRatThreshold = DOUBLE(read_info('lombRatThreshold', info_file))       
upenv = FIX(read_info('upenv', info_file))                                
period = FIX(read_info('period', info_file))                              
;resume_from_save=FIX(read_info('resume_from_save', info_file))            
ibel = DOUBLE(read_info('ibel', info_file))                               
iabo = DOUBLE(read_info('iabo', info_file))                               
fract_thresh = DOUBLE(read_info('fract_thresh', info_file))               
fract_thresh2 = DOUBLE(read_info('fract_thresh2', info_file))       

filein = read_info('filein', info_file)
acqJULDAY = read_info('acqJULDAY', info_file)
mask =  read_info('mask', info_file)
ns = FIX(read_info('ns', info_file))
nl = FIX(read_info('nl', info_file))
nb =  FIX(read_info('nb', info_file))
fst_dek = FIX(read_info('fst_dek', info_file))
fst_day = FIX(read_info('fst_day', info_file))
fst_month = FIX(read_info('fst_month', info_file))
first_year = FIX(read_info('first_year', info_file))
last_year = FIX(read_info('last_year', info_file))

;Compute segments width                        
nlf = FLOOR(nl/n)             ;number of lines in files (each file excluding last)
;nllf = nlf + nl MOD nlf       ;number of lines in the last file
nllf = nl - (nlf*(n-1));number of lines in the last file
fstline=INDGEN(n)*nlf
lstline=fstline+nlf-1
lstline[n-1]=fstline[n-1]+nllf-1


;Open and create the batch dos file for executing the job on the remote machine
IF (FILE_TEST(out_path+'\'+bat_fn) EQ 1) THEN FILE_DELETE, out_path+'\'+bat_fn
OPENW, ubat, out_path+'\'+bat_fn,  /APPEND, /GET_LUN

;Batch file: move to IDL dir
PRINTF, ubat, 'cd ' + rem_idl_path
PRINTF, ubat, STRMID(rem_idl_path, 0, 2)

;Define names of the directories where to store the segments
dir = STRARR(n)
rem_dir = STRARR(n)
FOR i = 0, n-1 DO BEGIN
  dir[i]=out_path+'\'+ 'DIR'+STRTRIM(i,2)
  rem_dir[i]=rem_out_path + dirsep + 'DIR'+STRTRIM(i,2) ;same as above but seen from the remote machine
ENDFOR

;Open RS file
;FAPAR
OPENR, R1, path+'\'+filein, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
;ACQ day in Julian Day
OPENR, R2, path+'\'+acqJULDAY , /GET_LUN
line_ass_acqJD = ASSOC(R2, LONARR(ns,nb))
IF (mask NE 'no_mask') THEN BEGIN
  OPENR, umask, path+'\'+mask, /GET_LUN
  line_ass_mask = ASSOC(umask, BYTARR(ns))
ENDIF

;Prepare the n segments
FOR i= 0, n-1 DO BEGIN
  FILE_MKDIR, dir[i]              ;created the directory
  ;open output files
  ;FAPAR
  fileout=dir[i]+'\'+filein
  IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
  OPENW, W1, fileout,  /APPEND, /GET_LUN
  ;ACQ
  fileout=dir[i]+'\'+acqJULDAY
  IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
  OPENW, W3, fileout,  /APPEND, /GET_LUN
  
  IF (mask NE 'no_mask') THEN BEGIN
    fileout=dir[i]+'\'+mask
    IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
    OPENW, W2, fileout,  /APPEND, /GET_LUN
  ENDIF
  PRINTF, ubat, command+' "'+rem_dir[i]+dirsep+'info.txt"'
  ;read and write files
  ret=write_info(dir[i]+'\info.txt', rem_dir[i], faprangeminthresh, use_lomb, lombRatThreshold, $
                 upenv, period, ibel, iabo, fract_thresh, fract_thresh2, $
                 filein, acqJULDAY, mask, ns, (lstline[i]-fstline[i]+1), nb, $
                 fst_dek, fst_day, fst_month, first_year,last_year)
  FOR l = fstline[i], lstline[i] DO BEGIN
    WRITEU, W1, line_ass_data[l]
    WRITEU, W3, line_ass_acqJD[l]
    IF (mask NE 'no_mask') THEN WRITEU, W2, line_ass_mask[l]  
  ENDFOR
  FREE_LUN, W1
  FREE_LUN, W3
 
  ; WRITE HEADER OF THE OUTPUTS
  ;FAPAR
  HEADER_OUT=dir[i]+'\'+filein+'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = {part of job}'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(lstline[i]-fstline[i]+1)
  printf,3,'bands   ='+STRCOMPRESS(nb)
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 4'
  printf,3,'interleave = bil'
  printf,3,'byte order = 0'
  CLOSE, 3
  ;ACQ JD
  HEADER_OUT=dir[i]+'\'+acqJULDAY +'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = {part of job}'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(lstline[i]-fstline[i]+1)
  printf,3,'bands   ='+STRCOMPRESS(nb)
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 3'
  printf,3,'interleave = bil'
  printf,3,'byte order = 0'
  CLOSE, 3
  IF (mask NE 'no_mask') THEN  BEGIN
    FREE_LUN, W2
    HEADER_OUT=dir[i]+'\'+mask+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = {part of job}'
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(lstline[i]-fstline[i]+1)
    printf,3,'bands   ='+STRCOMPRESS(1)
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 1'
    printf,3,'interleave = bsq'
    printf,3,'byte order = 0'
    CLOSE, 3 
  ENDIF
ENDFOR

CLOSE, /ALL
tmp='Please run the bat on the remote machine > '+out_path+'\'+bat_fn
PRINT, tmp
RETURN, 0
END