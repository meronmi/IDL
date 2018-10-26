FUNCTION recomposer, info_file
;This function rebuild the pheno_results from decomposer output
;use the same input of decomposer, WATCH OUT FOR THE USER SETTINGS

; for example 
; PRINT, recomposer('K:\MODIS_Niger\info.txt')
; PRINT, recomposer('K:\HoA\VGT data SWETS\bil\info.txt')

@cb_parallel.comm
;##############################################################################################
;USER SETTING
ret = par_settings()
;##############################################################################################

;dir_recomposed = 'DIR_RECOMPOSED_UppEnv_13May2016'
dir_recomposed = 'DIR_RECOMPOSED'
dir_products = 'Pheno_products'   ;this is set in pheno_image

path = read_info('path', info_file)

ns = FIX(read_info('ns', info_file))
nl = FIX(read_info('nl', info_file))


                        
nlf = FLOOR(nl/n)             ;number of lines in files (each file excluding last)
nllf = nlf + nl MOD nlf       ;number of lines in the last file
fstline=INDGEN(n)*nlf
lstline=fstline+nlf-1
lstline[n-1]=fstline[n-1]+nllf-1

ns_out=ns
nl_out=nl


;Define names of the directories where the segments are stored
dir = STRARR(n)
FOR i = 0, n-1 DO dir[i]=out_path+'\DIR'+STRTRIM(i,2)

;make a list of the pheno files that have to be recomposed, the list is based on the first directory

path_in = dir[0]+'\'+dir_products
CD, path_in
phdrs=FILE_SEARCH('*.hdr')       ;pheno hdr names
pbnames=phdrs                             ;phenop files base names
FOR i = 0, N_ELEMENTS(phdrs)-1 DO BEGIN
  tmp=STRSPLIT(phdrs[i], '.', /EXTRACT)
  pbnames[i]=tmp[0]
ENDFOR  

;for each of the file open the recomposed image and fill it
;by keeping track of the datatype
FILE_MKDIR, path+'\'+dir_recomposed
FOR f = 0, N_ELEMENTS(pbnames)-1 DO BEGIN
 datatype =  FIX(read_info('data type', phdrs[f]))
 nb = FIX(read_info('bands', phdrs[f]))
 

 fileout=path+'\'+dir_recomposed+'\'+pbnames[f]
 IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
 ;open file for output
 OPENW, uout, fileout,  /APPEND , /GET_LUN
 ;open segment files for input
 FOR i = 0, n-1 DO BEGIN
  ;open segment i
  filein = dir[i]+'\'+dir_products+'\'+pbnames[f]
  nl = FIX(read_info('lines', dir[i]+'\'+dir_products+'\'+phdrs[f]))
  OPENR, uin, filein, /GET_LUN
  ;line = ASSOC(uin, BYTARR(ns))
  CASE datatype OF
    2: line = ASSOC(uin, INTARR(ns,nb))
    4: line = ASSOC(uin, FLTARR(ns,nb))
    ELSE: STOP
  ENDCASE
  ;read and write the segment into the output file
  FOR l = 0, nl-1 DO BEGIN
    WRITEU, uout, line[l]
  ENDFOR
  FREE_LUN, uin  
 ENDFOR
 FREE_LUN, uout
 ; WRITE HEADER OF THE OUTPUTS
HEADER_OUT=fileout+'.hdr'
OPENW, lun, HEADER_OUT, /GET_LUN
printf,lun,'ENVI'
printf,lun,'description = {part of job}'
printf,lun,'samples ='+STRCOMPRESS(ns_out)
printf,lun,'lines   ='+STRCOMPRESS(nl_out)
printf,lun,'bands   ='+STRCOMPRESS(nb)
printf,lun,'header offset = 0'
printf,lun,'file type = ENVI Standard'
printf,lun,'data type = '+STRCOMPRESS(datatype)
printf,lun,'interleave = bil'
printf,lun,'byte order = 0'
FREE_LUN, lun
ENDFOR
 

CLOSE, /ALL
RETURN, 0
END