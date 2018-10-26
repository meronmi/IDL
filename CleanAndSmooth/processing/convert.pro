Pro convert, path, list
;Directory of input
iDirName = path; 'F:\BP Tunisia\fAPAR\ALL\PROCESSED2\BIL\debug'; 'F:\BP Tunisia\fAPAR\ALL\'
;tile to mosaik (in spatial order)
;variable (ndvi, fapar)
variable = 'FAPAR'

;make dir for output
oDirName = iDirName + '\PROCESSED
FILE_MKDIR, oDirName
;find the zipped files (some are gz some are zip!) 
list1 = list

;delete h5 if any
CD, iDirName

;Convert and mosaik them
FOR i = 0, N_ELEMENTS(list1)-1 DO BEGIN
  ;check that they are pared
  dlmtr = '_'
  date1 = STRSPLIT(list1[i], dlmtr, /EXTRACT) & date1 = date1[3]
  
  
  Oimage = mread_hdf5(iDirName+'\'+list1[i], variable)
    
  IF (i LT 10) THEN stri ='00'+STRTRIM(i,2)
  IF (i GE 10) AND (i LT 100) THEN stri ='0'+STRTRIM(i,2)
  IF (i GE 100) THEN stri =STRTRIM(i,2)
  fname = STRTRIM(stri,2)+'_'+variable+'_'+date1
  
  OPENW, lun, oDirName+'\'+fname, /GET_LUN
  WRITEU, lun, Oimage
  FREE_LUN, lun
  ;write hdr
  OPENW, lun, oDirName+'\'+fname+'.hdr', /GET_LUN
    PRINTF, lun, 'ENVI'
    PRINTF, lun, 'description = mosaik'
    PRINTF, lun, 'samples ='+STRCOMPRESS(N_ELEMENTS(REFORM(Oimage[*,0])))
    PRINTF, lun, 'lines   ='+STRCOMPRESS(N_ELEMENTS(REFORM(Oimage[0,*])))
    PRINTF, lun, 'bands   ='+STRCOMPRESS(1)
    PRINTF, lun, 'header offset = 0'
    PRINTF, lun, 'file type = ENVI Standard'
    PRINTF, lun, 'data type = 1'
    PRINTF, lun, 'interleave = bsq'
    PRINTF, lun, 'byte order = 0'
  FREE_LUN, lun
ENDFOR  

END