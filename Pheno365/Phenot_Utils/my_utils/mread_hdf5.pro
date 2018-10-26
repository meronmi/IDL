PRO ProcessBP
;Directory of input
iDirName = 'F:\BP Tunisia\fAPAR\ALL\'
;tile to mosaik (in spatial order)
tiles = ['H18V05','H19V05']
;variable (ndvi, fapar)
variable = 'FAPAR'
;SET PATH of UNZIPPER
path_zipexe='F:\BP Tunisia'


;make dir for output
oDirName = iDirName + '\PROCESSED
FILE_MKDIR, oDirName
;find the zipped files (some are gz some are zip!) 
list = list_zipped(iDirName)

;delete h5 if any
CD, iDirName
;UNCOMMENT THE FOLLOWING TO UNZIP:
;
;h5list = FILE_SEARCH('*.h5')
;IF (h5list[0] NE '') THEN FILE_DELETE, h5list, /QUIET
;
;;unzip them
;FILE_COPY, path_zipexe + '\7za.exe', iDirName + '\7za.exe', /OVERWRITE
;CD, iDirName
;FOR i=0, N_ELEMENTS(list)-1 DO SPAWN, '7za e '+ list[i], /LOG_OUTPUT
;FILE_DELETE, iDirName + '\7za.exe', /QUIET

;list the first tile
arg = '*'+STRTRIM(tiles[0],2)+'*.h5'
list1 = FILE_SEARCH(arg)
arg = '*'+STRTRIM(tiles[1],2)+'*.h5'
list2 = FILE_SEARCH(arg)

;the two tiles should have the same number of files
IF N_ELEMENTS(list1) NE N_ELEMENTS(list1) THEN STOP

;Convert and mosaik them
FOR i = 0, N_ELEMENTS(list1)-1 DO BEGIN
  ;check that they are pared
  dlmtr = '_'
  date1 = STRSPLIT(list1[i], dlmtr, /EXTRACT) & date1 = date1[3]
  date2 = STRSPLIT(list1[i], dlmtr, /EXTRACT) & date2 = date2[3]
  IF (date1 NE date2) THEN STOP ELSE PRINT, date1, ' --- ', date2
  image1 = mread_hdf5(iDirName+'\'+list1[i], variable)
  image2 = mread_hdf5(iDirName+'\'+list2[i], variable)
  image = [image1,image2]
  Oimage = image
  Oimage = BYTARR(494,842)
  Oimage[*,0:841-2] = image[818:818+494-1,280:280+842-3]
  ;manca le ultime righe
  Oimage[*,840:841] = 255
  HELP, Oimage
  ;write image
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

FUNCTION list_zipped, dirName
CD, dirName
list = FILE_SEARCH('*.{gz,zip}')
RETURN, list
END


FUNCTION mread_hdf5, fName, variable
; example
; print, mread_hdf5('F:\BP Tunisia\g2_BIOPAR_FAPAR_200104230000_H18V05_VGT_V1.2demo.h5', 'FAPAR')

; Open the HDF5 file. 
file_id = H5F_OPEN(fName) 
; Open the image dataset within the file. 
; This is located within the /images group. 
; We could also have used H5G_OPEN to open up the group first. 
dataset_id1 = H5D_OPEN(file_id, 'FAPAR') 
    
; Read in the actual image data. 
image = H5D_READ(dataset_id1) 
    
; Open up the dataspace associated with the Eskimo image. 
dataspace_id = H5D_GET_SPACE(dataset_id1) 
    
; Retrieve the dimensions so we can set the window size. 
dimensions = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id) 
   
 
; Close all our identifiers so we don't leak resources. 
H5S_CLOSE, dataspace_id 
H5D_CLOSE, dataset_id1 
H5F_CLOSE, file_id 
 
RETURN, image
END