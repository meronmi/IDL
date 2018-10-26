FUNCTION SINGLE_FILE_CONVERSION_G2_to_MP, path, h5Fname0, var, suffix
;read h5
h5Fname = path + '\' + h5Fname0
file_id = H5F_OPEN(h5Fname)
dataset_id = H5D_OPEN(file_id,  var)
h5image = H5D_READ(dataset_id)
dtype = SIZE(h5image, /TYPE)
ulLAT = readH5attrbute(file_id, 'LAT')
ulLON = readH5attrbute(file_id, 'LONG')
pixsize = readH5attrbute(file_id, 'PIXEL_SIZE')
tStart = readH5attrbute(file_id, 'TEMPORAL_START')
offset = readH5attrbute(dataset_id, 'OFFSET')
gain = readH5attrbute(dataset_id, 'SCALING_FACTOR')
;IF STRMATCH(h5Fname0, '*_VGT_*') THEN BEGIN
;  ulLAT = readH5attrbute(file_id, 'LAT ')
;  ulLON = readH5attrbute(file_id, 'LONG ')
;  pixsize = readH5attrbute(file_id, 'PIXEL_SIZE ')
;  tStart = readH5attrbute(file_id, 'TEMPORAL_START ')
;  offset = readH5attrbute(dataset_id, 'OFFSET ')
;  gain = readH5attrbute(dataset_id, 'SCALING_FACTOR ')
;ENDIF ELSE BEGIN
;  ulLAT = readH5attrbute(file_id, 'LAT')
;  ulLON = readH5attrbute(file_id, 'LONG')
;  pixsize = readH5attrbute(file_id, 'PIXEL_SIZE')
;  tStart = readH5attrbute(file_id, 'TEMPORAL_START')
;  offset = readH5attrbute(dataset_id, 'OFFSET')
;  gain = readH5attrbute(dataset_id, 'SCALING_FACTOR')
;ENDELSE

dataspace_id = H5D_GET_SPACE(dataset_id)
dimsGIO = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)
H5S_CLOSE, dataspace_id
H5D_CLOSE, dataset_id
H5F_CLOSE, file_id

;compute the dekad from tempral start
IF STRMATCH(h5Fname0, '*_VGT_*') THEN BEGIN
  YYYYMMDD = FIX([STRMID(STRTRIM(tStart,2),0,4) ,STRMID(STRTRIM(tStart,2),4,2), STRMID(STRTRIM(tStart,2),6,2)])
ENDIF ELSE BEGIN
  YYYYMMDD = FIX(STRSPLIT(tStart, '-', /EXTRACT))
ENDELSE
YY = STRMID(STRTRIM(YYYYMMDD[0],2), 2, 2)
CASE YYYYMMDD[2] OF
  1: dek = 1 + (YYYYMMDD[1]-1)*3
  11: dek = 2 + (YYYYMMDD[1]-1)*3
  21: dek = 3 + (YYYYMMDD[1]-1)*3
  ELSE: RETURN, 10
 ENDCASE

;adjust for trailing zeros
IF (STRLEN(STRTRIM(FIX(YYYYMMDD[1]),2)) EQ 1) THEN YYYYMMDD[1] = '0' + STRTRIM(FIX(YYYYMMDD[1]),2) ELSE YYYYMMDD[1] = STRTRIM(FIX(YYYYMMDD[1]),2)  
IF (STRLEN(STRTRIM(FIX(YYYYMMDD[2]),2)) EQ 1) THEN YYYYMMDD[2] = '0' + STRTRIM(FIX(YYYYMMDD[2]),2) ELSE YYYYMMDD[2] = STRTRIM(FIX(YYYYMMDD[2]),2)
IF (STRLEN(STRTRIM(FIX(dek),2)) EQ 1) THEN dek = '0' + STRTRIM(FIX(dek),2) ELSE dek = STRTRIM(FIX(dek),2)



value_string = 'values = {NDVI-toc, -, 0, 250, 0, 250,' + STRTRIM(offset) + ', ' + STRTRIM(1.0/gain,2) +'}'
flag_string = 'flags = {251=missing, 252=cloud, 253=snow, 254=sea, 255=other}'

baseFNameOut = suffix + STRTRIM(YY,2) + STRTRIM(dek,2) + var
OPENW, lun,  path + '\' + baseFNameOut + '.img', /GET_LUN
WRITEU, lun, h5image
FREE_LUN, lun
OPENW, lun,   path + '\' + baseFNameOut  + '.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'description = {GIO hd5 imported into ENVI}'
PRINTF, lun, 'samples ='+STRCOMPRESS(dimsGIO[0])
PRINTF, lun, 'lines   ='+STRCOMPRESS(dimsGIO[1])
PRINTF, lun, 'bands   ='+STRCOMPRESS(1)
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = ' + STRTRIM(dtype,2)
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'map info = {Geographic Lat/Lon, 1.5, 1.5,' + STRTRIM(ulLON,2) + ',' + $
             STRTRIM(ulLAT,2) + ',' + STRTRIM(0.00892857143,2) + ',' + STRTRIM(0.00892857143,2) + '}'
PRINTF, lun, value_string
PRINTF, lun, flag_string
IF (YYYYMMDD[1] LT 10) THEN strMonth = '0' + STRTRIM(YYYYMMDD[1],2) ELSE strMonth = STRTRIM(YYYYMMDD[1],2)
IF (YYYYMMDD[2] LT 10) THEN strDay = '0' + STRTRIM(YYYYMMDD[2],2) ELSE strDay = STRTRIM(YYYYMMDD[2],2)
PRINTF, lun, 'date = ' + STRTRIM(YYYYMMDD[0],2) + strMonth + strDay
PRINTF, lun, 'days = 10'
PRINTF, lun, 'sensor type = VGT'
PRINTF, lun, 'program = {IDL code written by Michele}'
FREE_LUN, lun

RETURN, 0
END