FUNCTION convertGIO2MARSOP, zipFname,  tile_info, tt, storage_machine_GIO_DIR, storage_machine_MP_DIR, client_machine_DIR
; use print, H5_BROWSER() to inspect

;copy the file to be concerted to client machine
FILE_COPY, storage_machine_GIO_DIR + zipFname, client_machine_DIR + zipFname, /OVERWRITE
;unzip it (use 7za.ex as the IDL unzip gives error)
CD, client_machine_DIR
SPAWN, '7za e ' + zipFname +' *.h5 -r', /LOG_OUTPUT
;delete the zip
FILE_DELETE, client_machine_DIR + zipFname

;read h5
h5Fname = STRMID(zipFname, 0, STRLEN(zipFname)-3) + 'h5'
file_id = H5F_OPEN(h5Fname)
dataset_id = H5D_OPEN(file_id,  tile_info.h5Variable)
h5image = H5D_READ(dataset_id)
ulLAT = readH5attrbute(file_id, 'LAT')
ulLON = readH5attrbute(file_id, 'LONG')
tStart = readH5attrbute(file_id, 'TEMPORAL_START')
dataspace_id = H5D_GET_SPACE(dataset_id)
dimsGIO = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)
H5S_CLOSE, dataspace_id
H5D_CLOSE, dataset_id
H5F_CLOSE, file_id

;compute the dekad from tempral start
YYYYMMDD = FIX(STRSPLIT(tStart, '-', /EXTRACT))
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
imageMP = BYTARR(tile_info.ns_out[tt],  tile_info.nl_out[tt])*0B+254B
;see if the MARS ROI is within the GIO ROI or if it exceeds it (in this case add no data, 255)

;the UL corner of MARSOP is assumed to be within the GIO, the LR may be inside or outside the MARSOP
;UL corner
;coordinates of valid box in the input (UL_in1) and output imeges (UL_out)
xoff_in = tile_info.gio_Xoffset[tt]
XUL_in =  0 + xoff_in
IF (XUL_in LT 0) THEN BEGIN
  XUL_out = xoff_in
  XUL_in1 = 0 
ENDIF ELSE BEGIN
  XUL_out = 0
  XUL_in1 = xoff_in
ENDELSE
yoff_in = tile_info.gio_Yoffset[tt]
YUL_in =  0 + yoff_in
IF (YUL_in LT 0) THEN BEGIN
  YUL_out = yoff_in
  YUL_in1 = 0
ENDIF ELSE BEGIN
  YUL_out = 0
  YUL_in1 = yoff_in
ENDELSE
;LR corner
XLR_in = 0 + xoff_in + tile_info.ns_out[tt]
IF (XLR_in GT tile_info.ns_in[tt]) THEN BEGIN
  XLR_out =  0 + (tile_info.ns_in[tt] -1) - xoff_in
  XLR_in1 =  tile_info.ns_in[tt] - 1
ENDIF ELSE BEGIN
  XLR_out =  tile_info.ns_out[tt] - 1
  XLR_in1 =  0 + (tile_info.ns_out[tt] -1) + xoff_in
ENDELSE
YLR_in = 0 + yoff_in + tile_info.nl_out[tt]
IF (YLR_in GT tile_info.nl_in[tt]) THEN BEGIN
  YLR_out =  0 + (tile_info.nl_in[tt] - 1) - yoff_in
  YLR_in1 =  tile_info.nl_in[tt] - 1
ENDIF ELSE BEGIN
  YLR_out =  tile_info.nl_out[tt] - 1
  YLR_in1 =  0 + (tile_info.nl_out[tt] -1) + Yoff_in
ENDELSE
;lrX = tile_info.gio_Xoffset[tt]+tile_info.ns_in[tt]-1
;;if the required X is bigger than X GIO, just take the last one
;IF (lrX GT dimsGIO[0]-1) THEN lrX = dimsGIO[0]-1
;lrY = tile_info.gio_Yoffset[tt]+tile_info.nl_in[tt]-1
;;if the required Y is bigger than Y GIO, just take the last one
;IF (lrY GT dimsGIO[1]-1) THEN lrY = dimsGIO[1]-1
;print, [XUL_out, XLR_out, YUL_out,YLR_out], [XUL_in1,XLR_in1,YUL_in1,YLR_in1]
;print, [0,(lrX-tile_info.gio_Xoffset[tt]), 0,(lrY-tile_info.gio_Yoffset[tt])] , [tile_info.gio_Xoffset[tt],lrX, tile_info.gio_Yoffset[tt],lrY]

;cut out the image
imageMP[XUL_out : XLR_out, YUL_out : YLR_out] = h5image[XUL_in1 : XLR_in1, YUL_in1 : YLR_in1]



;save envi file
CASE tile_info.h5Variable OF
  'NDVI': BEGIN
    value_string = 'values = {NDVI-toc, -, 0, 250, 0, 250, -0.08, 0.004}'
    flag_string = 'flags = {251=missing, 252=cloud, 253=snow, 254=sea, 255=other}'
    suffix = 'i'
  END  
  ELSE: RETURN, 20
ENDCASE
baseFNameOut = 'vt' + STRTRIM(YY,2) + STRTRIM(dek,2) + suffix
;imageMP[0:(lrX-tile_info.gio_Xoffset[tt]), 0:(lrY-tile_info.gio_Yoffset[tt])] = $
;        h5image[tile_info.gio_Xoffset[tt]:lrX, tile_info.gio_Yoffset[tt]:lrY]
ulLAT = FLOAT(ulLAT)-tile_info.gio_Yoffset[tt]*tile_info.pixsize
ulLON = FLOAT(ulLON)+tile_info.gio_Xoffset[tt]*tile_info.pixsize
OPENW, lun,  client_machine_DIR + baseFNameOut + '.img', /GET_LUN
WRITEU, lun, imageMP
FREE_LUN, lun
OPENW, lun,  client_machine_DIR + baseFNameOut  + '.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'description = {GIO hd5 imported into ENVI, tile = ' + STRTRIM(tile_info.name[tt],2) + '}'
PRINTF, lun, 'samples ='+STRCOMPRESS(tile_info.ns_out[tt])
PRINTF, lun, 'lines   ='+STRCOMPRESS(tile_info.nl_out[tt])
PRINTF, lun, 'bands   ='+STRCOMPRESS(1)
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = 1'
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'map info = {Geographic Lat/Lon, 1.5, 1.5,' + STRTRIM(ulLON,2) + ',' + $
             STRTRIM(ulLAT,2) + ',' + STRTRIM(tile_info.pixsize,2) + ',' + STRTRIM(tile_info.pixsize,2) + '}'
PRINTF, lun, value_string
PRINTF, lun, flag_string
IF (YYYYMMDD[1] LT 10) THEN strMonth = '0' + STRTRIM(YYYYMMDD[1],2) ELSE strMonth = STRTRIM(YYYYMMDD[1],2)
IF (YYYYMMDD[2] LT 10) THEN strDay = '0' + STRTRIM(YYYYMMDD[2],2) ELSE strDay = STRTRIM(YYYYMMDD[2],2)
PRINTF, lun, 'date = ' + STRTRIM(YYYYMMDD[0],2) + strMonth + strDay
PRINTF, lun, 'days = 10'
PRINTF, lun, 'sensor type = VGT'
PRINTF, lun, 'program = {IDL code written by Michele}'
FREE_LUN, lun
;copy file on storage
FILE_COPY, client_machine_DIR + baseFNameOut + '.img', storage_machine_MP_DIR + baseFNameOut + '.img', /OVERWRITE
FILE_COPY, client_machine_DIR + baseFNameOut + '.hdr', storage_machine_MP_DIR + baseFNameOut + '.hdr', /OVERWRITE
;delete all envi files
FILE_DELETE, client_machine_DIR + baseFNameOut + '.img'
FILE_DELETE, client_machine_DIR + baseFNameOut + '.hdr'
FILE_DELETE, client_machine_DIR +  h5Fname
RETURN, 0
END