PRO Matteo_Zampieri_GIMMS_monthly_mean
fn_bil = '\\ies\d5\asap\users_data\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\up to 2015\global_NDVI3gV1_filteredSG.img'
;fn_bil = '\\ies\d5\asap\users_data\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\processed\global_NDVI3g_filteredSG.img'
noDataVal = [-10000, -3000, -5002]
fn_bsq_out = '\\ies\d5\asap\users_data\meronmi\El_NINO_ANTON\EL NINO Anton\GlobalGIMMS\up to 2015\global_NDVI3gV1_filteredSG_monthlyMean_up_to_2015.img'
ns = read_info('samples', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
nl = read_info('lines', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
nb = read_info('bands', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
mapinfo = read_info('map info', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
PRINT, 'nb=', nb
dt = read_info('data type', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
IF (dt NE 2) THEN STOP
intrlv= read_info('interleave', FILE_DIRNAME(fn_bil)+'\'+FILE_BASENAME(fn_bil, '.img')+'.hdr')
IF (intrlv NE 'bil') THEN STOP


dataIn = ReadBilWithHdr(fn_bil)
dataIn = FLOAT(dataIn)

FOR i = 0, noDataVal.LENGTH-1 DO BEGIN
  indNaN = WHERE(dataIn EQ noDataVal[i], countNaN)
  IF (countNaN GT 0) THEN dataIn[indNaN] = !VALUES.F_NAN
ENDFOR


dataOut = FLTARR(ns, nl, nb/2)
b = 0
FOR i = 0, nb-1, 2 DO BEGIN
  PRINT, i, i+1
  dataOut[*,*,b] = MEAN(dataIn[*,*,[i,i+1]], /NAN, DIMENSION = 3) 
  b = b + 1
ENDFOR

indNAN = WHERE(FINITE(dataOut) EQ 0, countNan)
IF (countNaN GT 0) THEN dataOut[indNaN] = noDataVal[0]
dataOut = FIX(ROUND(dataOut))
res = write_envi_img(dataOut, fn_bsq_out)
res = write_envi_hdr(FILE_DIRNAME(fn_bsq_out)+'\'+FILE_BASENAME(fn_bsq_out, '.img')+'.hdr', ns, nl, 2, NBANDS=nb/2, INTERLEAVE='bsq', $
  MAPINFO=mapinfo, FREE_TEXT = 'Michele, mean monthly NDVI computataion using Matteo_Zampieri_GIMMS_monthly_mean.pro', FLAGS = STRTRIM(noDataVal,2))

END