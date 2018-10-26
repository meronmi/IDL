PRO clean_anne_files
;***************************************************
;USER PAR
C_to_remove = [20];[23,18, 15, 20,21,17]
prj_id = 1;81
;to remove some time obs
T_to_remove = '';['2013-09-24'];17['2013-09-24']
dir = 'T:\Projects\GGW_monitoring\WFP\Test_Laura';'S:\Actions\FOODSEC\projects\GGW_monitoring\GGW_Landsat_analysis\USGS\BACI\site 81 new cleaning'
;***************************************************


fn = 'ndvi_control_and_project' + STRTRIM(prj_id,2)+'_ANNE_FORMAT'
fn = dir + '\' + fn 
PRINT, 'Attention: Project IDs have an offset for GGW project that is subtracted in the code' 
C_to_remove = C_to_remove -10

res = READ_CSV(fn + '.csv', HEADER=hdr)
;remove controls
ind = !NULL
FOR i = 0, N_ELEMENTS(C_to_remove)-1 DO BEGIN
  tmp = WHERE(res.FIELD2 EQ 'C'+STRTRIM(C_to_remove[i],2))
  ind = [ind, tmp]
ENDFOR
ind_all = LINDGEN(N_ELEMENTS(res.FIELD2))
ind_to_keep = SetDifference(ind_all,ind)
n = N_ELEMENTS(ind_to_keep)
out_struct = CREATE_STRUCT('f1', STRARR(n), 'f2', STRARR(n), 'f3', STRARR(n), $
                           'f4', STRARR(n), 'f5', STRARR(n), 'f6', DBLARR(n))
out_struct.f1 = res.FIELD1[ind_to_keep]
out_struct.f2 = res.FIELD2[ind_to_keep] 
out_struct.f3 = res.FIELD3[ind_to_keep] 
out_struct.f4 = res.FIELD4[ind_to_keep] 
out_struct.f5 = res.FIELD5[ind_to_keep] 
out_struct.f6 = res.FIELD6[ind_to_keep]     
;now remove years if asked
IF T_to_remove[0] NE '' THEN BEGIN
  ind = !NULL
  FOR i = 0, N_ELEMENTS(T_to_remove)-1 DO BEGIN
    tmp = WHERE(out_struct.f4 EQ STRTRIM(T_to_remove[i],2))
    ind = [ind, tmp]
  ENDFOR
  ind_all = LINDGEN(N_ELEMENTS(out_struct.f4))
  ind_to_keep = SetDifference(ind_all,ind)
  n = N_ELEMENTS(ind_to_keep)
  out_struct2 = CREATE_STRUCT('f1', STRARR(n), 'f2', STRARR(n), 'f3', STRARR(n), $
    'f4', STRARR(n), 'f5', STRARR(n), 'f6', DBLARR(n))
  out_struct2.f1 = out_struct.f1[ind_to_keep]
  out_struct2.f2 = out_struct.f2[ind_to_keep]
  out_struct2.f3 = out_struct.f3[ind_to_keep]
  out_struct2.f4 = out_struct.f4[ind_to_keep]
  out_struct2.f5 = out_struct.f5[ind_to_keep]
  out_struct2.f6 = out_struct.f6[ind_to_keep]
  WRITE_CSV, fn + '_cleaned.csv', out_struct2, HEADER=hdr
ENDIF ELSE BEGIN
  WRITE_CSV, fn + '_cleaned.csv', out_struct, HEADER=hdr
ENDELSE
OPENW, lun, fn + 'list_of_removed.txt', /GET_LUN
PRINTF, lun, C_to_remove
FREE_LUN, lun
PRINT, 'finished'



END