FUNCTION Raphael_analyse_input_data_newFormat, fn_sav_s1, fn_sav_s2
; res = Raphael_analyse_input_data_newFormat('D:\RAPHAEL_pheno_test\LUCAS_S1_extraction_postprocess_20190704_v0.sav', 'D:\RAPHAEL_pheno_test\LUCAS_S2_extraction_postprocess_20190703_v0.sav')

dlmtr = ','
RESTORE, fn_sav_s1
RESTORE, fn_sav_s2

stime = JD2DDMMYYYY(SYSTIME(/JULIAN))
stimeStr = STRTRIM(stime[2],2) + '_' + STRTRIM(stime[1],2) + '_' + STRTRIM(stime[0],2)
fn_out = FILE_DIRNAME(fn_sav_s1) + '\LUCAS_extraction_stats_'+stimeStr+'.csv'
OPENW, lun, fn_out, /GET_LUN
PRINTF, lun, STRJOIN(['CROP','n_polgygons_S2','n_polygons_S1', 'avgObsS2','minObsS2','maxObsS2','avgObsS1','minObsS1','maxObsS1'] + dlmtr)

cropTypesS1 = s1.crop.Uniq()
cropTypesS2 = s2.crop.Uniq()
IF N_ELEMENTS(cropTypesS1) NE N_ELEMENTS(cropTypesS2) THEN STOP ELSE cropTypes = cropTypesS1

ncrops = N_ELEMENTS(cropTypesS1)  
;n_per_crop_type = LONARR(N_ELEMENTS(cropTypes))
;FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
;  ind = WHERE(y.crop EQ cropTypes[i], count)
;  n_per_crop_type[i] = count
;ENDFOR
;;sort descending
;sub = REVERSE(SORT(n_per_crop_type))
;cropTypes = cropTypes[sub]
;n_per_crop_type = n_per_crop_type[sub]

FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
  indS2 = WHERE((s2.crop EQ cropTypes[i]), countS2)
  indS1 = WHERE((s1.crop EQ cropTypes[i]), countS1)
  ;get the unique IDs of the crop
  uniqIdsS2 = s2.pointID[indS2].Uniq()
  uniqIdsS1 = s1.pointID[indS1].Uniq()
  
  nObsS2 = INTARR(uniqIdsS2.LENGTH)
  FOR k = 0, uniqIdsS2.LENGTH - 1 DO BEGIN
    indS2 = WHERE((s2.crop EQ cropTypes[i]) AND (s2.pointID EQ uniqIdsS2[k]), countS2, /NULL)
    indF = WHERE(FINITE(s2.NDVI_mean[indS2]) EQ 1, countF)
    nObsS2[k] = countF 
  ENDFOR
  
  nObsS1 = INTARR(uniqIdsS1.LENGTH)
  FOR k = 0, N_ELEMENTS(uniqIdsS1) - 1 DO BEGIN
    indS1 = WHERE((s1.crop EQ cropTypes[i]) AND (s1.pointID EQ uniqIdsS1[k]), countS1, /NULL)
    indF = WHERE(FINITE(s1.VH_MEAN[indS1]) EQ 1, countF)
    nObsS1[k] = countF
  ENDFOR
  numArray = [N_ELEMENTS(uniqIdsS2),N_ELEMENTS(uniqIdsS1), $
              MEAN(nObsS2),MIN(nObsS2),MAX(nObsS2),MEAN(nObsS1),MIN(nObsS1),MAX(nObsS1)]
  str = [cropTypes[i], numArray.Convert(/STRING)]
  PRINTF, lun, Str.Join(dlmtr)
ENDFOR

FREE_LUN, lun
END  