FUNCTION Raphael_analyse_input_data, fn_sav
; res = Raphael_analyse_input_data('D:\RAPHAEL_pheno_test\LUCAS_S1-S2_extraction_20190520_v3.sav')
;y = CREATE_STRUCT('pointID', LONARR(ntot), 'dateJD', LONARR(ntot), 'x', FLTARR(ntot), 'y', FLTARR(ntot), 'nuts0', STRARR(ntot), $
;  'crop', STRARR(ntot), 'surveyDateJD', LONARR(ntot), 's12', BYTARR(ntot), $
;  'orbit', STRARR(ntot), 'CR', FLTARR(ntot), 'RVI', FLTARR(ntot), $
;  'NDVIm', FLTARR(ntot), 'NDVIsd', FLTARR(ntot))
dlmtr = ','
RESTORE, fn_sav
fn_out = FILE_DIRNAME(fn_sav) + '\' + FILE_BASENAME(fn_sav,'.sav') + '_stats.csv'
OPENW, lun, fn_out, /GET_LUN
PRINTF, lun, STRJOIN(['CROP','n_polgygons_S2','n_polygons_S1', 'avgObsS2','minObsS2','maxObsS2','avgObsS1','minObsS1','maxObsS1'] + dlmtr)

cropTypes = y.crop[UNIQ(y.crop,SORT(y.crop))]
ncrops = N_ELEMENTS(cropTypes)  ;this is summing s1 and s2 obs
n_per_crop_type = LONARR(N_ELEMENTS(cropTypes))
FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
  ind = WHERE(y.crop EQ cropTypes[i], count)
  n_per_crop_type[i] = count
ENDFOR
;sort descending
sub = REVERSE(SORT(n_per_crop_type))
cropTypes = cropTypes[sub]
n_per_crop_type = n_per_crop_type[sub]

FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
  indS2 = WHERE((y.crop EQ cropTypes[i]) AND (y.s12 EQ 2), countS2)
  indS1 = WHERE((y.crop EQ cropTypes[i]) AND (y.s12 EQ 1), countS1)
  ;get the unique IDs of the crop
  tmp = y.pointID[indS2]
  uniqIdsS2 = tmp[UNIQ(tmp, SORT(tmp))]
  tmp = y.pointID[indS1]
  uniqIdsS1 = tmp[UNIQ(tmp, SORT(tmp))]
  nObsS2 = INTARR(N_ELEMENTS(uniqIdsS2))
  nObsS1 = INTARR(N_ELEMENTS(uniqIdsS1))
  FOR k = 0, N_ELEMENTS(uniqIdsS2) - 1 DO BEGIN
    indS2 = WHERE((y.crop EQ cropTypes[i]) AND (y.s12 EQ 2) AND (y.pointID EQ uniqIdsS2[k]), countS2)
    indF = WHERE(FINITE(y.NDVIm[indS2]), countF)
    nObsS2[k] = countF 
  ENDFOR
  nObsS1 = INTARR(N_ELEMENTS(uniqIdsS1))
  FOR k = 0, N_ELEMENTS(uniqIdsS1) - 1 DO BEGIN
    indS1 = WHERE((y.crop EQ cropTypes[i]) AND (y.s12 EQ 1) AND (y.pointID EQ uniqIdsS1[k]), countS1)
    indF = WHERE(FINITE(y.NDVIm[indS1]), countF)
    nObsS1[k] = countF
    IF (countF GT 430) THEN PRINT, cropTypes[i], uniqIdsS1[k], countF
    IF (countF LT 50) THEN PRINT, cropTypes[i], uniqIdsS1[k], countF
  ENDFOR
  PRINTF, lun, STRJOIN([cropTypes[i], STRTRIM([N_ELEMENTS(uniqIdsS2),N_ELEMENTS(uniqIdsS1), MEAN(nObsS2),MIN(nObsS2),MAX(nObsS2),MEAN(nObsS1),MIN(nObsS1),MAX(nObsS1)],2)], dlmtr)
ENDFOR

FREE_LUN, lun
END  