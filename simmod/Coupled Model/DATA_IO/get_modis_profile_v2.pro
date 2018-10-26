FUNCTION get_modis_profile_v2, modis_fn, site_code
;to generate this csv, use R script in d:\Users\meronmi\Documents\R_scripts\MODIS-FORGE to download the data
;and then run readMODISprofile.pro IDL function to geneate the csv

;or use GEE

;MODIS
;bands R1_645 R2_858.5  R3_469  R4_555  R5_1240 R6_1640 R7_2130

;read the csv only once, then use the sav
dir = FILE_DIRNAME(modis_fn)
base = FILE_BASENAME(modis_fn, '.csv')
IF (FILE_TEST(dir + '\' + base + '.sav')) THEN BEGIN
  RESTORE, dir + '\' + base + '.sav'
ENDIF ELSE BEGIN
  tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
  SAVE, tmp, hdr, FILENAME = dir + '\' + base + '.sav'
ENDELSE

;tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;replace -9999 and -99999 with NaN
FOR i=1, N_TAGS(tmp)-1 DO BEGIN
  ind = WHERE((tmp.(i) EQ -9999) OR (tmp.(i) EQ -99999), count)
  IF (count GT 0) THEN tmp.(i)[ind] = !VALUES.D_NAN
ENDFOR
modis_data = rename_tags(tmp, $
  ['FIELD01','FIELD02','FIELD03','FIELD04','FIELD05','FIELD06','FIELD07',$
   'FIELD08','FIELD09','FIELD10','FIELD11','FIELD12','FIELD13','FIELD14','FIELD15','FIELD16','FIELD17'], $
  ['Site_code','qc','Year','DOY_comp','YearDayfract','Year_acq','DOY_acq','RAA','SZA','VZA','R1','R2','R3','R4','R5','R6','R7'])
ind = WHERE(modis_data.Site_code EQ site_code, count)
IF (count EQ 0) THEN STOP
modis_data2 = CREATE_STRUCT('Site_code', STRARR(count),'qc', INTARR(count), 'Year', INTARR(count), $
  'DOY_comp', INTARR(count),'YearDayfract', FLTARR(count),'Year_acq',INTARR(count),$
  'DOY_acq',DBLARR(count), 'RAA', DBLARR(count),'SZA', DBLARR(count),'VZA', DBLARR(count), $
  'R1', DBLARR(count),'R2', DBLARR(count),'R3', DBLARR(count),'R4', DBLARR(count),'R5', DBLARR(count),$
  'R6', DBLARR(count),'R7', DBLARR(count), 'JD', DBLARR(count))
FOR i = 0,16 DO BEGIN
  ;PRINT, i
  modis_data2.(i) = modis_data.(i)[ind]
ENDFOR

; here fix the fact that a date may have doy of acq but no data, if a doy is found, ref must be present
ind = WHERE(FINITE(modis_data2.RAA) EQ 0, count)
IF (count NE 0) THEN modis_data2.DOY_acq[ind] = !VALUES.F_NAN
modis_data2.JD = DOY_YEAR2JD(modis_data2.DOY_acq, modis_data2.Year_acq)
RETURN, modis_data2
END