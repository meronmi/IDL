FUNCTION get_modis_profile, modis_fn
;to generate this csv, use R script in d:\Users\meronmi\Documents\R_scripts\MODIS-FORGE to download the data
;and then run readMODISprofile.pro IDL function to geneate the csv

;or use GEE

;MODIS
;bands R1_645 R2_858.5  R3_469  R4_555  R5_1240 R6_1640 R7_2130


tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;replace -9999 and -99999 with NaN
FOR i=0, N_TAGS(tmp)-1 DO BEGIN
  ind = WHERE((tmp.(i) EQ -9999) OR (tmp.(i) EQ -99999), count)
  IF (count GT 0) THEN tmp.(i)[ind] = !VALUES.D_NAN
ENDFOR
modis_data = rename_tags(tmp, $
  ['FIELD01','FIELD02','FIELD03','FIELD04','FIELD05','FIELD06','FIELD07','FIELD08','FIELD09','FIELD10','FIELD11','FIELD12','FIELD13','FIELD14','FIELD15'], $
  ['Year','DOY_comp','YearDayfract','Year_acq','DOY_acq','RAA','SZA','VZA','R1','R2','R3','R4','R5','R6','R7'])
modis_data = CREATE_STRUCT(modis_data, 'JD', FLTARR(N_ELEMENTS(modis_data.DOY_acq)))
; here fix the fact that a date may have doy of acq but no data, if a doy is found, info must be present
ind = WHERE(FINITE(modis_data.RAA) EQ 0, count)
IF (count NE 0) THEN modis_data.DOY_acq[ind] = !VALUES.F_NAN
modis_data.JD = DOY_YEAR2JD(modis_data.DOY_acq, modis_data.Year_acq)
RETURN, modis_data
END