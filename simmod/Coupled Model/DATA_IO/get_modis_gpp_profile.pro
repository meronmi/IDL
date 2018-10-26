FUNCTION get_modis_gpp_profile, modis_fn
;to generate this csv, use R script in d:\Users\meronmi\Documents\R_scripts\MODIS-FORGE to download the data
;and then run readMODISprofile.pro IDL function to geneate the csv
;;or use GEE
;MODIS
tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;replace -9999 and -99999 with NaN
FOR i=0, N_TAGS(tmp)-1 DO BEGIN
  ind = WHERE((tmp.(i) EQ -9999) OR (tmp.(i) EQ -99999), count)
  IF (count GT 0) THEN tmp.(i)[ind] = !VALUES.D_NAN
ENDFOR
modis_data = rename_tags(tmp, $
  ['FIELD1','FIELD2','FIELD3','FIELD4'], $
  ['Year','DOY_comp','Gpp_daily','PsnNet_daily'])
modis_data = CREATE_STRUCT(modis_data, 'JD', FLTARR(N_ELEMENTS(modis_data.DOY_comp)))
modis_data.JD = DOY_YEAR2JD(modis_data.DOY_comp, modis_data.Year)
RETURN, modis_data
END