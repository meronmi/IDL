FUNCTION get_modis_gpp_profile_v2, modis_fn, site_code
;to generate this csv, use R script in d:\Users\meronmi\Documents\R_scripts\MODIS-FORGE to download the data
;and then run readMODISprofile.pro IDL function to geneate the csv
;;or use GEE
;MODIS
tmp =  READ_CSV2(modis_fn, HEADER=hdr, MISSING_VALUE=-9999)
;replace -9999 and -99999 with NaN
FOR i=1, N_TAGS(tmp)-1 DO BEGIN
  ind = WHERE((tmp.(i) EQ -9999) OR (tmp.(i) EQ -99999), count)
  IF (count GT 0) THEN tmp.(i)[ind] = !VALUES.D_NAN
ENDFOR
modis_data = rename_tags(tmp, $
  ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5'], $
  ['Site_code','Year','DOY_comp','Gpp_daily','PsnNet_daily'])
ind = WHERE(modis_data.Site_code EQ site_code, count)
IF (count EQ 0) THEN STOP
modis_data2 = CREATE_STRUCT('Site_code', STRARR(count), 'Year', INTARR(count), $
                            'DOY_comp', INTARR(count), 'Gpp_daily', FLTARR(count), $
                            'PsnNet_daily',  FLTARR(count), 'JD', FLTARR(count))
FOR i = 0,4 DO modis_data2.(i) = modis_data.(i)[ind]
modis_data2.JD = DOY_YEAR2JD(modis_data2.DOY_comp, modis_data2.Year)
;modis_data = CREATE_STRUCT(modis_data, 'JD', FLTARR(N_ELEMENTS(modis_data.DOY_comp)))
;modis_data.JD = DOY_YEAR2JD(modis_data.DOY_comp, modis_data.Year)
RETURN, modis_data2
END