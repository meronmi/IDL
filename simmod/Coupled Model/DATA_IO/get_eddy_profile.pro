FUNCTION get_eddy_profile, eddy_fn
; -9999, the standard Fluxnet no data, is assumed to be NaN

;EDDY
tmp =  READ_CSV2(eddy_fn, HEADER=hdr, MISSING_VALUE=-9999)
n = N_ELEMENTS(tmp.FIELD01)
eddy_data = { year: INTARR(n), $
  month: INTARR(n), $
  day:  INTARR(n), $
  JD: FLTARR(n), $
  radg: FLTARR(n), $
  ta: FLTARR(n), $
  vpd: FLTARR(n), $
  precip: FLTARR(n), $
  swc: FLTARR(n), $
  nee_mds: FLTARR(n), $
  nee_ann: FLTARR(n), $
  gpp_mds: FLTARR(n), $
  gpp_ann: FLTARR(n)}
  
ind = WHERE(hdr EQ 'Year')        & eddy_data.year = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'Month')       & eddy_data.month = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'Day')         & eddy_data.day = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'Rg_f')        & eddy_data.radg = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'Ta_f')        & eddy_data.ta = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'VPD_f')       & eddy_data.vpd = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'Precip')      & eddy_data.precip = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'SWC')         & eddy_data.swc = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'NEE_st_fMDS') & eddy_data.nee_mds = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'NEE_st_fANN') & eddy_data.nee_ann = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'GPP_st_MDS')  & eddy_data.gpp_mds = FLOAT(tmp.(ind))
ind = WHERE(hdr EQ 'GPP_st_ANN')  & eddy_data.gpp_ann = FLOAT(tmp.(ind))
eddy_data.JD = JULDAY(eddy_data.month, eddy_data.day, eddy_data.year)
;replace -9999 with NaN
FOR i=0,N_TAGS(eddy_data)-1 DO BEGIN
  ind = WHERE(eddy_data.(i) EQ -9999, count)
  IF (count GT 0) THEN eddy_data.(i)[ind] = !VALUES.D_NAN
ENDFOR
RETURN, eddy_data
END