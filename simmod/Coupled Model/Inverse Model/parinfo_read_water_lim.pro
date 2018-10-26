FUNCTION parinfo_read_water_lim, ini_fn, FG=fg, JD0=JD0, SJD=sJD, EJD=eJD
;reads parinfo from ini file

;set a common block to inform simForwMod_water_lim.pro that the wlim parameters are fixed and bewm is computed
;once and for all in the main
@cb_various.comm  
;COMMON wlim, wLimParFixed, wlim_type, kc, bewm, et0, bewm_et0, wpar, rsrLR, cwl, wl, P1_sen_model
wLimParFixed = 0


;SET PARINFO
nparms=N_ELEMENTS(fg)
parinfo = REPLICATE({FIXED:1, LIMITED:[0,0], LIMITS:[-999.d,-999.d], STEP:0.0d, MPFORMAT:'', PARNAME:''}, nparms)
parinfo[*].PARNAME = ['JD0','lai0','SLA', 'e_mx', 'gam', 'c', 'd', 'a', 'b', 'tb', 'Pcap', 'Phalf', 'Popt']

FOR pp = 0, nparms-1 DO BEGIN
  str2search = parinfo[pp].PARNAME + '_parinfo'
  values =  read_info(str2search, ini_fn) 
  values = STRSPLIT(values, ',', /EXTRACT)
  parinfo[pp].FIXED = FIX(values[0])
  parinfo[pp].LIMITED = FIX([values[1], values[2]])
  parinfo[pp].LIMITS  = [DOUBLE(values[3]), DOUBLE(values[4])]
;  IF (parinfo[pp].PARNAME EQ 'JD0') THEN BEGIN
;    parinfo[pp].LIMITS  = [DOUBLE(MAX([JD0+FIX(values[3]), sJD])), DOUBLE(MIN([JD0+FIX(values[4]), eJD]))]
;    IF (parinfo[pp].FIXED EQ 0) THEN BEGIN
;      PRINT, 'Limits for JD:', parinfo[pp].LIMITS
;      PRINT, FORMAT='("Limits for JD (date): ",I2,"/",I2,"/",I4," - ",I2,"/",I2,"/",I4)',JD2DDMMYYYY(parinfo[pp].LIMITS[0]), JD2DDMMYYYY(parinfo[pp].LIMITS[1])
;    ENDIF 
;  ENDIF ELSE BEGIN
;    parinfo[pp].LIMITS  = [DOUBLE(values[3]), DOUBLE(values[4])]
;  ENDELSE
  parinfo[pp].STEP = DOUBLE(values[5])
  ;parinfo[pp].MPFORMAT = '(F14.4)'
ENDFOR
;define format if needed
parinfo[0].MPFORMAT = '(F14.4)'
    
;if bewm parameter are fixed, store it in common block
indPcap = WHERE(parinfo[*].PARNAME EQ 'Pcap')
indPhalf = WHERE(parinfo[*].PARNAME EQ 'Phalf')
IF ((parinfo[indPcap].FIXED EQ 1) AND (parinfo[indPcap].FIXED EQ 1)) THEN wLimParFixed = 1

RETURN, parinfo
END