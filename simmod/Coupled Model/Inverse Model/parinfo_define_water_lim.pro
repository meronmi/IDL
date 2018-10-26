FUNCTION parinfo_define_water_lim, conf, FG=fg, JD0=JD0, SJD=sJD, EJD=eJD
;Function used to keep track of the different parameterizations

;set a common block to inform simForwMod_water_lim.pro that the wlim parameters are fixed and bewm is computed
;once and for all in the main
@cb_various.comm  
;COMMON wlim, wLimParFixed, wlim_type, kc, bewm, et0, bewm_et0, wpar, rsrLR, cwl, wl, P1_sen_model
wLimParFixed = 0
;SET PARINFO
nparms=N_ELEMENTS(fg)
parinfo = REPLICATE({FIXED:1, LIMITED:[0,0], LIMITS:[-999.d,-999.d], STEP:0.0d, MPFORMAT:'', PARNAME:''}, nparms)
parinfo[*].PARNAME = ['JD0','lai0','SLA', 'e_mx', 'gam', 'c', 'd', 'a', 'b', 'tb', 'Pcap', 'Phalf', 'Popt']
CASE conf OF
  1: BEGIN
      ;JD0, FREE within a +/- 60 gg
      parinfo[0].FIXED = 0
      parinfo[0].LIMITED = [1, 1]
      parinfo[0].LIMITS  = [DOUBLE(MAX([JD0-60, sJD])), DOUBLE(MIN([JD0+60, eJD]))]; [FLOAT(MAX([JD0-30, sJD])), FLOAT(MIN([JD0+30, eJD]))]
      parinfo[0].STEP = 8.0        ;1.0;8.0
      parinfo[0].MPFORMAT = '(F14.4)'
      PRINT, 'Limits for JD:', parinfo[0].LIMITS
      PRINT, 'Limits for JD:', JD2DDMMYYYY(parinfo[0].LIMITS)
      ;laiDOY0
      ;parinfo[1].FIXED = 0
    END
    2: BEGIN
      parinfo[1].MPFORMAT = '(F14.4)'
    ;laiDOY0
      parinfo[1].FIXED = 0
      parinfo[1].LIMITED = [1, 1]
      parinfo[1].LIMITS  = [0.01d, 0.08d]
      parinfo[1].STEP = 0.01d
    END
    4: BEGIN
      ;JD0 circa 2454278, FREE within a +/- 60 gg
      pp = 0
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [DOUBLE(MAX([JD0-60, sJD])), DOUBLE(MIN([JD0+60, eJD]))]; [FLOAT(MAX([JD0-30, sJD])), FLOAT(MIN([JD0+30, eJD]))]
      parinfo[pp].STEP = 8.0        ;1.0;8.0
      parinfo[pp].MPFORMAT = '(F14.4)'
      PRINT, 'Limits for JD:', parinfo[pp].LIMITS
      PRINT, FORMAT='("Limits for JD (date): ",I2,"/",I2,"/",I4," - ",I2,"/",I2,"/",I4)',JD2DDMMYYYY(parinfo[pp].LIMITS[0]), JD2DDMMYYYY(parinfo[pp].LIMITS[1])
;      ;laiDOY0 circa 0.05
;            pp = 1
;            parinfo[pp].FIXED =  0
;            parinfo[pp].LIMITED = [1, 1]
;            parinfo[pp].LIMITS  = [0.01d, 0.05d]
;            parinfo[pp].STEP = 0.005d
;            PRINT, 'Limits for laiDoy0:', parinfo[pp].LIMITS
      ;SLA = 0.018
      pp = 2
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [0.01d, 0.10d]
      parinfo[pp].STEP = 0.01d
      PRINT, 'Limits for SLA:', parinfo[pp].LIMITS
      ;e_amx = 2.23
      ;      pp = 3
      ;      parinfo[pp].FIXED = 0
      ;      parinfo[pp].LIMITED = [1, 1]
      ;      parinfo[pp].LIMITS  = [1.5d, 3.0d] ; approximately from Chen et al., 2014, Biogeosciences, 11, 3871-3880
      ;      parinfo[pp].STEP = 0.05d
      PRINT, 'Limits for Eps max:', parinfo[pp].LIMITS
      ;c = 1500
      pp = 5
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [250.0d, 2500.0d]
      parinfo[pp].STEP = 50.0d
      PRINT, 'Limits for c:', parinfo[pp].LIMITS
      ;d = 1
      pp = 6
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [0.0d, 3.0d] 
      ;parinfo[pp].LIMITS  = [-3.0d, 3.0d]
      parinfo[pp].STEP = 0.2d
      PRINT, 'Limits for d:', parinfo[pp].LIMITS
      ;a = 1
      pp = 7
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [1000.0d, 5000.0d] 
      parinfo[pp].STEP = 100.0d
      PRINT, 'Limits for a:', parinfo[pp].LIMITS
      ;b = 1
      pp = 8
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [-3.0d, 3.0d] 
      parinfo[pp].STEP =0.5d
      PRINT, 'Limits for b:', parinfo[pp].LIMITS
      ;Popt
      ;      pp = 12
      ;      parinfo[pp].FIXED = 0
      ;      parinfo[pp].LIMITED = [1, 1]
      ;      parinfo[pp].LIMITS  = [1.0, 3.0d] 
      ;      parinfo[pp].STEP =0.5d
      ;      PRINT, 'Limits for Popt:', parinfo[pp].LIMITS
    END
    5: BEGIN
     ;JD0 circa 2454278, FREE within a +/- 30 gg
      pp = 0
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [DOUBLE(MAX([JD0-60, sJD])), DOUBLE(MIN([JD0+60, eJD]))]; [FLOAT(MAX([JD0-30, sJD])), FLOAT(MIN([JD0+30, eJD]))]
      parinfo[pp].STEP = 8.0        ;1.0;8.0
      parinfo[pp].MPFORMAT = '(F14.4)'
      PRINT, 'Limits for JD:', parinfo[pp].LIMITS
      PRINT, FORMAT='("Limits for JD (date): ",I2,"/",I2,"/",I4," - ",I2,"/",I2,"/",I4)',JD2DDMMYYYY(parinfo[pp].LIMITS[0]), JD2DDMMYYYY(parinfo[pp].LIMITS[1])
      ;laiDOY0 circa 0.05
;      pp = 1
;      parinfo[pp].FIXED =  0
;      parinfo[pp].LIMITED = [1, 1]
;      parinfo[pp].LIMITS  = [0.01d, 0.08d]
;      parinfo[pp].STEP = 0.005d
;      PRINT, 'Limits for laiDoy0:', parinfo[pp].LIMITS
      ;SLA = 0.018
      pp = 2  
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [0.01d, 0.10d]
      parinfo[pp].STEP = 0.01d
      PRINT, 'Limits for SLA:', parinfo[pp].LIMITS
      ;e_amx = 2.23
;      pp = 3
;      parinfo[pp].FIXED = 0
;      parinfo[pp].LIMITED = [1, 1]
;      parinfo[pp].LIMITS  = [1.5d, 3.0d] ; approximately from Chen et al., 2014, Biogeosciences, 11, 3871-3880
;      parinfo[pp].STEP = 0.05d
      PRINT, 'Limits for Eps max:', parinfo[pp].LIMITS
      ;c = 1500
      pp = 5
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [250.0d, 2500.0d] 
      parinfo[pp].STEP = 50.0d
      PRINT, 'Limits for c:', parinfo[pp].LIMITS
      ;d = 1
      pp = 6
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [0.0d, 3.0d]
      parinfo[pp].STEP = 0.2d
      PRINT, 'Limits for d:', parinfo[pp].LIMITS
      ;a = 1
      pp = 7
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [1000.0d, 5000.0d] 
      parinfo[pp].STEP = 100.0d
      PRINT, 'Limits for a:', parinfo[pp].LIMITS
      ;b = 1
      pp = 8
      parinfo[pp].FIXED = 0
      parinfo[pp].LIMITED = [1, 1]
      parinfo[pp].LIMITS  = [-3.0d, 5.0d] 
      parinfo[pp].STEP =0.5d
      PRINT, 'Limits for b:', parinfo[pp].LIMITS
      ;Popt
;      pp = 12
;      parinfo[pp].FIXED = 0
;      parinfo[pp].LIMITED = [1, 1]
;      parinfo[pp].LIMITS  = [1.0, 3.0d] 
;      parinfo[pp].STEP =0.5d
;      PRINT, 'Limits for Popt:', parinfo[pp].LIMITS
    END
  ELSE: STOP
ENDCASE

;if bewm parameter are fixed, store it in common block
indPcap = WHERE(parinfo[*].PARNAME EQ 'Pcap')
indPhalf = WHERE(parinfo[*].PARNAME EQ 'Phalf')
IF ((parinfo[indPcap].FIXED EQ 1) AND (parinfo[indPcap].FIXED EQ 1)) THEN wLimParFixed = 1

RETURN, parinfo
END