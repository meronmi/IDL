PRO save_inversion_results, report_fn, $
      ini_fn, out_dir, base_name, sYear, $
      useEddyMet, ret, GPP_simVSobs, GPP_modisVSobs, eps_wlimOnOff
                            
                            
;CATCH, Error_status
;
;;This statement begins the error handler:
;IF Error_status NE 0 THEN BEGIN
;  PRINT, 'Error index: ', Error_status
;  PRINT, 'Error message: ', !ERROR_STATE.MSG
;  ; Handle the error by extending A:
;  A=FLTARR(12)
;  CATCH, /CANCEL
;ENDIF
;report_fn = out_dir + '\' + base_name + '_rep.csv'
dlmtr = ','
line = REPLICATE(CREATE_STRUCT('name', '', $
             'value', ''), 34)

i = 0
line[i].name = 'Ini_file'
line[i].value =  ini_fn
i = i+1
line[i].name = 'Out_dir'
line[i].value =  out_dir
i = i+1
line[i].name = 'Year_site'
line[i].value =  base_name
i = i+1
line[i].name = 'Year'
line[i].value =  STRTRIM(sYear,2)
i = i+1
IF (useEddyMet EQ 1) THEN tmp = 'Eddy' ELSE tmp = 'ECMWF'
line[i].name = 'Met_source'
line[i].value =  tmp
i = i+1
line[i].name = 'Status'
line[i].value =  STRTRIM(ret.info_mpfit.Status,2)
i = i+1
line[i].name = 'BestChi2'
line[i].value =  STRTRIM(ret.info_mpfit.BestChi2,2)
i = i+1
line[i].name = 'WgtScheme'
line[i].value =  STRTRIM(ret.info_mpfit.WgtScheme,2)
i = i+1
line[i].name = 'Fit2UpEnv'
line[i].value =  STRTRIM(FIX(ret.info_mpfit.Fit2UpEnv),2)
i = i+1
line[i].name = 'UpEnvItr'
line[i].value =  STRTRIM(ret.info_mpfit.UpEnvItr,2)
i = i+1
line[i].name = 'W_Lim'
line[i].value =  STRTRIM(ret.info_mpfit.eps_wlimOnOff,2)
i = i+1
line[i].name = 'nFree'
line[i].value =  STRTRIM(N_ELEMENTS(ret.info_mpfit.pfree_index),2)
i = i+1
line[i].name = 'Gs_R2'
line[i].value =  STRTRIM(GPP_simVSobs.R2,2)
i = i+1
line[i].name = 'Gs_slope'
line[i].value =  STRTRIM(GPP_simVSobs.slope,2)
i = i+1
line[i].name = 'Gs_offset'
line[i].value =  STRTRIM(GPP_simVSobs.offset,2)
i = i+1
line[i].name = 'Gs_rmse'
line[i].value =  STRTRIM(GPP_simVSobs.rmse,2)
i = i+1
line[i].name = 'Gs_bias'
line[i].value =  STRTRIM(GPP_simVSobs.bias,2)
i = i+1
line[i].name = 'Gs_%deltaCumGPP'
line[i].value =  STRTRIM(GPP_simVSobs.percGPPdelta,2)
i = i+1
line[i].name = 'Gm17_R2'
line[i].value =  STRTRIM(GPP_modisVSobs.R2,2)
i = i+1
line[i].name = 'Gm17_slope'
line[i].value =  STRTRIM(GPP_modisVSobs.slope,2)
i = i+1
line[i].name = 'Gm17_offset'
line[i].value =  STRTRIM(GPP_modisVSobs.offset,2)
i = i+1
line[i].name = 'Gm17_rmse'
line[i].value =  STRTRIM(GPP_modisVSobs.rmse,2)
i = i+1
line[i].name = 'Gm17_bias'
line[i].value =  STRTRIM(GPP_modisVSobs.bias,2)
i = i+1
line[i].name = 'Gm17_%deltaCumGPP'
line[i].value =  STRTRIM(GPP_modisVSobs.percGPPdelta,2)
i = i+1
line[i].name = 'nPegged'
line[i].value =  STRTRIM(ret.info_mpfit.npegged,2)

i = i+1
tmp = STRJOIN(STRTRIM('P_' + ret.info_mpfit.parinfo[*].PARNAME,2) + dlmtr)
line[i].name = STRMID(tmp, 0, STRLEN(tmp)-1)
tmp = STRARR(N_ELEMENTS(ret.info_mpfit.parms)) 
tmp[ret.info_mpfit.pfree_index] = ret.info_mpfit.parms[ret.info_mpfit.pfree_index]
tmp = STRJOIN(STRTRIM(tmp,2) + dlmtr)
line[i].value =  STRMID(tmp, 0, STRLEN(tmp)-1)
i = i+1
tmp = STRJOIN(STRTRIM('fg_' + ret.info_mpfit.parinfo[*].PARNAME,2) + dlmtr)
line[i].name = STRMID(tmp, 0, STRLEN(tmp)-1)
tmp = STRJOIN(STRTRIM(ret.info_mpfit.fg,2) + dlmtr)
line[i].value =  STRMID(tmp, 0, STRLEN(tmp)-1)
i = i+1
tmp = STRJOIN(STRTRIM('Pegged_' + ret.info_mpfit.parinfo[*].PARNAME,2) + dlmtr)
line[i].name = STRMID(tmp, 0, STRLEN(tmp)-1)
tmp = STRARR(N_ELEMENTS(ret.info_mpfit.parms))
IF (ret.info_mpfit.npegged GT 0) THEN BEGIN
  ll = ret.info_mpfit.parinfo[*].LIMITS[0]
  ul = ret.info_mpfit.parinfo[*].LIMITS[1]
  ;subscripts of parameters pegged to lower limit, add the info to the string
  ind_par_pegged_ll = WHERE((ret.info_mpfit.parms - ret.info_mpfit.parinfo[*].LIMITS[0]) EQ 0.0, count_par_pegged_ll)
  IF (count_par_pegged_ll GT 0) THEN  tmp[ind_par_pegged_ll] = 'L'
  ind_par_pegged_ul = WHERE((ret.info_mpfit.parms - ret.info_mpfit.parinfo[*].LIMITS[1]) EQ 0.0, count_par_pegged_ul)
  IF (count_par_pegged_ul GT 0) THEN  tmp[ind_par_pegged_ul] = 'U'
ENDIF
tmp = STRJOIN(STRTRIM(tmp,2) + dlmtr)
line[i].value =  STRMID(tmp, 0, STRLEN(tmp)-1)

;i = i+1
;line[i].name = 'x'
;line[i].value =  STRTRIM(x,2)


res = FILE_SEARCH(report_fn)
IF (res EQ '') THEN BEGIN
  ;it does not exists, open it and write hdr
  OPENW, lun, report_fn, /GET_LUN
  PRINTF, lun, STRJOIN(line[*].name + dlmtr)
ENDIF ELSE BEGIN
  ;it exists, no hdr needed, apppend results
  OPENW, lun, report_fn, /GET_LUN, /APPEND
ENDELSE
;write results
PRINTF, lun, STRJOIN(line[*].value + dlmtr)
;close and exit
FREE_LUN, lun



END