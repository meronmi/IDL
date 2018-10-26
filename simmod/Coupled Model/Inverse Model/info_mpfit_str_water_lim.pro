FUNCTION info_mpfit_str_water_lim, status, Bestnorm, npegged, pfree_index, parms, parinfo, fg, uppEnvFit, nIter, weightingScheme, eps_wlimOnOff
dlmtr = ', '
info_mpfit = STRARR(4)

info_mpfit[0] = 'Status: ' + STRTRIM(status,2) + dlmtr + 'BestChi2: ' + STRING(Bestnorm, FORMAT ='(F9.7)') + $
                dlmtr + 'WgtScheme: ' + weightingScheme + dlmtr + 'Fit2UpEnv: ' + STRTRIM(uppEnvFit, 2)
IF (uppEnvFit EQ 1) THEN info_mpfit[0] = info_mpfit[0] + dlmtr + 'UpEnvItr: ' + STRTRIM(nIter, 2)
IF (eps_wlimOnOff EQ 1) THEN info_mpfit[0] = info_mpfit[0] + dlmtr + 'W_Lim: ON' ELSE $
                             info_mpfit[0] = info_mpfit[0] + dlmtr + 'W_Lim: OFF'
info_mpfit[0] = info_mpfit[0] + dlmtr + 'nFree: ' + STRTRIM(N_ELEMENTS(pfree_index), 2)                              

;                        DOY0;JD0         lai0        SLA         e_max           gamma          c                 d          a            b           tb          cap        half      opt
;str_format =  '("------",A-13, "----",     A-7,"---", A-7, "-", A-6,"--",        A-7,"---",      A-7, "-------",  A-5, "----",A-8,"-----",A-7,"----", A-4, "---",A-6, "--", A-7,"-", A-7)'
;par_format_fg='("fg:--", I3,"--",F9.1,"--",F6.4,"--", F6.4,"--", F5.3,"----",     F4.2,"-------",F07.2,"----",   F5.3,"--", F07.2,"--",   F7.4,"--",  F4.2,"--",F05.1,"--", F06.1,"--", F04.2)'
;par_format_p ='("p :--", I3,"--",F9.1,"--",F6.4,"--", F6.4,"--", F5.3,"----",     F4.2,"-------",F07.2,"----",   F5.3,"--", F07.2,"--",   F7.4,"--",  F4.2,"--",F05.1,"--", F06.1,"--", F04.2)'
str_format =  '("____",  A-13, "___",        A-7,"__",  A-7, "__", A-6,"___",         A-6,"_",    A-7, "___",    A-5, "___",  A-8,"___",        A-7,"__",  A-4, "___",A-6, "_", A-7,"__", A-7)'
par_format_fg='("fg:__", I3,"__", F9.1,"__",F6.4,"__", F6.4,"___", F5.3,"____",     F4.2,"____",F07.2,"___",   F5.3,"__", F07.2,"___",   F7.4,"___",  F4.2,"__",  F05.1,"___", F06.1,"___", F04.2)'
par_format_p ='("p :__", I3,"__", F9.1,"__",F6.4,"__", F6.4,"___", F5.3,"____",     F4.2,"____",F07.2,"___",   F5.3,"__", F07.2,"___",   F7.4,"___",  F4.2,"__",  F05.1,"___", F06.1,"___", F04.2)'
tmp = parinfo[*].PARNAME
;tmp[0] = 'DOY0; '+tmp[0]
tmp[0] = 'DOY0_'+tmp[0]
tmp[pfree_index] = '['+ tmp[pfree_index] + ']'

;Check for free prameters pegged to limits
IF (npegged GT 0) THEN BEGIN
  info_mpfit[0] = info_mpfit[0] +dlmtr + 'nFree peg to lmts (*L,*U): ' + STRTRIM(npegged,2)
  ll = parinfo[*].LIMITS[0]
  ul = parinfo[*].LIMITS[1]
  ;subscripts of parameters pegged to lower limit, add the info to the string
  ind_par_pegged_ll = WHERE((parms - parinfo[*].LIMITS[0]) EQ 0.0, count_par_pegged_ll)
  IF (count_par_pegged_ll GT 0) THEN  tmp[ind_par_pegged_ll] = tmp[ind_par_pegged_ll] + '*L'
    ind_par_pegged_ul = WHERE((parms - parinfo[*].LIMITS[1]) EQ 0.0, count_par_pegged_ul)
  IF (count_par_pegged_ul GT 0) THEN  tmp[ind_par_pegged_ul] = tmp[ind_par_pegged_ul] + '*U'
  ENDIF
  
info_mpfit[1] = STRING(tmp, FORMAT=str_format, /PRINT)
info_mpfit[1] = mg_streplace(info_mpfit[1], ' ', '_', /GLOBAL)
;remove last _ characthers
p = STRPOS(info_mpfit[1], '_', /REVERSE_SEARCH)
WHILE p EQ STRLEN(info_mpfit[1])-1 DO BEGIN
  info_mpfit[1] = STRMID(info_mpfit[1],0,p)
  p = STRPOS(info_mpfit[1], '_', /REVERSE_SEARCH)
ENDWHILE
tmp_fg  = fg
tmp_parms  = parms
IF (eps_wlimOnOff EQ 0) THEN BEGIN
  tmp_fg[10:12]=0.0
  tmp_parms[10:12]=0.0
ENDIF
info_mpfit[2] = STRING([JD2DOY(fg[0]),tmp_fg], FORMAT=par_format_fg, /PRINT)
info_mpfit[3] = STRING([JD2DOY(parms[0]),tmp_parms], FORMAT=par_format_p, /PRINT)
RETURN, info_mpfit
END