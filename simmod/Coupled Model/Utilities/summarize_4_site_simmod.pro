PRO summarize_4_site_simmod
dir = 'Z:\SimMod_data\RUNS\non_reloc'
run_prefix = 'newRUN1_4sites_eps'
run_suffix = '_clump1.00' 
run_eps = STRTRIM(INDGEN(16)+10,2)
fn_prefix = 'AAA_overall_report_newRUN1_4sites_eps'

;open_output
OPENW, lunW, dir + '\' +run_prefix + '_comparison.csv', /GET_LUN
 
;use the first to get the hdr lines
OPENR, lun, dir + '\' +run_prefix + run_eps[0] + run_suffix + '\' + fn_prefix + run_eps[0] + '.csv', /GET_LUN
FOR l = 1, 2 DO BEGIN
  tmp=''
  READF, lun, tmp
  PRINTF, lunW, ',' + tmp
ENDFOR
READF, lun, tmp
PRINTF, lunW, 'Eps_max,' + tmp
READF, lun, tmp
PRINTF, lunW, STRTRIM( run_eps[0]/10.0,2)+ ',' + tmp
FREE_LUN, lun


FOR i = 1, N_ELEMENTS(run_eps)-1 DO BEGIN
  OPENR, lun, dir + '\' +run_prefix + run_eps[i] + run_suffix + '\' + fn_prefix + run_eps[i] + '.csv', /GET_LUN
  FOR l = 1, 3 DO BEGIN
    tmp=''
    READF, lun, tmp
  ENDFOR
  READF, lun, tmp
  PRINTF, lunW, STRTRIM( run_eps[i]/10.0,2)+ ',' + tmp
ENDFOR


FREE_LUN, lunW
END