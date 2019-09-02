FUNCTION Sentinel_pheno_bridge_gather_results, dir
;get and join the results of a bridge execution
;
;res = raphael_pheno_v3_bridge_gather_results('D:\RAPHAEL_pheno_test\Results_2019May24')
;
;
;copy childs output to a dir
dir_out_sav = 'Bridge_console_outputs_and_savs'
FILE_MKDIR, dir + '\'+ dir_out_sav
res = FILE_SEARCH(dir + '\out_*.txt')
 
IF FILE_TEST(dir + '\out_1.txt') EQ 1 THEN FILE_MOVE, dir + '\out_*.txt',  dir + '\'+ dir_out_sav
IF FILE_TEST(dir + '\*.sav') EQ 1 THEN FILE_MOVE, dir + '\*.sav',  dir + '\'+ dir_out_sav
;
dir_fail_miss = 'Bridge_Failures_missing'
FILE_MKDIR, dir + '\'+ dir_fail_miss

;put toghether pheno_failures
res = FILE_SEARCH(dir + '\pheno_failures_*.txt')
IF res[0] NE '' THEN BEGIN
  OPENW, lunw, dir + '\pheno_failures.csv', /GET_LUN
  ;get the hdr
  OPENR, lunr, res[0], /GET_LUN
  str=''
  READF, lunr, str
  PRINTF, lunw, str
  FREE_LUN, lunr
  FOR i = 0, N_ELEMENTS(res)-1 DO BEGIN
    OPENR, lunr, res[i], /GET_LUN
    ;skip the hdr
    str=''
    READF, lunr, str
    WHILE ~ EOF(lunr) DO BEGIN
      str=''
      READF, lunr, str
      PRINTF, lunw, str
    ENDWHILE
    FREE_LUN, lunr
  ENDFOR
  FREE_LUN, lunw
  FILE_MOVE, dir + '\pheno_failures_*.txt',  dir + '\'+ dir_fail_miss
ENDIF

;put toghether pheno_missing_data_
res = FILE_SEARCH(dir + '\pheno_missing_data_*.txt')
IF res[0] NE '' THEN BEGIN
  OPENW, lunw, dir + '\pheno_missing_data.csv', /GET_LUN
  ;get the hdr
  OPENR, lunr, res[0], /GET_LUN
  str=''
  READF, lunr, str
  PRINTF, lunw, str
  FREE_LUN, lunr
  FOR i = 0, N_ELEMENTS(res)-1 DO BEGIN
    OPENR, lunr, res[i], /GET_LUN
    ;skip the hdr
    str=''
    READF, lunr, str
    WHILE ~ EOF(lunr) DO BEGIN
      str=''
      READF, lunr, str
      PRINTF, lunw, str
    ENDWHILE
    FREE_LUN, lunr
  ENDFOR
  FREE_LUN, lunw
  FILE_MOVE, dir + '\pheno_missing_data_*.txt',  dir + '\'+ dir_fail_miss
ENDIF

dir_pheno_results = 'Bridge_results'
FILE_MKDIR, dir + '\'+ dir_pheno_results
;put toghether pheno_results_
res = FILE_SEARCH(dir + '\pheno_results_*.csv')
OPENW, lunw, dir + '\pheno_results.csv', /GET_LUN
;get the hdr
OPENR, lunr, res[0], /GET_LUN
str=''
READF, lunr, str
PRINTF, lunw, str
FREE_LUN, lunr
FOR i = 0, N_ELEMENTS(res)-1 DO BEGIN
  OPENR, lunr, res[i], /GET_LUN
  ;skip the hdr
  str=''
  READF, lunr, str
  WHILE ~ EOF(lunr) DO BEGIN
    str=''
    READF, lunr, str
    PRINTF, lunw, str
  ENDWHILE
  FREE_LUN, lunr
ENDFOR
FREE_LUN, lunw
FILE_MOVE, dir + '\pheno_results_*.csv',  dir + '\'+ dir_pheno_results

RETURN, 0
END