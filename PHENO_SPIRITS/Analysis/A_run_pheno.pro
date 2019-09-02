PRO A_run_pheno, all_in_a_STRUCTURE = all_in_a_STRUCTURE
;PRO A_run_pheno
;Note: with BOKU the the SPF are all the same (normal, max, range) becuase there are no clear problems detected

;# compute eos and sen (as a second run with higher FENeos)
;if rangeAndMaxRun EQ 1 it does the following:
;# fix the arid areas problem of an irrealistically long season by excluding those pixel that:
; have a length > maxLen (to be set in B_harvest_pheno_and_correct)
;   AND
; an absolute max <=0.18; it was: 0.3 (this to keep nearly evergreen) 
;   AND
; a min max range <= 0.048; it was: 0.2 (this to keep all normal season)
; 
; the idea is that in arid places (max<=0.3, range<=0.2) you cannot have a season for more than a half year
; A_run_pheno runs the various pheno runs that are later used to correct the pheno

IF KEYWORD_SET(all_in_a_STRUCTURE) EQ 1 THEN BEGIN
  ;"decompress" the big structure if it was sent here
  runVersion = all_in_a_STRUCTURE.runVersion
  prefix = all_in_a_STRUCTURE.prefix
  suffix = all_in_a_STRUCTURE.suffix
  dateformat = all_in_a_STRUCTURE.dateformat
  dir_lta = all_in_a_STRUCTURE.dir_lta
  dir_PHENOdef = all_in_a_STRUCTURE.dir_PHENOdef
  ;New thresholds 0.25-0.35
  phenoDefFullPath = all_in_a_STRUCTURE.phenoDefFullPath
  base_dir_pheno_out =  all_in_a_STRUCTURE. base_dir_pheno_out
  phenoDirOut = all_in_a_STRUCTURE.phenoDirOut
  merged_fixed_dir = all_in_a_STRUCTURE.merged_fixed_dir
  target_area_fn = all_in_a_STRUCTURE.target_area_fn
  cm_fn = all_in_a_STRUCTURE.cm_fn
  devDir = all_in_a_STRUCTURE.devDir
ENDIF ELSE BEGIN
  dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion, devDir
ENDELSE

on_lta = 1 ; run on lta

IF (on_lta NE 1) THEN STOP

fn_bat = 'IDL_runV' + runVersion + '.bat'                   
exePath = devDir+'\PHENOdef16.exe';"Y:\software\glimpse\EXE\PHENOdef16.exe"


;check that directory exists, if not make it
FOR i = 0, N_TAGS(phenoDirOut)-1 DO FILE_MKDIR, phenoDirOut.(i)

;set command for pheno
dlmtr = ' '
file_dlmtr = '"'
p1 = '0' & p2 = '""' & p3 = '""' & p4 = '""' & p5 = '""' & p6 = '""' 
p7 = file_dlmtr + dir_lta + '\' + prefix + file_dlmtr
p8 = dateformat ;date format
p9 = suffix
P10 = '""'
P12 = '""' & P13 = '""' & P14 = '""'
P16 = '1' & P17 = '0' & P18 = '0' & P19 = '1' & P20 = '0' & P21 = '0' 
;make a bat 
CD, devDir
;OPENW, lun, dir_PHENOdef + '\' + fn_bat, /GET_LUN
OPENW, lun, devDir + '\' + fn_bat, /GET_LUN
PRINTF, lun, 'call GLIMset.BAT'
;run the normal pheno
p11 = file_dlmtr + phenoDefFullPath.normal + file_dlmtr
;normal run
p15 = file_dlmtr + phenoDirOut.normal + '\' + prefix + file_dlmtr
cmd = exePath + ' ' + $
      p1 + dlmtr + p2 + dlmtr + p3 + dlmtr + p4 + dlmtr + p5 + dlmtr + p6 + dlmtr + p7 + dlmtr + p8 + dlmtr + $
      p9 + dlmtr + p10 + dlmtr + p11 + dlmtr + p12 + dlmtr + p13 + dlmtr + p14 + dlmtr + p15 + dlmtr + p16 + dlmtr + $
      p17 + dlmtr + p18 + dlmtr + p19 + dlmtr + p20 + dlmtr + p21
PRINTF, lun, cmd
;run the sen pheno
p11 = file_dlmtr + phenoDefFullPath.sen + file_dlmtr
;senescence run
p15 = file_dlmtr + phenoDirOut.sen + '\' + prefix + file_dlmtr
cmd = exePath + ' ' + $
  p1 + dlmtr + p2 + dlmtr + p3 + dlmtr + p4 + dlmtr + p5 + dlmtr + p6 + dlmtr + p7 + dlmtr + p8 + dlmtr + $
  p9 + dlmtr + p10 + dlmtr + p11 + dlmtr + p12 + dlmtr + p13 + dlmtr + p14 + dlmtr + p15 + dlmtr + p16 + dlmtr + $
  p17 + dlmtr + p18 + dlmtr + p19 + dlmtr + p20 + dlmtr + p21
PRINTF, lun, cmd
;run the max03 pheno
p11 = file_dlmtr + phenoDefFullPath.max + file_dlmtr
;max run
p15 = file_dlmtr + phenoDirOut.max + '\' + prefix + file_dlmtr
cmd = exePath + ' ' + $
  p1 + dlmtr + p2 + dlmtr + p3 + dlmtr + p4 + dlmtr + p5 + dlmtr + p6 + dlmtr + p7 + dlmtr + p8 + dlmtr + $
  p9 + dlmtr + p10 + dlmtr + p11 + dlmtr + p12 + dlmtr + p13 + dlmtr + p14 + dlmtr + p15 + dlmtr + p16 + dlmtr + $
  p17 + dlmtr + p18 + dlmtr + p19 + dlmtr + p20 + dlmtr + p21
PRINTF, lun, cmd
;run the range018 pheno
p11 = file_dlmtr + phenoDefFullPath.range + file_dlmtr
;range run
p15 = file_dlmtr + phenoDirOut.range + '\' + prefix + file_dlmtr
cmd = exePath + ' ' + $
  p1 + dlmtr + p2 + dlmtr + p3 + dlmtr + p4 + dlmtr + p5 + dlmtr + p6 + dlmtr + p7 + dlmtr + p8 + dlmtr + $
  p9 + dlmtr + p10 + dlmtr + p11 + dlmtr + p12 + dlmtr + p13 + dlmtr + p14 + dlmtr + p15 + dlmtr + p16 + dlmtr + $
  p17 + dlmtr + p18 + dlmtr + p19 + dlmtr + p20 + dlmtr + p21
PRINTF, lun, cmd
FREE_LUN, lun
;execute it (if it is on another pc run it manually)
tmp = ' > IDL_run_outputV' + runVersion +'.txt'
SPAWN, devDir + '\' + fn_bat + tmp, res

PRINT, res


END