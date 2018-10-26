PRO C_check_pheno
save_output = 1
dir_pheno_def, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn, prefix, suffix, dateformat, runVersion
;dir_pheno_def_vgt_old, dir_PHENOdef, dir_lta, phenoDefFullPath, phenoDirOut, merged_fixed_dir, target_area_fn, cm_fn
dir = phenoDirOut.dir_out + '\Filtered_seasonality'
;dir = 'Y:\remote_sensing\vgt\Pheno_Oct_2016\PhenoV2\test'
fn_pheno_check = 'pheno_check'
;fn_pheno_check = 'pheno_check_v2'
ns = 40320
nl = 14673

season = ['1','2']
;prefix = 'vt'
;prefix = 'bk'
suffix = season + '.img' 
fn_s = prefix + 's' + suffix
fn_m = prefix + 'm' + suffix
fn_sen = prefix + 'sen' + suffix
fn_e = prefix + 'e' + suffix

;open all and result
s = BYTARR(ns,nl,2)
m = s
sen = s
e = s



FOR i = 0, 1 DO BEGIN
  OPENR, lun, dir + '\' + fn_s[i], /GET_LUN
  tmp = BYTARR(ns,nl)
  READU, lun, tmp
  s[*,*,i] = tmp
  FREE_LUN, lun
  
  OPENR, lun, dir + '\' + fn_m[i], /GET_LUN
  tmp = BYTARR(ns,nl)
  READU, lun, tmp
  m[*,*,i] = tmp
  FREE_LUN, lun
  
  OPENR, lun, dir + '\' + fn_sen[i], /GET_LUN
  tmp = BYTARR(ns,nl)
  READU, lun, tmp
  sen[*,*,i] = tmp
  FREE_LUN, lun
  
  OPENR, lun, dir + '\' + fn_e[i], /GET_LUN
  tmp = BYTARR(ns,nl)
  READU, lun, tmp
  e[*,*,i] = tmp
  FREE_LUN, lun
ENDFOR 

n_indicators = 3 
nb = 1 + n_indicators * 2 + 1 + 1 + 1
res = BYTARR(ns,nl,nb)
;res bands (value is 1 if the problem is found)
string_out = [ $
'Band 0: overlap betwen seasons', $
'Band 1 sos1 >= eos1', $
'Band 2 sen1 > eos1', $
'Band 3 max1 not between sos1 and sen1 (can be equal)', $
'Band 4 sos2 >= eos2', $
'Band 5 sen2 > eos2', $
'Band 6 max2 not between sos2 and sen2 (can be equal)', $
'Band 7 sen is present but sos no (so the first run was 252 and the second normal values)', $
'Band 8 sen is not present but sos yes (so the second run was 252 and the first normal values)', $
'Band 9 at least one problem (max of previous bands)']


FOR i = 0, ns - 1 DO BEGIN
  FOR j = 0, nl - 1 DO BEGIN
    IF (((i/FLOAT(ns)*100) MOD 10) EQ 0.0) AND (j EQ 0) THEN PRINT, STRTRIM(i/FLOAT(ns)*100,2) + ' % done'
    ;IF (i EQ 23352-1) AND (j EQ 4220-1) THEN STOP
    ;check that there is at least one
    ss = 0
    IF s[i,j,ss] LT 250 THEN BEGIN
      ;analyse it
      ;1 sos1 >= eos1
      IF s[i,j,ss] GE e[i,j,ss] THEN res[i,j,1+ss*n_indicators]=1
      ;2 sen1 > eos1
      IF sen[i,j,ss] GT e[i,j,ss] THEN res[i,j,2+ss*n_indicators]=1
      ;3 max1 not between sos1 and sen1
      IF ~((m[i,j,ss] GE s[i,j,ss]) AND (m[i,j,ss] LE sen[i,j,ss])) THEN BEGIN
        res[i,j,3+ss*n_indicators]=1
      ENDIF
      ;check if there is another one, in case chek it and check overlap
      ss = 1
      IF s[i,j,ss] LT 250 THEN BEGIN
        ;analyse it
        ;check overlap
        l1 = e[i,j,0] - s[i,j,0] ;length derived from 1-108 data
        l2 = e[i,j,1] - s[i,j,1]
        ;pheno dates derived in 1-36 format
        s1 = SPIRITS_from_108_to_36_ARRAY(s[i,j,0])
        s2 = SPIRITS_from_108_to_36_ARRAY(s[i,j,1])
        tmp1 = BYTARR(72)
        e1 = SPIRITS_from_108_to_36_ARRAY(e[i,j,0])
        e2 = SPIRITS_from_108_to_36_ARRAY(e[i,j,1])
        tmp2 = BYTARR(72)
        tmp1[s1-1:s1+l1-1]=1
        tmp2[s2-1:s2+l2-1]=1
        ;now check tha only one is active at a time
        IF (TOTAL(tmp1*tmp2) GT 0) THEN BEGIN
          res[i,j,0] = 1
        ENDIF
        ;1 sos1 >= eos1
        IF (s[i,j,ss] GE e[i,j,ss]) THEN res[i,j,1+ss*n_indicators]=1
        ;2 sen1 > eos1
        IF (sen[i,j,ss] GT e[i,j,ss]) THEN res[i,j,2+ss*n_indicators]=1
        ;3 max1 not between sos1 and sen1
        IF ~((m[i,j,ss] GE s[i,j,ss]) AND (m[i,j,ss] LE sen[i,j,ss])) THEN BEGIN
          res[i,j,3+ss*n_indicators]=1
        ENDIF
      ENDIF    
    ENDIF
    IF ((s[i,j,0] GT 250) AND (sen[i,j,0] LT 250)) OR $
       ((s[i,j,1] GT 250) AND (sen[i,j,1] LT 250)) THEN res[i,j,7]=1
    IF ((s[i,j,0] LT 250) AND (sen[i,j,0] GT 250)) OR $
       ((s[i,j,1] LT 250) AND (sen[i,j,1] GT 250)) THEN res[i,j,8]=1
    ;fill the last band summarising problems
    res[i,j,-1] = MAX(res[i,j,*])
  ENDFOR
ENDFOR
IF (save_output) THEN BEGIN
  OPENW, lun, dir + '\' + fn_pheno_check + '.img', /GET_LUN
  WRITEU, lun, res
  FREE_LUN, lun
  tmp =write_envi_hdr(dir + '\' + fn_pheno_check + '.hdr', ns, nl, 1, NBANDS=nb)
  OPENW, lun, dir + '\' + fn_pheno_check + '.hdr', /GET_LUN, /APPEND
  PRINTF, lun, 'band names = {1 season_overlap, 2 sos1 GE eos1, 3 sen1 GT eos1, 4 max1_not_in_sos1-sen1,'
  PRINTF, lun, '5 sos2 GE eos2, 6 sen2 GT eos2, 7 max2_not_in_sos2-sen2, 8 sen is present but sos no, 9 sos present but sen no, 9any_problem}'
  FREE_LUN, lun
ENDIF
FOR i = 0, nb-1 DO BEGIN
  ;PRINT, i
  ind = WHERE(res[*,*,i] GT 0, count)
  PRINT, strtrim(i,2)  + '- ' + string_out[i] + ', n pix with 1: ' + STRTRIM(count,2)
ENDFOR
ind = WHERE(res[*,*,-1] GT 0, count)
IF (count EQ 0) THEN BEGIN
  OPENW, lun, dir + '\' + 're_checked_0_error.txt', /GET_LUN
  PRINTF, lun, 'Checked on ' + STRTRIM(SYSTIME(),2)
  FREE_LUN, lun
ENDIF
END 