PRO aaa_find_best_at_pixel_linear_reg
;13 apr 2016, modified to compute r2 instead of r, indicator before sos dropped

saveDataForTest = 0
out_of = 1
;**************************************************************************
rfe_dataset = 'chirps';'tamsat' ;'chirps'
CASE rfe_dataset OF
  'tamsat': BEGIN
    ;TAMSAT RESOLUTION
    mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
    fn_intersection = 'eco_crop_pasture_res_TAMSAT.img' ;pasrure = 1, crop = 2
    ns = 1867
    nl = 348
    dir = 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\correlation'
  END
  'chirps': BEGIN
    ;CHIRPS RESOLUTION
    mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
    fn_intersection = 'eco_crop_pasture_res_CHIRPS.img' ;pasrure = 1, crop = 2
    ns = 1402
    nl = 262
    dir = 'E:\WA\EWP\CFAPAR_work\CHIRPS resolution\correlation_chirps'
  END
  ELSE: STOP
ENDCASE
;**************************************************************************

suffixes = ['_atProg0','_atProg25','_atProg50','_atProg75','_atProg100']
prefixes = !NULL
;hl = half_life_define_all()
;FOR i = 0, N_ELEMENTS(hl)- 1 DO prefixes = [prefixes, 'standardized_cwp_hl'+STRTRIM(hl[i],2)]
;prefixes=[prefixes,'SPI3','SPI4','SPI5','SPI6','SPI7','SPI8','SPI9','SPI10','SPI11','SPI12','SPI13','SPI14','SPI15']
;now only on monthly to make it more understandable:
prefixes=[prefixes,'SPI3','SPI6','SPI9','SPI12','SPI15','SPI18']
;prefixes=['standardized_cwp_hl250','standardized_cwp_hl500','standardized_cwp_hl1000',$
;    'standardized_cwp_hl2000','standardized_cwp_hl3000','SPI3', $
;    'SPI4','SPI5','SPI6','SPI7','SPI8','SPI9','SPI10','SPI12']
fnames_corr = !NULL
fnames_p = !NULL
;from numerical is using spi and progress 
numerical_id = [[3,0],[3,25],[3,50],[3,75],[3,100],$
                [6,0],[6,25],[6,50],[6,75],[6,100],$
                [9,0],[9,25],[9,50],[9,75],[9,100],$
                [12,0],[12,25],[12,50],[12,75],[12,100],$
                [15,0],[15,25],[15,50],[15,75],[15,100],$
                [18,0],[18,25],[18,50],[18,75],[18,100]]

FOR i= 0, N_ELEMENTS(prefixes)-1 DO BEGIN
  FOR j= 0, N_ELEMENTS(suffixes)-1 DO BEGIN
    ;fnames_corr = [fnames_corr, prefixes[i] + suffixes[j] + '-corr_with-zCFAPAR']
    ;use coef of det instead
    fnames_corr = [fnames_corr, prefixes[i] + suffixes[j] + '-cod_with-zCFAPAR']
    fnames_p = [fnames_p, prefixes[i] + suffixes[j] + '-p_with-zCFAPAR']
    fnames_gain = [fnames_p, prefixes[i] + suffixes[j] + '-gain_with-zCFAPAR']
  ENDFOR
ENDFOR


corr_mat = FLTARR(N_ELEMENTS(fnames_corr)+1, 3)  ;overall, crop, pastures

mask = BYTARR(ns,nl)*0B
OPENU, lun, mask_dir+'\'+fn_intersection, /GET_LUN
READU, lun, mask
FREE_LUN, lun
;IF (saveDataForTest EQ 1) THEN BEGIN
;  dirT = dir + '\FOR TUKEY test'
;  fn_data_for_test = dirT + '\data_cod_best_at_pix.csv'
;  ;if present delete
;  res = FILE_SEARCH(fn_data_for_test) 
;  IF (res NE '') THEN FILE_DELETE, fn_data_for_test
;  OPENW, lunD, fn_data_for_test, /GET_LUN
;  PRINTF, lunD, 'Id,r' 
;  fn_reduced_data_for_test = dirT + '\reduced_data_cod_best_at_pix.csv'
;  ;if present delete
;  res = FILE_SEARCH(fn_reduced_data_for_test)
;  IF (res NE '') THEN FILE_DELETE, fn_reduced_data_for_test
;  OPENW, lunRD, fn_reduced_data_for_test, /GET_LUN
;  PRINTF, lunRD, 'Id,r'
;  fn_ids = dirT + '\id_cod_best_at_pix.csv'
;  res = FILE_SEARCH(fn_ids)
;  IF (res NE '') THEN FILE_DELETE, fn_ids
;  OPENW, lunI, fn_ids, /GET_LUN
;  PRINTF, lunI, 'Id,Name'
;ENDIF

ids = INDGEN(N_ELEMENTS(fnames_corr))
r2 = FLTARR(ns,nl,N_ELEMENTS(fnames_corr))
p = FLTARR(ns,nl,N_ELEMENTS(fnames_corr))
gain = FLTARR(ns,nl,N_ELEMENTS(fnames_corr))
tmp = FLTARR(ns,nl)
r2_best = FLTARR(ns,nl)*!VALUES.F_NAN
r2_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN

p_best = FLTARR(ns,nl)*!VALUES.F_NAN
gain_best = FLTARR(ns,nl)*!VALUES.F_NAN
p_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN
id_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN
id_best = FLTARR(ns,nl,2)*!VALUES.F_NAN
p_best = FLTARR(ns,nl,2)*!VALUES.F_NAN
FOR i = 0, N_ELEMENTS(fnames_corr) -1 DO BEGIN
  OPENR, lun, dir +  '\' + fnames_corr[i], /GET_LUN
  READU, lun, tmp 
  r2[*,*,i] = tmp
  FREE_LUN, lun
  OPENR, lun, dir +  '\' + fnames_p[i], /GET_LUN
  READU, lun, tmp
  p[*,*,i] = tmp
  FREE_LUN, lun
  OPENR, lun, dir +  '\' + fnames_gain[i], /GET_LUN
  READU, lun, tmp
  gain[*,*,i] = tmp
  FREE_LUN, lun
ENDFOR
FOR s = 0, ns-1 DO BEGIN
  FOR l = 0, nl-1 DO BEGIN
    IF (TOTAL(FINITE(r2[s,l,*])) GT 0) THEN BEGIN
      r2_best[s,l] = MAX(r2[s,l,*], max_sub, /NAN)
      p_best[s,l] = p[s,l,max_sub]
      gain_best[s,l] = gain[s,l,max_sub]
      id_best[s,l,0] = numerical_id[0, max_sub]
      id_best[s,l,1] = numerical_id[1,max_sub]
      ;only progress 0
      FOR pr = 0, 4 DO BEGIN
        ind = WHERE(REFORM(numerical_id[1,*]) EQ numerical_id[1,pr])
        ;extract val and spi
        valr2 = r2[s,l,ind]
        valp = p[s,l,ind]
        spi = numerical_id[0,ind]
        IF (TOTAL(FINITE(valr2)) GT 0) THEN BEGIN
          r2_best_prog[s,l,pr] = MAX(valr2, max_sub, /NAN)
          p_best_prog[s,l,pr] = valp[max_sub]
          id_best_prog[s,l,pr] = spi[max_sub]
        ENDIF ELSE BEGIN
          ;IF (TOTAL(FINITE(r2[s,l,*])) GT 0) THEN STOP
          r2_best_prog[s,l,p] = !VALUES.F_NAN
          id_best_prog[s,l,pr] = !VALUES.F_NAN
        ENDELSE   
      ENDFOR
    ENDIF ELSE BEGIN
      r2_best[s,l] = !VALUES.F_NAN
      id_best[s,l,*] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
ENDFOR
;save files
OPENW, lun, dir + '\AAA_cod_of_best', /GET_LUN
WRITEU, lun, r2_best
FREE_LUN, lun
OPENW, lun,dir + '\AAA_cod_of_best.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lun, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lun, 'bands = 1'
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = 4'
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'byte order = 0'
FREE_LUN, lun

OPENW, lun, dir + '\AAA_p_of_best', /GET_LUN
WRITEU, lun, p_best
FREE_LUN, lun
OPENW, lun,dir + '\AAA_p_of_best.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lun, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lun, 'bands = 1'
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = 4'
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'byte order = 0'
FREE_LUN, lun

OPENW, lun, dir + '\AAA_gain_of_best', /GET_LUN
WRITEU, lun, gain_best
FREE_LUN, lun
OPENW, lun,dir + '\AAA_gain_of_best.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lun, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lun, 'bands = 1'
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = 4'
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'byte order = 0'
FREE_LUN, lun

OPENW, lun, dir + '\AAA_id_of_best', /GET_LUN
WRITEU, lun, id_best
FREE_LUN, lun
OPENW, lun,dir + '\AAA_id_of_best.hdr', /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'samples = ' + STRTRIM(ns,2);1867'
PRINTF, lun, 'lines = ' + STRTRIM(nl,2);348'
PRINTF, lun, 'bands = 2'
PRINTF, lun, 'header offset = 0'
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'data type = 4'
PRINTF, lun, 'interleave = bsq'
PRINTF, lun, 'byte order = 0'
FREE_LUN, lun

;SAVE FILE at progress

;...
ind_all = WHERE(FINITE(r2_best) AND (mask GT 0), count_can)

ind_rangelnd = WHERE(FINITE(r2_best) AND (mask EQ 1))
ind_croplnd =   WHERE(FINITE(r2_best) AND (mask EQ 2))
;some stats
PRINT, 'Best R2 selection'
PRINT, 'Mean R2 (overall) = ', +STRTRIM(MEAN(r2_best[ind_all]),2)
PRINT, 'Mean R2 (crops) = ', +STRTRIM(MEAN(r2_best[ind_croplnd]),2)
PRINT, 'Mean R2 (pastures) = ', +STRTRIM(MEAN(r2_best[ind_rangelnd]),2)
PRINT, ''
ind_sig_all = WHERE(p_best[ind_all] LT 0.05, count_sig_all)
PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(N_ELEMENTS(ind_all)),2)
PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(r2_best[ind_all[ind_sig_all]]),2)

;print occurrence (%) of accum perio as a function of progress for those significant
prog = REFORM(id_best[*,*,1])
accup = REFORM(id_best[*,*,0])
h2d = HIST_2D(prog[ind_all[ind_sig_all]], accup[ind_all[ind_sig_all]]/3.0, bin1=25.0, bin2=1.0, MIN1 = 0.0, MAX1=100.0, MIN2=1.0, MAX2=6.0)
h2db = FLOAT(h2d)
PRINT, 'Best R2 selection, % of selected accumulation period and progress'
PRINT, '    ',0,'  ',25,'   ',50,'  ',75,'  ',100
FOR i = 0, 5 DO h2db[*,i] = h2db[*,i]/FLOAT(TOTAL(h2d,2))*100
PRINT, h2db

;plot istogram of best
ind_all = WHERE(FINITE(r2_best) AND (mask GT 0), count_can)
pdf= HISTOGRAM(r2_best[ind_all], BINSIZE=0.05, LOCATIONS=xbin, MIN = 0.0, MAX = 1.0)
h0 = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[-1.0,1,0], TITLE='pixel-best r2', XTITLE='R2 with zCFAPAR', $
  YTITLE=' Frequency ', AXIS_STYLE=1, COLOR='black', NAME = fn_best)
ind = WHERE(xbin GE 0.5)
PRINT, 'Fraction with R2 GT 0.5 =', TOTAL(pdf[ind[0]:*])/TOTAL(pdf)
IF (saveDataForTest EQ 1) THEN BEGIN
  FREE_LUN, lunD
  FREE_LUN, lunRD
  FREE_LUN, lunI
ENDIF

PRINT, ''
PRINT, 'Best R2 by progress selection'
;add also a best at progress that selct an old info it had a better R2
FOR pr = 0, 4 DO BEGIN
  PRINT, '###Progress ' + STRTRIM(pr*25,2)
  valr2 = r2_best_prog[*,*,pr]
  ind_all = WHERE(FINITE(valr2) AND (mask GT 0), count_can)
  ind_rangelnd = WHERE(FINITE(valr2) AND (mask EQ 1))
  ind_croplnd =   WHERE(FINITE(valr2) AND (mask EQ 2))
  valp = p_best_prog[*,*,pr]
  val_accump = id_best_prog[*,*,pr]
  ;find best up to now, not matter which progress
  IF (pr GT 0) THEN BEGIN
    valr2up2prog = r2_best_prog[*,*,0:pr]
    valpup2prog = p_best_prog[*,*,0:pr]
    val_accumup2prog = id_best_prog[*,*,0:pr]
    valr2best_up_to_prog = MAX(valr2up2prog, indsubMax, DIMENSION = 3, /NAN)
    valpbest_up_to_prog = valpup2prog[indsubMax]
    valAccuBest_up_to_prog = val_accumup2prog[indsubMax]
    ;res = ARRAY_INDICES(valr2up2prog, indsubMax)
    valProg_up_to_prog = INTARR(ns, nl, pr+1)
    FOR k = 0, pr DO valProg_up_to_prog[*,*,k] = k * 25
    valProgressBest_up_to_prog = valProg_up_to_prog[indsubMax]
;    FOR S = 0, NS -1 DO BEGIN
;      FOR L = 0, NL -1 DO BEGIN
;        VALPBEST_UP_TO_PROG[S,L] = VALPUP2PROG[S,L,INDSUBMAX[S,L]]
;      ENDFOR
;    ENDFOR
  ENDIF 
  PRINT, '***Stats of the selected progress
  PRINT, 'Mean R2 (overall) = ', +STRTRIM(MEAN(valr2[ind_all]),2)
  PRINT, 'Mean R2 (crops) = ', +STRTRIM(MEAN(valr2[ind_croplnd]),2)
  PRINT, 'Mean R2 (pastures) = ', +STRTRIM(MEAN(valr2[ind_rangelnd]),2)
  PRINT, ''
  ind_sig_all = WHERE(valp[ind_all] LT 0.05, count_sig_all)
  PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(N_ELEMENTS(ind_all))*100,2)
  PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(valr2[ind_all[ind_sig_all]]),2)
  PRINT, 'Accumulation period selcted'
  ;SPI accumulation period by progress (for the sig)
  h = HISTOGRAM(val_accump[ind_all[ind_sig_all]]/3.0, BINSIZE = 1, MIN=1, MAX=6)
  PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100
  
 
  ;save R2 and p by progress
  OPENW, lunp, dir + '\' + 'best_cod_at_prog' + STRTRIM(pr*25,2) + '.img', /GET_LUN
  WRITEU, lunp, valr2
  FREE_LUN, lunp
  res = write_envi_hdr('best_cod_at_prog' + STRTRIM(pr*25,2) + '.hdr', $
    ns, nl, 4, DIR = dir)
  OPENW, lunp, dir + '\' + 'best_p_at_prog' + STRTRIM(pr*25,2) + '.img', /GET_LUN
  WRITEU, lunp, valp
  FREE_LUN, lunp
  res = write_envi_hdr('best_p_at_prog' + STRTRIM(pr*25,2) + '.hdr', $
    ns, nl, 4, DIR = dir)
  
  IF (pr GT 0) THEN BEGIN
    PRINT, '*********Stats of the best indicator up to current progress
    PRINT, 'Mean R2 (overall) = ', +STRTRIM(MEAN(valr2best_up_to_prog[ind_all]),2)
    PRINT, 'Mean R2 (crops) = ', +STRTRIM(MEAN(valr2best_up_to_prog[ind_croplnd]),2)
    PRINT, 'Mean R2 (pastures) = ', +STRTRIM(MEAN(valr2best_up_to_prog[ind_rangelnd]),2)
    PRINT, ''
    ind_sig_all = WHERE(valpbest_up_to_prog[ind_all] LT 0.05, count_sig_all)
    PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(N_ELEMENTS(ind_all))*100,2)
    PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(valr2best_up_to_prog[ind_all[ind_sig_all]]),2)
    PRINT, 'Accumulation period selcted'
    ;SPI accumulation period by progress (for the sig)
    h = HISTOGRAM(valAccuBest_up_to_prog[ind_all[ind_sig_all]]/3.0, BINSIZE = 1, MIN=1, MAX=6)
    PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100
    PRINT, 'Progress selcted'
    ;SPI accumulation period by progress (for the sig)
    h = HISTOGRAM(valProgressBest_up_to_prog[ind_all[ind_sig_all]], BINSIZE = 25, MIN=0, MAX=100)
    PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100
    ;save R2 and p by up to progress
    OPENW, lunp, dir + '\' + 'best_cod_up_to_prog' + STRTRIM(pr*25,2) + '.img', /GET_LUN
    WRITEU, lunp, valr2best_up_to_prog
    FREE_LUN, lunp
    res = write_envi_hdr('best_cod_up_to_prog' + STRTRIM(pr*25,2) + '.hdr', $
      ns, nl, 4, DIR = dir)
    OPENW, lunp, dir + '\' + 'best_p_up_to_prog' + STRTRIM(pr*25,2) + '.img', /GET_LUN
    WRITEU, lunp, valpbest_up_to_prog
    FREE_LUN, lunp
    res = write_envi_hdr('best_p_up_to_prog' + STRTRIM(pr*25,2) + '.hdr', $
      ns, nl, 4, DIR = dir)
  ENDIF  

ENDFOR
        
 

;IF (saveDataForTest EQ 1) THEN BEGIN
;  FREE_LUN, lunD
;  FREE_LUN, lunRD
;  FREE_LUN, lunI
;ENDIF
END