PRO bbb_find_best_at_pixel_linear_reg
;13 apr 2016, modified to compute r2 instead of r, indicator before sos dropped

;mask_dir = '\\ies\d5\asap\TEST_PREDICTORS\Masks\'
;mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
;fn_intersection = 'eco_crop_pasture_res_CHIRPS.img' ;pasrure = 1, crop = 2
fn_afi_crops = '\\ies\d5\asap\TEST_PREDICTORS\Masks\mask_crop_afi_asap2_0.img'
fn_afi_range = '\\ies\d5\asap\TEST_PREDICTORS\Masks\mask_rangeland_afi_asap2_0.img'
afi_thresold = 100 ; remember tha afi is 0-200 values, so 100 is 50%
dir = '\\ies\d5\asap\TEST_PREDICTORS\YX_correlation'
dir_out = '\\ies\d5\asap\TEST_PREDICTORS\YX_correlation\BEST'
FILE_MKDIR, dir_out
ns = 8178
nl = 8234

saveDataForTest = 0
out_of = 1

;make_file_names
vars = ['SPI1','SPI3','zSWI015','zSWI040','zSWI060','zBECsm']
var_num_id = INDGEN(N_ELEMENTS(vars))+1 
suffixes = ['_atProg0','_atProg25','_atProg50','_atProg75','_atProg100']
prog_num_id = INDGEN(N_ELEMENTS(suffixes))*25
WRITE_CSV, dir_out + '\id_book.csv', var_num_id, vars, prog_num_id, STRING([0,25,50,75,100])+'%', HEADER = ['Var_id', 'Var_name', 'Prog_id', 'Prov_val', ] 

;we work on season1 and 2 separately
FOR seas = 0, 1 DO BEGIN
  seasonPref = 'season' + STRTRIM(seas+1,2)
  fnames_cod = !NULL
  fnames_sig = !NULL
  fnames_gain = !NULL
  var_ids = !NULL
  prog_ids = !NULL
  ;amke the list of filenames to be used
  FOR i= 0, N_ELEMENTS(vars)-1 DO BEGIN
    FOR j= 0, N_ELEMENTS(suffixes)-1 DO BEGIN
      fnames_cod = [fnames_corr, vars[i] + '\' + seasonPref + vars[i] + suffixes[j] + '_bil-cod_with-zcNDVI']
      fnames_sig = [fnames_p, vars[i] + '\' + seasonPref + vars[i] + suffixes[j] + '_bil-sig_with-zcNDVI']
      fnames_gain = [fnames_p, vars[i] + '\' + seasonPref + vars[i] + suffixes[j] + '_bil-gain_with-zcNDVI']
      var_ids = [var_ids, var_num_id[i]]
      prog_ids = [prog_ids, prog_num_id]
    ENDFOR
  ENDFOR
  
  map_info = read_info('map info', fnames_cod[i])
  
  ;build teh mask. binary. rangeland = 1, crop = 2
  afiCrops = ReadEnviWithHdr(fn_afi_crops)
  afiRange = ReadEnviWithHdr(fn_afi_range)
  mask = afiRange * 0B  ;pasture = 1, crop = 2
  ;thrshold the afis
  indCrop = WHERE(afiCrops GT afi_thresold, countCrop)
  IF (countCrop GT 0) then mask[indCrop] = 2
  indRang = WHERE(afiRange GT afi_thresold, countRang)
  IF (countRang GT 0) then mask[indRang] = 1
  afiCrops = 0 & afiRange = 0 & indCrop = 0 & indRang = 0
  
  ;prepare variables
  ;corr_mat = FLTARR(N_ELEMENTS(fnames_corr)+1, 3)  ;overall, crop, pastures
  ids = INDGEN(N_ELEMENTS(fnames_cod))
  r2 = FLTARR(ns,nl,N_ELEMENTS(fnames_cod))
  sig = FLTARR(ns,nl,N_ELEMENTS(fnames_cod))
  gain = FLTARR(ns,nl,N_ELEMENTS(fnames_cod))
  r2_best = FLTARR(ns,nl)*!VALUES.F_NAN
  sig_best = FLTARR(ns,nl)*!VALUES.F_NAN
  gain_best = FLTARR(ns,nl)*!VALUES.F_NAN
  id_best = FLTARR(ns,nl,2)*!VALUES.F_NAN     ;2 for range and crop? see later and then remove this comment
  sig_best = FLTARR(ns,nl,2)*!VALUES.F_NAN
  
  r2_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN
  sig_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN
  id_best_prog = FLTARR(ns,nl,5)*!VALUES.F_NAN
  id_best_var = FLTARR(ns,nl,5)*!VALUES.F_NAN

  
  ;read files
  FOR i = 0, N_ELEMENTS(fnames_corr) -1 DO BEGIN
    r2[*,*,i] =   ReadEnviWithHdr(dir +  '\' + fnames_cod[i])
    sig[*,*,i] =  ReadEnviWithHdr(dir +  '\' + fnames_sig[i])
    gain[*,*,i] = ReadEnviWithHdr(dir +  '\' + fnames_gain[i])
  ENDFOR
  
  FOR s = 0, ns-1 DO BEGIN
    FOR l = 0, nl-1 DO BEGIN
      IF (TOTAL(FINITE(r2[s,l,*])) GT 0) THEN BEGIN
        r2_best[s,l] = MAX(r2[s,l,*], max_sub, /NAN)
        sig_best[s,l] = sig[s,l,max_sub]
        gain_best[s,l] = gain[s,l,max_sub]
        id_best[s,l,0] = var_ids[max_sub]
        id_best[s,l,1] = prog_ids[max_sub]
        ;only progress 0
        FOR pr = 0, 4 DO BEGIN
          ind = WHERE(REFORM(prog_ids[*]) EQ prog_ids[pr])
          ;extract values for that particulat progress (all variables at that progress)
          r2_pr = r2[s,l,ind]
          sig_pr = sig[s,l,ind]
          var_pr = var_ids[ind]
          prog_pr = prog_ids[ind]
          IF (TOTAL(FINITE(r2_pr)) GT 0) THEN BEGIN
            r2_best_prog[s,l,pr] = MAX(r2_pr, max_sub, /NAN)
            sig_best_prog [s,l,pr] = sig_pr[max_sub]
            id_best_prog[s,l,pr] = prog_pr[max_sub]
            id_best_var[s,l,pr] =  var_pr[max_sub]
          ENDIF ELSE BEGIN
            ;IF (TOTAL(FINITE(r2[s,l,*])) GT 0) THEN STOP
            r2_best_prog[s,l,pr] = !VALUES.F_NAN
            id_best_prog[s,l,pr] = !VALUES.F_NAN
          ENDELSE
        ENDFOR
      ENDIF ELSE BEGIN
        r2_best[s,l] = !VALUES.F_NAN
        id_best[s,l,*] = !VALUES.F_NAN
      ENDELSE
    ENDFOR
  ENDFOR
  
  ;save files (overall best)
  res = write_envi_img(r2_best, dir_out + '\AAA_cod_of_best') 
  res = write_envi_hdr(dir_out + '\AAA_cod_of_best', ns, nl, 4, MAPINFO=map_info)
  
  res = write_envi_img(sig_best, dir_out + '\AAA_sig_of_best')
  res = write_envi_hdr(dir_out + '\AAA_sig_of_best', ns, nl, 4, MAPINFO=map_info)
  
  res = write_envi_img(gain_best, dir_out + '\AAA_gain_of_best')
  res = write_envi_hdr(dir_out + '\AAA_gain_of_best', ns, nl, 4, MAPINFO=map_info)
  
  res = write_envi_img(id_best, dir_out + '\AAA_id_of_best')
  res = write_envi_hdr(dir_out + '\AAA_id_of_best', ns, nl, 4, NBANDS=2, BAND_NAMES=['var_id','prog_id'], MAPINFO=map_info)
  
  ;make some stats
  ind_all =      WHERE(FINITE(r2_best) AND (mask GT 0), count_all)
  ind_rangelnd = WHERE(FINITE(r2_best) AND (mask EQ 1))
  ind_croplnd =  WHERE(FINITE(r2_best) AND (mask EQ 2))
  PRINT, 'Best R2 selection'
  PRINT, 'Mean R2 (overall) = ', +  STRTRIM(MEAN(r2_best[ind_all]),2)
  PRINT, 'Mean R2 (crops) = ', +    STRTRIM(MEAN(r2_best[ind_croplnd]),2)
  PRINT, 'Mean R2 (rangeland) = ', + STRTRIM(MEAN(r2_best[ind_rangelnd]),2)
  PRINT, ''
  ind_sig_all = WHERE(sig_best[ind_all] EQ 1, count_sig_all)
  PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(count_all),2)
  PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(r2_best[ind_all[ind_sig_all]]),2)
  
  ;print occurrence (%) of var as a function of progress for those significant
  prog = REFORM(id_best[*,*,1])
  varSelected = REFORM(id_best[*,*,0])
  h2d = HIST_2D(prog[ind_all[ind_sig_all]], varSelected[ind_all[ind_sig_all]], bin1=25.0, bin2=1.0, MIN1 = 0.0, MAX1=100.0, MIN2=1.0, MAX2=N_ELEMENTS(var_num_id_))
  h2db = FLOAT(h2d)
  PRINT, 'Best R2 selection, % of selected by var and progress'
  PRINT, '    ',0,'  ',25,'   ',50,'  ',75,'  ',100
  FOR i = 0, 5 DO h2db[*,i] = h2db[*,i]/FLOAT(TOTAL(h2d,2))*100
  PRINT, h2db ;.. to be checked...
  
  ;plot histogram of best
  pdf= HISTOGRAM(r2_best[ind_all], BINSIZE=0.05, LOCATIONS=xbin, MIN = 0.0, MAX = 1.0)
  h0 = PLOT(xbin, pdf/TOTAL(pdf)*100, XRANGE=[-1.0,1,0], TITLE='pixel-best R2', XTITLE='R2 with zCNDVI', $
    YTITLE=' Frequency ', AXIS_STYLE=1, COLOR='black', NAME = fn_best)
  ind = WHERE(xbin GE 0.5)
  PRINT, 'Fraction with R2 GT 0.5 =', TOTAL(pdf[ind[0]:*])/TOTAL(pdf)
  
  PRINT, ''
  PRINT, 'Best R2 by progress selection'
  ;add also a best at progress that selct an old info it had a better R       2???
  FOR pr = 0, 4 DO BEGIN
    r2_pr = r2_best_prog[*,*,pr]
    ind_all = WHERE(FINITE(r2_pr) AND (mask GT 0), count_all)
    ind_rangelnd = WHERE(FINITE(r2_pr) AND (mask EQ 1))
    ind_croplnd =   WHERE(FINITE(r2_pr) AND (mask EQ 2))
    var_pr = id_best_prog[*,*,pr]             
    sig_pr = sig_best_prog[*,*,pr]
    ;after progress 0 find best up to the current progress (i.e. 0 progress to current progress). 
    IF (pr GT 0) THEN BEGIN
      r2_up2pr = r2_best_prog[*,*,0:pr]
      sig_up2pr = p_best_prog[*,*,0:pr]
      var_up2pr = id_best_prog[*,*,0:pr]
      r2best_up2pr = MAX(r2_up2pr, indsubMax, DIMENSION = 3, /NAN)
      sigBest_up2pr = sig_up2pr[indsubMax]
      varBest_up2pr = var_up2pr[indsubMax]
      prog_up2pr = INTARR(ns, nl, pr+1)
      FOR k = 0, pr DO prog_up2pr[*,*,k] = k * 25
      progBest_up2pr = progBest_up2pr[indsubMax]
    ENDIF
    PRINT, '***Best var, stats of the single progress ' + STRTRIM(prog_num_id[pr],2) + ' %'
    PRINT, 'Mean R2 (overall) = ', +  STRTRIM(MEAN(r2_pr[ind_all]),2)
    PRINT, 'Mean R2 (crops) = ', +    STRTRIM(MEAN(r2_pr[ind_croplnd]),2)
    PRINT, 'Mean R2 (pastures) = ', + STRTRIM(MEAN(r2_pr[ind_rangelnd]),2)
    PRINT, ''
    ind_sig_all = WHERE(sig_pr[ind_all] EQ 1, count_sig_all)
    PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(count_all)*100,2)
    PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(r2_pr[ind_all[ind_sig_all]]),2)
    PRINT, 'Variable selected (' + STRJOIN(vars, ',') + ')' 
    ;SPI accumulation period by progress (for the sig)
    h = HISTOGRAM(var_pr[ind_all[ind_sig_all]]/3.0, BINSIZE = 1, MIN=1, MAX=N_ELEMENTS(var_num_id_))
    PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100


    ;save R2 and sig by progress
    res = write_envi_img(r2_pr, dir_out + '\best_cod_at_prog' + STRTRIM(pr*25,2) + '.img')
    res = write_envi_hdr(dir_out + 'best_cod_at_prog' + STRTRIM(pr*25,2) + '.hdr', ns, nl, 4, MAPINFO=map_info)

    res = write_envi_img(sig_pr, dir_out + '\best_sig_at_prog' + STRTRIM(pr*25,2) + '.img')
    res = write_envi_hdr(dir_out + 'best_sig_at_prog' + STRTRIM(pr*25,2) + '.hdr', ns, nl, 4, MAPINFO=map_info)
    
    IF (pr GT 0) THEN BEGIN
      PRINT, '*********Stats of the best indicator up to progress ' + STRTRIM(prog_num_id[pr],2) + ' %'
      ind_all = WHERE(FINITE(r2best_up2pr) AND (mask GT 0), count_all)
      ind_rangelnd = WHERE(FINITE(r2best_up2pr) AND (mask EQ 1))
      ind_croplnd =   WHERE(FINITE(r2best_up2pr) AND (mask EQ 2))
      PRINT, 'Mean R2 (overall) = ', +STRTRIM(MEAN(r2best_up2pr[ind_all]),2)
      PRINT, 'Mean R2 (crops) = ', +STRTRIM(MEAN(r2best_up2pr[ind_croplnd]),2)
      PRINT, 'Mean R2 (pastures) = ', +STRTRIM(MEAN(r2best_up2pr[ind_rangelnd]),2)
      PRINT, ''
      ind_sig_all = WHERE(sigBest_up2pr[ind_all] EQ 1, count_sig_all)
      PRINT, '% sig-p<0.05 (overall) = ', STRTRIM(count_sig_all/FLOAT(count_all)*100,2)
      PRINT, 'Mean R2 of significant (overall) = ', +STRTRIM(MEAN(r2best_up2pr[ind_all[ind_sig_all]]),2)
      PRINT, 'Variable selected (' + STRJOIN(vars, ',') + ')' 
      ;SPI accumulation period by progress (for the sig)
      h = HISTOGRAM(varBest_up2pr[ind_all[ind_sig_all]]/3.0, BINSIZE = 1, MIN=1, MAX=6)
      PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100
      PRINT, 'Progress selected'
      ;SPI accumulation period by progress (for the sig)
      h = HISTOGRAM(progBest_up2pr[ind_all[ind_sig_all]], BINSIZE = 25, MIN=0, MAX=100)
      PRINT, FLOAT(h)/ TOTAL(FLOAT(h)) * 100
      ;save R2 and sig by up to progress
      res = write_envi_img(r2best_up2pr, dir_out + '\best_cod_up_to_prog' + STRTRIM(pr*25,2) + '.img')
      res = write_envi_hdr(dir_out + 'best_cod_up_to_prog' + STRTRIM(pr*25,2) + '.hdr', ns, nl, 4, MAPINFO=map_info)

      res = write_envi_img(sigBest_up2pr, dir_out + '\best_sig_up_to_prog' + STRTRIM(pr*25,2) + '.img')
      res = write_envi_hdr(dir_out + 'best_sig_up_to_prog' + STRTRIM(pr*25,2) + '.hdr', ns, nl, 4, MAPINFO=map_info)
    ENDIF
  ENDFOR
ENDFOR ; season seas
END