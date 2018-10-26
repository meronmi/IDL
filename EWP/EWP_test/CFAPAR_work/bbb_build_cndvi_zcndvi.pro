PRO forthewe
run_BBB_build_cNDVI_zcNDVI
run_BBB_build_indicator_at_progresses

END

PRO run_BBB_build_cNDVI_zcNDVI
  fn_meta = '\\ies\d5\asap\TEST_PREDICTORS\DATA_Y\metaNDVI_200701-2017-14.mta'
  dir_out = '\\ies\d5\asap\TEST_PREDICTORS\DATA_Y\cNDVI'
  dir_prog = '\\ies\d5\asap\TEST_PREDICTORS\PROGRESS_SUBSCRIPTS'
  base_fn_prog = '200701-201714'
  offset = -0.2
  gain = 0.0048
  BBB_build_cNDVI_zcNDVI, fn_meta, dir_out, dir_prog, base_fn_prog, offset, gain
END


PRO BBB_build_cNDVI_zcNDVI, fn_meta, dir_out, dir_prog, base_fn_prog, offset, gain

info = read_meta(fn_meta)
nb_prog = FIX(read_info('bands', dir_prog + '\season1_sub_in_period_' + base_fn_prog + '_of_prog_0.hdr'))

FOR spy = 0, 1 DO BEGIN     ;loop on the two seasons
  sub_prog0 = INTARR(info.ns, info.nl, nb_prog)
  sub_prog100 = INTARR(info.ns, info.nl, nb_prog)
  
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_0', /GET_LUN
  READU, lun, sub_prog0
  FREE_LUN, lun
  OPENR, lun, dir_prog + '\season' + STRTRIM(spy+1,2) + '_sub_in_period_' + base_fn_prog + '_of_prog_100', /GET_LUN
  READU, lun, sub_prog100
  FREE_LUN, lun
  FILE_MKDIR, dir_out
  OPENW, lunW, dir_out + '\season'+STRTRIM(spy+1,2)+'CNDVI_' + base_fn_prog, /GET_LUN
  cX = FLTARR(info.ns, info.nl, nb_prog) * !VALUES.F_NAN
 
  ;use read read meta instead of assoc
;  OPENR, lunR, fapar_fn, /GET_LUN
;  line_ass = ASSOC(lunR, BYTARR(info.ns,info.nb))
 
  FOR l = 0, info.nl-1 DO BEGIN
    IF ((l MOD 100) EQ 0) THEN PRINT, 'Reading Line: ' + STRTRIM(l) 
    tmp = FLOAT(READ_LINE_META(info, l)) ;tmp = FLOAT(line_ass[l])
    ;use BOKU MODIS scaling here
    indNaN = WHERE((tmp LT 0) OR (tmp GT 250), count)
    tmp = offset + tmp * gain
    tmp[indNaN] = !VALUES.F_NAN
    FOR s = 0, info.ns-1 DO BEGIN
      ;here I am looking at sinle pixels  
;      FOR yy = 1999, 2013 DO BEGIN
;        y = yy-1999
;        IF (sub_prog0[s,l,y] EQ -999) OR (sub_prog100[s,l,y] EQ -999) OR (sub_prog100[s,l,y] GT 539) THEN BEGIN
;          cX[s,l,y] = !VALUES.F_NAN
;        ENDIF ELSE BEGIN
;          cX[s,l,y] = TOTAL(tmp[s,sub_prog0[s,l,y]:sub_prog100[s,l,y]], /NAN)
;        ENDELSE
;      ENDFOR
      FOR y = 0, nb_prog-1 DO BEGIN
        IF (sub_prog0[s,l,y] EQ -999) OR (sub_prog100[s,l,y] EQ -999) THEN BEGIN
          cX[s,l,y] = !VALUES.F_NAN
        ENDIF ELSE BEGIN
          cX[s,l,y] = TOTAL(tmp[s,sub_prog0[s,l,y]:sub_prog100[s,l,y]], /NAN)
        ENDELSE
      ENDFOR
    ENDFOR
  ENDFOR
 ; FREE_LUN, lunR
  
  WRITEU,lunW, cX
  FREE_LUN, lunW 
  res = write_envi_hdr(dir_out + '\season'+STRTRIM(spy+1,2)+'CNDVI_' + base_fn_prog + '.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bsq', $
                       MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)
   
  ;compute and save the zcX
  OPENW, lunW, dir_out + '\season'+STRTRIM(spy+1,2)+'zCNDVI_' + base_fn_prog, /GET_LUN
  zcX = FLTARR(info.ns, info.nl, nb_prog) * !VALUES.F_NAN
  FOR l = 0, info.nl-1 DO BEGIN
    FOR s = 0, info.ns-1 DO BEGIN
      IF (TOTAL(FINITE(cX[s,l,*])) GT 5) THEN BEGIN
        tmp = cX[s,l,*]
        indFin = WHERE(FINITE(tmp))
        zcX[s,l,indFin] = (tmp[indFin] - MEAN(tmp[indFin]))/ STDDEV(tmp[indFin])
      ENDIF 
    ENDFOR
  ENDFOR
  WRITEU,lunW, zcX
  FREE_LUN, lunW
  res = write_envi_hdr(dir_out + '\season'+STRTRIM(spy+1,2)+'zCNDVI_' + base_fn_prog + '.hdr' , info.ns, info.nl, 4, NBANDS=nb_prog, INTERLEAVE='bsq', $
    MAPINFO=info.geo1_val, COORDINFO=info.geo1_val)
ENDFOR  ; spy loop on 1 and 2 growing seasons per year
END