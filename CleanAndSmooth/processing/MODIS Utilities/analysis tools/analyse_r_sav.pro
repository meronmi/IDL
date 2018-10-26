PRO analyse_r_sav
RESTORE, 'K:\ILRI\Comparison results\aggregate files\Results\r_glob.sav'
;r2_gx = FLTARR(6,4)  ;6 archives, 4 different r2 (r2, r2cv, r2cv_wd, r2_cv_wd_ws
;r2_xy = FLTARR(6,6,4) ;as above but 6x6

rname = ['R2','R2cv','R2cv_wd','R2cv_wd_ws']
paired_ptest = FLTARR(6,4) ;results of the p test
ptest = FLTARR(6,4)
FOR k = 0, 0  DO BEGIN
  PRINT, '**********************************'
  PRINT, rname[k]
  PRINT, '      ' , arcs
  PRINT, '      ', r2_gx[*,k]
  PRINT, '**********************************'
  FOR j = 0, 5 DO BEGIN
    PRINT, arcs[j], REFORM(r2_xy[*,j,k])
    ;test the r GIMMS-MODIS_A_NASA aginst all others
    paired_ptest[j,k] = paired_test(SQRT(r2_gx[5,k]), SQRT(r2_gx[j,k]), SQRT(r2_xy[5,j,k]), 1512, 1512) ;paried test(r_xy, r_xz, r_yz, n, n)
    ptest[j,k] = RTEST(SQRT(r2_gx[5,k]), SQRT(r2_gx[j,k]), 1512, 1512)
  ENDFOR
ENDFOR

 PRINT, 'P against GIMMS-MODIS_A-NASA**********************************'
FOR j = 0, 5 DO BEGIN
  PRINT, arcs[j],   paired_ptest[j,0], ptest[j,0], SQRT(r2_gx[5,k]), SQRT(r2_gx[j,k]), SQRT(r2_xy[5,j,k])
ENDFOR

END