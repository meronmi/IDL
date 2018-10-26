FUNCTION get_avg_max_fapar, maxFapar_fn
;open the pheno product (it is a bil file) and load it all in the pheno matrix
IF FILE_TEST(maxFapar_fn+'_avg') EQ 0 THEN BEGIN
  avg_max_fapar=avg3Dimage(maxFapar_fn, ns, nl, nb)
  IF (N_ELEMENTS(avg_max_fapar) EQ 1) THEN STOP
  OPENW, W1, maxFapar_fn+'_avg', /GET_LUN 
  WRITEU, W1, avg_max_fapar & FREE_LUN, W1 ;hdr is not written
ENDIF ELSE BEGIN
  PRINT, 'Mask already present, use the existing one'
  avg_max_fapar=fltarr(ns,nl)
  OPENR, R1, maxFapar_fn+'_avg', /GET_LUN & READU, R1, avg_max_fapar & FREE_LUN, R1
ENDELSE
;remove NaN and put -999 to avoid error using it as logical avg_max_fapar
indNaN=WHERE(FINITE(avg_max_fapar) NE 1, countNaN)
IF (countNaN NE 0) then avg_max_fapar(indNaN)=-999


RETURN, avg_max_fapar
END