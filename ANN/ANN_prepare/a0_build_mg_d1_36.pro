PRO run_s1
dir_pheno = 'E:\WA MODIS\Phenology\SPRITS-ASAP\Dek1-36'
sos_fn = 'OF_LTAs1_1_36.img
tom_fn = 'OF_LTAm1_1_36.img'
mg_fn = 'OF_LTAmg1_1_36.img'
ret  = A0_build_MG_d1_36(dir_pheno, sos_fn, tom_fn, mg_fn)
END

PRO run_s2
  dir_pheno = 'E:\WA MODIS\Phenology\SPRITS-ASAP\Dek1-36'
  sos_fn = 'OF_LTAs2_1_36.img
  tom_fn = 'OF_LTAm2_1_36.img'
  mg_fn = 'OF_LTAmg2_1_36.img'
  ret  = A0_build_MG_d1_36(dir_pheno, sos_fn, tom_fn, mg_fn)
END

FUNCTION A0_build_MG_d1_36, dir_pheno, sos_fn, tom_fn, mg_fn
; Build the MG (mid-growth, middle point between SOS and TOM) 

;open the images
s = load_envi_bsq_file(dir_pheno + '\' + sos_fn)
m = load_envi_bsq_file(dir_pheno + '\' + tom_fn)
t = s
indFin = WHERE(FINITE(s))
indValid = WHERE((s[indFin] GE 1) AND (s[indFin] LE 36), countValid)
indValid = indFin[indValid]

indMgtS = WHERE(m[indValid] GT s[indValid], countMgtS)
indMgtS = indValid[indMgtS]
indSgtM = WHERE(s[indValid] GT m[indValid], countSgtM)
indSgtM = indValid[indSgtM]
IF ((countMgtS + countSgtM) NE countValid) THEN STOP 

;avg for the case m > s
IF (countMgtS GT 0) THEN t[indMgtS] = ROUND((s[indMgtS] + m[indMgtS])/2.0) 
;avg for the case s > m
IF (countSgtM GT 0) THEN BEGIN
  length = m[indSgtM] + (36 - s[indSgtM])
  tt = s[indSgtM] + ROUND(length / 2.0)
  ;now tt may be longer than 36
  ind = WHERE(tt GT 36, count)
  IF (count GT 0) THEN tt[ind] = tt[ind] - 36
  t[indSgtM] = tt
ENDIF

ret = write_envi_img(t, dir_pheno + '\' + mg_fn)
sz = SIZE(t)
hdr_fn = FILE_BASENAME(mg_fn, '.img') +'.hdr'
ret = write_envi_hdr(dir_pheno + '\' + hdr_fn, sz[1], sz[2], sz[3])
END

