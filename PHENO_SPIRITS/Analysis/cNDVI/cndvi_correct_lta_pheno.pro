PRO cNDVI_correct_lta_pheno

;this is to align the cNDVI to calendar year

;cNDVI is computed with SPIRITS isng LTA pheno
;with the notation
;pheno 1-36 refres to YYYY-1
;pheno 37-72 refers to YYYY
;pheno 73-108 refers to YYYY+1
;for the yeay YYYY I will get a cNDVI for both s1 and s2 and I have the following cases (TOM is time of max):
;1 EOS is in YYYY+1 (73-108), TOM is in YYYY (37-72)  -> assign to CNDV_YYYY-1 to YYYY
;2 all other cases -> assign to cNDVI_YYYY to YYYY (for instance EOS and TOM are in YYYY (37-72)  -> assign to cNDVI_YYYY to YYYY)

; instead of the calendar year I would like to assign to YYYY+1 also those ending in December
;1 EOS is in YYYY+1 (73-108), TOM is in YYYY (37-72)  -> assign to CNDV_YYYY-1 to YYYY
;2 EOS is in December (70,71,72), thus TOM is in YYYY (37-72) -> assign to CNDV_YYYY to YYYY+1
;3 all other cases, do nothing

;this apply to both s1 and s2
;to get the overall cumulative value for the year I then have to sum the corrected s1 and s2



;****************************************************************************************************************
gain_out = 10
nanval = -32000/gain_out

;set the data to be used
nyear = 14 ;(2003 to 20016)
dirsep = '\'
pheno_dir = 'X:\Active Projects\FAO SOFI\ASAP20data\Phenology'
pheno_base_name = 'pheno'
cndvi_dir = 'X:\Active Projects\FAO SOFI\ASAP20data\CumulatedNDVI'
cndvi_base_name = 'cNDVI_phenoLTA' 


;get ngspy
kk = ReadEnviWithHdr(pheno_dir+dirsep+pheno_base_name+'KK0.img')
ind1 = WHERE((kk GE 100) AND (kk LT 200))
ind2 = WHERE((kk GE 200) AND (kk LT 250))
sz = SIZE(kk)
kk = 0
ns = sz[1]
nl = sz[2]
ngspy = BYTARR(ns,nl) * 0 
ngspy[ind1] = 1
ngspy[ind2] = 2
ind1 = 0
ind2 = 0

;get eos and TOM
eos1 = BYTARR(ns,nl)
tom1 = BYTARR(ns,nl)
eos2 = eos1
tom2 = tom2
eos1  = ReadEnviWithHdr(pheno_dir+dirsep+pheno_base_name+'e1.img')
eos2  = ReadEnviWithHdr(pheno_dir+dirsep+pheno_base_name+'e2.img')
tom1  = ReadEnviWithHdr(pheno_dir+dirsep+pheno_base_name+'m1.img')
tom2  = ReadEnviWithHdr(pheno_dir+dirsep+pheno_base_name+'m2.img')

;commented here because too much mem
;;get cndvi form spirits years 2002 - 2016, n = 15
;n_sp = 15 
;frst_year = 2002
;cndvi_sp = FLTARR(ns,nl,n_sp,2) * !VALUES.F_NAN
;FOR i = 0, n_sp-1 DO BEGIN
;  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year,2) + '01' + 's1.img' 
;  cndvi_sp[*,*,i,0] =  ReadEnviWithHdr(fn)
;  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year,2) + '01' + 's2.img'
;  cndvi_sp[*,*,i,1] =  ReadEnviWithHdr(fn)
;ENDFOR

;prepare output 
mapinfo = read_info('map info', pheno_dir+dirsep+pheno_base_name+'e1.hdr')
;indNoSeas2D = WHERE(ngspy EQ 0, countNoSeas)


;do the work:
; instead of the calendar year I would like to assign to YYYY+1 also those ending in December
;1 EOS is in YYYY+1 (73-108), TOM is in YYYY (37-72)  -> assign to CNDV_YYYY-1 to YYYY
;2 EOS is in December (70,71,72), thus TOM is in YYYY (37-72) -> assign to CNDV_YYYY to YYYY+1
;3 all other cases, do nothing
frst_year_cndvi = 2002
frst_year_out = 2003
FOR y = 0, nyear -1 DO BEGIN
  cndvi_s1s2 = FLTARR(ns,nl)
  ;y = 0 is 2003, note that  cndvi_sp starts the year before, so same year is y cndvi_s1s2 and  y+1 for  cndvi_sp
  ;here open the cumulative ndvi of the yeay YYYY and of year YYYY-1
  cndvi_sp = FLTARR(ns,nl,2,2) * !VALUES.F_NAN
  ;YYYY-1
  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year_cndvi+y,2) + '01' + 's1.img'
  cndvi_sp[*,*,0,0] =  ReadEnviWithHdr(fn)
  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year_cndvi+y,2) + '01' + 's2.img'
  cndvi_sp[*,*,0,1] = ReadEnviWithHdr(fn)
  ;YYYY
  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year_cndvi+y+1,2) + '01' + 's1.img'
  cndvi_sp[*,*,1,0] =  ReadEnviWithHdr(fn)
  fn = cndvi_dir + dirsep + cndvi_base_name + STRTRIM(frst_year_cndvi+y+1,2) + '01' + 's2.img'
  cndvi_sp[*,*,1,1] =  ReadEnviWithHdr(fn)
  
  FOR s = 0, 1 DO BEGIN
    tmpYYYYminus1 = REFORM(cndvi_sp[*,*,0,s])
    tmpOut = REFORM(cndvi_sp[*,*,1,s])
    indNeg = WHERE(tmpOut LT 0, countNeg)
    IF (countNeg GT 0) THEN tmpOut[indNeg] = 0
    indNeg = 0
    
    ;eos and tom of season
    ;eoss = REFORM(eos[*,*,s])
    ;toms = REFORM(tom[*,*,s])
    CASE s OF
      0:BEGIN
        ind1_2D = WHERE(((eos1 GE 73) AND (eos1 LE 108)) AND ((tom1 GE 37) AND (tom1 LE 72)), count1)
        ind2_2D = WHERE(((eos1 GE 70) AND (eos1 LE 72))  AND ((tom1 GE 37) AND (tom1 LE 72)), count2)
      END
      1:BEGIN
        ind1_2D = WHERE(((eos2 GE 73) AND (eos2 LE 108)) AND ((tom2 GE 37) AND (tom2 LE 72)), count1)
        ind2_2D = WHERE(((eos2 GE 70) AND (eos2 LE 72))  AND ((tom2 GE 37) AND (tom2 LE 72)), count2)
      END
    ENDCASE
;    ind1_2D = WHERE(((eoss GE 73) AND (eoss LE 108)) AND ((toms GE 37) AND (toms LE 72)), count1)
;    ind2_2D = WHERE(((eoss GE 70) AND (eoss LE 72))  AND ((toms GE 37) AND (toms LE 72)), count2)
;    indCheck = WHERE((toms LE 36), countCheck)
;    IF (countCheck GT 0) THEN STOP ELSE indCheck = 0
;    indCheck = WHERE((toms GE 73) AND (toms LE 108), countCheck)
;    IF (countCheck GT 0) THEN STOP ELSE indCheck = 0
;    indAll_2D = WHERE(((eoss LE 69) AND                  ((toms GE 37) AND (toms LE 72))), countAll)
    ;IF (TOTAL([countNoSeas,count1,count2, countAll])) NE N_ELEMENTS(eoss) THEN STOP ;this does not hold as I have few pixels with ngspy gt 0 but eos is 252
    
    tmpOut[ind1_2D] = tmpYYYYminus1[ind1_2D]
    tmpOut[ind2_2D] = tmpYYYYminus1[ind2_2D]
    cndvi_s1s2 = TEMPORARY(cndvi_s1s2) + tmpOut  
  ENDFOR
  
  cndvi_s1s2[WHERE(ngspy EQ 0)] = nanval
  ind = WHERE(cndvi_s1s2 LT -1000000, count)
  IF (count GT 0) THEN cndvi_s1s2[ind] = nanval
  ind = 0
  
  
  res = write_envi_img(FIX(cndvi_s1s2*gain_out), cndvi_dir+dirsep+'AAA_cNDVI_by_year'+STRTRIM(frst_year_out+y,2)+'.img')
  res = write_envi_hdr(cndvi_dir+dirsep+'AAA_cNDVI_by_year'+STRTRIM(frst_year_out+y,2)+'.hdr', ns, nl, 2, INTERLEAVE='bsq', MAPINFO=mapinfo, $
        FLAGS=STRTRIM(nanval*10,2) + '=NoData', FREE_TEXT='gain = 10')
  cndvi_s1s2 = 0
  cndvi_sp = 0
  tmpOut = 0
  tmpYYYYminus1 = 0 
ENDFOR



END