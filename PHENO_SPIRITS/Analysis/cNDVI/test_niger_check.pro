PRO test_niger_check
 fn_gaul0 = 'F:\Pheno_year_by_year\References\gaul0_asap.img'
 fn_crop_afi ='F:\Pheno_year_by_year\References\mask_crop_afi_masked.img'
 fname_cndvi = 'F:\CumulatedNDVIfromX\AAAA_cNDVI_by_year_2003-2016.img'
 
 cndvi = ReadEnviWithHdr(fname_cndvi)
 cndvi = cndvi[20100:22000,5700:7200,*]
 gaul0 = ReadEnviWithHdr(fn_gaul0)
 gaul0 = gaul0[20100:22000,5700:7200]
 crop_afi = ReadEnviWithHdr(fn_crop_afi)
 crop_afi = crop_afi[20100:22000,5700:7200]
 indTraget = WHERE((gaul0 EQ 80) AND (crop_afi LE 200) AND (crop_afi GE 1))
 afiTarget = crop_afi[indTraget]*0.5
 
 FOR i = 0, N_ELEMENTS(cndvi[0,0,*])-1 DO BEGIN
  ;ind of niger, with some crop
  
  cndvi_year = REFORM(cndvi[*,*,i]) 
  cndvi_year = FLOAT(cndvi_year[indTraget])
  indNAN = WHERE(cndvi_year LE -32000, count) 
  IF (count GT 0) THEN cndvi_year[indNAN] = !VALUES.F_NAN
  PRINT, i, ', Year ' + STRTRIM(2003+i,2), ', Mean CNDVI = ' + STRTRIM(TOTAL(cndvi_year * afiTarget, /NAN)/TOTAL(afiTarget),2)
  
 ENDFOR
 

END