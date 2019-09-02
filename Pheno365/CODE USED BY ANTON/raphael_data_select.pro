FUNCTION Raphael_data_select, x, u, index, cropType

;BE CARE THI IS NOT USED (CAN'T BE USED)  BY Raphael_pheno_compare_S2andS1

;get the number of failed retrieval
tmp =  WHERE((x.index EQ index) AND (x.crop_type EQ cropType) AND ((x.SOS20 EQ -9999) OR (x.EOS20 EQ -9999)), countRetrFailure)
;get the index of successful retrieval
CASE u OF
  0: BEGIN
    ind = WHERE((x.index EQ index) AND (x.crop_type EQ cropType) AND (x.SOS20 NE -9999) AND (x.EOS20 NE -9999), count)
    subTitle = 'All retrieval';, n=' +STRTRIM(N_ELEMENTS(ind),2)
    suffix = 'all_retrieval'
  END
  1: BEGIN
    ind = WHERE((x.index EQ index) AND (x.crop_type EQ cropType) AND (x.SOS20 NE -9999) AND (x.EOS20 NE -9999), count)
    ;now get their ID
    ids = x.ID[ind]
    ind2Keep = !NULL
    ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
    FOR i = 0, count-1 DO BEGIN
      indNDVI = WHERE((x.ID EQ ids[i]) AND (x.Index EQ 'NDVIm'))
      IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, ind[i]]
    ENDFOR
    ind = ind2Keep
    subTitle = 'inSeasonNDVI_Het<20%'
    suffix = 'inSeasNdviHetLT20'
  END
  2: BEGIN   
    ;here I want to remove thos with hig q retrieval and a NDVI prctHeterogObs_inSeason GT 20
    ind = WHERE((x.index EQ index) AND $
      (x.crop_type EQ cropType) AND $
      (x.SOS20 NE -9999) AND (x.EOS20 NE -9999) AND $
      (x.maxGap LT 30) AND $
      (x.p2_err LT 30) AND $
      (x.p2_err NE 0) AND $
      (x.p5_err LT 30) AND $
      (x.p5_err NE 0), count)
    ;now get their ID
    ids = x.ID[ind]
    ind2Keep = !NULL
    ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
    FOR i = 0, count-1 DO BEGIN
      indNDVI = WHERE((x.ID EQ ids[i]) AND (x.Index EQ 'NDVIm'))
      IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, ind[i]]
    ENDFOR
    ind = ind2Keep
    subTitle = 'maxGap, inSeasonNDVI_Het<20%, (p2err, p5err) <30 && !=0'
    suffix = 'highQ_retrieval_inSeasNdviHetLT20'
;    ind = WHERE(x.crop_type EQ cropType) 
;    sd = x.NDVI_SD_in_season[ind]
;    percSD = percentiles(sd,value=0.90)
;    ind = WHERE((x.index EQ index) AND $
;      (x.crop_type EQ cropType) AND $
;      (x.SOS20 NE -9999) AND $
;      (x.maxGap LT 30) AND $
;      (x.p2_err LT 30) AND $
;      (x.p2_err NE 0) AND $
;      (x.p5_err LT 30) AND $
;      (x.p5_err NE 0) AND $
;      (x.NDVI_SD_in_season LE percSD), count)
;    subTitle = 'maxGap, NDVIsd<90perc, (p2err, p5err) <30 && !=0'
;    suffix = 'highQ_retrieval_SD90perc'
  END
ENDCASE
ret = CREATE_STRUCT('indRetr', ind, 'countRetr',N_ELEMENTS(ind), 'countRetrFailure', countRetrFailure, 'subTitle', subTitle, 'suffix', suffix) 
RETURN, ret
END