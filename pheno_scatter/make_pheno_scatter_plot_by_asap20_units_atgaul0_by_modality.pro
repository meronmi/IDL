PRO make_pheno_scatter_plot_by_asap20_units_atGaul0_by_modality
fn_units = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul0_asap.img'
fn_sos = ['\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenos1_1_36.img', $
          '\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenos2_1_36.img']
fn_eos = ['\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenoe1_1_36.img', $
          '\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenoe2_1_36.img']
fn_table = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul1_asap_analyzed_3.csv'
out_dir = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\GAUL0'
;read table with units id and admin name
table = READ_CSV(fn_table, HEADER=hdr)
table = rename_tags(table, TAG_NAMES(table), hdr) ;table.asap1_id, .ADM1_NAME, .ADM0_NAME
;read gaul raster
units =  ReadEnviWithHdr(fn_units)
;replace strange chars 
tmp = table.ADM0_NAME_SHORT 
STRREPLACE, tmp, '/', '_'
table.ADM0_NAME_SHORT = tmp

;replace strange chars
tmp = table.ADM1_NAME_SHORT
STRREPLACE, tmp, '/', '_'
table.ADM1_NAME_SHORT = tmp
tmp=0
;get size
sz = SIZE(units)

;read sos and eos
sos1 = ReadEnviWithHdr(fn_sos[0])
sos2 = ReadEnviWithHdr(fn_sos[1])
eos1 = ReadEnviWithHdr(fn_eos[0])
eos2 = ReadEnviWithHdr(fn_eos[1])

;make a list of asap countries
;delect asap country
ind = WHERE(table.asap_country EQ 't')
tmp = table.asap0_id[ind]
;get unique aspa0_id
tmp = tmp[SORT(tmp)]
uniqueASAP0ids = tmp[UNIQ(tmp)]

;loop on each asap id and produce scatterplots
;c = 1
FOR i = 0, N_ELEMENTS(uniqueASAP0ids)-1 DO BEGIN
  ;subs of season 1 and 2 (if any)
  ind1 = WHERE((units EQ uniqueASAP0ids[i]) AND (sos1 GE 1) AND (sos1 LE 36), count1)
  ind2 = WHERE((units EQ uniqueASAP0ids[i]) AND (sos2 GE 1) AND (sos2 LE 36), count2)
  indCountry = WHERE(table.asap0_id EQ uniqueASAP0ids[i])
  country_name = table.ADM0_NAME_SHORT[indCountry[0]]
  IF (count1 EQ 0) THEN BEGIN
    PRINT, 'Id ' + STRTRIM(uniqueASAP0ids[i], 2) + ', '+country_name+' has no vegetated crop (AFI GT 25%) pixels'
  ENDIF ELSE BEGIN
    ;mono and bi together
    dataSos = REFORM(sos1[ind1])
    dataEos = REFORM(eos1[ind1])
    IF (count2 GT 0) THEN BEGIN
      dataSos = [dataSos, REFORM(sos2[ind2])]
      dataEos = [dataEos, REFORM(eos2[ind2])]
    ENDIF  
    rout = [0,0]
    DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
      TITLE = 'ALL MOD, asap0ID' + STRTRIM(uniqueASAP0ids[i], 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
      FILESUFFIX = country_name + '_ID' + STRTRIM(uniqueASAP0ids[i], 2) + '_ALL_MODALITIES', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
      RANGEINLOW = 1, RANGEOUT = rout, SAVECSV = 1, TOTALN = count1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm
    
    ;new part for producing 3 graphs
    
    DELVAR, dataSos, dataEos
    ;only bimodal
    IF (count2 GT 0) THEN BEGIN
      dataSos = REFORM(sos1[ind2])
      dataEos = REFORM(eos1[ind2])
      dataSos = [dataSos, REFORM(sos2[ind2])]
      dataEos = [dataEos, REFORM(eos2[ind2])]
      DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        TITLE = 'BI-MOD, asap0ID' + STRTRIM(uniqueASAP0ids[i], 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $
        FILESUFFIX = country_name + '_ID' + STRTRIM(uniqueASAP0ids[i], 2) + '_ONLY_BIMODAL', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
        RANGE_IN = rout, SAVECSV = 1, TOTALN = count1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm
      DELVAR, dataSos, dataEos
    ENDIF
    ;only monomodal
    tmpSos = sos1
    ;assign 250 to those having a second season also
    tmpSos[ind2] = 250
    ;get them all for the first season (here I have both mono and bi)
    dataSos = REFORM(tmpSos[ind1])
    dataEos = REFORM(eos1[ind1])
    ;remove those at 250  
    ind2keep = WHERE(dataSos NE 250, count2keep)
    IF (count2keep GT 0) THEN BEGIN ;not: if all are bimodal, this is not executed and no file will be produced
      dataSos = dataSos[ind2keep]
      dataEos = dataEos[ind2keep]
      DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        TITLE = 'MONO-MOD, asap0ID' + STRTRIM(uniqueASAP0ids[i], 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $
        FILESUFFIX = country_name + '_ID' + STRTRIM(uniqueASAP0ids[i], 2) + '_ONLY_MONOMODAL', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
        RANGE_IN = rout, SAVECSV = 1, TOTALN = count1
    ENDIF 
    DELVAR, dataSos, dataEos, tmpSos, tmpEos
    DELVAR, aspaId, ind1, ind2
  ENDELSE
  
ENDFOR



END