PRO make_pheno_scatter_plot_by_asap20_units_by_modality_convention108
;set min max of pheno
phenMin = 1
phenMax = 108
PRINT, 'Phen min e max considered:', phenMin, phenMax
;set filenames
fn_units = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul1_asap.img'
pheno_dir = '\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-108'
fn_sos = pheno_dir + '\' + ['phenos1_AfiGT50.img', $
                            'phenos2_AfiGT50.img']
fn_eos = pheno_dir + '\' + ['phenoe1_AfiGT50.img', $
                            'phenoe2_AfiGT50.img']
fn_table = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul1_asap_analyzed_3.csv'
out_dir = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\test';'\\ies\h04\Foodsec\users\trainee\DensScatPlot\GAUL1'
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

;loop on each asap id and produce scatterplots
;c = 1
FOR i = 0, N_ELEMENTS(table.asap1_id)-1 DO BEGIN
   aspaId = table.asap1_id[i]
  IF ((table.asap_country[i] EQ 't') AND (table.analyzed_crop[i] EQ 't')) THEN BEGIN ;ANALYZE IT IF IT IS ASAP COUNTRY AND HAS ENOUGH CROP
    ;subs of season 1 and 2 (if any)
    ind1 = WHERE((units EQ aspaId) AND (sos1 GE phenMin) AND (sos1 LE phenMax), count1)
    ind2 = WHERE((units EQ aspaId) AND (sos2 GE phenMin) AND (sos2 LE phenMax), count2)
    IF (count1 EQ 0) THEN BEGIN
      PRINT, 'Id ' + STRTRIM(aspaId, 2) + ' has no vegetated crop (AFI GT 25%) pixels'
    ENDIF ELSE BEGIN
  ;   ***********************
;      dataSos = REFORM(sos1[ind1])
;      dataEos = REFORM(eos1[ind1])
;      IF (count2 GT 0) THEN BEGIN
;        dataSos = [dataSos, REFORM(sos2[ind2])]
;        dataEos = [dataEos, REFORM(eos2[ind2])]
;      ENDIF
;      DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
;        TITLE = 'asapID' + STRTRIM(aspaId, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
;        FILESUFFIX = table.ADM0_NAME_SHORT[i] + '_' + table.ADM1_NAME_SHORT[i] + '_ID' + STRTRIM(aspaId, 2), DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm
;      dataSos = 0
;      dataEos = 0
;      aspaId = 0
;      ind1 = 0
;      ind2 = 0
      ;   ***********************
      ;mono and bi together
      dataSos = REFORM(sos1[ind1])
      dataEos = REFORM(eos1[ind1])
      IF (count2 GT 0) THEN BEGIN
        dataSos = [dataSos, REFORM(sos2[ind2])]
        dataEos = [dataEos, REFORM(eos2[ind2])]
      ENDIF
      rout = [0,0]
      ;test for clustering
      WRITE_CSV, out_dir + '\' + table.ADM0_NAME_SHORT[i] + '_' + table.ADM1_NAME_SHORT[i] + '_ID' + STRTRIM(aspaId, 2) + '_ALL_MODALITIES_data.csv', $
        dataSos, dataEos, HEADER=['sos','eos']
      ;end test clustering
      ;DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
       DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [phenMin,phenMax], [phenMin,phenMax], phenMax, 30, $
        TITLE = 'ALL MOD, asapID' + STRTRIM(aspaId, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
        FILESUFFIX =  table.ADM0_NAME_SHORT[i] + '_' + table.ADM1_NAME_SHORT[i] + '_ID' + STRTRIM(aspaId, 2) + '_ALL_MODALITIES', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
        RANGEINLOW = 1, RANGEOUT = rout, SAVECSV = 1, TOTALN = count1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm

      ;new part for producing 3 graphs

      DELVAR, dataSos, dataEos
      ;only bimodal
      IF (count2 GT 0) THEN BEGIN
        dataSos = REFORM(sos1[ind2])
        dataEos = REFORM(eos1[ind2])
        dataSos = [dataSos, REFORM(sos2[ind2])]
        dataEos = [dataEos, REFORM(eos2[ind2])]
        ;DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [phenMin,phenMax], [phenMin,phenMax], phenMax, 30, $
          TITLE = 'BI-MOD, asapID' + STRTRIM(aspaId, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
          FILESUFFIX =  table.ADM0_NAME_SHORT[i]+ '_' + table.ADM1_NAME_SHORT[i]  + '_ID' + STRTRIM(aspaId, 2) + '_ONLY_BIMODAL', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
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
      ;remove those at 250t
      ind2keep = WHERE(dataSos NE 250, count2keep)
      IF (count2keep GT 0) THEN BEGIN ;not: if all are bimodal, this is not executed and no file will be produced
        dataSos = dataSos[ind2keep]
        dataEos = dataEos[ind2keep]
        ;DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [phenMin,phenMax], [phenMin,phenMax], phenMax, 30, $
          TITLE = 'MONO-MOD, asapID' + STRTRIM(aspaId, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
          FILESUFFIX =  table.ADM0_NAME_SHORT[i]+ '_' + table.ADM1_NAME_SHORT[i]  + '_ID' + STRTRIM(aspaId, 2) + '_ONLY_MONOMODAL', DOFIT=0, NOWIN = 1, RGBTAB = 4, SIMPLE = 1, $
          RANGE_IN = rout, SAVECSV = 1, TOTALN = count1
      ENDIF
      tmpSos = 0
      DELVAR, dataSos, dataEos, tmpSos, tmpEos
      DELVAR, aspaId, ind1, ind2    
      
      
    ENDELSE
  ENDIF
ENDFOR



END