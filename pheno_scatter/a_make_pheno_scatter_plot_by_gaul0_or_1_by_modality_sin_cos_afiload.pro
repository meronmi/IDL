PRO A_make_pheno_scatter_plot_by_gaul0_or_1_by_modality_sin_cos_afiLoad
gaul_level = 1;1 ;can be gaul 0 or 1 
saveSineCos4cluster = 1

base_dir = '\\ies\d5\foodsec\Users\dimouma\trainee'

CASE gaul_level OF
  0: BEGIN
      fn_units = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul0_asap.img'
      out_dir = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\out_gaul0
      FILE_MKDIR, out_dir
    END
  1: BEGIN
      ;fn_units = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul1_asap.img'
      fn_units = base_dir + '\DensScatPlot\Input\ASAP4\gaul' + STRTRIM(gaul_level,2) + '_asap.img' ;0 or 1
      ;out_dir = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\out_gaul1
      out_dir = base_dir + '\DensScatPlot\ASAP4_out_gaul' + STRTRIM(gaul_level,2)
      FILE_MKDIR, out_dir
    END
  ELSE: STOP
ENDCASE

;fn_sos = ['\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenos1_1_36.img', $
;          '\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenos2_1_36.img']
;fn_eos = ['\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenoe1_1_36.img', $
;          '\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\crop_masked_pheno1-36\cropAfiGT25_phenoe2_1_36.img']
;fn_table = '\\ies\h04\Foodsec\users\trainee\DensScatPlot\Input\gaul1_asap_analyzed_3.csv'
fn_sos = base_dir + ['\Pheno_asap4\pheno1-36\phenos1_1_36.img', $
  '\Pheno_asap4\pheno1-36\phenos2_1_36.img']
fn_eos = base_dir + ['\Pheno_asap4\pheno1-36\phenoe1_1_36.img', $
  '\Pheno_asap4\pheno1-36\phenoe2_1_36.img']
  
fn_afi = 'Y:\asap.4.0\data\ref\land_cover\mask_crop_afi_masked.img'  

fn_table = base_dir + '\4CALENDAR_asap4\asap4level1_chad_RDC.csv'

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
;read afi
afi = ReadEnviWithHdr(fn_afi)
indNaN = WHERE(afi GT 200, countNaN)
afi = afi / 200.0

CASE gaul_level OF
  0: BEGIN
    ;make a list of asap countries
    ind = WHERE(table.asap_country EQ 't')
    tmp = table.asap0_id[ind]
    ;get unique aspa0_id
    tmp = tmp[SORT(tmp)]
    idList = tmp[UNIQ(tmp)]
  END
  1: BEGIN
    ind = WHERE((table.asap_country EQ 't') AND (table.analyzed_crop EQ 't')) ;ANALYZE IT IF IT IS ASAP COUNTRY AND HAS ENOUGH CROP
    idList = table.asap1_id[ind]
  END
  ELSE: STOP
ENDCASE

;loop on each asap id and produce scatterplots
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  id01 = idList[i]
  ;subs of season 1 and 2 (if any)
  ind1 = WHERE((units EQ id01) AND (sos1 GE 1) AND (sos1 LE 36) AND (afi GT 0.25), count1)
  ind2 = WHERE((units EQ id01) AND (sos2 GE 1) AND (sos2 LE 36) AND (afi GT 0.25), count2)
  IF (count1 EQ 0) THEN BEGIN
    PRINT, 'Gaul level'+ STRTRIM(gaul_level,2) +' Id ' + STRTRIM(id01, 2) + ' has no vegetated crop (AFI GT 25%) pixels'
  ENDIF ELSE BEGIN
    CASE gaul_level OF
    0: BEGIN
        indCountry = WHERE(table.asap0_id EQ id01)
        strName = table.ADM0_NAME_SHORT[indCountry[0]]
       END
    1: BEGIN
        indGaul1 = WHERE(table.asap1_id EQ id01)
        strName = table.ADM0_NAME_SHORT[indGaul1] + '_' + table.ADM1_NAME_SHORT[indGaul1]
       END 
  ENDCASE
    PRINT, strName
    ;mono and bi together
    dataSos = REFORM(sos1[ind1])
    dataEos = REFORM(eos1[ind1])
    IF (count2 GT 0) THEN BEGIN
      dataSos = [dataSos, REFORM(sos2[ind2])]
      dataEos = [dataEos, REFORM(eos2[ind2])]
    ENDIF
    rout = [0,0]
    ;test for clustering
    IF (saveSineCos4cluster EQ 1) THEN BEGIN
      sosSinCos = [TRANSPOSE(SIN(dataSos*10*!DTOR)),TRANSPOSE(COS(dataSos*10*!DTOR))]
      eosSinCos = [TRANSPOSE(SIN(dataEos*10*!DTOR)),TRANSPOSE(COS(dataEos*10*!DTOR))]    
      WRITE_CSV, out_dir + '\' + strName + '_ID' + STRTRIM(id01, 2) + '_ALL_MODALITIES_data.csv', $
        [TRANSPOSE(dataSos), TRANSPOSE(dataEos), sosSinCos, eosSinCos], HEADER=['sos','eos','sin(sos)','cos(sos)','sin(eos)','cos(eos)']
    ENDIF
    ;end test clustering
    DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
      TITLE = 'ALL MOD, asapID' + STRTRIM(id01, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
      FILESUFFIX =  strName + '_ID' + STRTRIM(id01, 2) + '_ALL_MODALITIES', DOFIT=0, NOWIN = 0, RGBTAB = 4, SIMPLE = 1, $
      RANGEINLOW = 1, RANGEOUT = rout, SAVECSV = 1, TOTALN = count1, DEK36PLOT=1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm

    ;new part for producing 3 graphs

    DELVAR, dataSos, dataEos
    ;only bimodal
    IF (count2 GT 0) THEN BEGIN
      dataSos = REFORM(sos1[ind2])
      dataEos = REFORM(eos1[ind2])
      dataSos = [dataSos, REFORM(sos2[ind2])]
      dataEos = [dataEos, REFORM(eos2[ind2])]
      ;test for clustering
      IF (saveSineCos4cluster EQ 1) THEN BEGIN
        sosSinCos = [TRANSPOSE(SIN(dataSos*10*!DTOR)),TRANSPOSE(COS(dataSos*10*!DTOR))]
        eosSinCos = [TRANSPOSE(SIN(dataEos*10*!DTOR)),TRANSPOSE(COS(dataEos*10*!DTOR))]
        WRITE_CSV, out_dir + '\' + strName + '_ID' + STRTRIM(id01, 2) + '_ONLY_BIMODAL_data.csv', $
          [TRANSPOSE(dataSos), TRANSPOSE(dataEos), sosSinCos, eosSinCos], HEADER=['sos','eos','sin(sos)','cos(sos)','sin(eos)','cos(eos)']
      ENDIF
      ;end test clustering
      DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        TITLE = 'BI-MOD, asapID' + STRTRIM(id01, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
        FILESUFFIX =  strName  + '_ID' + STRTRIM(id01, 2) + '_ONLY_BIMODAL', DOFIT=0, NOWIN = 0, RGBTAB = 4, SIMPLE = 1, $
        RANGE_IN = rout, SAVECSV = 1, TOTALN = count1, DEK36PLOT=1; 72 ;file:///C:/Program Files/Exelis/IDL85/help/online_help/IDL/Content/LoadingDefaultColorTables.htm
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
      ;test for clustering
      IF (saveSineCos4cluster EQ 1) THEN BEGIN
        sosSinCos = [TRANSPOSE(SIN(dataSos*10*!DTOR)),TRANSPOSE(COS(dataSos*10*!DTOR))]
        eosSinCos = [TRANSPOSE(SIN(dataEos*10*!DTOR)),TRANSPOSE(COS(dataEos*10*!DTOR))]
        WRITE_CSV, out_dir + '\' + strName + '_ID' + STRTRIM(id01, 2) + '_ONLY_MONOMODAL_data.csv', $
          [TRANSPOSE(dataSos), TRANSPOSE(dataEos), sosSinCos, eosSinCos], HEADER=['sos','eos','sin(sos)','cos(sos)','sin(eos)','cos(eos)']
      ENDIF
      ;end test clustering
      DensityAndFit_log_scale, dataSos, dataEos,'crop SOS (dek)', 'crop EOS (dek)', out_dir, [1,36], [1,36], 36, 30, $
        TITLE = 'MONO-MOD, asapID' + STRTRIM(id01, 2) + ', n=' + STRTRIM(N_ELEMENTS(dataSos),2) + ', ' + STRTRIM(ROUND(count2/FLOAT(count1)*100),2) + '% is bimodal', $ 
        FILESUFFIX =  strName + '_ID' + STRTRIM(id01, 2) + '_ONLY_MONOMODAL', DOFIT=0, NOWIN = 0, RGBTAB = 4, SIMPLE = 1, $
        RANGE_IN = rout, SAVECSV = 1, TOTALN = count1, DEK36PLOT=1
    ENDIF
    tmpSos = 0
    dataSos = 0
    dataEos = 0 
    tmpSos = 0
    tmpEos = 0
    id01 = 0
    ind1 = 0
    ind2 = 0    
    
    
  ENDELSE
  ;ENDIF
ENDFOR



END