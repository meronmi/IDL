FUNCTION ARC_corr_NDVI_WSI_startup
  ;WSI7chirps Jan 2019
  disk_letter = 'D';'X';'D'
  dir = '\ARC\2019 01 24 WSIchirps'
  version = 'wsi7chirps_MOS'
  ;version = 'wsi7chirps_END'
  excludeHLwsi = 0
  IF (excludeHLwsi EQ 1) THEN  version = version + '_WSI20-80'
  startup = CREATE_STRUCT($
    'dir', disk_letter + ':\' + dir, $
    'version', version, $
    'fn_NDVI', disk_letter + ':\' + dir + '\' + ['seasonalNDVI_unimodal.csv', 'seasonalNDVI_bimodal1.csv', 'seasonalNDVI_bimodal2.csv'], $
    'fn_WSI', disk_letter + ':\' + dir + '\' + ['seasonalWSIendValue_unimodal_CHIRPS.csv','seasonalWSIendValue_bimodal1_CHIRPS.csv','seasonalWSIendValue_bimodal2_CHIRPS.csv'], $
    ;'fn_WSI', disk_letter + ':\' + dir + '\' + ['seasonalWSImosValue_unimodal_CHIRPS.csv','seasonalWSImosValue_bimodal1_CHIRPS.csv','seasonalWSImosValue_bimodal2_CHIRPS.csv'], $
    'fn_corr', disk_letter + ':\' + dir + '\' + version + ['corr_unimodal.csv','corr_bimodal1.csv','corr_bimodal2.csv'], $
    'fn_nonParNEP', disk_letter + ':\' + dir + '\' + version + ['nonParNEP_unimodal.csv','nonParNEP_bimodal1.csv','nonParNEP_bimodal2.csv'], $
    'fn_ids', disk_letter + ':\' + dir + '\' + 'IdCorrespondence.csv', $
    'frstYear', 2002, $
    'nyear', 18, $
    'excludeHLwsi', excludeHLwsi) ;this is a test to exclude from regression those regions having a mean WSI LE 20 or GT 80%
  
;  ;WSI7 Nov 2018
;  disk_letter = 'D';'X';'D'
;  dir = 'ARC\2018 11 13 lastWSI';'ARC\2018 10 24 Anton';'ARC'
;  version = 'vTest'
;  excludeHLwsi = 0
;  IF (excludeHLwsi EQ 1) THEN  version = version + '_WSI20-80'
;  startup = CREATE_STRUCT($
;    'dir', disk_letter + ':\' + dir, $
;    'version', version, $
;    'fn_NDVI', disk_letter + ':\' + dir + '\' + ['seasonalNDVI_unimodal.csv', 'seasonalNDVI_bimodal1.csv', 'seasonalNDVI_bimodal2.csv'], $
;    'fn_WSI', disk_letter + ':\' + dir + '\' + ['seasonalWSIendValue_unimodal.csv','seasonalWSIendValue_bimodal1.csv','seasonalWSIendValue_bimodal2.csv'], $
;    'fn_corr', disk_letter + ':\' + dir + '\' + version + ['corr_unimodal.csv','corr_bimodal1.csv','corr_bimodal2.csv'], $
;    'fn_nonParNEP', disk_letter + ':\' + dir + '\' + version + ['nonParNEP_unimodal.csv','nonParNEP_bimodal1.csv','nonParNEP_bimodal2.csv'], $
;    'fn_ids', disk_letter + ':\' + dir + '\' + 'IdCorrespondence.csv', $
;    'frstYear', 2002, $
;    'nyear', 18, $
;    'excludeHLwsi', excludeHLwsi) ;this is a test to exclude from regression those regions having a mean WSI LE 20 or GT 80%
  RETURN, startup
END

PRO ARC_corr_NDVI_WSI
  ind_of_2002_in_wsi = 11
  startup = ARC_corr_NDVI_WSI_startup()
  ;read  the id correspondence file
  id_coodebook = READ_CSV(startup.fn_ids,HEADER= hdr)
  id_coodebook = RENAME_TAGS(id_coodebook,TAG_NAMES(id_coodebook), hdr)    ;rename_tags, struct, oldtags_in, newtags_in, verbose=verbose

  FOR i = 0, 2 DO BEGIN ;mono, bi1, bi2
    ;read NDVI
    ndvi = READ_CSV(startup.fn_NDVI[i],N_TABLE_HEADER=2, TABLE_HEADER = hdr)
    hdr = STRSPLIT(hdr[1],',',/EXTRACT)
    id_infoNdvi = [TRANSPOSE(ndvi.(0)),TRANSPOSE(ndvi.(1)),TRANSPOSE(ndvi.(2)),TRANSPOSE(ndvi.(3)),TRANSPOSE(ndvi.(4))]
    indNaN = WHERE(id_infoNdvi EQ -99, countNaN)
    IF (countNaN GT 0) THEN id_infoNdvi[indNaN] = !VALUES.F_NAN
    ndvi02_19 = !NULL
    FOR k = 5, 22 DO ndvi02_19 = [ndvi02_19, TRANSPOSE(ndvi.(k))]
    indNaN = WHERE(ndvi02_19 EQ -99, countNaN)
    IF (countNaN GT 0) THEN ndvi02_19[indNaN] = !VALUES.F_NAN

    ;read WSI 
    wsi = READ_CSV(startup.fn_WSI[i],N_TABLE_HEADER=2, TABLE_HEADER = hdr)
    hdr = STRSPLIT(hdr[1],',',/EXTRACT)
    id_infoWsi = [TRANSPOSE(ndvi.(0)),TRANSPOSE(ndvi.(1)),TRANSPOSE(ndvi.(2)),TRANSPOSE(ndvi.(3)),TRANSPOSE(ndvi.(4))]
    indNaN = WHERE(id_infoWsi EQ -99, countNaN)
    IF (countNaN GT 0) THEN id_infoWsi[indNaN] = !VALUES.F_NAN
    wsi91_19 = !NULL
    FOR k = 5, 33 DO wsi91_19 = [wsi91_19,TRANSPOSE(wsi.(k))]
    indNaN = WHERE(wsi91_19 EQ -99, countNaN)
    IF (countNaN GT 0) THEN wsi91_19[indNaN] = !VALUES.F_NAN
    
    ;Init variables to store all values pooled together
    all_N = !NULL & all_W = !NULL & all_zN = !NULL & all_zW = !NULL  & all_nepN = !NULL & all_non_par_nepN = !NULL & all_nepW = !NULL
    ;OPEN OUTPUT FILE
    OPENW, lun, startup.fn_corr[i], /GET_LUN
    ;write hdr
    hdr = 'unitID,Area,Country,'+$
      'nNormTestP,wNormTestP,'+$
      'nTS,nMKp,wTS,wMKp,' +$   ;new trend test
      'nwOffset,nwGain,nwCorr,nwP,'+$
      'nwSpRankCorr,nwSpRankCorrP,'+$
      'nw_nep_Offset,nw_nep_Gain,nw_nep_Corr,nw_nep_P,'+$
      'nw_non_par_nep_Offset,nw_non_par_nep_nep_Gain,nw_non_par_nep_nep_Corr,nw_non_par_nep_nep_P,'+$
      'nw_z_Offset,nw_z_Gain,nw_z_Corr,nw_z_P,'+$
      STRJOIN('n_non_par_nep_'+STRTRIM(startup.frstYear+INDGEN(startup.nyear),2),',')+','+$
      STRJOIN('w_nep_'+STRTRIM(startup.frstYear+INDGEN(startup.nyear),2),',') + ',meanWSI,sdWSI'
    PRINTF, lun, hdr
    ncolumns= N_ELEMENTS(STRSPLIT(hdr,',',/EXTRACT))
    ;OPEN the file used to rank non parametric NEP disagreement
    OPENW, lun2, startup.fn_nonParNEP[i], /GET_LUN
    ;write hdr
    hdr2 = 'unitID,Area,Country,'+$
      STRJOIN('n_non_par_nep_'+STRTRIM(startup.frstYear+INDGEN(startup.nyear),2),',')+','+$
      STRJOIN('w_nep_'+STRTRIM(startup.frstYear+INDGEN(startup.nyear),2),',') 
    PRINTF, lun2, hdr2
    ncolumns2= N_ELEMENTS(STRSPLIT(hdr2,',',/EXTRACT))
    ;compute various correlations and write
    FOR j = 0, N_ELEMENTS(id_infoWsi[0,*])-1 DO BEGIN
      IF (id_infoNdvi[0,j] NE id_infoWsi[0,j]) THEN STOP
      ;IF (id_infoWsi[0,j] EQ 116) THEN STOP
      ;take a row of ndv and swi and start computation
      n = ndvi02_19[*,j]
      w = wsi91_19[ind_of_2002_in_wsi:ind_of_2002_in_wsi+17,j]
      IF (N_ELEMENTS(n) NE N_ELEMENTS(w)) THEN STOP
      indCode = WHERE(id_coodebook.NEWID EQ id_infoWsi[0,j], count)
      IF (count NE 1) THEN STOP
      ;do some computation if I have at least 4 common data points or use wsi rule
      exclude_rule_WSI = 0
      IF (startup.excludeHLwsi EQ 1) THEN BEGIN
        indFin = WHERE(FINITE(w), countFin)
        IF (countFin GE 4) THEN  w_mean = MEAN(w[indFin]) ELSE w_mean = -9999
        IF ((w_mean LE 20) OR (w_mean GE 80)) THEN exclude_rule_WSI = 1
      ENDIF
      IF ((TOTAL(FINITE(n*w)) GE 4) AND (exclude_rule_WSI EQ 0)) THEN BEGIN
        ;correlate directly n and w    
        resNW = linregstat(n,w)   ;linregstat, x0, y0: RETURN, [offset, gain, corr, P-value for gain]
        ;Sperman's rank correlation
        SpRankCorr =  R_CORRELATE(n, w)
        ;check if there is a trend
        ;on NDVI
        ;Theil_sen slope
        yrs = INDGEN(N_ELEMENTS(n)) ;no need to be real years
        indFin = WHERE(FINITE(n), countFin)
        ret = theil_sen(yrs[indFin],n[indFin])
        n_TS = ret[1]
        ;Mann-Kendall tau
        ret = R_CORRELATE(yrs[indFin],n[indFin], /KENDALL)
        n_MKt = ret[0]
        n_MKp = ret[1]
        ;on WSI
        ;Theil_sen slope
        yrs = INDGEN(N_ELEMENTS(w))
        indFin = WHERE(FINITE(w), countFin)
        ret = theil_sen(yrs[indFin],w[indFin])
        w_TS = ret[1]
        ;mean and sd of w
        w_mean = MEAN(w[indFin])
        w_sd = STDDEV(w[indFin])
        ;Mann-Kendall tau
        ret = R_CORRELATE(yrs[indFin],w[indFin], /KENDALL)
        w_MKt = ret[0]
        w_MKp = ret[1]
        ;Test normality test using Shapiro-Wilk W
        nNormP = IMSL_NORMALITY(n)
        wNormP = IMSL_NORMALITY(w)
        ;compute n z-score
        zN = (n-MEAN(n,/NAN))/STDDEV(n,/NAN)
        ;compute n NEP
        nepN = zN * !VALUES.F_NAN
        ind = WHERE(FINITE(zN), count)
        IF (count GT 0) THEN nepN[ind] = GAUSS_PDF(zN[ind])*100.0
        ;compute non parametric nep of n directly on n data
        non_par_nepN = NEP_array(n)
        ;compute w NEP
        nepW = NEP_array(w)
        ;compute w z-score
        zW = nepW * !VALUES.F_NAN
        ind = WHERE(FINITE(nepW), count)
        IF (count GT 0) THEN BEGIN
          FOR q = 0, count-1 DO zW[ind[q]] = GAUSS_CVF(1.0-nepW[ind[q]]/100.0)
        ENDIF
        ;store the data
        all_N = [all_N, n]
        all_W = [all_W, w]
        all_zN = [all_zN, zN]
        all_zW = [all_zW, zW]
        all_nepN = [all_nepN, nepN]
        all_non_par_nepN = [all_non_par_nepN, non_par_nepN]
        all_nepW = [all_nepW, nepW]
        ; Pot w and N for an ID
        plotID = 341  ;set it to 0 to skip 341 is Bermo Niger
        IF (plotID NE 0) AND (id_infoWsi[0,j] EQ plotID) THEN BEGIN
;         IF ((i EQ 1) AND (id_infoWsi[0,j] GT 161))THEN BEGIN
          u = non_par_nepN ;all_N
          v = nepW ;all_W
          uVar = 'npnep_cNDVI'
          vVar = 'nep_WSI'
          res = linregstat(u, v)
          x = [MIN(u, /NAN),MAX(u, /NAN)]
          ;LAYOUT=[3,1,1] position -> [X1, Y1, X2, Y2]
          simsiz = 1.5
          gh =  PLOT(startup.frstYear+INDGEN(N_ELEMENTS(u)), u, POSITION=[0.075,0.15,0.3,0.9], DIMENSIONS= [1000,250], SYMBOL = '+', SYM_SIZE=simsiz, LINESTYLE='-', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv') + $
                    ' ' +id_coodebook.AREA[indCode] +'-'+ id_coodebook.COUNTRY[indCode], XTITLE = 'Time', YTITLE = uVar, XRANGE=[2001,2020], AXIS_STYLE=2)
          gh['axis3'].hide = 1
          gh1 = PLOT(startup.frstYear+INDGEN(N_ELEMENTS(u)), v, POSITION=[0.075,0.15,0.3,0.9], SYMBOL = '+', COLOR='red',SYM_SIZE=simsiz, LINESTYLE='-', AXIS_STYLE = 0, XRANGE=[2001,2020], /CURRENT)
          a_1 = AXIS('y', TARGET = gh1, LOCATION = [max(gh1.xrange),0,0], TITLE = vVar, YRANGE=gh1.yrange, TICKDIR=1, TEXTPOS = 1, COLOR='r')
          gh = PLOT(u, v, POSITION=[0.075+0.325,0.15,0.3+0.325,0.9], DIMENSIONS= [1000,250], SYMBOL = '+', SYM_SIZE=simsiz, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = uVar, YTITLE = vVar, /CURRENT)
          gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='green', /OVERPLOT)
          gtxt = TEXT(0.3+0.325+0.02,0.03,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2), ORIENTATION=90)
          u = zN ;all_zN
          v = zW ;all_zW
          uVar = 'zcNDVI' 
          vVar = 'zWSI'
          res = linregstat(u, v)
          x = [MIN(u,/NAN),MAX(u,/NAN)]
          gh = PLOT(u, v, POSITION=[0.075+0.325+0.3,0.15,0.3+0.325+0.3,0.9], SYMBOL = '+', SYM_SIZE=simsiz, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = uVar, YTITLE = vVar, /CURRENT)
          gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='green', /OVERPLOT)
          gtxt = TEXT(0.955,0.03,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2), ORIENTATION=90)
          PRINT, 'debug'
          gh.close
        ENDIF
        ;now correlations
        ;n nep vs w nep
        resNW_non_par_nep = linregstat(non_par_nepN,nepW)
        resNW_nep = linregstat(nepN,nepW)
        ;n z-score vs w zscore
        resNW_z = linregstat(zN,zW)

        ;IF (nNormP LT 0.05) OR (nNormP LT 0.05) THEN STOP

        ;     IF ((id_infoWsi[0,j] EQ 88) AND (i EQ 2))THEN BEGIN
        ;       PRINT, 'debug this'
        ;       gh1 = qq_plot(n, TITLESTRING = 'cNDVI', /TESTNORM)
        ;       gh1 = qq_plot(w, TITLESTRING = 'WSI', /TESTNORM)
        ;       h0 = PLOT(N,W, SYMBOL='o', LINESTYLE='', TITLE = 'Data', XTITLE='N', YTITLE='W')
        ;       gh0 = PLOT(zN,zW, SYMBOL='o', LINESTYLE='', TITLE = 'Z', XTITLE='N', YTITLE='W')
        ;       gh0 = PLOT(nepN,nepW, SYMBOL='o', LINESTYLE='', TITLE = 'NEP', XTITLE='N', YTITLE='W')
        ;       PRINT, 'qui'
        ;     ENDIF
        indNaN = WHERE(~FINITE(non_par_nepN), countNaN)
        str_non_par_nepN =  STRING(non_par_nepN)
        IF (countNaN GT 0) THEN str_non_par_nepN[indNaN] = '-9999'
        indNaN = WHERE(~FINITE(nepW), countNaN)
        str_nepW =  STRING(nepW)
        IF (countNaN GT 0) THEN str_nepW[indNaN] = '-9999'
        PRINTF, lun, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode], $
          STRING(nNormP),STRING(wNormP),$
          STRING(n_TS),STRING(n_MKp),STRING(w_TS),STRING(w_MKp),$;'nTS,nMKp,wTS,wMKp' +$   ;new trend test
          STRING(resNW),$
          STRING(SpRankCorr),$
          STRING(resNW_nep),$
          STRING(resNW_non_par_nep),$
          STRING(resNW_z), $
          STRING(str_non_par_nepN), $
          STRING(str_nepW), STRING(w_mean), STRING(w_sd) $
          ],2), ',')
        PRINTF, lun2, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode], $
          STRING(str_non_par_nepN), $
          STRING(str_nepW)],2), ',')
      ENDIF ELSE BEGIN
        PRINT, 'Less then 4 obs for ' + STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode]],2), ' ')
        PRINTF, lun, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode],STRING(FLTARR(ncolumns-3)-9999)],2), ',')
        PRINTF, lun2, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode],STRING(FLTARR(ncolumns2-3)-9999)],2), ',')
      ENDELSE
    ENDFOR
    FREE_LUN, lun
    FREE_LUN, lun2
    ;reopen file 2 and rank the disagreement
    npNEP = READ_CSV(startup.fn_nonParNEP[i],N_TABLE_HEADER=1, TABLE_HEADER = hdr)
    npNEPn = FLTARR(startup.nyear,N_ELEMENTS(npNEP.(0)))
    npNEPw = npNEPn
    FOR k = 3, 3 + startup.nyear-1 DO npNEPn[k-3,*] = REFORM(npNEP.(k))
    FOR k = (3 + startup.nyear), (3 + startup.nyear + startup.nyear-1) DO npNEPw[k-(3 + startup.nyear),*] = REFORM(npNEP.(k)) ;note: time is on rows
    ;replace NaN
    indNan = WHERE(npNEPn EQ -9999, countNaN)
    IF (countNaN GT 0) THEN npNEPn[indNan] = !VALUES.F_NAN
    indNan = WHERE(npNEPw EQ -9999, countNaN)
    IF (countNaN GT 0) THEN npNEPw[indNan] = !VALUES.F_NAN
    absdiff = ABS(npNEPn-npNEPw)
    ;set NaN to -99999 so they will be sorted last
    ind = WHERE(~FINITE(absdiff), count)
    IF (count GT 0) THEN absdiff[ind] = -9999
    ;sort them   
    subSort = REVERSE(SORT(absdiff)) ;largest difference to smallest
    ;save the rank
    rank = FIX(absdiff * 0)
    rank[subSort] = INDGEN(N_ELEMENTS(subSort))
    ;find smallest sub by row (by region)
    maxRankByRow = MIN(rank, DIMENSION=1, sub)
    ;find the year where this ocurred
    cl =  ARRAY_INDICES(rank, sub)
    yearOfRank = cl[0,*] + startup.frstYear
    ;NOW UPDATE THE FILE corr WITH NEW COLUMNS
    OPENR, lun, startup.fn_corr[i], /GET_LUN
    textLine = !NULL
    tmp = ''
    READF, lun, tmp
    textLine = [textLine, tmp + ',rank_disagreement,year,']
    c = 0 
    WHILE ~ EOF(lun) DO BEGIN
      READF, lun, tmp
      textLine = [textLine, tmp + ',' + STRTRIM(maxRankByRow[c],2) + ',' + STRTRIM(yearOfRank[c],2) + ',']
      c = c + 1
    ENDWHILE
    FREE_LUN, lun
    ;overwrite with new info
    OPENW, lun, startup.fn_corr[i], /GET_LUN
    FOR k=0, N_ELEMENTS(textLine)-1 DO BEGIN
      PRINTF, lun, textLine[k]
    ENDFOR  
    FREE_LUN, lun
    ;plot overall correlations [offset, gain, corr, P-value for gain]
    ngrid = 10  ;50
    nlevel = 20
    dolog = 0
    PRINT, 'Modality = ' + STRTRIM(i,2) + ' (0=mono,1=1st bimod,2=2nd bimod). [offset, gain, corr, P-value for gain]'
    PRINT, 'cN vs W '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_N, all_W),2),',')
    x = [MIN(all_N,/NAN),MAX(all_N,/NAN)]
    DensityAndFit_log_scale, all_N, all_W, 'cNDVI', 'WSI', startup.dir, x, [0,100], ngrid, nlevel, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= dolog, FILESUFFIX = '_'+STRTRIM(i,2) + startup.version
    PRINT, 'zcN vs zW '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_zN, all_zW),2),',')
    x = [MIN(all_zN,/NAN),MAX(all_zN,/NAN)]
    y = [MIN(all_zW,/NAN),MAX(all_zW,/NAN)]
    DensityAndFit_log_scale, all_zN, all_zW, 'zcNDVI', 'zWSI', startup.dir, x, y, ngrid, nlevel, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= dolog, FILESUFFIX = '_'+STRTRIM(i,2) + startup.version
    PRINT, 'nep_cN vs nepW '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_nepN, all_nepW),2),',')
    x = [MIN(all_nepN,/NAN),MAX(all_nepN,/NAN)]
    DensityAndFit_log_scale, all_nepN, all_nepW, 'nepcNDVI', 'nepWSI', startup.dir, [0,100], [0,100], ngrid, nlevel, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= dolog, FILESUFFIX = '_'+STRTRIM(i,2) + startup.version
    PRINT, 'non_parametric_nep_cN vs nepW '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_non_par_nepN, all_nepW),2),',')
    x = [MIN(all_non_par_nepN,/NAN),MAX(all_non_par_nepN,/NAN)]
    DensityAndFit_log_scale, all_non_par_nepN, all_nepW, 'non_par_nepcNDVI', 'nepWSI', startup.dir, [0,100], [0,100], ngrid, nlevel, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= dolog, FILESUFFIX = '_'+STRTRIM(i,2) + startup.version
    

;    res = linregstat(all_zN, all_zW)
;    x = [MIN(all_zN),MAX(all_zN)]
;    gh = PLOT(all_zN, all_zW, SYMBOL = '+', SYM_SIZE=.5, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = 'zcNDVI', YTITLE = 'zWSI')
;    gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='red', /OVERPLOT)
;    gtxt = TEXT(0.15,0.02,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2))
    PRINT, 'Finished'
  ENDFOR
  CLOSE, /ALL
END