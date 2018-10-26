FUNCTION ARC_corr_NDVI_WSI_startup
  disk_letter = 'X';'D'
  dir = 'Active Projects\Kenya ILRI\ARC\work dir\2018 10 24 Anton';'ARC'
  startup = CREATE_STRUCT($
    'dir', disk_letter + ':\' + dir, $
    'fn_NDVI', disk_letter + ':\' + dir + '\' + ['seasonalNDVI_unimodal.csv', 'seasonalNDVI_bimodal1.csv', 'seasonalNDVI_bimodal2.csv'], $
    'fn_WSI', disk_letter + ':\' + dir + '\' + ['seasonalWSI_unimodal.csv','seasonalWSI_bimodal1.csv','seasonalWSI_bimodal2.csv'], $
    'fn_corr', disk_letter + ':\' + dir + '\' + ['corr_unimodal.csv','corr_bimodal1.csv','corr_bimodal2.csv'], $
    'fn_ids', disk_letter + ':\' + dir + '\' + 'IdCorrespondence.csv')

  RETURN, startup
END

PRO ARC_corr_NDVI_WSI
  ind_of_2012_in_wsi = 11
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

    ;read WSI (not does not have 2002
    wsi = READ_CSV(startup.fn_WSI[i],N_TABLE_HEADER=2, TABLE_HEADER = hdr)
    hdr = STRSPLIT(hdr[1],',',/EXTRACT)
    id_infoWsi = [TRANSPOSE(ndvi.(0)),TRANSPOSE(ndvi.(1)),TRANSPOSE(ndvi.(2)),TRANSPOSE(ndvi.(3)),TRANSPOSE(ndvi.(4))]
    indNaN = WHERE(id_infoWsi EQ -99, countNaN)
    IF (countNaN GT 0) THEN id_infoWsi[indNaN] = !VALUES.F_NAN
    wsi91_19 = !NULL
    FOR k = 5, 33 DO wsi91_19 = [wsi91_19,TRANSPOSE(wsi.(k))]
    indNaN = WHERE(wsi91_19 EQ -99, countNaN)
    IF (countNaN GT 0) THEN wsi91_19[indNaN] = !VALUES.F_NAN
    all_N = !NULL
    all_W = !NULL
    all_zN = !NULL
    all_zW = !NULL
    all_nepN = !NULL
    all_non_par_nepN = !NULL
    all_nepW = !NULL
    ;OPEN OUTPUT FILE
    OPENW, lun, startup.fn_corr[i], lun, /GET_LUN
    ;write hdr
    hdr = 'unitID,Area,Country,'+$
      'nNormTestP,wNormTestP,'+$
      'nwOffset,nwGain,nwCorr,nwP,'+$
      'nwSpRankCorr,nwSpRankCorrP,'+$
      'nw_nep_Offset,nw_nep_Gain,nw_nep_Corr,nw_nep_P,'+$
      'nw_non_par_nep_Offset,nw_non_par_nep_nep_Gain,nw_non_par_nep_nep_Corr,nw_non_par_nep_nep_P,'+$
      'nw_z_Offset,nw_z_Gain,nw_z_Corr,nw_z_P,'
    PRINTF, lun, hdr
    ncolumns= N_ELEMENTS(STRSPLIT(hdr,',',/EXTRACT))
    ;compute various correlations and write
    FOR j = 0, N_ELEMENTS(id_infoWsi[0,*])-1 DO BEGIN
      IF (id_infoNdvi[0,j] NE id_infoWsi[0,j]) THEN STOP
      ;take a row of ndv and swi and start computation
      n = ndvi02_19[*,j]
      w = wsi91_19[ind_of_2012_in_wsi:ind_of_2012_in_wsi+17,j]
      IF (N_ELEMENTS(n) NE N_ELEMENTS(w)) THEN STOP
      indCode = WHERE(id_coodebook.NEWID EQ id_infoWsi[0,j], count)
      IF (count NE 1) THEN STOP
      ;do some computation if I have at least 4 common data points
      IF (TOTAL(FINITE(n*w)) GE 4) THEN BEGIN
        ;correlate directly n and w
        ;linregstat, x0, y0: RETURN, [offset, gain, corr, P-value for gain]
        resNW = linregstat(n,w)
        ;Sperman's rank correlation
        SpRankCorr =  R_CORRELATE(n, w)
        ;normality test using Shapiro-Wilk W
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
        ;     IF (i EQ 2) THEN BEGIN
        ;       u = N ;all_N
        ;       v = W ;all_W
        ;       res = linregstat(u, v)
        ;       x = [MIN(u),MAX(u)]
        ;       ;LAYOUT=[3,1,1] position -> [X1, Y1, X2, Y2]
        ;       gh =  PLOT(2002+INDGEN(N_ELEMENTS(u)), u, POSITION=[0.075,0.15,0.3,0.9], DIMENSIONS= [1000,250], SYMBOL = '+', SYM_SIZE=1, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv') + $
        ;                  ' ' +id_coodebook.AREA[indCode] + id_coodebook.COUNTRY[indCode], XTITLE = 'Time', YTITLE = 'cNDVI', XRANGE=[2001,2020])
        ;       gh1 = PLOT(2002+INDGEN(N_ELEMENTS(u)), v, POSITION=[0.075,0.15,0.3,0.9], SYMBOL = '+', COLOR='red',SYM_SIZE=1, LINESTYLE='', AXIS_STYLE = 0, XRANGE=[2001,2020], /CURRENT)
        ;       a_1 = AXIS('y', TARGET = gh1, LOCATION = [max(gh1.xrange),0,0], TITLE = 'WSI', YRANGE=gh1.yrange, TICKDIR=1, TEXTPOS = 1)
        ;       gh = PLOT(u, v, POSITION=[0.075+0.325,0.15,0.3+0.325,0.9], DIMENSIONS= [1000,250], SYMBOL = '+', SYM_SIZE=1, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = 'cNDVI', YTITLE = 'WSI', /CURRENT)
        ;       gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='red', /OVERPLOT)
        ;       gtxt = TEXT(0.3+0.325+0.02,0.03,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2), ORIENTATION=90)
        ;       u = zN ;all_zN
        ;       v = zW ;all_zW
        ;       res = linregstat(u, v)
        ;        x = [MIN(u),MAX(u)]
        ;       gh = PLOT(u, v, POSITION=[0.075+0.325+0.3,0.15,0.3+0.325+0.3,0.9], SYMBOL = '+', SYM_SIZE=1, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = 'zcNDVI', YTITLE = 'zWSI', /CURRENT)
        ;       gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='red', /OVERPLOT)
        ;       gtxt = TEXT(0.955,0.03,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2), ORIENTATION=90)
        ;       gh.close
        ;       PRINT, 'debug'
        ;     ENDIF
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
        PRINTF, lun, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode], $
          STRING(nNormP),STRING(wNormP),$
          STRING(resNW),$
          STRING(SpRankCorr),$
          STRING(resNW_nep),$
          STRING(resNW_non_par_nep),$
          STRING(resNW_z)],2), ',')
      ENDIF ELSE BEGIN
        PRINT, 'Less then 4 obs for ' + STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode]],2), ' ')
        PRINTF, lun, STRJOIN(STRTRIM([STRING(id_coodebook.NEWID[indCode]),id_coodebook.AREA[indCode],id_coodebook.COUNTRY[indCode],STRING(FLTARR(ncolumns-3)-9999)],2), ',')
      ENDELSE
    ENDFOR
    FREE_LUN, lun
    ;plot overall correlations
    PRINT, 'cN vs W, Season (0=mono,1=1st bi,22nd bi) '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_N, all_W),2),',')
    x = [MIN(all_N,/NAN),MAX(all_N,/NAN)]
    DensityAndFit_log_scale, all_N, all_W, 'cNDVI', 'WSI', startup.dir, x, [0,100], 50, 20, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= 1, FILESUFFIX = '_'+STRTRIM(i,2)
    PRINT, 'zcN vs zW, Season (0=mono,1=1st bi,22nd bi) '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_zN, all_zW),2),',')
    x = [MIN(all_zN,/NAN),MAX(all_zN,/NAN)]
    y = [MIN(all_zW,/NAN),MAX(all_zW,/NAN)]
    DensityAndFit_log_scale, all_zN, all_zW, 'zcNDVI', 'zWSI', startup.dir, x, y, 50, 20, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= 1, FILESUFFIX = '_'+STRTRIM(i,2)
    PRINT, 'nep_cN vs nepW, Season (0=mono,1=1st bi,22nd bi) '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_nepN, all_nepW),2),',')
    x = [MIN(all_nepN,/NAN),MAX(all_nepN,/NAN)]
    DensityAndFit_log_scale, all_nepN, all_nepW, 'nepcNDVI', 'nepWSI', startup.dir, [0,100], [0,100], 50, 20, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= 1, FILESUFFIX = '_'+STRTRIM(i,2)
    PRINT, 'non_parametric_nep_cN vs nepW, Season (0=mono,1=1st bi,22nd bi) '+STRING(i)+STRJOIN(STRTRIM(linregstat(all_non_par_nepN, all_nepW),2),',')
    x = [MIN(all_non_par_nepN,/NAN),MAX(all_non_par_nepN,/NAN)]
    DensityAndFit_log_scale, all_non_par_nepN, all_nepW, 'non_par_nepcNDVI', 'nepWSI', startup.dir, [0,100], [0,100], 50, 20, TITLE='All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), DOFIT=1, SIMPLE =1, DOLOG= 1, FILESUFFIX = '_'+STRTRIM(i,2)
    PRINT,'debug'  ;plot(all_nepN,all_non_par_nepN,symbol='.',linestyle='');plot(all_non_par_nepN,all_nepW,symbol='.',linestyle='')

    ;  res = linregstat(all_zN, all_zW)
    ;  x = [MIN(all_zN),MAX(all_zN)]
    ;  gh = PLOT(all_zN, all_zW, SYMBOL = '+', SYM_SIZE=.5, LINESTYLE='', TITLE= 'All data, ' + FILE_BASENAME(startup.fn_corr[i],'.csv'), XTITLE = 'zcNDVI', YTITLE = 'zWSI')
    ;  gh = PLOT(x, res[0]+res[1]*x, LINESTYLE='-', COLOR='red', /OVERPLOT)
    ;  gtxt = TEXT(0.15,0.02,'r = ' + STRTRIM(res[2],2) + ', P = ' + STRTRIM(res[3],2))

  ENDFOR
END