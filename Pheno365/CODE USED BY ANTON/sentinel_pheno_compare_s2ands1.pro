FUNCTION Sentinel_pheno_compare_S2andS1, dir
;dir =  'D:\RAPHAEL_pheno_test\Results_2019_6_17';'D:\RAPHAEL_pheno_test\Results_2019May29';'Z:\RAPHAEL_pheno_test\Results_2019May27'
dir_out = dir + '\Pheno_from_index_corr'
FILE_MKDIR, dir_out
csv_file = dir + '\pheno_results.csv'
res = READ_CSV(csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN )
x = rename_tags(res, TAG_NAMES(res), [hdr[0:-2],'Empty'])
PRINT, hdr
;cropTypes = x.crop_type[UNIQ(x.crop_type,SORT(x.crop_type))]
;indices = x.index[UNIQ(x.index,SORT(x.index))]
h1_array = !NULL
cropTypes = raphael_crop_select()

;NDVI index
S2index = 'NDVIm'
;s1 indices
S1index = ['CR_ad','CR_a','CR_d','RVI_ad','RVI_a','RVI_d']

;make the table for all crops
OPENW, lun, dir_out + '\Pheno_from_index_corr.csv', /GET_LUN
OPENW, lunRetr, dir_out + '\Pheno_retrieval_success_and_failure.csv', /GET_LUN
OPENW, lunPolyErrors, dir_out + '\Pheno_poly_erros_RVI.csv', /GET_LUN
dlmtr = ','
PRINTF, lun, STRJOIN(['Crop','Index','Sampling','n','SOS20_r','SOS20_P','SOS20_RMSD','SOS20_MAE','SOS20_ME',$
                                            'EOS20_r','EOS20_P','EOS20_RMSD','EOS20_MAE','EOS20_ME',$
                                            'GSL20_r','GSL20_P','GSL20_RMSD','GSL20_MAE','GSL20_ME',$
                                            'SOS50_r','SOS50_P','SOS50_RMSD','SOS50_MAE','SOS50_ME',$
                                            'EOS50_r','EOS50_P','EOS50_RMSD','EOS50_MAE','EOS50_ME',$
                                            'GSL50_r','GSL50_P','GSL50_RMSD','GSL50_MAE','GSL50_ME'], dlmtr)
PRINTF, lunRetr, STRJOIN(['Crop','Sampling', 'nPolygons', 'nS2ok_S1ok', 'nS2failS1fail','nS2successS1fail', 'nS2failS1success'], dlmtr)            
PRINTF, lunPolyErrors, STRJOIN(['Poly_ID','Crop','Lat','Lon','mean_cv_inSeason','prctHeterogObs_inSeason','Sampling', 'dSOS20', 'dEOS20', 'mean_abs_dev'], dlmtr)
          
FOR u = 0, 2 DO BEGIN ;all data, only selected based on gaps and perror
  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    FOR k = 0, N_ELEMENTS(S1index)-1 DO BEGIN
      tmp = STRSPLIT(cropTypes[i],'-',/EXTRACT)
      tmp = STRMID(STRTRIM(tmp[0],2),0,18)
      tit = tmp + ', Pheno NDVI vs. Pheno ' + S1index[k]
      PRINT, cropTypes[i], ' ', S1index[k]
      CASE u OF
        0: BEGIN
          indS2 = WHERE((x.index EQ S2index) AND (x.crop_type EQ cropTypes[i]), n_sample_S2) 
          indS1 = WHERE((x.index EQ S1index[k]) AND (x.crop_type EQ cropTypes[i]), n_sample_S1)
          IF ((n_sample_S2 NE n_sample_S1) AND (n_sample_S2 GT 0)) THEN BEGIN
            ;find out what happens
             idS2 = x.ID[indS2]
             idS1 = x.ID[indS1]
             FOR m = 0, N_ELEMENTS(idS2) - 1 DO BEGIN
              indTmp = WHERE(idS1 EQ idS2[m], countTmp) 
              IF (countTmp EQ 0) THEN BEGIN
                PRINT,  idS2[m]
                STOP ;this should not happen (i save also fit failures)
              ENDIF
             ENDFOR
            STOP ;this should not happen (i save also fit failures)
          ENDIF
          ;those retrievals that were successful
          indS2s = indS2[WHERE(x.SOS20[indS2] NE -9999 AND x.EOS20[indS2] NE -9999, /NULL)]
          indS1s = indS1[WHERE(x.SOS20[indS1] NE -9999 AND x.EOS20[indS1] NE -9999, /NULL)]
          ;those retrievals that failed
          indS2f = indS2[WHERE(x.SOS20[indS2] EQ -9999 OR x.EOS20[indS2] EQ -9999, /NULL)]
          indS1f = indS1[WHERE(x.SOS20[indS1] EQ -9999 OR x.EOS20[indS1] EQ -9999, /NULL)] 
          samp = 'all retrieval'
          tit2 = 'All retrieval'
          suffix = 'all_retrieval'
          ;prefix = indices[k]
        END
        1: BEGIN
          indS2 = WHERE((x.index EQ S2index) AND (x.crop_type EQ cropTypes[i]), count)
          ids = x.ID[indS2]
          ind2Keep = !NULL
          ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
          FOR ii = 0, count-1 DO BEGIN
            indNDVI = WHERE((x.ID EQ ids[ii]) AND (x.Index EQ 'NDVIm'))
            IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, indS2[ii]]
          ENDFOR
          indS2 = ind2Keep
          n_sample_S2 = N_ELEMENTS(indS2)
          
          indS1 = WHERE((x.index EQ S1index[k]) AND (x.crop_type EQ cropTypes[i]), count)
          ids = x.ID[indS1]
          ind2Keep = !NULL
          ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
          FOR ii = 0, count-1 DO BEGIN
            indNDVI = WHERE((x.ID EQ ids[ii]) AND (x.Index EQ 'NDVIm'))
            IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, indS1[ii]]
          ENDFOR
          indS1 = ind2Keep
          n_sample_S1 = N_ELEMENTS(indS1)
          IF ((n_sample_S2 NE n_sample_S1) AND (n_sample_S2 GT 0)) THEN STOP 
          ;those retrievals that were successful
          indS2s = indS2[WHERE(x.SOS20[indS2] NE -9999 AND x.EOS20[indS2] NE -9999, /NULL)]
          indS1s = indS1[WHERE(x.SOS20[indS1] NE -9999 AND x.EOS20[indS1] NE -9999, /NULL)]
          ;those retrievals that failed
          indS2f = indS2[WHERE(x.SOS20[indS2] EQ -9999 OR x.EOS20[indS2] EQ -9999, /NULL)]
          indS1f = indS1[WHERE(x.SOS20[indS1] EQ -9999 OR x.EOS20[indS1] EQ -9999, /NULL)]
          samp = 'InSeasNdviHetLT20'
          tit2 = 'inSeasonNDVI_Het<20%'
          suffix = 'inSeasNdviHetLT20'
        END
        2: BEGIN
          indS2 = WHERE((x.index EQ S2index) AND (x.crop_type EQ cropTypes[i]))
          ind = WHERE((x.maxGap[indS2] LT 30) AND $
                      (x.p2_err[indS2] LT 30) AND $
                      (x.p2_err[indS2] NE 0) AND $
                      (x.p5_err[indS2] LT 30) AND $
                      (x.p5_err[indS2] NE 0), n_sample_S2)
          indS2 = indS2[ind]
          ids = x.ID[indS2]
          ind2Keep = !NULL
          ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
          FOR ii = 0, n_sample_S2-1 DO BEGIN
            indNDVI = WHERE((x.ID EQ ids[ii]) AND (x.Index EQ 'NDVIm'))
            IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, indS2[ii]]
          ENDFOR
          indS2 = ind2Keep
          n_sample_S2 = N_ELEMENTS(indS2)
          
          indS1 = WHERE((x.index EQ S1index[k]) AND (x.crop_type EQ cropTypes[i]))
          ind = WHERE((x.maxGap[indS1] LT 30) AND $
                      (x.p2_err[indS1] LT 30) AND $
                      (x.p2_err[indS1] NE 0) AND $
                      (x.p5_err[indS1] LT 30) AND $
                      (x.p5_err[indS1] NE 0), n_sample_S1)
          indS1 = indS1[ind]
          ids = x.ID[indS1]
          ind2Keep = !NULL
          ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
          FOR ii = 0, n_sample_S1-1 DO BEGIN
            indNDVI = WHERE((x.ID EQ ids[ii]) AND (x.Index EQ 'NDVIm'))
            IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, indS1[ii]]
          ENDFOR
          indS1 = ind2Keep
          n_sample_S1 = N_ELEMENTS(indS1)
          ;IF ((n_sample_S2 NE n_sample_S1) AND (n_sample_S2 GT 0)) THEN STOP 
          ;those retrievals that were successful
          indS2s = indS2[WHERE(x.SOS20[indS2] NE -9999 AND x.EOS20[indS2] NE -9999, /NULL)]
          indS1s = indS1[WHERE(x.SOS20[indS1] NE -9999 AND x.EOS20[indS1] NE -9999, /NULL)]
          ;those retrievals that failed
          indS2f = indS2[WHERE(x.SOS20[indS2] EQ -9999 OR x.EOS20[indS2] EQ -9999, /NULL)]
          indS1f = indS1[WHERE(x.SOS20[indS1] EQ -9999 OR x.EOS20[indS1] EQ -9999, /NULL)] 
          samp = 'highQ_retrieval_inSeasNdviHetLT20'
          tit2 = 'maxGap, inSeasonNDVI_Het<20%, (p2err, p5err) <30 && !=0'
          suffix = 'highQ_retrieval_inSeasNdviHetLT20'
        END
      ENDCASE
      IF ((indS2s[0] EQ !NULL) OR (indS1s[0] EQ !NULL)) THEN STOP
      
      
      ;now I have to match them so that they point to the same samples
      idS2s = x.ID[indS2s]
      idS1s = x.ID[indS1s]
      MATCH, idS2s, idS1s, subS2s, subS1s, COUNT = count
      IF (count GT 0) THEN BEGIN ;it may happen that they are different items
        ;index of samples where both were successful
        indS2ss = indS2s[subS2s]
        indS1ss = indS1s[subS1s]
        IF (N_ELEMENTS(indS2ss) NE N_ELEMENTS(indS1ss)) THEN STOP
        n_ss = STRTRIM(N_ELEMENTS(indS2ss),2) ;both successful
      ENDIF ELSE BEGIN
        n_ss = 0
      ENDELSE
      
      ;here make a structure to store the difference in pheno timing for each polygon, ONLY FOR RVI_ad
      IF (S1index[k] EQ 'RVI_ad') THEN BEGIN
        poly_errors = CREATE_STRUCT('PolyID', LONARR(n_ss), 'CROP', STRARR(n_ss) + cropTypes[i], 'Lat', FLTARR(n_ss), 'Lon', FLTARR(n_ss), $
                                    'mean_cv',FLTARR(n_ss), 'prctHeterog', FLTARR(n_ss), 'Retrival_selection', STRARR(n_ss) + suffix,$
                                    'dSOS20', FLTARR(n_ss), 'dEOS20', FLTARR(n_ss), 'mean_abs_dev', FLTARR(n_ss))
      ENDIF
      ;find those that both failed
      ;check first that some failed
      IF ((indS2f NE !NULL) AND (indS1f NE !NULL)) THEN BEGIN
        idS2f = x.ID[indS2f]
        idS1f = x.ID[indS1f]
        MATCH, idS2f, idS1f, subS2f, subS1f, COUNT = count
        IF (count GT 0) THEN BEGIN
          ;index of samples where both failed
          indS2ff = indS2f[subS2f]
          indS1ff = indS1f[subS1f]
          IF (N_ELEMENTS(indS2ff) NE N_ELEMENTS(indS1ff)) THEN STOP
          n_ff = STRTRIM(N_ELEMENTS(indS2ff),2) ;both failed
        ENDIF ELSE BEGIN
          n_ff = 0
        ENDELSE
      ENDIF ELSE BEGIN
        n_ff = 0
      ENDELSE
      
      ;find those where S2 success and S1 fail
      IF ((indS2s NE  !NULL) AND (indS1f NE  !NULL)) THEN BEGIN
        idS2s = x.ID[indS2s]
        idS1f = x.ID[indS1f]
        MATCH, idS2s, idS1f, subS2s, subS1f, COUNT = count
        IF (count GT 0) THEN BEGIN 
          ;index of samples where s2 success and s1 fail
          indS2sf = indS2s[subS2s]
          indS1sf = indS1f[subS1f]
          IF (N_ELEMENTS(indS2sf) NE N_ELEMENTS(indS1sf)) THEN STOP
          n_sf = STRTRIM(N_ELEMENTS(indS2sf),2) ;both failed
        ENDIF ELSE BEGIN
          n_sf = 0
        ENDELSE
      ENDIF ELSE BEGIN
        n_sf = 0
      ENDELSE
      
      ;find those where S2 fail and S1 success
      IF ((indS2f NE  !NULL) AND (indS1s NE  !NULL)) THEN BEGIN
        idS2f = x.ID[indS2f]
        idS1s = x.ID[indS1s]
        MATCH, idS2f, idS1s, subS2f, subS1s, COUNT = count
        IF (count GT 0) THEN BEGIN
          ;index of samples where s2 success and s1 fail
          indS2fs = indS2f[subS2f]
          indS1fs = indS1s[subS1s]
          IF (N_ELEMENTS(indS2fs) NE N_ELEMENTS(indS1fs)) THEN STOP
          n_fs = STRTRIM(N_ELEMENTS(indS2fs),2) ;both failed
        ENDIF ELSE BEGIN
          n_fs = 0
        ENDELSE
      ENDIF ELSE BEGIN
        n_fs = 0
      ENDELSE
      
      str = STRTRIM(n_sample_S2,2) + ' samples, retr: ' +  STRTRIM(n_ss,2) + ' both, ' + $
            STRTRIM(n_ff,2) + ' none, ' + $
            STRTRIM(n_sf,2) + ' S2sS1f, ' + $
            STRTRIM(n_fs,2) + ' S2fS1s' 
      PRINTF, lunRetr, STRJOIN([cropTypes[i],samp,STRTRIM(n_sample_S2,2),STRTRIM(n_ss,2),STRTRIM(n_ff,2),STRTRIM(n_sf,2),STRTRIM(n_fs,2)], dlmtr)     
      tit2 = tit2 + ', ' + str
     
      symb = '.'
      ;SOS20
      strArrOut = [cropTypes[i],S1index[k],samp,n_ss]
      xx = x.SOS20[indS2ss]
      yy = x.SOS20[indS1ss]
      IF (S1index[k] EQ 'RVI_ad') THEN BEGIN
        poly_errors.PolyID = x.ID[indS2ss]
        poly_errors.mean_cv = x.mean_cv_inSeason[indS2ss]
        poly_errors.prctHeterog = x.prctHeterogObs_inSeason[indS2ss]
        poly_errors.dSOS20 = xx-yy
        poly_errors.Lat = x.lat[indS2ss]
        poly_errors.Lon = x.lon[indS2ss]
      ENDIF
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF 
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0,0.1,0.25]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND SOS20', YTITLE=S1index[k] + ' SOS20', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,1], DIMENSIONS=[800,800], XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
            
      
      ;EOS20
      xx = x.EOS20[indS2ss]
      yy = x.EOS20[indS1ss]
      ;I only checked SOS, so -9999 may be present in EOS and LGS
      ret = removeNodataCouplesFrom2Array(xx, yy, -9999)
      xx = ret.x
      yy = ret.y 
      IF (S1index[k] EQ 'RVI_ad') THEN BEGIN
        poly_errors.PolyID = x.ID[indS2ss[ret.subOriginalKept]]
        poly_errors.dEOS20 = xx-yy
        FOR p = 0, N_ELEMENTS(poly_errors.PolyID) - 1 DO BEGIN
          poly_errors.mean_abs_dev[p] = MEAN([ABS(poly_errors.dSOS20[p]),ABS(poly_errors.dEOS20[p])])
          PRINTF, lunPolyErrors, STRJOIN([STRING(poly_errors.PolyID[p]),poly_errors.CROP[p], STRING(poly_errors.Lat[p]),STRING(poly_errors.Lon[p]), $
                                          STRING(poly_errors.mean_cv[p]),STRING(poly_errors.prctHeterog[p]),poly_errors.Retrival_selection[p], STRING( poly_errors.dSOS20[p]), $
                                          STRING(poly_errors.dEOS20[p]), STRING(poly_errors.mean_abs_dev[p])], dlmtr)
        ENDFOR
      ENDIF
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0.1,0.1,0.25]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND EOS20', YTITLE=S1index[k] + ' EOS20', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,3], /CURRENT, XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      ;LGS20
      xx = x.LGS20[indS2ss]
      yy = x.LGS20[indS1ss]
      ;I only checked SOS, so -9999 may be present in EOS and LGS
      ret = removeNodataCouplesFrom2Array(xx, yy, -9999)
      xx = ret.x
      yy = ret.y
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0.2,0.1,0.15]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND LGS20', YTITLE=S1index[k] + ' LGS20', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,5], /CURRENT, XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10

      ;SOS50
      xx = x.SOS50[indS2ss]
      yy = x.SOS50[indS1ss]
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0,0.1,0.25]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND SOS50', YTITLE=S1index[k] + ' SOS50', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,2], /CURRENT, XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      ;EOS50
      xx = x.EOS50[indS2ss]
      yy = x.EOS50[indS1ss]
      ;I only checked SOS, so -9999 may be present in EOS and LGS
      ret = removeNodataCouplesFrom2Array(xx, yy, -9999)
      xx = ret.x
      yy = ret.y
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0.1,0.1,0.25]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND EOS50', YTITLE=S1index[k] + ' EOS50', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,4], /CURRENT, XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10     
      
      ;LGS50
      xx = x.LGS50[indS2ss]
      yy = x.LGS50[indS1ss]
      ;I only checked SOS, so -9999 may be present in EOS and LGS
      ret = removeNodataCouplesFrom2Array(xx, yy, -9999)
      xx = ret.x
      yy = ret.y
      res = linregstat(xx, yy)
      r = 'n.a' & p = 'n.a' 
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        r = STRTRIM(res[2],2)
        p = STRTRIM(res[3],2)
      ENDIF
      es = error_stats(xx, yy)
      strArrOut = [strArrOut, r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
      ;now plot it
      mrg = [0.2,0.2,0.1,0.15]
      minMax = [MIN([xx,yy]), MAX([xx,yy])]
      h1 = PLOT(xx, yy, XTITLE='ND LGS50', YTITLE=S1index[k] + ' LGS50', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,6], /CURRENT, XRANGE=minMax, YRANGE=minmax)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
            
      PRINTF, lun, STRJOIN(strArrOut, dlmtr)
      ht = TEXT(0.5,0.975,tit, ALIGNMENT=0.5,FONT_SIZE= 16)
      ht = TEXT(0.5,0.95,tit2, ALIGNMENT=0.5,FONT_SIZE= 12)

      h1.save, dir_out + '\' + tmp + '_ND_'+S1index[k]+'_pheno_corr_' + suffix + '.png'
      h1.close
    ENDFOR ; k
  ENDFOR  ;i
ENDFOR  ;u
FREE_LUN, lun
FREE_LUN, lunRetr
FREE_LUN, lunPolyErrors 
RETURN, 0 
END