PRO CAP_pheno_v2

@cb_optimization.comm

;read the file
csv_file = 'D:\CAP_pheno_test\for_michele_l2a\l2a_wb_ww_rape.csv';'D:\CAp_pheno_test\NDVI_time_series_update11March.csv'
res = READ_CSV(csv_file, HEADER=hdr)
x = rename_tags(res, TAG_NAMES(res), hdr)
;cropTypes = ['winter wheat - soft', 'sunflower', 'maize for grain', 'alfalfa','Permanenet grassland - pastures', $
;          'winter barley', 'winter rapeseed']
cropTypes = ['winter wheat - soft','winter rapeseed','winter barley']
;prepare for output
OPENW, lun, FILE_DIRNAME(csv_file) + '\RESULTS\pheno_results.csv',/GET_LUN
dlmtr = ','
PRINTF, lun, STRJOIN(['CROP_TYPE','ID','Area','SD@MaxNDVI','SOS20','EOS20','maxFitNDVI','LSG20','Slope_growth','Slope_decay','r_fir','RMSD_fit','nImages','maxGap','p0','p1','p2','p3','p4','p5','p6']+dlmtr)
cro_type= !NULL
sos20 = !NULL
maxFitNDVI = !NULL
eos20 = !NULL
eos50 = !NULL
lsg20 = !NULL
slopeUp = !NULL
slopeDown = !NULL

clrs = ['red','blue','green']
;hAll = PLOT([JULDAY(10,01,2016),JULDAY(10,15,2017)], [0,0], XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', $
;            LAYOUT=[1,3,1], XTITLE='Time', YTITLE='S2 L1C NDVI',DIMENSIONS=[1200,900],/NODATA)
hAllLeg = !NULL
;for each crop
FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
  indCrop = WHERE(x.crop_name EQ cropTypes[i], countCrop)
  IF (countCrop EQ 0) THEN STOP
  ;get the unique IDs of the crop
  tmp = x.id[indCrop]
  uniqIds = tmp[UNIQ(tmp, SORT(tmp))]
  ;for each single id
  FOR j = 0, N_ELEMENTS(uniqIds)-1 DO BEGIN
    ;get sub of the id
    ind = WHERE(x.id EQ uniqIds[j], count)
    ;get the date, NDVI, sd at max and area
    date = x.date[ind]
    dateJd = MMbDDdYYYY2jd(date)
    ;order date ascending
    subAsc = SORT(dateJd)
    dateJd = dateJd[subAsc]
    nd = x.mean[ind]
    maxNDVI = MAX(x.mean[ind], subMax)
    sdAtMax = x.stddev[ind[subMax]]
    nd = nd[subAsc]
    area = x.area[ind[0]]
    PRINT, uniqIds[j], ', ',cropTypes[i]
    ;make a plot with orginal data (all)
    IF (j EQ 0) THEN BEGIN
      IF (i EQ 0) THEN $
        hAll = PLOT(dateJd, nd, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=cropTypes[i], COLOR = clrs[i], TRANSPARENCY= 85, LAYOUT=[1,3,i+1], $
                    XRANGE=[JULDAY(10,01,2016),JULDAY(10,15,2017)], XTITLE='Time', YTITLE='S2 L1C NDVI',DIMENSIONS=[1200,900]) $
      ELSE $
        hAll = PLOT(dateJd, nd, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME=cropTypes[i], COLOR = clrs[i], TRANSPARENCY= 85, LAYOUT=[1,3,i+1], $
                    XRANGE=[JULDAY(10,01,2016),JULDAY(10,15,2017)], XTITLE='Time', YTITLE='S2 L1C NDVI', /CURRENT) 
      hAllLeg = [hAllLeg, hAll]
    ENDIF ELSE BEGIN
      hAll2 = PLOT(dateJd, nd, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', COLOR = clrs[i], LAYOUT=[1,3,i+1], TRANSPARENCY= 85, /OVERPLOT)
    ENDELSE
    h1 = PLOT(dateJd, nd, XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', NAME='S2', TITLE= cropTypes[i] + ', ID ' + STRTRIM(uniqIds[j],2), LINESTYLE='', SYMBOL='o')
    
    

    ;OLD STUFF
;    dateRange = [JULDAY(1,15,2017),JULDAY(7,15,2017)];[JULDAY(2,11,2016),JULDAY(2,15,2017)]  ; window range for finding phenology
;    baseDate = JULDAY(12,31,2016)  ; base date for writing info (so 1 = 1 January x)
;    pheno = phenoMainDouble(dateJd, nd, 2, dateRange, baseDate) 
;    dateTmp = INDGEN(pheno.jdInterval[1]-pheno.jdInterval[0])+pheno.jdInterval[0]     ; array with all days (JD) in the modelled year (automatically adapts for shorter years in case year started with decay)
;    fitNDVIseries = TANH_DOUBLE(dateTmp,pheno.PARAM_X)   
    
    ;NEW STUFF
    dateRange = [JULDAY(1,15,2017),JULDAY(7,15,2017)];[JULDAY(2,11,2016),JULDAY(2,15,2017)]  ; window range for finding phenology
    season = locate_largest_season_in_jd_range(dateJd, nd, dateRange)
    dateRange = [dateJd[season.subLeft],dateJd[season.subRight]];[JULDAY(2,11,2016),JULDAY(2,15,2017)]  ; window range for finding phenology
    baseDate = JULDAY(01,01,2017)  ; base date for writing info (so 1 = 1 January x)
    pheno = phenoFitOverKnownInterval(dateJd, nd, dateRange, dateJd[season.subMax], season.ySmoothAtSubLeft, season.ySmoothAtSubRight, season.ySmoothAtSubMax, baseDate)
    dateTmp = INDGEN(pheno.jdInterval[1]-pheno.jdInterval[0])+pheno.jdInterval[0]     ; array with all days (JD) in the modelled year (automatically adapts for shorter years in case year started with decay)
    fitNDVIseries = TANH_DOUBLE(dateTmp,pheno.PARAM_X)
    
    h2 = PLOT(dateTmp, fitNDVIseries, COLOR='r', NAME='DHT fit',/OVERPLOT)
    hl = LEGEND(TARGET=[h1,h2], POSITION=[0.3,0.85], LINESTYLE='')
    h1.save, FILE_DIRNAME(csv_file) + '\RESULTS\fit_' + cropTypes[i] + '_ID_' + STRTRIM(uniqIds[j],2)+'.png'
    h1.close
    PRINTF, lun, STRJOIN($
      ;['CROP_TYPE','ID','Area','SD@MaxNDVI','SOS20','EOS20','maxFitNDVI',
      [cropTypes[i], STRING([uniqIds[j], area, sdAtMax, pheno.SOS20, pheno.EOS20, pheno.maxNDVI,  $
      ;'LSG20','Slope_growth','Slope_decay','r_fir','RMSD_fit','nImages','maxGap']) $
      pheno.LGS20, pheno.PARAM_x[3], pheno.PARAM_x[6], pheno.PearsonCC, pheno.RMSD, pheno.nImages, pheno.maxGap, pheno.PARAM_X])] $
      +dlmtr)
    
    cro_type= [cro_type, cropTypes[i]]
    sos20 = [sos20,pheno.SOS20]
    maxFitNDVI = [maxFitNDVI, pheno.maxNDVI]
    eos20 = [eos20, pheno.EOS20]
    eos50 = [eos50, pheno.eos50]
    lsg20 = [lsg20,  pheno.LGS20]
    slopeUp = [slopeUp,pheno.PARAM_x[3]]
    slopeDown = [slopeDown, pheno.PARAM_x[6]]
    pheno = 0
    ;PRINT, 'debug' 
  ENDFOR
ENDFOR
hlAll = LEGEND(TARGET=hAllLeg, POSITION=[0.77,0.985], ORIENTATION= 1, LINESTYLE='')
hAll.save, FILE_DIRNAME(csv_file) + '\RESULTS\All_input_data.png'
hAll.close
SAVE, /ALL, FILENAME = FILE_DIRNAME(csv_file) + '\RESULTS\pheno_run.sav' 

FREE_LUN, lun
PRINT, 'Ended'
END