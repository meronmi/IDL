FUNCTION FitRes_old, x, y, doPlot, title, ytitle
;Compute and optionally plots the fitting stats at the global level
res = FLTARR(3)
gain = REGRESS(REFORM(x), REFORM(y), CONST=offset, CORRELATION=r)
res = [r^2, gain, offset]
IF (doPlot EQ 1) THEN BEGIN
  ;window, /FREE, TITLE=title[j], XSIZE = 1200, YSIZE = 400
  ;!P.Multi=[0,3,1]
  xrange = [MIN([x,y]) - (MAX([x,y])-MIN([x,y]))/10.0, MAX([x,y]) + (MAX([x,y])-MIN([x,y]))/10.0]
  yrange = xrange
;  gh =  plot(REFORM(x), REFORM(y),TITLE = title, FONT_SIZE=32, FONT_STYLE=1)
  plot, REFORM(x), REFORM(y), PSYM = 3, $
      TITLE = title,CHARSIZE=2.5, BACKGROUND = 16777215, COLOR = 0, XRANGE=xrange, YRANGE=yrange, XTITLE='GIMMS', YTITLE=ytitle
  oplot, [0,1000000],[0,1000000],  COLOR = 0,  LINESTYLE=2 ;
  oplot, xrange, xrange*gain[0]+offset, color = 255L + 256*0L + (256^2)*0L, LINESTYLE=0
ENDIF
RETURN, res
END

FUNCTION FitRes, x, y, doPlot, title, ytitle
;Compute and optionally plots the fitting stats at the global level
res = FLTARR(3)
gain = REGRESS(REFORM(x), REFORM(y), CONST=offset, CORRELATION=r)
res = [r^2, gain, offset]
IF (doPlot EQ 1) THEN BEGIN

  xrange = [FLOOR(MIN([x,y]) - (MAX([x,y])-MIN([x,y]))/10.0), CEIL(MAX([x,y]) + (MAX([x,y])-MIN([x,y]))/10.0)]
  yrange = xrange
  
  ;gh =  plot(REFORM(x), REFORM(y),TITLE = title, FONT_SIZE=16, FONT_STYLE=1, ASPECT_RATIO=1, $
  gh =  plot(REFORM(x), REFORM(y), FONT_SIZE=16, FONT_STYLE=1, ASPECT_RATIO=1, $
             LINESTYLE="none", SYMBOL="dot", XTITLE='GIMMS', YTITLE=ytitle,  XRANGE=xrange, YRANGE=yrange)
  ;plot, REFORM(x), REFORM(y), PSYM = 3, $
  ;    TITLE = title,CHARSIZE=2.5, BACKGROUND = 16777215, COLOR = 0, XRANGE=xrange, YRANGE=yrange, XTITLE='GIMMS-Chen', YTITLE=ytitle
  gh = plot([0,1000000],[0,1000000],  COLOR = "blue",  LINESTYLE=2,  XRANGE=xrange, YRANGE=yrange, /OVERPLOT)
  gh = plot(xrange, xrange*gain[0]+offset, color = "red", LINESTYLE=0,  XRANGE=xrange, YRANGE=yrange, /OVERPLOT)
  ;print,  'K:\ILRI\Comparison results\aggregate files\Results\'+title+'_'+ytitle+'.png'
  gh.save, 'K:\ILRI\Comparison results\aggregate files\Results\'+title+'_'+ytitle+'.png'
  print, title
  IF (title NE 'avgNDVI (LRLD + SRSD)') THEN gh.close
ENDIF
RETURN, res
END


FUNCTION predDIVres, x, y, years
;Compute  fitting and cross val stats at the division level
dims = SIZE(y)
nYears =N_ELEMENTS(uniqlist(y[*, 0]))
res = { $
  id: INTARR(dims[2]), $
  r2: FLTARR(dims[2]), $
  r2cv: FLTARR(dims[2]), $
  gain: FLTARR(dims[2]), $
  offset: FLTARR(dims[2]), $
  residuals: FLTARR(dims[2],nYears)*!VALUES.F_NAN}
;res = FLTARR(5, dims[2])  ;div_id, R2, R2cv, gain, offset
FOR i = 0, dims[2]-1 DO BEGIN
  ;extract the data for the division
  xd = x[*, i] & yd = y[*, i] ; years is already fine
  gain = REGRESS(REFORM(xd), REFORM(yd), CONST=offset, CORRELATION=r)
  ;if i eq 89 then stop
  rescv = cv_stats_lo_year_o(REFORM(xd), REFORM(yd), years) ; returned [cvr2,rmse,mod_eff]
  res.id[i] = i+1
  res.r2[i] = r^2
  res.r2cv[i] = rescv.r2cv
  res.gain[i] = gain
  res.offset[i] = offset
  res.residuals[i,*] = rescv.residuals
  ;res[*,i] = [i+1, r^2, rescv[0], gain, offset]
ENDFOR
RETURN, res
END

FUNCTION DivWithinRes, x, y, years, season_id
;Compute  fitting and cross val stats at the division level
dims = SIZE(y)
nGroups =N_ELEMENTS(uniqlist(season_id))
nYears =N_ELEMENTS(uniqlist(years))
;res = FLTARR(2+nGroups, dims[2])  ;div_id, R2cv_within, R2cv_pooled_model_group1,R2cv_pooled_model_group2, ..
res = {$
  id: INTARR(dims[2]), $
  r2cv_within: FLTARR(dims[2]), $
  r2cv_pooled_on_group: FLTARR(nGroups, dims[2]),$
  residualsL: FLTARR(nYears,dims[2])*!VALUES.F_NAN, $
  residualsS: FLTARR(nYears,dims[2])*!VALUES.F_NAN} 
;uncomment to compute the mean and Coeff of var
;meanVal = FLTARR(dims[2])
;CVVal = FLTARR(dims[2])
FOR i = 0, dims[2]-1 DO BEGIN
  ;extract the data for the division
  xd = x[*, i] & yd = y[*, i] ; years is already fine
  ;rescv_within = cv_stats_lo_year_o_within2(REFORM(xd), REFORM(yd), years, season_id) ; returned [cvr2,rmse,mod_eff]
  ;if i eq 99 then stop
  rescv_within = cv_stats_lo_year_o_within(REFORM(xd), REFORM(yd), years, season_id) ; returned [cvr2,rmse,mod_eff]
  res.id[i]=i+1
  res.r2cv_within[i] = rescv_within.cvr2_within
  res.r2cv_pooled_on_group[*,i]=rescv_within.cvr2pooled_model_by_group
  res.residualsL[0:N_ELEMENTS(REFORM(rescv_within.residuals[0,*]))-1,i]=rescv_within.residuals[0,*]
  res.residualsS[0:N_ELEMENTS(REFORM(rescv_within.residuals[1,*]))-1,i]=rescv_within.residuals[1,*]
  ;res[*,i] = [i+1, rescv_within]=rescv_within
  ;uncomment to compute the mean and Coeff of var
  ;meanVal[i] = MEAN(yd, /DOUBLE)
  ;CVVal[i] = STDDEV(yd, /DOUBLE)/meanVal[i]*100.0
ENDFOR
RETURN, res
END

PRO wrap
FOR q=0,0 DO compare_aggregate2, q  ;5
END 
    
PRO compare_aggregate2, base
;compare aggregate results (as .csv from Anton's code) against GIMMS 
;Note that GIMMS is multiplied by 10 because Anton divided by 10 to keept it as integer 
CLOSE, /ALL
;***********************************************************
; set pah and file_names
path = 'K:\ILRI\Comparison results\aggregate files'
gimms_base_fn = 'aggregated_GIMMS.csv'
archives_base_fn = ['aggregated_MODIS_C5_TERRA.csv','aggregated_BOKU_C5_TERRA.csv','aggregated_BOKU_C5_TERRAandAQUA.csv', 'aggregated_eMODIS.csv', 'aggregated_SPOT.csv','aggregated_MODIS_C5_AQUA.csv']     ;file names of the archives to be compared against gimms
fig_names = ['MODIS!DT-NASA', 'MODIS!DT-BOKU', 'MODIS!DT+A-BOKU', 'eMODIS!DT', 'SPOT-VGT', 'MODIS!DA-NASA']
; number of periods summed to get the cumulative values [GIMMS, MODIS_C5_Terra, BOKU_C5_TERRA, BOKU_C5_TERRAandAQUA] 
n_obs_long =  FLOAT([14,13,13,26,21,21,13])
n_obs_short = FLOAT([10,10,10,20,15,15,10]) 
ndiv = 84   ;number of divisions
which = ['cumNDVI','maxNDVI']                   ; select which to aggregate
which = which[0]
doplot = 1;0
;base = 5                   ; give number between 0 and 4, 0=GIMMS, 1=aggregated_MODIS_C5_TERRA, .. 5= last one now in wrap
;***********************************************************

;Matrixes for r test
;global
IF (base GT 0) THEN BEGIN
  RESTORE, 'K:\ILRI\Comparison results\aggregate files\Results\r_glob.sav'
ENDIF ELSE BEGIN
  r2_gx = FLTARR(6,4)  ;6 archives, 4 different r2 (r2, r2cv, r2cv_wd, r2_cv_wd_ws
  r2_xy = FLTARR(6,6, 4) ;as above but 6x6
  arcs = fig_names
  ;     a1  a2  a3  ..  a6
  ; a1
  ; a2
  ; ..
  ; a6
  ; This variables are filled at each run (base from 0 to 5 and saved into an idl variable
  ; after running 6 times the code, restore the variable and make the analysis
ENDELSE

; adapt the files to take another as baseFile (even though we call the variable still gimms_base_fn)
IF base gt 0 THEN BEGIN
  gimms_base_fn = archives_base_fn[base-1]
  archives_base_fn = archives_base_fn[base:*]
  n_obs_long =  n_obs_long[base:*]
  n_obs_short = n_obs_short[base:*]
  fig_names = fig_names[base:*]
ENDIF


; adjust names according to which
gimms_fn = path + '\'+ which + '_' + gimms_base_fn
archives_fn = path + '\'+ which + '_' + archives_base_fn
outFile = path + '\Results\' + which + '_' + gimms_base_fn + '_GLOBcomparision02_07bis.csv' ;regressions at the global level, both cum and average  
outFile2 = path + '\Results\'+ which + '_' + gimms_base_fn +'_DIVcomparision02_07bis.csv' ;regressions at the division level, both cum and average

;open outputs
IF FILE_TEST(outFile) eq 1 THEN FILE_DELETE, outFile
OPENW, W1, outFile, /GET_LUN
PRINTF, W1, 'Archive(dependent var),L+S R2,L+S R2cv,L+S R2cv_withinDiv,L+S R2cv_withinDivSeas,L+S gain,L+S offset,L R2,L R2cv,L R2cv_within,L gain,L offset,S R2,S R2cv,S R2cv_within,S gain,S offset, L+S rmse_cv, L+S bias_cv' 

IF FILE_TEST(outFile2) eq 1 THEN FILE_DELETE, outFile2
OPENW, W2, outFile2, /GET_LUN
PRINTF, W2, 'Archive(dependent var),DIV_ID,L+S R2,L+S R2cv,L+S R2cv_within,rmse_cv_sp,bias_cv_sp,'+ $ 
            'L+S DSP_ModelR2cv,rmse_cv_dsp,bias_cv_dsp,p-val(DPSvsSP),DSP_better_at_SP(R2cv&RMSE),L+S gain,L+S offset,' + $
            'L R2,L NP_ModelR2cv,L SP_ModelR2cv,rmse_cv_np_l, bias_cv_np_l,L p-val (SPdiffNP),rmse_cv_sp_l,bias_cv_sp_l,SP_better_at_L(R2cv&RMSE),L gain,L offset,' + $
            'S R2,S NP_ModelR2cv,S SP_ModelR2cv,rmse_cv_np_s,bias_cv_np_s,S p-val (SPdiffNP),rmse_cv_sp_s, bias_cv_sp_s,SP_better_at_S(R2cv&RMSE),S gain,S offset'

years_mat = 1981 + INDGEN(33)  ;1981-2013
gimms_mat_Long = FLTARR(N_ELEMENTS(years_mat), ndiv)*0-999  ; dim 2 is divisions
gimms_mat_Short = FLTARR(N_ELEMENTS(years_mat), ndiv)*0-999  ; dim 2 is divisions

;loop on different archives to be tested against GIMMS
FOR i = 0, N_ELEMENTS(archives_fn) -1 DO BEGIN
  ytitle = STRMID(archives_base_fn[i], 11, STRLEN(archives_base_fn[i])-15)
  ;open read GIMMS
  read_file, gimms_fn, gimms_mat_Long, gimms_mat_Short, years_mat

  ; ADDITION ANTON+MICHELE: adapt gimms_mat variable to NDVI units if one of the other becomes the base (gimms_mat)
  IF NOT((gimms_base_fn EQ 'aggregated_eMODIS.csv') OR (gimms_base_fn EQ 'aggregated_SPOT.csv') $
          OR gimms_base_fn EQ 'aggregated_GIMMS.csv') THEN BEGIN
    gimms_mat_Long =  FLOAT(gimms_mat_Long)  /10000.0
    gimms_mat_Short = FLOAT(gimms_mat_Short) /10000.0
  ENDIF
    
  ;open read other archive
  archive_mat_Long = gimms_mat_Long * 0 - 999
  archive_mat_Short = gimms_mat_Short * 0 - 999
  read_file, archives_fn[i], archive_mat_Long, archive_mat_Short, years_mat 
  
  ;extract the overallapping period, reduced to a common period 
  ;ranging from 2001 Long to 2011 Long to restrict it to eMODIS range
  ;Note that BOKU terra&aqua will have less data (it starts in 2002)
;  indL = WHERE((archive_mat_Long[*,0] NE -999) AND (gimms_mat_Long[*,0] NE -999) $
;                AND (years_mat GE 2001) AND (years_mat LE 2011))
;  indS = WHERE((archive_mat_Short[*,0] NE -999) AND (gimms_mat_Short[*,0] NE -999)$
;                AND (years_mat GE 2001) AND (years_mat LE 2010))
  ;MM 21 June 2013. With Anton it's decided to work on a common temporal window 2002 S - 2011 L
  ;based on the shortest archive (ie., MODIS Aqua)
  indL = WHERE((archive_mat_Long[*,0] NE -999) AND (gimms_mat_Long[*,0] NE -999) $
                AND (years_mat GE 2003) AND (years_mat LE 2011))
  indS = WHERE((archive_mat_Short[*,0] NE -999) AND (gimms_mat_Short[*,0] NE -999)$
                AND (years_mat GE 2002) AND (years_mat LE 2010))
  
  ;in the last version GIMMS, SPOT and eMODIS, they are in NDVI units, make the other archive in NDVI units as well
  IF NOT((archives_base_fn[i] EQ 'aggregated_eMODIS.csv') OR (archives_base_fn[i] EQ 'aggregated_SPOT.csv')) THEN BEGIN
    archive_mat_Long[indL,*] =  FLOAT(archive_mat_Long[indL,*])  /10000.0
    archive_mat_Short[indS,*] = FLOAT(archive_mat_Short[indS,*]) /10000.0
  ENDIF
  
  ;In the case of cumulated NDVI consider the cumulation and the mean
  ;here below we set the denominator (01 for the cum, = nobs in the two seasons for the mean)
  IF (which EQ 'cumNDVI') THEN BEGIN
    n_obs_long_archive = FLOAT([1, n_obs_long[i+1]])
    n_obs_short_archive = FLOAT([1, n_obs_short[i+1]]) 
    n_obs_long_gimms = FLOAT([1, n_obs_long[0]])
    n_obs_short_gimms = FLOAT([1, n_obs_short[0]])
    title = [archives_base_fn[i], 'average_' + archives_base_fn[i]]
    which2 = ['cumNDVI','avgNDVI']
  ENDIF ELSE BEGIN
    n_obs_long_archive = 1.0
    n_obs_short_archive = 1.0
    n_obs_long_gimms = 1.0
    n_obs_short_gimms = 1.0
    title = archives_base_fn[i]
  ENDELSE
  
  ;loop on the two cases of cumulated NDVI: the cumulation and the mean
  FOR j=0, N_ELEMENTS(n_obs_long_archive)-1 DO BEGIN
    PRINT, 'Analyzing -- ' + title[j] 
    ;compute and print stats
;    IF (doplot EQ 1) THEN BEGIN
;      window, /FREE, TITLE=title[j], XSIZE = 1200, YSIZE = 365
;      !P.Multi=[0,3,1]
;    ENDIF
  
    ;CASE 1: long and short toghether
    y = [archive_mat_Long[indL, *]/n_obs_long_archive[j],archive_mat_Short[indS, *]/n_obs_short_archive[j]]
    x = [gimms_mat_Long[indL, *]/n_obs_long_gimms[j],gimms_mat_Short[indS, *]/n_obs_short_gimms[j]]
    ;GLOBAL Stats
    ;compute cross val R2 leaving one full year out
    CVresLS = crossValRes(x, y, [years_mat[indL],years_mat[indS]])
    rmse_cv = SQRT(TOTAL((CVresLS.residuals)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(CVresLS.residuals)))
    bias_cv = TOTAL(CVresLS.residuals, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(CVresLS.residuals))
    ;remove effect of division
    CV_within_resLS = crossValWithinRes(x, y, [years_mat[indL],years_mat[indS]])
    ; in CV_within_resLS.residuals I have the rsiduals of the global model when applied at division level

    CV_withinDivAndSeason_resLS = crossValWithinDivAndSeasonRes(x, y, [years_mat[indL],years_mat[indS]], [years_mat[indL]*0+0,years_mat[indS]*0+1])
    R2cvDSPmodel_div_level = CV_withinDivAndSeason_resLS[1 : N_ELEMENTS(CV_withinDivAndSeason_resLS)-1]
    ;DIVresWithinLS = CV_withinDivAndSeason_resLS[0]
    ;compute fitting stats
    dims = SIZE(y)
    FITresLS = FitRes(REFORM(x, dims[1]*dims[2], 1),  REFORM(y, dims[1]*dims[2], 1), doplot, which2[j] + ' (LRLD + SRSD)', fig_names[i])
    ;for response to rev
    
    IF (j EQ 1) THEN BEGIN
      ffnn = 'X:\works\pubblicazioni\in preparazione\2013 Index Insurance\JAG last\for revision\GIMMS-' + fig_names[i] + '.csv'
      gimms = REFORM(TRANSPOSE(REFORM(x, dims[1]*dims[2], 1)))
      other = REFORM(TRANSPOSE(REFORM(y, dims[1]*dims[2], 1)))
      data = DBLARR(2,N_ELEMENTS(gimms))
      data[0,*]=gimms
      data[1,*]=other
      WRITE_CSV, ffnn, data
    END
    ;DIV STATS
    ;R2 cv of the SP model
    DIVresLS = predDIVres(x, y, [years_mat[indL],years_mat[indS]])
    ;R2cv of the SP model when applyed by season
    DIVresWithinLS = DivWithinRes(x, y, [years_mat[indL],years_mat[indS]], [years_mat[indL]*0+0,years_mat[indS]*0+1]);pass a seson identificator
    ;resduals of pooled applied to seasons are stored in
    ;DIVresWithinLS.residualsL[*,i]=rescv_within.residulas[0,*]
    ;DIVresWithinLS.residualsS[*,i]=rescv_within.residulas[1,*]
  
    ;CASE 1: only long             
    y = archive_mat_Long[indL, *]/n_obs_long_archive[j]
    x = gimms_mat_Long[indL, *]/n_obs_long_gimms[j]
    ;GLOBAL Stats
    ;compute cross val R2 leaving one full year out
    CVresL = crossValRes(x,y,years_mat[indL])
    CV_within_resL = crossValWithinRes(x, y, years_mat[indL])
    ;compute fitting stats
    dims = SIZE(y)
    FITresL = FitRes(REFORM(x, dims[1]*dims[2], 1),  REFORM(y, dims[1]*dims[2], 1), doplot, which2[j] + ' (LRLD)', fig_names[i])
    ;DIV STATS
    ;R2 of the NP model
    DIVresL = predDIVres(x, y, years_mat[indL])
    ;the residuals are in
    ;DIVresL.residuals[i,*]
    ;COMMENT/UNCOMMENT TO MAKE THE ANALYS OF THE LONG
    ;plotcolor, gimms_mat_Long[indL, *], archive_mat_Long[indL, *],  which + ' Long '
    
    ;CASE 3: only short
    y = archive_mat_Short[indS, *]/n_obs_short_archive[j]
    x = gimms_mat_Short[indS, *]/n_obs_short_gimms[j]
    ;GLOBAL Stats
    ;compute cross val R2 leaving one full year out
    CVresS = crossValRes(x,y,years_mat[indS])
    CV_within_resS = crossValWithinRes(x, y, years_mat[indS])
    ;compute fitting stats
    dims = SIZE(y)
    FITresS = FitRes(REFORM(x, dims[1]*dims[2], 1),  REFORM(y, dims[1]*dims[2], 1), doplot,  which2[j] + ' (SRSD)', fig_names[i])
   
    ;DIV STATS
    ;R2 of the NP model
    DIVresS = predDIVres(x, y, years_mat[indS])
    ;the residuals are in
    ;DIVresS.residuals[i,*]
    
    ;Print to file 
    ;GLOBAL STATS 
    PRINTF, W1, title[j] + ',' + STRTRIM(FITresLS[0],2) + ',' + STRTRIM(CVresLS.r2cv,2) + ',' + STRTRIM(CV_within_resLS.cvr2_within,2) + ',' + STRTRIM(CV_withinDivAndSeason_resLS[0],2)+ ',' + STRTRIM(FITresLS[1],2) + ',' + STRTRIM(FITresLS[2],2) + ',' + $
                                 STRTRIM(FITresL[0],2) +  ',' + STRTRIM(CVresL.r2cv,2)  + ','  + STRTRIM(CV_within_resL.cvr2_within,2) + ','+ STRTRIM(FITresL[1],2) + ','  + STRTRIM(FITresL[2],2) +  ',' + $
                                 STRTRIM(FITresS[0],2) +  ',' + STRTRIM(CVresS.r2cv,2)  + ','  + STRTRIM(CV_within_resS.cvr2_within,2) + ','+ STRTRIM(FITresS[1],2) + ','  + STRTRIM(FITresS[2],2) + ',' + $
                                 STRTRIM(rmse_cv,2)  +  ',' + STRTRIM(bias_cv,2)                                
    
    IF (j EQ 1) AND (base EQ 0) THEN BEGIN
      r2_gx[i, *] = [FITresLS[0], CVresLS.r2cv,CV_within_resLS.cvr2_within,CV_withinDivAndSeason_resLS[0]] 
    ENDIF
    IF (j EQ 1) AND (base GT 0) THEN BEGIN
      r2_xy[base+i, base-1, *] = [FITresLS[0], CVresLS.r2cv,CV_within_resLS.cvr2_within,CV_withinDivAndSeason_resLS[0]] 
    ENDIF
    ;DIV STATS
    sumDSP_better_at_SP = 0
    sumSP_better_at_L = 0
    sumSP_better_at_S = 0
    FOR k = 0, dims[2]-1 DO BEGIN
      ;for each division test that the SP model applied at the season level is sifnificantly different from the NP model (by season)
      resTestL = testModel(REFORM(DIVresWithinLS.residualsL[*,k]),REFORM(DIVresL.residuals[k,*]))
      resTestS = testModel(REFORM(DIVresWithinLS.residualsS[*,k]),REFORM(DIVresS.residuals[k,*]))
      
      ;SP and NP rmse and bias at the season level
      ;Long
      tmpResid = REFORM(DIVresWithinLS.residualsL[*,k])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_sp_l = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_sp_l = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      tmpResid = REFORM(DIVresL.residuals[k,*])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_np_l = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_np_l = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      ;Short
      tmpResid = REFORM(DIVresWithinLS.residualsS[*,k])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_sp_s = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_sp_s = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      tmpResid = REFORM(DIVresS.residuals[k,*])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_np_s = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_np_s = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      
      ;for each division test that the DSP model applied at the season level is sifnificantly different from the SP model (by division)
      ;resTestDSPvsSP = testModel(REFORM([DIVresWithinLS.residualsL[*,k],DIVresWithinLS.residualsS[*,k]]),REFORM(CV_within_resLS.residuals[k,*]))
      resTestDSPvsSP = testModel(REFORM(DIVresLS.residuals[k,*]),REFORM(CV_within_resLS.residuals[k,*]))
      ;a P <0.05 meand that there is correlation, and r2 of the two model are different
      ;compute bias and rmse
      tmpResid = REFORM(CV_within_resLS.residuals[k,*])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_dsp = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_dsp = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      tmpResid = REFORM(DIVresLS.residuals[k,*])
      indf = WHERE(FINITE(tmpResid), countf)
      IF (countf NE N_ELEMENTS(tmpResid)) THEN tmpResid= tmpResid[WHERE(FINITE(tmpResid))]
      rmse_cv_sp = SQRT(TOTAL((tmpResid)^2, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid)))
      bias_cv_sp = TOTAL(tmpResid, /DOUBLE, /NAN)/FLOAT(N_ELEMENTS(tmpResid))
      
      
      IF ((DIVresLS.r2cv[k] LT R2cvDSPmodel_div_level[k]) AND (rmse_cv_sp GT rmse_cv_dsp) ) THEN $ ;AND (ABS(bias_cv_sp) GT ABS(bias_cv_dsp))
        DSP_better_at_SP=1 ELSE DSP_better_at_SP=0
      IF ((DIVresL.r2cv[k] LT DIVresWithinLS.r2cv_pooled_on_group[0,k]) AND (rmse_cv_np_l GT rmse_cv_sp_l) ) THEN $ ;AND (ABS(bias_cv_np_l) GT ABS(bias_cv_sp_l))
        SP_better_at_L=1 ELSE SP_better_at_L=0
      IF ((DIVresS.r2cv[k] LT DIVresWithinLS.r2cv_pooled_on_group[1,k]) AND (rmse_cv_np_s GT rmse_cv_sp_s) ) THEN $ ;AND (ABS(bias_cv_np_s) GT ABS(bias_cv_sp_s))
        SP_better_at_S=1 ELSE SP_better_at_S=0  
      sumDSP_better_at_SP = sumDSP_better_at_SP + DSP_better_at_SP
      sumSP_better_at_L = sumSP_better_at_L + SP_better_at_L
      sumSP_better_at_S = sumSP_better_at_S + SP_better_at_S
      
      PRINTF, W2, title[j] + ',' + STRTRIM(DIVresLS.id[k], 2) +  ',' + $
                  STRTRIM(DIVresLS.r2[k], 2)            +  ','  + STRTRIM(DIVresLS.r2cv[k], 2) +  ','  + STRTRIM(DIVresWithinLS.r2cv_within[k],2)            + ',' + STRTRIM(rmse_cv_sp,2) + ',' + STRTRIM(bias_cv_sp,2) + ',' +$
                  STRTRIM(R2cvDSPmodel_div_level[k],2)  + ',' + STRTRIM(rmse_cv_dsp,2)         + ',' + STRTRIM(bias_cv_dsp,2)                                + ',' + STRTRIM(resTestDSPvsSP,2) + ','+ STRTRIM(DSP_better_at_SP,2) + ','+ STRTRIM(DIVresLS.gain[k], 2) +  ','  + STRTRIM(DIVresLS.offset[k], 2) +  ',' + $
                  STRTRIM(DIVresL.r2[k], 2)             +  ','  + STRTRIM(DIVresL.r2cv[k], 2)  +  ','  + STRTRIM(DIVresWithinLS.r2cv_pooled_on_group[0,k],2) + ',' + STRTRIM(rmse_cv_np_l,2)+ ',' + STRTRIM(bias_cv_np_l,2) + ',' + STRTRIM(resTestL,2) + ',' + STRTRIM(rmse_cv_sp_l,2) + ',' + STRTRIM(bias_cv_sp_l,2) + ',' +  STRTRIM(SP_better_at_L,2) + ',' + STRTRIM(DIVresL.gain[k], 2)  +  ','  + STRTRIM(DIVresL.offset[k], 2)  +  ',' + $                         
                  STRTRIM(DIVresS.r2[k], 2)             +  ','  + STRTRIM(DIVresS.r2cv[k], 2)  +','  + STRTRIM(DIVresWithinLS.r2cv_pooled_on_group[1,k],2) +  ','  + STRTRIM(rmse_cv_np_s,2)                             +  ','  + STRTRIM(bias_cv_np_s,2) +   ',' + STRTRIM(resTestS,2) + ',' + STRTRIM(rmse_cv_sp_s,2) + ',' + STRTRIM(bias_cv_sp_s,2) + ',' + STRTRIM(SP_better_at_S,2) + ',' + STRTRIM(DIVresS.gain[k], 2)  +  ','  + STRTRIM(DIVresS.offset[k], 2)
    ENDFOR
    ;PRINT, sumDSP_better_at_SP/FLOAT(dims[2])*100, sumSP_better_at_L/FLOAT(dims[2])*100, sumSP_better_at_S/FLOAT(dims[2])*100 
  ENDFOR
ENDFOR

FREE_LUN, W1
FREE_LUN, W2
save, r2_gx, r2_xy, arcs, FILENAME = 'K:\ILRI\Comparison results\aggregate files\Results\r_glob.sav'


END