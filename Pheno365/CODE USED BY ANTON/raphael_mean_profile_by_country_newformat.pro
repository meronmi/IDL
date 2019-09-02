PRO RAPHAEL_mean_profile_by_country_newFormat, fn_sav_s1, fn_sav_s2

dir_out = 'D:\RAPHAEL_pheno_test\meanProfileByCountry'
xrng = [JULDAY(7, 1, 2017), JULDAY(4, 1, 2019)]


crop_set = raphael_crop_select()
stime = JD2DDMMYYYY(SYSTIME(/JULIAN))
stimeStr = STRTRIM(stime[2],2) + '_' + STRTRIM(stime[1],2) + '_' + STRTRIM(stime[0],2)
dir_out = FILE_DIRNAME(fn_sav_s1) + '\meanProfileByCountry_' + stimeStr

FILE_MKDIR, dir_out
RESTORE, fn_sav_s1
RESTORE, fn_sav_s2
mrg = [0.05,0,0.2,0.2]

selected_indices = ['NDVI', 'CR', 'RVI']
;sub_selected_indices= [11,9,10]
colors = ['blue','red','green','orange', 'purple', 'dark blue', 'dark red', 'dark green', 'dark orange']
FOR k=0, N_ELEMENTS(selected_indices)-1 DO BEGIN
  FOR i = 0, N_ELEMENTS(crop_set)-1 DO BEGIN
    crop2process = crop_set[i]
    index2process = selected_indices[k]
    IF (index2process EQ 'NDVI') THEN s = s2 ELSE s = s1
    indCrop = WHERE(s.crop EQ crop2process, countCrop)
    ;countries
    tmp = s.nuts0[indCrop]
    uniqNuts0 = tmp[UNIQ(tmp, SORT(tmp))]
    latNuts0 = FLTARR(N_ELEMENTS(uniqNuts0))
    ;max 28 countries, order them by lat
    FOR nu = 0, N_ELEMENTS(uniqNuts0)-1 DO BEGIN
      indCropNuts0 = WHERE(s.nuts0[indCrop] EQ uniqNuts0[nu])
      latNuts0[nu] = MEAN(s.y[indCrop[indCropNuts0]])
    ENDFOR
    uniqNuts0 = uniqNuts0[SORT(latNuts0)]
    latNuts0 = latNuts0[SORT(latNuts0)]
    
      
    ;define parameters for compositing
    ;min and max time for all sample in all nuts
    n_day_composite = 10
    xjdRange = [MIN(s.dateJD[indCrop]), MAX(s.dateJD[indCrop])]
    n_comp = FLOOR((xjdRange[1]-xjdRange[0]+1)/FLOAT(n_day_composite)) ;the last remaining days aredropped
    xjdcomp =  xjdRange[0] + FINDGEN(n_comp) * n_day_composite
    c = -1
    hndl = !NULL
    allAvgs = FLTARR(n_comp, N_ELEMENTS(uniqNuts0))
    pos = 0
    FOR nu = 0, N_ELEMENTS(uniqNuts0)-1 DO BEGIN   
      c = c + 1 
      IF (c EQ 5) THEN c = 0
      ;get the number of samples in this nut
      ind = WHERE((s.crop EQ crop2process) AND (s.nuts0 EQ uniqNuts0[nu]))
      tmp = s.pointID[ind]
      uniqIds = tmp[UNIQ(tmp, SORT(tmp))]
      ;make the big matrix
      ymat = FLTARR(n_comp, N_ELEMENTS(uniqIds))*!VALUES.F_NAN
      ;fill it
      FOR id = 0, N_ELEMENTS(uniqIds)-1 DO BEGIN
        indSamp = WHERE(s.pointID[ind] EQ uniqIds[id])
        indSamp= ind[indSamp]
        FOR d = 0, n_comp-1 DO BEGIN
          indSampDek = WHERE((s.dateJD[indSamp] GE  xjdcomp[d]) AND (s.dateJD[indSamp] LT  xjdcomp[d] + n_day_composite), count)
          IF (count GT 0) THEN BEGIN
            CASE selected_indices[k] OF
              'NDVI': ts = s.NDVI_mean[indSamp[indSampDek]]
              'CR': ts = s.CR[indSamp[indSampDek]]
              'RVI': ts = s.RVI[indSamp[indSampDek]]
            ENDCASE
            ymat[d,id] = MEAN(ts)
          ENDIF
        ENDFOR
      ENDFOR
      ;compute mean and sd by dek
      n_polygons = N_ELEMENTS(uniqIds)
      IF (n_polygons GT 1) THEN BEGIN 
        avg = MEAN(ymat, DIMENSION = 2, /NAN) 
        allAvgs[*,nu] = avg    
        sd = STDDEV(ymat, DIMENSION = 2, /NAN)
      ENDIF ELSE BEGIN
        avg = ymat
        allAvgs[*,nu] = avg
        sd =  ymat*!VALUES.F_NAN
      ENDELSE
      ;plot it
      name = uniqNuts0[nu] + ', Lat '+ STRTRIM(latNuts0[nu],2) + ', n=' + STRTRIM(n_polygons,2)
      IF (nu MOD 5 EQ 0) THEN BEGIN
        pos = pos + 1
        IF (nu EQ 0) THEN BEGIN
          h1 = ERRORPLOT(xjdcomp, avg, sd, XTITLE='Time', YTITLE=index2process,  XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', THICK = 2, NAME = name, $
            LINESTYLE='-', COLOR=colors[c], ERRORBAR_COLOR=colors[c], ERRORBAR_CAPSIZE= 0.1, MARGIN = mrg, LAYOUT=[1,6,pos], DIMENSIONS=[1200,1500], XMINOR=5, XRANGE=xrng) ;, YRANGE=minmax)
        ENDIF ELSE BEGIN
          h1 = ERRORPLOT(xjdcomp, avg, sd, XTITLE='Time', YTITLE=index2process,  XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', THICK = 2, NAME = name, $
            LINESTYLE='-', COLOR=colors[c], ERRORBAR_COLOR=colors[c], ERRORBAR_CAPSIZE= 0.1, MARGIN = mrg, LAYOUT=[1,6,pos], /CURRENT, XMINOR=5, XRANGE=xrng)
        ENDELSE
        hndl = [hndl, h1]
      ENDIF ELSE BEGIN
        h1 = ERRORPLOT(xjdcomp, avg, sd, XTITLE='Time', YTITLE=index2process,  XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', THICK = 2, NAME = name, $
          LINESTYLE='-',  COLOR=colors[c], ERRORBAR_COLOR=colors[c], ERRORBAR_CAPSIZE= 0.1, MARGIN = mrg, LAYOUT=[1,6,pos], DIMENSIONS=[1200,1500], /OVERPLOT) ;, YRANGE=minmax)
        hndl = [hndl, h1]
      ENDELSE
      
      ; all others 
      
      IF (c EQ 4) OR (nu EQ N_ELEMENTS(uniqNuts0)-1) THEN BEGIN ;add a legnd
        ;hl = LEGEND(TARGET=hndl, POSITION=[0.5, 0.01], LINESTYLE='', TRANSPARENCY = 75, ORIENTATION = 1, HORIZONTAL_ALIGNMENT=0.5)
        yr = h1.YRANGE
        hcover = PLOT(xjdRange, [yr[1],yr[1]], FILL_BACKGROUND=1, FILL_COLOR='white', FILL_TRANSPARENCY=30,FILL_LEVEL=yr[0], YRANGE = yr, /OVERPLOT)
        cc = -1
        IF (c EQ 4) THEN BEGIN
          jstart = nu-4
          jend = nu
        ENDIF ELSE BEGIN
          jstart = jend+1
          jend = N_ELEMENTS(uniqNuts0)-1
        ENDELSE
        FOR j = jstart, jend DO BEGIN
          cc = cc + 1
          hrewrite = PLOT(xjdcomp, allAvgs[*,j], LINESTYLE='-', COLOR=colors[cc], THICK = 2.5, /OVERPLOT)
        ENDFOR
        hl = LEGEND(TARGET=hndl, LINESTYLE='', SAMPLE_WIDTH=0.05, THICK = 2.5, POSITION= [0.8,0.99-(pos-1)*0.1675], HORIZONTAL_ALIGNMENT='LEFT');, TRANSPARENCY = 75, ORIENTATION = 1)
        ;allAvgs[*,*] = !VALUES.F_NAN
        hndl = !NULL
      ENDIF
      
      
      ;h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      ;hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='black', LINESTYLE='--',/OVERPLOT)
      ;hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      PRINT, nu
    ENDFOR ;nu
    ht = TEXT(0.5,0.975,crop2process, ALIGNMENT=0.5,FONT_SIZE= 16)
    h1.save, dir_out + '\' + crop2process + '_'+index2process+'_mean_profile.png'
    h1.close
  ENDFOR  ;i crop
ENDFOR ;index

END
