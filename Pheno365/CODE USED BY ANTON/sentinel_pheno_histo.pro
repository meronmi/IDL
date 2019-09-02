FUNCTION Sentinel_pheno_histo, dir
  vars =   ['SOS50', 'EOS50','LGS50','SOS20','p6','maxFitNDVI']
  titles = ['SOS50_DOY', 'EOS50_DOY','LGS50_DOY','SOS20_DOY','Slope-sen','maxFitNDVI']  
  ;dir =  'Z:\RAPHAEL_pheno_test\Results_2019May31';'D:\RAPHAEL_pheno_test\Results_2019May24'
  dir_out = dir + '\Pheno_Histograms'
  FILE_MKDIR, dir_out
  csv_file = dir + '\pheno_results.csv'
  res = READ_CSV(csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN )
  x = rename_tags(res, TAG_NAMES(res), [hdr[0:-2],'Empty'])
  PRINT, hdr
  ;make histograms
  ;clrs = ['red','blue','green']
  cropTypes = x.crop_type[UNIQ(x.crop_type,SORT(x.crop_type))]
  indices = x.index[UNIQ(x.index,SORT(x.index))]
  h1_array = !NULL
  ;n_biggest_to_select = 8
;  n_per_crop_type = INTARR(N_ELEMENTS(cropTypes))
;  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
;    ind = WHERE(x.crop_type EQ cropTypes[i], count)
;    n_per_crop_type[i] = count
;  ENDFOR
   
  ;IF (count GT 100) THEN  bigCropTypes = [bigCropTypes, cropTypes[i]]
  ;sub = REVERSE(SORT(n_per_crop_type))
  ;bigCropTypes = cropTypes[sub[0:n_biggest_to_select-1]]
  bigCropTypes = raphael_crop_select()

  ;make histograms
  ctable = COLORTABLE(13, NCOLORS = N_ELEMENTS(bigCropTypes), /TRANSPOSE)
  FOR k = 0, N_ELEMENTS(indices)-1 DO BEGIN
    prefix = indices[k]
    ;now regression with lat
    FOR u = 0, 1 DO BEGIN ;all data, only selected based on gaps and perror  
      FOR i = 0, N_ELEMENTS(bigCropTypes)-1 DO BEGIN 
        tmpName = STRSPLIT(bigCropTypes[i],'-',/EXTRACT)
        tmpName = STRMID(STRTRIM(tmpName[0],2),0,18)
        tit = 'Pheno from ' + indices[k] ;+ ', ' + STRTRIM(N_ELEMENTS(bigCropTypes),2) + ' crops'
        res = raphael_data_select(x, u, indices[k], bigCropTypes[i])
        ;ret = CREATE_STRUCT('indRetr', ind, 'countRetr',count, 'countRetrFailure', countRetrFailure, 'subTitle', subTitle, 'res.suffix', res.suffix) 

        ;y = x.sos20[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[0])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        nameStr = tmpName + ' s' + STRTRIM(res.countRetr,2) + ' f' + STRTRIM(res.countRetrFailure,2) 
        IF (i EQ 0) THEN BEGIN
          mrg = [0.25,0.05,0.05,0.2] ; [left, bottom, right, top]
          h1 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[0], NAME=nameStr, DIMENSIONS=[1200,600], LAYOUT=[4,2,1], MARGIN=mrg)
        ENDIF ELSE BEGIN
          h1.Select
          h1 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=nameStr, /OVERPLOT)
        ENDELSE
        h1_array = [h1_array, h1]
      
        ;y = x.eos20[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[1])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        IF (i EQ 0) THEN BEGIN
          h2 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[1], NAME=bigCropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,2], MARGIN=mrg, /CURRENT)
        ENDIF ELSE BEGIN
          h2.Select
          h2 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=bigCropTypes[i], /OVERPLOT)
        ENDELSE
        
        ;y = x.lsg20[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[2])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        IF (i EQ 0) THEN BEGIN
          h3 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[2], NAME=bigCropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,3], MARGIN=mrg, /CURRENT)
        ENDIF ELSE BEGIN
          h3.Select
          h3 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=bigCropTypes[i], /OVERPLOT)
        ENDELSE
      
        ;y = x.eos50[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[3])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        IF (i EQ 0) THEN BEGIN
          mrg = [0.25,0.125,0.05,0.14] ; [left, bottom, right, top]
          h4 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[3], NAME=bigCropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,5], MARGIN=mrg, /CURRENT)
        ENDIF ELSE BEGIN
          h4.Select
          h4 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=bigCropTypes[i], /OVERPLOT)
        ENDELSE
      
        ;y = x.p6[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[4])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        IF (i EQ 0) THEN BEGIN
          h5 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[4], NAME=bigCropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,6], MARGIN=mrg, /CURRENT)
        ENDIF ELSE BEGIN
          h5.Select
          h5 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=bigCropTypes[i], /OVERPLOT)
        ENDELSE
      
        ;y = x.maxFitNDVI[res.indRetr]
        y =  x.(WHERE(TAG_NAMES(x) EQ STRUPCASE(vars[5])))[res.indRetr]
        pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
        pdf = pdf / FLOAT(TOTAL(pdf))
        IF (i EQ 0) THEN BEGIN
          h6 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', XTITLE=titles[5], NAME=bigCropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,7], MARGIN=mrg, /CURRENT)
        ENDIF ELSE BEGIN
          h6.Select
          h6 = PLOT(xbin, pdf, COLOR=ctable[*,i], YTITLE='Frequency', NAME=bigCropTypes[i], /OVERPLOT)
        ENDELSE
      
      ENDFOR ;i
      ht = TEXT(0.5,0.965,tit, ALIGNMENT=0.5,FONT_SIZE= 16)
      ht = TEXT(0.5,0.925,res.subTitle, ALIGNMENT=0.5,FONT_SIZE= 14)
      hl = LEGEND(TARGET=h1_array, POSITION=[0.93,0.92], LINESTYLE='', SAMPLE_WIDTH=0.05)
      h1.save, dir_out + '\Histo_pheno_params_' + prefix + '_' + res.suffix + '.png'
      h1.close
    ENDFOR; k
  ENDFOR; u
  RETURN, 0
END