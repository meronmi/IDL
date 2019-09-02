PRo Cap_histo
  csv_file = 'D:\CAp_pheno_test\NDVI_time_series_for_parcels_confirmed_S2_L1C.csv'
  RESTORE, FILE_DIRNAME(csv_file) + '\RESULTS\pheno_run.sav'
  ;make histograms
  clrs = ['red','blue','green']
  h1_array = !NULL

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i], count)
    y = sos20[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='SOS20_DOY', NAME=cropTypes[i]+', n='+STRTRIM(count,2), DIMENSIONS=[1200,600], LAYOUT=[4,2,1])
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='SOS20_DOY', NAME=cropTypes[i]+', n='+STRTRIM(count,2), /OVERPLOT)
    ENDELSE
    h1_array = [h1_array, h1]
  ENDFOR
  hl = LEGEND(TARGET=h_array, POSITION=[0.95,0.95], LINESTYLE='')

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i])
    y = eos20[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='EOS20_DOY', NAME=cropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,2], /CURRENT)
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='EOS20_DOY', NAME=cropTypes[i], /OVERPLOT)
    ENDELSE
  ENDFOR

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i])
    y = lsg20[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='LSG20_days', NAME=cropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,3], /CURRENT)
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='LSG20_days', NAME=cropTypes[i], /OVERPLOT)
    ENDELSE
  ENDFOR

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i])
    y = eos50[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='EOS50_DOY', NAME=cropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,5], /CURRENT)
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='EOS50_DOY', NAME=cropTypes[i], /OVERPLOT)
    ENDELSE
  ENDFOR

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i])
    y = slopeDown[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='slope-senesc', NAME=cropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,6], /CURRENT)
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='slope-senesc', NAME=cropTypes[i], /OVERPLOT)
    ENDELSE
  ENDFOR

  FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
    ind = WHERE(cro_type EQ cropTypes[i])
    y = maxFitNDVI[ind]
    pdf = HISTOGRAM(y, NBINS=20, LOCATIONS=xbin)
    pdf = pdf / FLOAT(TOTAL(pdf))
    IF (i EQ 0) THEN BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='maxFitNDVI', NAME=cropTypes[i], DIMENSIONS=[1200,600], LAYOUT=[4,2,7], /CURRENT)
    ENDIF ELSE BEGIN
      h1 = PLOT(xbin, pdf, COLOR=clrs[i], YTITLE='Frequency', XTITLE='maxFitNDVI', NAME=cropTypes[i], /OVERPLOT)
    ENDELSE
  ENDFOR
  h1.save, FILE_DIRNAME(csv_file) + '\RESULTS\Histo_pheno_params.png'
  PRINT, 'Ended'
  h1.close

END