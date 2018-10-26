PRO plot_handler
  dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI'
  pos = [460, 821] ;crop in somalia

  year1 = 2010
  year2 = 2011
  xrng = [JULDAY(03,15,year1), JULDAY(03,15,year2)] ;use central day, special case Somalia
  ;year1 = 2006
  ;year2 = 2011
  ;xrng = [JULDAY(01,05,year1), JULDAY(12,25,year2)] ;use central day
  ;------------------------------end of user part---------------
  x = pos[0] & y = pos[1]
  res =  plot_profiles(dir, x, y, xrng, 1)
END

FUNCTION plot_profiles, dir, x, y, xrng, useSave
  ;xrng = [JULDAY(01,01,2003), JULDAY(12,31,2005)]
  strXrng = STRTRIM(JD2YEAR(xrng[0]),2)+'-'+STRTRIM(JD2YEAR(xrng[1]),2)
  gain = 0.0048
  offset = -0.2000
  ; EXAMPLE
  ;res =  plot_profiles('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI_old_with_missing', 542, 607)
  fn_Ox = 'O'+STRTRIM(INDGEN(5),2)+'2003-2016_bil'
  fn_OF =  'OF2003-2016_bil'
  fn_unfilt = 'O0unfilte2003-2016_bil'
  fn_unfiltUDM = 'O0UnfilteUDM_2003-2016_bil'
  fn_unconstr = 'O0Unsconstr2003-2016_bil'    ; 11 04 2018: be care now unScontrain becaus ethere was a type in tif2envi, now it should be ok and should be unconstrain in the nex run
  fn_active = '\ACTIVE\all_504bands_active.img'
  IF (useSave EQ 0) THEN BEGIN
    oFF = ReadBilWithHdr(dir + '\' + fn_OF)
    oFF = REFORM(oFF[x,y,*]) * gain + offset
    oX = FLTARR(5,N_ELEMENTS(oFF))
    FOR i = 0, N_ELEMENTS(fn_Ox) - 1 DO BEGIN
      oTmp = ReadBilWithHdr(dir + '\' + fn_Ox[i])
      oX[i,*] = REFORM(oTmp[x,y,*]) * gain + offset
    ENDFOR
    oTmp = 0
    uf = ReadBilWithHdr(dir + '\' + fn_unfilt)
    uf = REFORM(uf[x,y,*]) * gain + offset
    ufDM = ReadBilWithHdr(dir + '\' + fn_unfiltUDM)
    ufDM = REFORM(ufDM[x,y,*])
    ;Anja: The files *UDM.tif contain the date of the assigned observation counted in days starting from 1.1.1950.
    jduf = ufDM + JULDAY(01,01,1950)
    uc = ReadBilWithHdr(dir + '\' + fn_unconstr)
    uc = REFORM(uc[x,y,*]) * gain + offset
    active = ReadEnviWithHdr(dir + '\' + fn_active)
    active = REFORM(active[x,y,*])
    SAVE, oFF, oX, uf, ufDM, jduf, uc, active, $
      FILENAME = dir + '\' + 'plotsave' + STRTRIM(x,2) + '_' + STRTRIM(y,2) + '.sav'
  ENDIF ELSE BEGIN
    RESTORE, FILENAME = dir + '\' + 'plotsave' + STRTRIM(x,2) + '_' + STRTRIM(y,2) + '.sav'
  ENDELSE



  ;check that they have equal size
  IF ((N_ELEMENTS(REFORM(oX[0,*])) NE N_ELEMENTS(uf)) OR (N_ELEMENTS(uf) NE N_ELEMENTS(uc))) THEN STOP
  ;make x with time
  deksInAYear = INDGEN(36)+1
  years = INDGEN(2016-2002) + 2003
  jds = !NULL
  FOR y = 0, N_ELEMENTS(years) -1 DO BEGIN
    FOR t = 0, 35 DO BEGIN
      doy = DekYear2centralDOY(deksInAYear[t], years[y])
      jds = [jds, DOY_YEAR2JD(doy, years[y])]
    ENDFOR
  ENDFOR

  indll = WHERE(jds GE xrng[0])
  indul  = WHERE(jds GE xrng[1])
  rng = indll[0] + INDGEN(indul[0]-indll[0])
  yrng = [MIN([MIN(oX[*,rng]),MIN(uf[rng]),MIN(uc[rng]),MIN(oFF[rng])]),MAX([MAX(oX[*,rng]),MAX(uf[rng]),MAX(uc[rng]),MAX(oFF[rng])])]

  marg = [0.15,0.15,0.2,0.05]
  xLeft = 0.18
  dY = 0.29
  dYintra = 0.02
  YofUR = 0.98
  YofURSTAT = YofUR
  dummy = LABEL_DATE(DATE_FORMAT=['%M %Z']);LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
  linThick = 2  ;THICK of lines
  linThick2 = 2  ;THICK of lines
  linThick3 = 5  ;THICK of lines
  nonActTransparency = 10
  h = PLOT(jds, oFF, $
    YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', $ ;XTICKUNITS = ['Time', 'Time']
    XTITLE = 'Time', YTITLE = 'NDVI', THICK = linThick, $
    ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=[500,300],/NODATA)
    POSITION = [xLeft,YofUR-0.82,0.8,YofUR], FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=[500,300],/NODATA)


  hf = PLOT(jds, oFF, NAME = 'CF', COLOR='blue', THICK = linThick3, /OVERPLOT)
  hr = PLOT(jduf, uf, NAME = 'raw', COLOR='black', THICK = linThick2, /OVERPLOT)

  hu = PLOT(jds, uc, NAME = 'U0', COLOR='grey', THICK = linThick, /OVERPLOT)
  colors = ['firebrick','orange red','gold','yellow green','green']
  hx = [!NULL]
  FOR i = 0, N_ELEMENTS(fn_Ox) - 1 DO BEGIN
    htmp = PLOT(jds, oX[i,*], NAME = 'C'+STRTRIM(i,2), COLOR=colors[i], THICK = linThick2, /OVERPLOT)
    hx = [hx, htmp]
  ENDFOR

  ind1 = WHERE(active EQ 1, COMPLEMENT=ind0)
  inact = active
  inact[ind1] = yrng[0]
  inact[ind0] = 100
  ha = PLOT(jds,inact, LINESTYLE='',FILL_BACKGROUND = 1, FILL_COLOR = 'light grey', FILL_TRANSPARENCY = nonActTransparency, /HISTOGRAM, /OVERPLOT)
  hleg = LEGEND(TARGET=[hr,hu,hx,hf], SHADOW = 0, TRANSPARENCY = 100, POSITION = [.95,0.95], ORIENTATION = 0);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  h.save, dir + '\' + 'ndvi' + STRTRIM(x,2) + '_' + STRTRIM(y,2) + '_' + strXrng + '.png'

  ;now another graph with anomalies
  a_types = ['z','n','v']
  FOR a = 0, N_ELEMENTS(a_types)-1 DO BEGIN
    oX_aNxHx = ox*!VALUES.F_NAN
    oX_aNxHf = ox*!VALUES.F_NAN
    oX_stat1 = FLTARR(5,36)
    oX_stat2 = FLTARR(5,36)
    ;compare aNxHx with aNxHf by consolidation level
    ;Yout = CREATE_STRUCT('value', Y, 'stat1', stat1, 'stat1name' = stat1name, 'stat2', stat2, 'stat1name' = stat2name)
    uc_aNxHx =  a_by_dek_array1d(uc, a_types[a], 1, 36, INCLUDECURRENTDATE = 1)
    ;uc_aNxHx = tmp.value
    uc_aNxHf = a_by_dek_array1d(uc, a_types[a], 1, 36, XHIS = OFF, INCLUDECURRENTDATE = 0)
    ;uc_aNxHf = tmp.value
    FOR i = 0, N_ELEMENTS(fn_Ox) - 1 DO BEGIN
      tmp = a_by_dek_array1d(REFORM(oX[i,*]), a_types[a], 1, 36, INCLUDECURRENTDATE = 1)
      oX_aNxHx[i,*] = tmp.value
      oX_stat1[i,*] = tmp.stat1
      oX_stat2[i,*] = tmp.stat2
      tmp = a_by_dek_array1d(REFORM(oX[i,*]), a_types[a], 1, 36, XHIS = OFF, INCLUDECURRENTDATE = 0)
      oX_aNxHf[i,*] = tmp.value
    ENDFOR  ;i
    aNfHf = a_by_dek_array1d(oFF, a_types[a], 1, 36, INCLUDECURRENTDATE = 1)
    ;aNfHf = tmp.value

    ;plot anomalies them
    yrng = [MIN([MIN(oX_aNxHx[*,rng]),MIN(uc_aNxHx.value[rng]),MIN(uc_aNxHf.value[rng]),MIN(aNfHf.value[rng])]), $
      MAX([MAX(oX_aNxHx[*,rng]),MAX(uc_aNxHx.value[rng]),MAX(uc_aNxHf.value[rng]),MAX(aNfHf.value[rng])])]

    linThick = 2  ;THICK of lines
    linThick2 = 2  ;THICK of lines
    IF (a EQ 0) THEN BEGIN
      h = PLOT(jds, uc_aNxHx.value, $
        YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', XSHOWTEXT=0, $ ;XTICKUNITS = ['Time', 'Time']
        YTITLE = a_types[a]+'NxHy', THICK = linThick, $
        ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,600], LAYOUT=[1,3,a+1],/NODATA)
        POSITION = [xLeft,YofUR-dY,0.8,YofUR], FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,600],/NODATA)
    ENDIF
    IF (a EQ 1) THEN BEGIN
      YofUR = YofUR - dY - dYintra
      h.select
      h = PLOT(jds, uc_aNxHx.value, $
        YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', XSHOWTEXT=0, $ ;XTICKUNITS = ['Time', 'Time']
        YTITLE = a_types[a]+'NxHy', THICK = linThick, $
        ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,600], LAYOUT=[1,3,a+1],/CURRENT,/NODATA)
        POSITION = [xLeft,YofUR-dY,0.8,YofUR], FONT_SIZE = 13, XMINOR = 12, /CURRENT,/NODATA)
    ENDIF
    IF (a EQ 2) THEN BEGIN
      YofUR = YofUR - dY - dYintra
      h.select
      h = PLOT(jds, uc_aNxHx.value, $
        YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', $ ;XTICKUNITS = ['Time', 'Time']
        XTITLE = 'Time', YTITLE = a_types[a]+'NxHy', THICK = linThick, $
        ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,600], LAYOUT=[1,3,a+1],/CURRENT,/NODATA)
        POSITION = [xLeft,YofUR-dY,0.8,YofUR], FONT_SIZE = 13, XMINOR = 12, /CURRENT,/NODATA)
    ENDIF

    h.select
    haNfHf = PLOT(jds, aNfHf.value, NAME =  'NfHf', COLOR='blue', THICK = linThick3, /OVERPLOT)
    huc_aNxHx = PLOT(jds, uc_aNxHx.value, NAME = 'NuHu', COLOR='grey', THICK = linThick, /OVERPLOT)
    huc_aNxHf = PLOT(jds, uc_aNxHf.value, NAME = 'NuHf', COLOR='grey', THICK = linThick, LINESTYLE='--',/OVERPLOT)
    colors = ['firebrick','orange red','gold','yellow green','green']
    hx = [!NULL]
    OtoPlot=[0,4]
    FOR i = 0, 1 DO BEGIN;FOR i = 0, N_ELEMENTS(fn_Ox) - 1 DO BEGIN
      j = OtoPlot[i]
      htmp = PLOT(jds, oX_aNxHx[j,*], NAME = 'N'+STRTRIM(j,2)+'H'+STRTRIM(j,2), COLOR=colors[j], THICK = linThick2, /OVERPLOT)
      hx = [hx, htmp]
      htmp = PLOT(jds, oX_aNxHf[j,*], NAME = 'N'+STRTRIM(j,2)+'Hf', COLOR=colors[j], THICK = linThick2, LINESTYLE='--', /OVERPLOT)
      hx = [hx, htmp]
    ENDFOR
    ind1 = WHERE(active EQ 1, COMPLEMENT=ind0)
    inact = active
    inact[ind1] = yrng[0]
    inact[ind0] = yrng[1]
    ha = PLOT(jds,inact, LINESTYLE='',FILL_BACKGROUND = 1, FILL_COLOR = 'light grey', FILL_TRANSPARENCY = nonActTransparency, /HISTOGRAM, /OVERPLOT) ; FILL_LEVEL = yrng[0],
    IF (a EQ 0) THEN hleg = LEGEND(TARGET=[huc_aNxHx,huc_aNxHf,hx,haNfHf], SHADOW = 0, TRANSPARENCY = 100, POSITION = [1.01,YofUR], ORIENTATION = 0);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)

    ;plot stats on another graph
    ;now another graph stats (only u, o0, of): uc_aNxHx.stat1, uc_aNxHx.stat2, oX_stat1[i,*], oX_stat2[i,*], aNfHf.stat1, aNfHf.stat2
    IF (a EQ 0) THEN BEGIN
      ;mean and sd
      first =  MIN([MIN(oX_stat1-oX_stat2),MIN(uc_aNxHx.stat1-uc_aNxHx.stat2),MIN(aNfHf.stat1-aNfHf.stat2)])
      second = MAX([MAX(oX_stat1+oX_stat2),MAX(uc_aNxHx.stat1+uc_aNxHx.stat2),MAX(aNfHf.stat1+aNfHf.stat2)])
      yrng = [first, second]
      hSTAT = PLOT(jds, uc_aNxHx.value, $
        YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', $ ;XTICKUNITS = ['Time', 'Time']
        XTITLE = 'Time', YTITLE = 'NDVI His. Stats', THICK = linThick, $
        ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,600], LAYOUT=[1,3,a+1],/NODATA)
        POSITION = [xLeft,YofURSTAT-0.82,0.8,YofURSTAT], FONT_SIZE = 13, XMINOR = 12,  DIMENSIONS=[500,300],/NODATA)
    ENDIF
    hSTAT.select
    IF (a EQ 0) THEN BEGIN
      nYears = JD2YEAR(jds[-1])-JD2YEAR(jds[0])+1
      hOFavg = PLOT(jds, repeatArray(aNfHf.stat1, nYears), NAME =  'CF avg', COLOR='blue', THICK = 2, /OVERPLOT)
      hOFsd = PLOT(jds, repeatArray(aNfHf.stat1+aNfHf.stat2, nYears),FILL_BACKGROUND=1,FILL_COLOR='blue',FILL_TRANSPARENCY=80,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)
      hOFsd = PLOT(jds, repeatArray(aNfHf.stat1-aNfHf.stat2, nYears),FILL_BACKGROUND=1,FILL_COLOR = 'white',FILL_TRANSPARENCY=0,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)
      baseA = repeatArray(aNfHf.stat1-aNfHf.stat2, nYears)
      hO0avg =  PLOT(jds, repeatArray(REFORM(oX_stat1[0,*]), nYears), NAME =  'C0 avg', COLOR=colors[0], THICK = 2, /OVERPLOT)
      ;colors = ['firebrick','orange red','gold','yellow green','green']
      hO0sd = PLOT(jds, repeatArray(REFORM(oX_stat1[0,*])+REFORM(oX_stat2[0,*]), nYears),FILL_BACKGROUND=1,FILL_COLOR=colors[0],FILL_TRANSPARENCY=80,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)
      hO0sd = PLOT(jds, repeatArray(REFORM(oX_stat1[0,*])-REFORM(oX_stat2[0,*]), nYears),FILL_BACKGROUND=1,FILL_COLOR = 'white',FILL_TRANSPARENCY=0,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)
      baseB = repeatArray(REFORM(oX_stat1[0,*])-REFORM(oX_stat2[0,*]), nYears)
      indAlwrB = WHERE(baseB-baseA GT 0)  ;where baseA is amaller
      baseC = baseB
      baseC[indAlwrB] = baseA[indAlwrB] ; now baseC is the very min, from baseB down put it blue and the remove from baseC
      tmp = PLOT(jds, baseB,FILL_BACKGROUND=1,FILL_COLOR='blue',FILL_TRANSPARENCY=80,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)
      tmp = PLOT(jds, baseC,FILL_BACKGROUND=1,FILL_COLOR = 'white',FILL_TRANSPARENCY=0,FILL_LEVEL=-100,LINESTYLE='',/OVERPLOT)

    ENDIF
    IF (a EQ 2) THEN BEGIN
      nYears = JD2YEAR(jds[-1])-JD2YEAR(jds[0])+1
      hOFmin = PLOT(jds, repeatArray(aNfHf.stat1, nYears), NAME =  'CF min', LINESTYLE = '--', COLOR='blue', THICK = 2, /OVERPLOT)
      hOFmax = PLOT(jds, repeatArray(aNfHf.stat2, nYears), NAME =  'CF max', LINESTYLE = '--', COLOR='blue', THICK = 2, /OVERPLOT)
      hO0min = PLOT(jds, repeatArray(REFORM(oX_stat1[0,*]), nYears), NAME =  'C0 min', LINESTYLE = '--', COLOR=colors[0], THICK = 2, /OVERPLOT)
      hO0max = PLOT(jds, repeatArray(REFORM(oX_stat2[0,*]), nYears), NAME =  'C0 max', LINESTYLE = '--', COLOR=colors[0], THICK = 2, /OVERPLOT)
      hleg = LEGEND(TARGET=[hO0avg,hO0min,hO0max,hOFavg,hOFmin,hOFmax], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.98,0.95], ORIENTATION = 0);, HORIZONTA
      ha = PLOT(jds,inact, LINESTYLE='',FILL_BACKGROUND = 1, FILL_COLOR = 'light grey', FILL_TRANSPARENCY = nonActTransparency, /HISTOGRAM, /OVERPLOT) ; FILL_LEVEL = yrng[0],
    ENDIF

  ENDFOR ;a
  h.save, dir + '\' + 'ndvi_anomalies' + STRTRIM(x,2) + '_' + STRTRIM(y,2) + '_' + strXrng + '.png'
  hSTAT.save, dir + '\' + 'ndvi_stats' + STRTRIM(x,2) + '_' + STRTRIM(y,2) + '_' + strXrng + '.png'



  RETURN, 0
END