PRO plot_handler_video_filtering
  dir = '\\ies\d5\asap\users_data\meronmi\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI';'\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI'
  pos = [460, 821] ;crop in somalia

  year1 = 2014
  year2 = 2016
  ;use this for the first plot (smoothing vs. filtering)
  xrng = [JULDAY(01,05,year1), JULDAY(06,25,year2)] ;use central day, special case Somalia
  ;use this for the update plot 
  xrng = [JULDAY(01,05,year1), JULDAY(02,15,year2)] ;use central day, special case Somalia
  ;year1 = 2006
  ;year2 = 2011
  ;xrng = [JULDAY(01,05,year1), JULDAY(12,25,year2)] ;use central day
  ;------------------------------end of user part---------------
  x = pos[0] & y = pos[1]
  res =  video_filtering(dir, x, y, xrng, 1)
END

FUNCTION video_filtering, dir, x, y, xrng, useSave
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

  ;other plot for presentation (smoothing vs filtering), done in a separate pro

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
  xrng[1] = xrng[1] + 60
  rng = indll[0] + INDGEN(indul[0]-indll[0])
  yrng = [MIN([MIN(oX[*,rng]),MIN(uf[rng]),MIN(uc[rng]),MIN(oFF[rng])]),MAX([MAX(oX[*,rng]),MAX(uf[rng]),MAX(uc[rng]),MAX(oFF[rng])])]
  yrng = [0.0001, 0.8]
  ;marg = [0.15,0.15,0.2,0.05]
  dms = [1250,300]  ;[width, height]
  xLeft = 0.1
  dY = 0.29
  dYintra = 0.02
  YofUR = 0.975
  YofURSTAT = YofUR
  dummy = LABEL_DATE(DATE_FORMAT=['%M']);LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
  linThick = 2  ;THICK of lines
  linThick2 = 2  ;THICK of lines
  linThick3 = 5  ;THICK of lines
  nonActTransparency = 10
  
  plotFirst = 0
  IF (plotFirst EQ 1) THEN BEGIN
    h = PLOT(jds[indll[0]:indul[0]], oFF[indll[0]:indul[0]], $
      YRANGE = yrng, XRANGE =  xrng, XTICKNAME=['Y-2','Y-1', 'Y', ''], $;XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', $ ;XTICKUNITS = ['Time', 'Time']
      XTITLE = 'Time', YTITLE = 'NDVI', THICK = linThick,  XTICKFORMAT='LABEL_DATE',$
      ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=[500,300],/NODATA)
      POSITION = [xLeft,YofUR-0.8,0.8,YofUR-0.05], FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=dms,/NODATA)
    ht = TEXT(0.73, 0.0725,'Today', FONT_SIZE = 12)
    
    ;video part
    fps = 15
    step = 1 ;put it to 1 when ok
    fn_video = dir + '\video_filtering.mp4'
    oVid = IDLffVideoWrite(fn_video, FORMAT='mp4')
    vidStream = oVid.AddVideoStream(dms[0], dms[1], fps)
    
    hr = PLOT(jduf[indll[0]:indul[0]], uf[indll[0]:indul[0]], NAME = 'raw obs', COLOR='black', LINESTYLE='', SYMBOL = '+', SYM_SIZE = 2,/OVERPLOT)
    frame = hr.CopyWindow()
    !NULL = oVid.Put(vidStream, frame)
    tk = 2
    stopRec = 0 
    halfWin = 5  ;half window for sommthinh
    n = N_ELEMENTS(jds[0:indul[0]])
    left_start = indll[0] + halfWin  ;should leave halfWin points to the left
    i = left_start + 1
    ;plot the smoothing
    hf = PLOT(jds[left_start:i], oFF[left_start:i], NAME = 'Smoothing   ', LINESTYLE='-',COLOR='blue', THICK=tk, /OVERPLOT, /NODATA)
    hleg = LEGEND(TARGET=[hr,hf], SHADOW = 0, TRANSPARENCY=100, POSITION = [.975,0.95], ORIENTATION = 0, SAMPLE_WIDTH = 0.25, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
    WHILE (stopRec EQ 0) DO BEGIN
      IF (N_ELEMENTS(hw) GT 0) THEN BEGIN
        hw.delete
        hl.delete
      ENDIF
     
      hw = PLOT(jds[i-halfWin:i+halfWin], yrng[1]+FLTARR(2*halfWin+1),NAME = 'w', LINESTYLE='-', $
                FILL_COLOR='light grey', FILL_LEVEL=yrng[0], /FILL_BACKGROUND, FILL_TRANSPARENCY = 75, /OVERPLOT)
      hl = PLOT([jds[i],jds[i]], [yrng[0], yrng[1]], NAME = 'l', LINESTYLE='-', COLOR = 'dark grey', /OVERPLOT) 
      hs = PLOT(jds[left_start:i], oFF[left_start:i], NAME = 'CF', LINESTYLE='-',COLOR='blue', THICK = tk, /OVERPLOT)
      IF (N_ELEMENTS(hRunText) GT 0) THEN hRunText.delete
      hRunText = TEXT(jds[i],0.72,'smoothing',ALIGNMENT=0.5,/DATA)
      frame = hs.CopyWindow()
      !NULL = oVid.Put(vidStream, frame)
      IF (i GT 450) THEN step = 1 ;slow down at end
      i = i + step
      ;PRINT, i
      IF ((i + halfWin) GE n-1) THEN BEGIN
        stopRec  = 1
        lastVal0 = oFF[i-1]
      ENDIF
    ENDWHILE
    left_start = i
    ;now we are closer than half window to the right edge
    colors = ['firebrick','orange red','gold','yellow green','green']
    hw.delete
    hl.delete
    hRunText.delete
    hRunText = !NULL
    ;repetat the last filtering part r times
    FOR r = 0, 2 DO BEGIN
      lastVal = lastVal0
      hf = !NULL
      FOR i = 0, 4 DO BEGIN
        j = left_start + i;+ 1
        IF (N_ELEMENTS(hw1) GT 0) THEN BEGIN
          hw1.delete
          hw2.delete
          hl.delete
        ENDIF
        hw1 = PLOT(jds[j-halfWin:j], yrng[1]+jds[j-halfWin:j]*0.0,NAME = 'w', LINESTYLE='-', $
                   FILL_COLOR='light grey', FILL_LEVEL=yrng[0], /FILL_BACKGROUND, FILL_TRANSPARENCY = 75, /OVERPLOT)
        hw2 = PLOT(jds[j:j+5-i], yrng[1]+jds[j:j+4-i]*0.0,NAME = 'w', LINESTYLE='-', $
                   FILL_COLOR=colors[i], FILL_LEVEL=yrng[0], /FILL_BACKGROUND, FILL_TRANSPARENCY = 75, /OVERPLOT)
        hl = PLOT([jds[j],jds[j]], [yrng[0], yrng[1]], NAME = 'l', LINESTYLE='-', COLOR = colors[i], /OVERPLOT)
        hf = [hf,PLOT(jds[j-1:j], [lastVal, oX[4-i, j]], NAME = 'Filter C' + STRTRIM(4-i,2), LINESTYLE='-',colors[i], THICK = tk, /OVERPLOT)]
        IF (N_ELEMENTS(hRunText) GT 0) THEN hRunText.delete
        hRunText = TEXT(jds[j],0.72,'filtering',ALIGNMENT=0.5,/DATA)
        ;IF (r EQ 0) THEN  
        hleg = LEGEND(TARGET=hf[i], SHADOW = 0, TRANSPARENCY=100, POSITION = [.945,0.82-(i)*0.075], ORIENTATION = 0, SAMPLE_WIDTH = 0.25, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
        lastVal = oX[4-i, j]
        nSec = 1 ;number of sec to keep the same
        FOR f = 0, nSec*fps-1 DO BEGIN
          frame = hf[i].CopyWindow()
          !NULL = oVid.Put(vidStream, frame)
        END
      ENDFOR
      FOR k = 0, N_ELEMENTS(hf)-1 DO hf[k].delete
      IF (r LT 2) THEN BEGIN
        hw1.delete
        hw2.delete
        hl.delete 
        hRunText.delete
        hw1 = !NULL
        hw2 = !NULL
        hl = !NULL
        hRunText = !NULL
        FOR f = 0, nSec*fps-1 DO BEGIN
          frame = hs.CopyWindow()
          !NULL = oVid.Put(vidStream, frame)
        ENDFOR
      ENDIF
    ENDFOR ;r  
    oVid = 0
  ENDIF
  ;********************************************************************************************************************
  ;SECOND VIDEO SHOWING SUBSEQUENT UPDATE
  ;focus on the last year
  fps = 5
  indll = WHERE(jds GE xrng[0])
  indul  = WHERE(jds GE xrng[1])
  initToday = indul[0]           ; today at the beginning of the video, termine iniziale richiesto
  start = initToday - 18   ; tre mesi prima del termine iniziale richiesto 
  xrng[0] = jds[start] - 10
  xrng[1] = jds[initToday] + 150
  ;rng = indll[0] + INDGEN(indul[0]-indll[0])
  h = PLOT(jds[start:initToday], oFF[start:initToday], $
    YRANGE = yrng, XRANGE =  xrng, XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE', $ ;XTICKUNITS = ['Time', 'Time']
    XTITLE = 'Time', YTITLE = 'NDVI', THICK = linThick, $
    ;MARGIN = marg, FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=[500,300],/NODATA)
    POSITION = [xLeft,YofUR-0.8,0.8,YofUR-0.05], FONT_SIZE = 13, XMINOR = 12, COLOR='blue', DIMENSIONS=dms,/NODATA)
  ;ht = TEXT(0.73, 0.0725,'Today', FONT_SIZE = 12)

  ;video part
  fps = 1
  step = 1 ;put it to 1 when ok
  fn_video = dir + '\video_filtering_subsequent_updates3.mp4'
  oVid = IDLffVideoWrite(fn_video, FORMAT='mp4')
  vidStream = oVid.AddVideoStream(dms[0], dms[1], fps)
  ;to plot the raw data I have to understand where start and initToday, expressed in jds are
  indUnfiltStart = WHERE(jduf GE jds[start])
  indUnfiltStart = indUnfiltStart[0]
  indUnfiltInitToday  = WHERE(jduf LE jds[initToday])
  indUnfiltInitToday = indUnfiltInitToday[-1]
  hr = PLOT(jduf[indUnfiltStart:indUnfiltInitToday], uf[indUnfiltStart:indUnfiltInitToday], NAME = 'raw obs', COLOR='black', LINESTYLE='', SYMBOL = '+', SYM_SIZE = 2,/OVERPLOT)
  hl = PLOT([jds[initToday],jds[initToday]], [yrng[0], yrng[1]], NAME = 'l', LINESTYLE='-', COLOR = 'gray', /OVERPLOT)
  hRunText = TEXT(jds[initToday],0.72,'Today',ALIGNMENT=1,/DATA)
  frame = hr.CopyWindow()
  FOR j = 1,2 DO !NULL = oVid.Put(vidStream, frame)
  
  tk = 2
  stopRec = 0
  i = initToday 
  smsz = 2
  smt = 2
  ;plot no data to add a permanent legen
  htmp = PLOT(jds[0:1], oFF[0:1], NAME = 'Smoothing   ', LINESTYLE='-',COLOR='blue', THICK=tk, /OVERPLOT, /NODATA)
  hleg = LEGEND(TARGET=[hr,htmp], SHADOW = 0, TRANSPARENCY=100, POSITION = [.975,0.95], ORIENTATION = 0, SAMPLE_WIDTH = 0.25, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
  colors = ['firebrick','orange red','gold','yellow green','green']
  h4tmp = PLOT(jds[0:1], oX[4,0:1], NAME = 'Filter C4', LINESTYLE='',SYMBOL='o',SYM_SIZE=smsz,SYM_THICK=smt,colors[0], THICK = tk, /OVERPLOT);, /NODATA)
  h3tmp = PLOT(jds[0:i], oX[3,0:i], NAME = 'Filter C3', LINESTYLE='',SYMBOL='*',SYM_SIZE=smsz,SYM_THICK=smt,colors[1], THICK = tk, /OVERPLOT, /NODATA)
  h2tmp = PLOT(jds[0:i], oX[2,0:i], NAME = 'Filter C2', LINESTYLE='',SYMBOL='D',SYM_SIZE=smsz,SYM_THICK=smt,colors[2], THICK = tk, /OVERPLOT, /NODATA)
  h1tmp = PLOT(jds[0:i], oX[1,0:i], NAME = 'Filter C1', LINESTYLE='',SYMBOL='tu',SYM_SIZE=smsz,SYM_THICK=smt,colors[3], THICK = tk, /OVERPLOT, /NODATA)
  h0tmp = PLOT(jds[0:i], oX[0,0:i], NAME = 'Filter C0', LINESTYLE='',SYMBOL='p',SYM_SIZE=smsz,SYM_THICK=smt,colors[4], THICK = tk, /OVERPLOT, /NODATA)
  hleg = LEGEND(TARGET=[h4tmp,h3tmp,h2tmp,h1tmp,h0tmp], SHADOW = 0, TRANSPARENCY=100, POSITION = [.942,0.78], ORIENTATION = 0, SAMPLE_WIDTH = 0.2, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1)
 
;  htmp = PLOT(jds[0:1], oFF[0:1], NAME = 'Smoothing   ', LINESTYLE='-',COLOR='blue', THICK=tk, /OVERPLOT, /NODATA)
;  hleg = LEGEND(TARGET=[hr], SHADOW = 0, TRANSPARENCY=100, POSITION = [.975,0.95], ORIENTATION = 0, SAMPLE_WIDTH = 0.25, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1);, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
;  colors = ['firebrick','orange red','gold','yellow green','green']
;  h4tmp = PLOT(jds[0:1], oX[4,0:1], NAME = 'C4', LINESTYLE='',SYMBOL='o',SYM_SIZE=smsz,SYM_THICK=smt,colors[0], THICK = tk, /OVERPLOT);, /NODATA)
;  h3tmp = PLOT(jds[0:i], oX[3,0:i], NAME = 'C3', LINESTYLE='',SYMBOL='*',SYM_SIZE=smsz,SYM_THICK=smt,colors[1], THICK = tk, /OVERPLOT, /NODATA)
;  h2tmp = PLOT(jds[0:i], oX[2,0:i], NAME = 'C2', LINESTYLE='',SYMBOL='D',SYM_SIZE=smsz,SYM_THICK=smt,colors[2], THICK = tk, /OVERPLOT, /NODATA)
;  h1tmp = PLOT(jds[0:i], oX[1,0:i], NAME = 'C1', LINESTYLE='',SYMBOL='tu',SYM_SIZE=smsz,SYM_THICK=smt,colors[3], THICK = tk, /OVERPLOT, /NODATA)
;  h0tmp = PLOT(jds[0:i], oX[0,0:i], NAME = 'C0', LINESTYLE='',SYMBOL='p',SYM_SIZE=smsz,SYM_THICK=smt,colors[4], THICK = tk, /OVERPLOT, /NODATA)
;  hRunTextT = TEXT(0.865,0.78,'NRT filtering:',ALIGNMENT=0, FONT_STYLE='Bold', FONT_SIZE = 12)
;  hleg = LEGEND(TARGET=[h4tmp,h3tmp,h2tmp,h1tmp,h0tmp], SHADOW = 0, TRANSPARENCY=100, POSITION = [.935,0.78], ORIENTATION = 0, SAMPLE_WIDTH = 0.2, FONT_SIZE = 12, LINESTYLE='', HORIZONTAL_SPACING =.1, VERTICAL_SPACING =.1)
  
  
  
  time4OF = 12 ;lag of OF, expressed in subs
  time4C4 = 4
  time4C3 = 3
  time4C2 = 2
  time4C1 = 1
  time4C0 = 0
  hl.delete
  hRunText.delete
  init = 1
  WHILE (stopRec EQ 0) DO BEGIN
    IF (N_ELEMENTS(hs) GT 0) THEN BEGIN
      hs.delete
;      ;test, make them vanish 
;      sytr = 65
;      h4old = h4
;      h3old = h3
;      h2old = h2
;      h1old = h1
;      h0old = h0
;      h4old.SYM_TRANSPARENCY = sytr
;      h3old.SYM_TRANSPARENCY = sytr
;      h2old.SYM_TRANSPARENCY = sytr
;      h1old.SYM_TRANSPARENCY = sytr
;      h0old.SYM_TRANSPARENCY = sytr
;      h4old.SYM_COLOR = 'gray'
;      h3old.SYM_COLOR = 'gray'
;      h2old.SYM_COLOR = 'gray'
;      h1old.SYM_COLOR = 'gray'
;      h0old.SYM_COLOR = 'gray'
      h4.delete
      h3.delete
      h2.delete
      h1.delete
      h0.delete      
      hl.delete
      hRunText.delete
    ENDIF
    ;at each time update smoothin
    ;to plot the raw data I have to understand where start and initToday, expressed in jds are
    
    indUnfiltInitToday  = WHERE(jduf LE jds[i])
    indUnfiltInitToday = indUnfiltInitToday[-1]
    hr = PLOT(jduf[indUnfiltStart:indUnfiltInitToday], uf[indUnfiltStart:indUnfiltInitToday], NAME = 'raw obs', COLOR='black', LINESTYLE='', SYMBOL = '+', SYM_SIZE = 2,/OVERPLOT)
    
    hs = PLOT(jds[start:i-time4OF], oFF[start:i-time4OF], NAME = 'CF', LINESTYLE='-',COLOR='blue', THICK = tk, /OVERPLOT)
    h4 = PLOT(jds[i-time4OF+1:i-time4C4], oX[4,i-time4OF+1:i-time4C4], NAME = 'Filter update4', LINESTYLE='',SYMBOL='o',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR = colors[0], THICK = tk, /OVERPLOT)
    h3 = PLOT(jds[i-time4C4+1:i-time4C3], oX[3,i-time4C4+1:i-time4C3], NAME = 'Filter update3', LINESTYLE='',SYMBOL='*',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR = colors[1], THICK = tk, /OVERPLOT)
    h2 = PLOT(jds[i-time4C3+1:i-time4C2], oX[2,i-time4C3+1:i-time4C2], NAME = 'Filter update2', LINESTYLE='',SYMBOL='D',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR = colors[2], THICK = tk, /OVERPLOT)
    h1 = PLOT(jds[i-time4C2+1:i-time4C1], oX[1,i-time4C2+1:i-time4C1], NAME = 'Filter update1', LINESTYLE='',SYMBOL='tu',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR = colors[3], THICK = tk, /OVERPLOT)
    h0 = PLOT(jds[i-time4C1+1:i-time4C0], oX[0,i-time4C1+1:i-time4C0], NAME = 'Filter', LINESTYLE='',SYMBOL='p',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR= colors[4], THICK = tk, /OVERPLOT)
    hl = PLOT([jds[i],jds[i]], [yrng[0], yrng[1]], NAME = 'l', LINESTYLE='-', COLOR = 'gray', /OVERPLOT)
    hRunText = TEXT(jds[i],0.72,'Today',ALIGNMENT=1,/DATA)
    frame = hs.CopyWindow()
    ;some adjustment for a static figure showing the 5 stages, comment the if below to exclude iy
;    IF (i EQ 483) THEN BEGIN
;     hs.xrange =  [jds[start+15],jds[start+25]]
;     h4.delete
;     h4 = PLOT(jds[start+15:start+19], oX[4,start+15:start+19], NAME = 'Filter update4', LINESTYLE='',SYMBOL='o',SYM_SIZE=smsz,SYM_THICK=smt,SYM_COLOR = colors[0], THICK = tk, /OVERPLOT)
;     hs.save, 'D:\fig_5conso.png', RESOLUTION = 600
;     h0.delete
;     h1.delete
;     h2.delete
;     h3.delete
;     h4.delete
;     hRunTextT.delete
;     hleg.delete
;     hs.save, 'D:\fig_5conso_raw.png', RESOLUTION = 600
;     PRINT,''
;    ENDIF
    
    !NULL = oVid.Put(vidStream, frame)
    IF (init EQ 1) THEN BEGIN
       FOR j = 1,2 DO !NULL = oVid.Put(vidStream, frame)
       init = 0
    ENDIF
    i = i + 1
    IF ((i-initToday) GT 14) THEN stopRec  = 1
;    IF (N_ELEMENTS(h4old) GT 0) THEN BEGIN
;      h4old.delete
;      h3old.delete
;      h2old.delete
;      h1old.delete
;      h0old.delete
;    ENDIF
  ENDWHILE
  oVid = 0
  
 
  RETURN, 0
END