FUNCTION hysto_for_random_places, ns, kernel_size
  ;compute the pdf over the requested pixels and over a kernel_size by kernel_size kernel around
  
  ;number of bins for pdf, single pixel
  n= 10
  ;number of bins for pdf, pixels of the kerne
  nk = 20
  ;random extract (1)or use last (0)
  do_rnd_extract = 0
  
  ks = kernel_size
  IF (ks MOD 2) EQ 0 THEN STOP ;the kernel must be odd
  hks = (ks-1)/2  ;this is the hals a kernel
  ;hysto_for_sample_line(306,225)
  ;hysto_for_sample_line(329,344)
  ;s and l starting from 1
  samples = 5265
  lines = 1681
  bands = 25
  dir = 'Y:\meteo\GWSI\westafrica\wsi_distribution'
  dir_out = dir + '\images20samples'
  prog = [0, 25, 50, 75, 100]
  fn = dir + '\wsi_at_prog_' + STRTRIM(prog,2)
  y = FLTARR(bands, N_ELEMENTS(prog),ns)
  ;same to stor kernel results
  yk = FLTARR(bands*ks*ks, N_ELEMENTS(prog),ns)
  
  mmap = BYTARR(samples, lines)*0+200
  ;get ind of valid pixels
  tmp = FLTARR(samples, lines, bands)
  OPENR, lun, fn[0], /GET_LUN
  READU, lun, tmp
  FREE_LUN, lun
  tmp = tmp[*,*,0]
  ind = WHERE(FINITE(tmp), count)
  
  IF (do_rnd_extract) THEN BEGIN
    ;extract n random
    randomNumbers = RANDOMU(seed, count)
    sortedRandomNumbers = SORT(randomNumbers)
    rndind = sortedRandomNumbers[0:ns-1] 
    rndind = ind[rndind]
    ;make sure that all of them have valid kernel pixels, if one has no, make a new random slection
    selected = 0
    i = 0
    WHILE (selected NE ns) DO BEGIN 
      res = ARRAY_INDICES(mmap,rndind[i])
      ;if there is a least one missing, redraw 
      ;check also that adding the kernel does not go outside the image
      out = 0
      IF (res[0]-20 LT 0) THEN out = 1
      IF (res[0]+20 GT samples-1) THEN out = 1
      IF (res[1]-20 LT 0) THEN out = 1
      IF (res[1]+20 GT lines-1) THEN out = 1
      IF (TOTAL(~FINITE(tmp[res[0]-hks:res[0]+hks,res[1]-hks:res[1]+hks])) OR (out EQ 1)) THEN BEGIN
        ; rndind[i] has to be changed
        randomNumbers = RANDOMU(seed, count)
        sortedRandomNumbers = SORT(randomNumbers)
        sorted = sortedRandomNumbers[0]
        rndind[i] = ind[sorted]
      ENDIF ELSE BEGIN
        selected = selected + 1
        i = i + 1
      ENDELSE
    ENDWHILE
    ;save the indexes for re-use
    SAVE, rndind, FILENAME = dir_out + '\ind_of_last_run.sav'
  ENDIF ELSE BEGIN
    ;load the last one
    rndind = 0
    RESTORE, dir_out + '\ind_of_last_run.sav'
  ENDELSE
  ;save a map
  mmap[ind]=130
  FOR i = 0, N_ELEMENTS(rndind)-1 DO BEGIN
    res = ARRAY_INDICES(mmap,rndind[i])
    mmap[res[0]-20:res[0]+20,res[1]-20:res[1]+20]=0
  ENDFOR
  fn_map = dir_out + '\random_selection_position'
  im = IMAGE(mmap, /ORDER, MIN_VALUE=0, MAX_VALUE=255)
  ;add number
  FOR i = 0, N_ELEMENTS(rndind)-1 DO BEGIN
    res = ARRAY_INDICES(mmap,rndind[i])
    ;landingsite = ARROW([100, 100],[res[0],res[1]], TARGET=im, /DATA, ARROW_STYLE=1, COLOR='blue', THICK=2)
    text = TEXT(res[0],lines-res[1], STRTRIM(i+1,2), 'red', TARGET=im, /DATA, FONT_SIZE=12)
  ENDFOR
  im.save, fn_map + '.png'

  FOR f = 0, N_ELEMENTS(prog) - 1 DO BEGIN
    mat = FLTARR(samples, lines, bands)
    OPENR, lun, fn[f], /GET_LUN
    READU, lun, mat
    FREE_LUN, lun
    ;test, aborted is bsq
    ;val = READ_BINARY(fn[f], DATA_DIMS=bands, DATA_TYPE=4, DATA_START=(LONG(samples)*(l-1)+s-1)*4)
    FOR j = 0, ns-1 DO BEGIN
      ;get the values for this progress
      res = ARRAY_INDICES(mmap,rndind[j])
      s = res[0] 
      l = res[1]
      y[*,f,j] = mat[s-1,l-1,*]
      ;get the values of the whole kernel
      yk[*,f,j] = REFORM(mat[s-1-hks:s-1+hks,l-1-hks:l-1+hks,*], bands*ks*ks )
    ENDFOR
  ENDFOR
  FOR j = 0, ns-1 DO BEGIN
    res = ARRAY_INDICES(mmap,rndind[j])
    s = res[0]
    l = res[1]
    ;make the histogram for all
    pdf = LONARR(n, N_ELEMENTS(prog))
    pdfk = LONARR(nk, N_ELEMENTS(prog))
    FOR i = 0, N_ELEMENTS(prog) - 1 DO BEGIN
      ;remove Nan coming from places that have 24 observations
      tmp = y[*,i,j]
      indf = WHERE(FINITE(tmp))
      tmp = tmp[indf]
      pdf[*,i] =  HISTOGRAM(tmp, MIN=10.0, MAX=100.0, NBINS=n,  LOCATIONS=xbin)
      tmp = yk[*,i,j]
      indf = WHERE(FINITE(tmp))
      tmp = tmp[indf]
      pdfk[*,i] =  HISTOGRAM(tmp, MIN=10.0, MAX=100.0, NBINS=nk,  LOCATIONS=xbink)
    ENDFOR
    ymax= MAX(pdf) + 2
    ymaxk = MAX(pdfk) + 2
    wtitle = 's,l = ' + STRTRIM(s,2) + ',' + STRTRIM(l,2)
    fn = dir_out + '\' + 'pixel_' + STRTRIM(j+1,2) + '_sl_'+ STRTRIM(s,2) + '_' + STRTRIM(l,2) + 'n' + STRTRIM(n,2)
    histo = PLOT(xbin, pdf[*,0], LAYOUT=[3,2,1], XRANGE=[0,100], YRANGE=[0,ymax], $
      TITLE='Progress ' + STRTRIM(prog[0],2), XTITLE='WSI', YTITLE='Frequency', /STAIRSTEP , $
      AXIS_STYLE=1, COLOR='red', WINDOW_TITLE = wtitle)
    FOR i = 1, N_ELEMENTS(prog) - 1 DO BEGIN
      histo = PLOT(xbin, pdf[*,i], LAYOUT=[3,2,i+1], /CURRENT, XRANGE=[0,100], YRANGE=[0,ymax], $
        TITLE='Progress ' + STRTRIM(prog[i],2), XTITLE='WSI', YTITLE='Frequency', /STAIRSTEP, $
        AXIS_STYLE=1, COLOR='red')
    ENDFOR
    pwin = GetWindows('histo')
    text1 = TEXT(0.65,0.35, "S,L = " + STRTRIM(s,2) + ', ' + STRTRIM(l,2), TARGET=pwin, 'green' )
    text2 = TEXT(0.65,0.30, "Kernel = none", 'green')
    histo.save, fn + '.png', BORDER=10, RESOLUTION=300, /TRANSPARENT
    
    ;kernel pdf
    wtitle = 's,l = ' + STRTRIM(s,2) + ',' + STRTRIM(l,2) + '; Kernel = '  + STRTRIM(ks,2)
    fn = 'kernel_'+ STRTRIM(j+1,2) + '_ks_'+STRTRIM(ks,2) + '_sl_'+ STRTRIM(s,2) + '_' + STRTRIM(l,2) + 'nk' + STRTRIM(nk,2)
    histok = PLOT(xbink, pdfk[*,0], LAYOUT=[3,2,1], XRANGE=[0,100], YRANGE=[0,ymaxk], $
      TITLE='Progress ' + STRTRIM(prog[0],2), XTITLE='WSI-Kernel', YTITLE='Frequency', /STAIRSTEP , $
      AXIS_STYLE=1, COLOR='red', WINDOW_TITLE = wtitle)
    FOR i = 1, N_ELEMENTS(prog) - 1 DO BEGIN
      histok = PLOT(xbink, pdfk[*,i], LAYOUT=[3,2,i+1], /CURRENT, XRANGE=[0,100], YRANGE=[0,ymaxk], $
        TITLE='Progress ' + STRTRIM(prog[i],2), XTITLE='WSI-Kernel', YTITLE='Frequency', /STAIRSTEP, $
        AXIS_STYLE=1, COLOR='red')
    ENDFOR
    pwin = GetWindows('histo')
    text1 = TEXT(0.65,0.35, "S,L = " + STRTRIM(s,2) + ', ' + STRTRIM(l,2), TARGET=pwin, 'green' )
    text2 = TEXT(0.65,0.30, "Kernel = " + STRTRIM(ks,2) + ' X ' + STRTRIM(ks,2), 'green')
    histok.save, dir_out + '\' + fn + '.png', BORDER=10, RESOLUTION=300, /TRANSPARENT
  ENDFOR
  PRINT, 'Enter to close all window'
  tmp=''
  READ, tmp
  w = getwindows()
  if n_elements(w) gt 0 then foreach i, w do i.close
  
  RETURN, 0
END