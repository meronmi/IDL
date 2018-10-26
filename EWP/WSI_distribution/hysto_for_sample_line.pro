FUNCTION hysto_for_sample_line, s, l, kernel_size
;compute the pdf over the requested pixels and over a kernel_size by kernel_size kernel around
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
prog = [0, 25, 50, 75, 100]
fn = dir + '\wsi_at_prog_' + STRTRIM(prog,2)
y = FLTARR(bands, N_ELEMENTS(prog))
;same to stor kernel results
yk = FLTARR(bands*ks*ks, N_ELEMENTS(prog))

FOR f = 0, N_ELEMENTS(prog) - 1 DO BEGIN
  mat = FLTARR(samples, lines, bands)
  OPENR, lun, fn[f], /GET_LUN
  READU, lun, mat
  FREE_LUN, lun
  ;test, aborted is bsq
  ;val = READ_BINARY(fn[f], DATA_DIMS=bands, DATA_TYPE=4, DATA_START=(LONG(samples)*(l-1)+s-1)*4)
  
  ;get the values for this progress
  y[*,f] = mat[s-1,l-1,*]
  ;get the values of the whole kernel
  yk[*,f] = REFORM(mat[s-1-hks:s-1+hks,l-1-hks:l-1+hks,*], bands*ks*ks )
ENDFOR
;make the histogram for all
n= 10
pdf = LONARR(n, N_ELEMENTS(prog))
pdfk = LONARR(n, N_ELEMENTS(prog))  
FOR i = 0, N_ELEMENTS(prog) - 1 DO BEGIN
  pdf[*,i] =  HISTOGRAM(y[*,i], MIN=10.0, MAX=100.0, NBINS=n,  LOCATIONS=xbin)
  pdfk[*,i] =  HISTOGRAM(yk[*,i], MIN=10.0, MAX=100.0, NBINS=n,  LOCATIONS=xbink)
ENDFOR
ymax= MAX(pdf) + 2
ymaxk = MAX(pdfk) + 2
wtitle = 's,l = ' + STRTRIM(s,2) + ',' + STRTRIM(l,2)
fn = dir + '\' + 'sl_'+ STRTRIM(s,2) + '_' + STRTRIM(l,2)
histo = PLOT(xbin, pdf[*,0], LAYOUT=[3,2,1], XRANGE=[0,100], YRANGE=[0,ymax], $
  TITLE='Progress ' + STRTRIM(prog[0],2), XTITLE='WSI', YTITLE='Frequency', /STAIRSTEP , $
  AXIS_STYLE=1, COLOR='red', WINDOW_TITLE = wtitle)
FOR i = 1, N_ELEMENTS(prog) - 1 DO BEGIN
  histo = PLOT(xbin, pdf[*,i], LAYOUT=[3,2,i+1], /CURRENT, XRANGE=[0,100], YRANGE=[0,ymax], $
    TITLE='Progress ' + STRTRIM(prog[i],2), XTITLE='WSI', YTITLE='Frequency', /STAIRSTEP, $
    AXIS_STYLE=1, COLOR='red')
ENDFOR
histo.save, fn + '.png', BORDER=10, RESOLUTION=300, /TRANSPARENT
wtitle = 's,l = ' + STRTRIM(s,2) + ',' + STRTRIM(l,2) + '; Kernel = '  + STRTRIM(ks,2)
fn = 'kernel_' + STRTRIM(ks,2) + '_sl_'+ STRTRIM(s,2) + '_' + STRTRIM(l,2)
histok = PLOT(xbink, pdfk[*,0], LAYOUT=[3,2,1], XRANGE=[0,100], YRANGE=[0,ymaxk], $
  TITLE='Progress ' + STRTRIM(prog[0],2), XTITLE='WSI-Kernel', YTITLE='Frequency', /STAIRSTEP , $
  AXIS_STYLE=1, COLOR='red', WINDOW_TITLE = wtitle)
FOR i = 1, N_ELEMENTS(prog) - 1 DO BEGIN
  histok = PLOT(xbink, pdfk[*,i], LAYOUT=[3,2,i+1], /CURRENT, XRANGE=[0,100], YRANGE=[0,ymaxk], $
    TITLE='Progress ' + STRTRIM(prog[i],2), XTITLE='WSI-Kernel', YTITLE='Frequency', /STAIRSTEP, $
    AXIS_STYLE=1, COLOR='red')
ENDFOR
histok.save, dir + '\' + fn + '.png', BORDER=10, RESOLUTION=300, /TRANSPARENT
RETURN, 0
END