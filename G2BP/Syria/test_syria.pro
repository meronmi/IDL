PRO test_syria
;load unsmothed, scale and set nan
dir = 'X:\SYRIA\vgt\herve roi on s'
;fn_images =['vt1108i_c.img', 'vt1208i_c.img','vt1308i_c.img','vt1408i_c.img']
;fn_images =['vt1125i_c.img', 'vt1225i_c.img','vt1325i_c.img']
fn_images =['vt1109i_c.img', 'vt1209i_c.img','vt1309i_c.img','vt1409i_c.img']
ns = 7841
nl = 8289

FOR i=0, N_ELEMENTS(fn_images)-1 DO BEGIN
  img = BYTARR(ns,nl)
  OPENR, lun, dir + '\' + fn_images[i], /GET_LUN
  READU, lun, img
  FREE_LUN, lun
  ind = WHERE(img GT 250, count)
  imgf = FLOAT(img)
  imgf[ind] = !VALUES.F_NAN
  imgf = -0.08 + 0.004 * imgf
  basename = STRSPLIT(fn_images[i], '.', /EXTRACT)
  basename = basename[0]
  OPENW, lun, dir + '\' + basename +'_sc', /GET_LUN
  WRITEU, lun, imgf
  FREE_LUN, lun
  FILE_COPY, dir + '\' +basename +'.hdr', dir + '\' +basename +'_sc.hdr'
  ;rember to change data type to 4 if using the line above  
   
ENDFOR
END