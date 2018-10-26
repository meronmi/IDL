PRO compute_delta
  
  dir = 'X:\SYRIA\vgt\herve roi on s'
  out_dir = 'X:\SYRIA\vgt\delta band-avg_irr_turkey\GUIDO ROI'
  ;fn_images =['vt1125i_c_sc', 'vt1225i_c_sc','vt1325i_c_sc']
  fn_images =['vt1109i_c_sc', 'vt1209i_c_sc','vt1309i_c_sc','vt1409i_c_sc']
  irr_avg = [0.654626, 0.651556,  0.792, 0.799879];[0.659919, 0.680768, 0.723515]
  ns = 7841
  nl = 8289
  
  FOR i=0, N_ELEMENTS(fn_images)-1 DO BEGIN
    img = FLTARR(ns,nl)
    OPENR, lun, dir + '\' + fn_images[i], /GET_LUN
    READU, lun, img
    FREE_LUN, lun
    
    imgf =  img - irr_avg[i]
    basename = STRSPLIT(fn_images[i], '.', /EXTRACT)
    basename = basename[0]
    OPENW, lun, out_dir + '\delta_' + basename+'.img', /GET_LUN
    WRITEU, lun, imgf
    FREE_LUN, lun
    FILE_COPY, dir + '\' +basename +'.hdr', out_dir + '\delta_' + basename +'.hdr'
    ;rember to change data type to 4 if using the line above
    
  ENDFOR
END