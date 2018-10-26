FUNCTION mean_wsi_by_progress_and_seasonality, suffix
no_data_out = 252
dir_in = 'X:\WSI_v6\WSI_by_prog';'X:\WSI_v3\STACKS\WSI_by_prog'
dir_out = 'X:\WSI_v6\WSI_by_prog\mean';'X:\WSI_v3\STACKS\WSI_by_prog\mean'
FILE_MKDIR, dir_out

ns = 8177
nl = 8233
nb = 26

prog = [0, 25, 50, 75, 100]
seasNames = ['mono','bi1','bi2']
FOR seas = 0, 2 DO BEGIN
  PRINT, 'Season: ' + STRTRIM(seasNames[seas])
  FOR i = 0, N_ELEMENTS(prog)-1 DO BEGIN
    PRINT, 'Progress: ' + STRTRIM(prog[i])
    PRINT, 'Memory() ' , MEMORY()
    PRINT, SYSTIME()
    fn_in = dir_in + '\'+seasNames[seas] + '_wsi_at_prog_' + STRTRIM(prog[i],2) + '_' + suffix
    fn_out = dir_out + '\'+seasNames[seas] + '_mean_wsi_at_prog_' + STRTRIM(prog[i],2) + '_' + suffix
    mat_in = BYTARR(ns, nl, nb)
    OPENR, lun, fn_in, /GET_LUN
    READU, lun, mat_in
    FREE_LUN, lun
    mat_in = FLOAT(mat_in)

    ind = WHERE(mat_in EQ no_data_out, count)
    IF (count GT 0) THEN mat_in[ind] = !VALUES.F_NAN

    ;don't bother % Program caused arithmetic error: Floating illegal operand,
    ;it is the mean function that divede by 0, th result is correct
    band_out = MEAN(mat_in, DIMENSION = 3 ,/NAN)
    ;make -9999 for arcgis
    ind = WHERE(~FINITE(band_out), count)
    IF (count GT 0) THEN band_out[ind] = -9999
    
    OPENW, lun, fn_out  + '.img', /GET_LUN
    WRITEU, lun, band_out
    FREE_LUN, lun
    res = write_envi_hdr(fn_out + '.hdr', ns, nl, 4)
  ENDFOR
ENDFOR


RETURN, 0
END