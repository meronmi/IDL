PRO add_img_extension_to_envi_file
  res = DIALOG_PICKFILE(/MULTIPLE_FILES, GET_PATH = dir)
  n = N_ELEMENTS(res)
  FOR i=0, n-1 DO BEGIN
    tmp = STRPOS(res[i], '.')
    IF (tmp EQ -1) THEN FILE_MOVE, res[i], res[i]+'.img'
  ENDFOR
END