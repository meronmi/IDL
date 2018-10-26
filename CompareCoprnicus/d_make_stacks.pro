PRO D_make_stacks
startup = C_start_up()
;FOR i = 0, 2 DO BEGIN
FOR i = 2, 2 DO BEGIN
  ret = make_bil_from_meta(startup.meta_fn[i], startup.bil_fn[i])
ENDFOR
PRINT, 'D_make_stack is done'
END