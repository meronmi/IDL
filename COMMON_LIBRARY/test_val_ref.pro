PRO test001
 x = FLTARR(3,3)+2
 PRINT, x
 ;y = test_val_ref(x[*,*]) ; x is not changed in the function
 y = test_val_ref(TEMPORARY(x)) ; x is not changed in the function
 PRINT, x
END


FUNCTION test_val_ref, x
x = x/2
RETURN, x
END

