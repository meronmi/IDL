FUNCTION bSub_with_value_matching_a_values, b, a
;given two vectors b and a it returns
;an array of subscrpts to vector b.
;The returned array has the dimension of a, and for each element of a it provides the subscript of b element having the value of the a element

;if one element of a is not represented in b, it returns -1
;if one element is represented more than once in b it returns -1 

;example
;b = [1,2,3,4,5]
;a = [1,1,2]
;PRINT, bSub_with_value_matching_a_values(b, a)
;0  0   1
n = N_ELEMENTS(a)
ret = INTARR(n)
FOR i = 0, n-1 DO BEGIN
  ind = WHERE(b EQ a[i], count)
  IF (count EQ 1) THEN ret[i] = ind ELSE RETURN, -1 
ENDFOR
RETURN, ret
END