FUNCTION remove_sub_from_array, array, sub
CASE sub OF
  0:                   RETURN, array[1:N_ELEMENTS(array)-1]
  N_ELEMENTS(array)-1: RETURN, array[0:N_ELEMENTS(array)-2]
  ELSE:                RETURN, [array[0:sub-1],array[sub+1:N_ELEMENTS(array)-1]]
ENDCASE
END