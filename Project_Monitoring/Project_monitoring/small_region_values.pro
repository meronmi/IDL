;http://www.idlcoyote.com/idl_way/smregval.html
FUNCTION Small_Region_Values, array, value, KERNEL=kernel

  Compile_Opt idl2

  On_Error, 2 ; Return to caller on error.

  ; Check parameters.
  IF N_Elements(array) EQ 0 THEN STOP
  IF N_Elements(value) EQ 0 THEN STOP
  IF N_Elements(kernel) EQ 0 THEN STOP

  ; Create a mask array.
  s = Size(array, /DIMENSIONS)
  mask = BytArr(s[0], s[1])
  indices = Where(array EQ value, count)
  IF count GT 0 THEN mask[indices] = 1

  RETURN, Convol(Float(array) * mask, kernel, /EDGE_ZERO)

END