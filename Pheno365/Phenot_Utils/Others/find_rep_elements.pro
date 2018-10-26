Function find_rep_elements, x
;return -1 if no replicated values occur. Return the array of subscript
;of replicated values otherways

;find uniq values
u = x[UNIQ(x, SORT(x))]
IF (N_ELEMENTS(u) EQ N_ELEMENTS(x)) THEN RETURN, -1
r = INTARR(1)
FOR i = 0, N_ELEMENTS(u) - 1 DO BEGIN
  ind = WHERE(x EQ u[i], count)
  IF (count GT 1) THEN r = [r, ind]
ENDFOR
RETURN, r[1:(N_ELEMENTS(r)-1)]
END

