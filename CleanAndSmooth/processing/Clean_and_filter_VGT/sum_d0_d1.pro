Function sum_d0_d1, x, d0, d1
;x is the matrix of ND, dekads are in the columns
;regions and years are in the rows
;d0 and d1 are first and last dekads to be summed
  y=dblarr(N_ELEMENTS(x[0,*]))
  for i=0, N_ELEMENTS(x[0,*])-1 do begin
    y[i]=TOTAL(x[d0[i]:d1[i],i])
  endfor
  return, y
End