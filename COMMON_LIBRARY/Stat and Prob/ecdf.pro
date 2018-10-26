FUNCTION ecdf, x_in
  fin = where(finite(x_in), sz)
  fin = x_in[fin]
  s = sort(fin)
  x = fin[s]
  RETURN, [[x], [(findgen(sz)) / sz]]

END