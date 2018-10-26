function ind_int_HISTOGRAM, a, b
;provide indexes of intersection, return index of A elements that were found in B
;note that if A contains replicate equal values, only one and the first is returned 
  minab = min(a, MAX=maxa) > min(b, MAX=maxb)
  maxab = maxa < maxb
  ha = histogram(a, MIN=minab, MAX=maxab, reverse_indices=reva)
  hb = histogram(b, MIN=minab, MAX=maxab)
  r = where((ha ne 0) and (hb ne 0), cnt)
  if cnt eq 0 then return, -1
  return,reva[reva[r]]
end