Function Lambert_Beer_compute_fa_from_lai, lai, k
  ;basic treament of fapar - lai relationship
  return, 1.0-exp(-k*lai)
End