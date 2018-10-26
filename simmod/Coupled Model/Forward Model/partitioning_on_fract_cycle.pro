Function partitioning_on_fract_cycle, fract, type
  ;this function prescribe the partitioning as a function of develop stage [0,1] and type
  ;type may be "a" -> above ground (leaf and stem) "g" -> grain, storage organs
  ;partitioning is compute for above and below biomass, grain is the residual
  ;cardinal points
  ;y=y1+(x-x1) dy/dx , dy=y2-y1, dx=x2-x1
  x0=0.35 & x1=0.5 & x2=1.0
  ry0=0.5 & ry1=0.0           ;root
  ay1=0.5 & ay2=0             ;above ground
  if (type eq 'a') or (type eq 'g') then begin
    ;compute above, which is needed also for grain
    if fract le x1 then alpha=ay1 else alpha=ay1+(fract-x1)*(ay2-ay1)/(x2-x1)
    if alpha lt 0 then alpha=0.0
  endif
  if (type eq 'g') then begin
    if fract le x0 then alphab=ry0 else alphab=ry0+(fract-x0)*(ry1-ry0)/(x1-x0)
    if alphab lt 0 then alphab=0.0
    alpha=1.0-alphab-alpha
  endif
  return, alpha
End