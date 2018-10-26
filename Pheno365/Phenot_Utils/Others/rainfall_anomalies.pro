Function rainfall_anomalies, mat0
mat=FLOAT(mat0)
boi = 13
nb = N_ELEMENTS(mat[0,0,*])
ns = N_ELEMENTS(mat[*,0,0])
nl = N_ELEMENTS(mat[0,*,0])
nodata = -32768
out = FLTARR(ns,nl)
ind = WHERE(mat EQ nodata, count)*!VALUES.F_NAN
IF (count GT 0) then mat[ind]=!VALUES.F_NAN
;remove 1998
mat[*,*,0] = !VALUES.F_NAN
FOR s=0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    ind = WHERE(FINITE(mat[s,l,*]) EQ 1, count)
    IF (count GT 0) THEN out(s,l)=(mat[s,l,boi]-mean(mat[s,l,*], /NAN, /DOUBLE))/stddev(mat[s,l,*], /NAN, /DOUBLE)
    ;IF (count GT 0) THEN out(s,l)=(mat[s,l,boi]-mean(mat[s,l,*], /NAN, /DOUBLE))
  ENDFOR
ENDFOR
RETURN, out
END

Function rainfall_anomalies2, mat0, eos0
mat=FLOAT(mat0)
eos=FLOAT(eos0)

boi = [13,12]
nb = N_ELEMENTS(mat[0,0,*])
ns = N_ELEMENTS(mat[*,0,0])
nl = N_ELEMENTS(mat[0,*,0])
nodata = -32768
out = FLTARR(ns,nl)*!VALUES.F_NAN
ind = WHERE(mat EQ nodata, count)
IF (count GT 0) then mat[ind]=!VALUES.F_NAN
;remove 1998
mat[*,*,0] = !VALUES.F_NAN
FOR s=0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    IF (eos[s,l] LT 27) THEN BEGIN 
      ind = WHERE(FINITE(mat[s,l,*]) EQ 1, count)
      ;IF (count GT 0) THEN out(s,l)=(mat[s,l,boi[0]]-mean(mat[s,l,*], /NAN, /DOUBLE))/stddev(mat[s,l,*], /NAN, /DOUBLE)
      IF (count GT 0) THEN out(s,l)=(mat[s,l,boi[0]]-mean(mat[s,l,*], /NAN, /DOUBLE))
    ENDIF ELSE BEGIN
      ind = WHERE(FINITE(mat[s,l,*]) EQ 1, count)
      ;IF (count GT 0) THEN out(s,l)=(mat[s,l,boi[1]]-mean(mat[s,l,*], /NAN, /DOUBLE))/stddev(mat[s,l,*], /NAN, /DOUBLE)
      IF (count GT 0) THEN out(s,l)=(mat[s,l,boi[1]]-mean(mat[s,l,*], /NAN, /DOUBLE))
    ENDELSE
  ENDFOR
ENDFOR
RETURN, out
END