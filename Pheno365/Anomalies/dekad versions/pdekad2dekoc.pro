Function pdekad2dekoc, data, ord_offset_dek_in, ord_offset_dek_out, avg_vec=avg_vec

;optionally return a vector containing all the averages expressed in pdekad

;ord_offset_dek_in, ord_offset_dek_out are givenin ordinal dekads
;as output you get dekoc (that can be 1-36 if its in the right year, -x if it falls in the previous,
;GT 36 = 36+x if it falls in the following

abs_offset_dek_in = ord_offset_dek_in - 1
abs_offset_dek_out = ord_offset_dek_out - 1 
nb = N_ELEMENTS(DATA)

;store positions of season failures
ind999=WHERE(data EQ -999, count999)

;Manage the case when all pdekad are NaN or failures
indf = WHERE((FINITE(data) EQ 1) AND (data NE -999), countf)
IF (countf EQ 0) THEN BEGIN ;this profile would have no data at all. It shoul be impossible with pheno after 17 aug 2011), because periodicity is found but all failed (all -999). Anyhow, data are never present, this shortcut should work (?)
  dataOut = data * !VALUES.F_NAN
  IF KEYWORD_SET(avg_vec) THEN avg_vec = dataOut
ENDIF ELSE BEGIN
  ;Replace -999 with Nan
  dataN = data 
  dataN[indf] = dataN[indf] + abs_offset_dek_in
  IF (count999 NE 0) THEN dataN[ind999] = !VALUES.F_NAN 
  ;the maximum shift should be 1, however use 3 to be on the safe side
  n_in = nb
  ;n_sh = nb + 3
  n_sh = nb + 3 + 3 ;(3 on each side)
  midpoint = FLOAT((INDGEN(n_sh)-3)*36 +  abs_offset_dek_out + (36/2))
  midpoint[0:2]=!VALUES.F_NAN
  ;now align it according to the criterion of minimum distance with respect to the midpoint
  cost = FLTARR(n_sh - n_in + 1)
  indf = WHERE(FINITE(dataN) EQ 1)
  a = dataN[indf]
  FOR i = 0, (n_sh - n_in) DO BEGIN
    b = midpoint[i : i + n_in - 1]
    b = b[indf]
    ;print, FLOAT(a)
    ;print, float(b)
    cost[i] = TOTAL((a - b)^2, /DOUBLE, /NAN)
    ;manage the case where ther are NaN in b
    indNaN = WHERE(FINITE(a - b) NE 1, countNaN)
    indFin =  WHERE(FINITE(a - b) EQ 1, countFin) 
    IF (countNaN EQ N_ELEMENTS(a - b)) THEN $
      cost[i]=!VALUES.F_NAN $
    ELSE cost[i] = cost[i]/countFin
    ;print, cost[i] &  print,'***'
  ENDFOR 
  
  mincost = MIN(cost, minInd, /NAN)
  minInd = minInd - 3
  ;now build the corresponding array with first dekad of the cycle
  arrayFstDek = (INDGEN(n_in) + minInd) * 36 + abs_offset_dek_out
  dataOut =  dataN - arrayFstDek
  IF KEYWORD_SET(avg_vec) THEN BEGIN
   avg_vec = arrayFstDek + MEAN(dataOut, /NAN)
  ENDIF
  IF (count999 NE 0) THEN dataOut[ind999] = -999
ENDELSE
RETURN, dataOut
END