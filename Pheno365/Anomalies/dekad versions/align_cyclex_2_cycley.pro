FUNCTION align_cycleX_2_cycleY, pdekadX, ord_Xfrst_dek, Xfrst_year, $
                                ord_Yfrst_dek, Yfrst_year
; Purpose:
;   Align the output of Pheno algorithm (pdekadX, progressive dekad from ordinal dekad
;   X, for VGT is X=10, the 10th, so is from dekad 9.1-10.0) which can only contain 1 season per year to 
;   an arbitrary 36 dekads cycle starting at ordinat Y (e.f. Y = 7 is the 7th, 6.1-7.0, the year going
;   from beginning of March to end of February).
;   
;   In order to define the correct match between the SOS and the arbitrary annual cycle 
;   we use the criterion of minimum distance of SOSs with respect to the midpoint of the 
;   arbitrary annual cycle. 
;   We exploit here the fact that the SOS time series is ordered by algorithm definition.
;   So there are two possibility for matching it with the midpoint of the arbitrary cycle:
;   midpoints align with the previous or with the following SOS. Which one is correct is 
;   decided by finding the matching minimizing the sum of square (SOS(i)-midpoint(i)).
;   
;   The output is represented by an array of dimension     

; Input parameters: 
;   pdekadX: progressive dekad of cycle X (0.5 added at realign)
;   ord_Xfrst_dek: ordinal dekad offsetting the series of cycle X (10 for VGT)
;              it's the actual dekad of the value 0
;   Xfrst_year: first calendar year in the input cycle X (1998 for VGT)
;   ord_Yfrst_dek: aribtrary first ordinal dekad of cycle Y
;   Yfrst_year: first calendar year in the output cycle Y (<X)      
;
; Output parameters:
;   Array (dimension = number of years in the ouput cycle Y) pointing to
;   the correct band in the pheno output. Elements not pointing to any band
;   are set to -999
;
; Restrictions:
;  pdekadX must be a pheno date (e.g. SOS) output of Pheno so that only one
;  event is recorder each year (36 dekads) 

Xfrst_dek = ord_Xfrst_dek -1
Yfrst_dek = ord_Yfrst_dek -1 

n_year_in = N_ELEMENTS(pdekadX)
IF (Yfrst_year GE Xfrst_year) THEN STOP
n_year_out = n_year_in + (Xfrst_year - Yfrst_year)

;Manage the case when all pdekad are NaN or failures
indf = WHERE((FINITE(pdekadX) EQ 1) AND (pdekadX NE -999), countf)
IF (countf EQ 0) THEN BEGIN ;this profile would have no data at all. It shoul be impossible with pheno after 17 aug 2011), because periodicity is found but all failed (all -999). Anyhow, data are never present, this shortcut should work (?)
  tmp = INDGEN(n_year_out) & tmp[n_year_out-1]=-999
  RETURN, tmp 
END
;Replace -999 with Nan
pdekadXn = pdekadX
ind999 = WHERE(pdekadX EQ -999, count999)
IF (count999 NE 0) THEN pdekadXn[ind999] = !VALUES.F_NAN 
;Express pdekadX as pdekadY from dekad 1
pdekadX2Yf1 = pdekadXn +  Xfrst_dek + (Xfrst_year - Yfrst_year) * 36
;Compute midpoint of each cycle Y expressed as pdekadY from1 
mid_point_of_cycle_pdekadYf1 = INDGEN(n_year_out)*36 +  Yfrst_dek + (36/2) 
;now align it according to the criterion of minimum distance with respect to
;the midpoint
cost = FLTARR(n_year_out-n_year_in + 2)
indf = WHERE(FINITE(pdekadX2Yf1) EQ 1)
a = pdekadX2Yf1[indf]
;FOR i = 0, (n_year_out-n_year_in) DO BEGIN
;  b = mid_point_of_cycle_pdekadYf1[i:i+n_year_in-1]
;  b = b[indf]
;  cost[i] = TOTAL((a - b)^2, /DOUBLE)
;ENDFOR 
;MM 17/02/2012. it may happen that the minimum is not reached within (n_year_out-n_year_in)
FOR i = 0, (n_year_out - n_year_in + 1) DO BEGIN
  ii = i + n_year_in - 1
  IF (ii GT N_ELEMENTS(mid_point_of_cycle_pdekadYf1) - 1) THEN ii = N_ELEMENTS(mid_point_of_cycle_pdekadYf1) - 1  
  b = mid_point_of_cycle_pdekadYf1[i:ii]
  b = b[indf]
  cost[i] = TOTAL((a - b)^2, /DOUBLE)
ENDFOR 

mincost = MIN(cost, minind)
res = INTARR(n_year_out) - 999
ii = minind + n_year_in - 1
IF (ii GT N_ELEMENTS(res) - 1) THEN ii = N_ELEMENTS(res) - 1
res[minind : ii] = INDGEN(ii - minind + 1)
;res[minind : (minind + n_year_in - 1)] = INDGEN(n_year_in)
;res[minind : ii] = INDGEN(n_year_in)
;print, res
;ind = WHERE(res NE -999)
;print, pdekadX2Yf1[ind]
RETURN, res
END