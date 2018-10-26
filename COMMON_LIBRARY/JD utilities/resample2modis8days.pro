PRO resample2MODIS8days, x, JD, x_out, jd_out
doyModComp = INDGEN(46)*8+1
;make JD to DOY 
doy = JD2DOY(JD)
;determine the composite number for each day (comp number = 1 (DOY 1 to 9) to 46 (DOY 361 to end of the year, that is maximum DOY 366)
compID = CEIL(doy/8.0)

;now the CONTIGUOUS elmenent with the same compID belongs to the same 8 day composite
;Build a vector of index 
uniqueID = LONARR(N_ELEMENTS(JD)) ;this is initally set to 0 (example doy=[1,2,3,4,5,6,7,8,11,15,359,360,362,365], uniqueID = [ 0,0,0,0,0,0,0,0,1,1,2,2,3,3]
FOR i = 1, N_ELEMENTS(JD)-1 DO BEGIN
  IF (compID[i] EQ compID[i-1]) THEN uniqueID[i] = uniqueID[i-1] ELSE uniqueID[i] = uniqueID[i-1] + 1 
ENDFOR
;for the new x assign the first JD of the composite
;for the new y assign the mean (not all elements may be present)
x_out = FLTARR(uniqueID[-1] + 1)
jd_out = x_out
FOR i = 0, uniqueID[-1] DO BEGIN
  ind = WHERE(uniqueID EQ i)
  IF (TOTAL(FINITE(x[ind])) EQ 0) THEN x_out[i] = !VALUES.F_NAN ELSE x_out[i] = MEAN(x[ind], /NAN)
  ;retrive DOY of first day of composite
  doy_of_comp = (compID[ind[0]]-1) * 8 + 1
  ;retriev the year
  year_of_comp = JD2YEAR(JD[ind[0]])
  jd_out[i] = DOY_YEAR2JD(doy_of_comp, year_of_comp)
ENDFOR
END