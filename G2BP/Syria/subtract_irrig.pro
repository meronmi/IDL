FUNCTION subtract_irrig, img

file = 'X:\SYRIA\profiles of irrigated polygons\mean_prof_irrigated_turkey up to 09 2014.csv'
res = READ_CSV(file)
ndvi_irr= res.FIELD1
ndvi_irr = FLOAT(ndvi_irr)
help, ndvi_irr
help, img
gh = PLOT(ndvi_irr)
img2 = img * !VALUES.F_NAN
FOR i = 0, N_ELEMENTS(ndvi_irr)-1 DO BEGIN
  img2[*,*,i] = img[*,*,i] - ndvi_irr[i]
ENDFOR

RETURN, img2
END