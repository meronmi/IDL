FUNCTION AVG_arr1_with_arr2_spatial_support, arr1, arr2
;  Purpose:
;     To make a spatial average of array1. The spatial extent to support 
;     the average is given by array2. The position of all elements of arr2 having the
;     same value are averaged and written in arr3.
;     For example: arr1=fAPAR, arr2=GAUL2 department, arr3 will be the fAPAR average at 
;     the department level
;     NaN and season failures (-999) are ignored
;  Outcome:
;     arr3, spatial avg of arr1, being arr2 the spatial support
;     to be performed are initialized.

;  Usage:
;     arr3 = AVG_arr1_with_arr2_spatial_support(arr1, arr2)

;  Input parameters: 
;     arr1: array to average, 2D, can by any datatype
;     arr2: spatial support, 2D, must be integer type (normal, long unsigned..). The pixel value
;           is assuemd to be the code used to avg arr1
;
;  Return value: 
;     arr3: spatial avg of arr1 according to arr2
;       -1: an error has occurred
;       -2: imput arrays has inconsistent dimensions


; Check that array dimension are 2D and consistent
szarr1=SIZE(arr1, /DIMENSIONS)
szarr2=SIZE(arr2, /DIMENSIONS)
IF (N_ELEMENTS(szarr1) NE 2) OR (N_ELEMENTS(szarr2) NE 2) THEN RETURN, -2  
IF (szarr1[0] NE szarr2[0]) OR (szarr1[1] NE szarr2[1]) THEN RETURN, -2 

;Find all unique codes
ind=WHERE(FINITE(arr2) EQ 1)
arr=arr2[ind]
sortarr2=arr[SORT(arr)]
ids=sortarr2[UNIQ(sortarr2)]

;Treat season failures, excluding them
arr1nf=arr1
ind999=WHERE(arr1 EQ -999, count999)
IF (count999 NE 0) THEN arr1nf[ind999]=!VALUES.F_NAN

;Perform spatial avg
arr3=arr1*!VALUES.F_NAN
FOR i=0, N_ELEMENTS(ids)-1 DO BEGIN
  IF FINITE(ids[i]) EQ 1 THEN BEGIN
    ind2=WHERE(arr2 EQ ids[i])
    arr3[ind2]=MEAN(arr1nf[ind2], /NAN, /DOUBLE)
  ENDIF ;if it is NaN do nothing, arr3 was initialized as NaN
ENDFOR

RETURN, arr3
END