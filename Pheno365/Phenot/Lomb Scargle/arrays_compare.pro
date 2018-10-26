FUNCTION arrays_compare, array1, array2

combined = [array1,array2]
combined = combined[uniq(combined,sort(combined))]

for i=0,n_elements(combined)-1 do begin

array1_ind = WHERE(array1 EQ combined[i]) 
array2_ind = WHERE(array2 EQ combined[i])

IF array1_ind EQ -1 THEN BEGIN

  IF n_elements(array1_dif) EQ 0 THEN array1_dif = combined[i] ELSE array1_dif = [array1_dif,combined[i]]
  IF n_elements(array2_dif_ind) EQ 0 THEN array2_dif_ind = array2_ind ELSE array2_dif_ind = [array2_dif_ind, array2_ind] 

ENDIF

IF array2_ind EQ -1 THEN BEGIN

  IF n_elements(array2_dif) EQ 0 THEN array2_dif = combined[i] ELSE array2_dif = [array2_dif,combined[i]]
  IF n_elements(array1_dif_ind) EQ 0 THEN array1_dif_ind = array1_ind ELSE array1_dif_ind = [array1_dif_ind, array1_ind]

ENDIF

IF (array1_ind NE -1) AND (array2_ind NE -1) THEN BEGIN

  IF n_elements(arrays_same) EQ 0 THEN arrays_same = combined[i] ELSE arrays_same = [arrays_same,combined[i]]
  IF n_elements(array1_same_ind) EQ 0 THEN array1_same_ind = array1_ind ELSE array1_same_ind = [array1_same_ind, array1_ind]  
  IF n_elements(array2_same_ind) EQ 0 THEN array2_same_ind = array2_ind ELSE array2_same_ind = [array2_same_ind, array2_ind]  

ENDIF

endfor
  
  IF n_elements(array1_dif) EQ 0 THEN array1_dif = 'Nan'
  IF n_elements(array1_dif_ind) EQ 0 THEN array1_dif_ind = 'Nan'
  IF n_elements(array2_dif) EQ 0 THEN array2_dif = 'Nan'
  IF n_elements(array2_dif_ind) EQ 0 THEN array2_dif_ind = 'Nan'
  IF n_elements(arrays_same) EQ 0 THEN arrays_same = 'Nan'
  IF n_elements(array1_same_ind) EQ 0 THEN array1_same_ind = 'Nan'
  IF n_elements(array2_same_ind) EQ 0 THEN array2_same_ind = 'Nan'

arr1 = {name:'', mis:array1_dif, ind_dif:array1_dif_ind, same:arrays_same, ind_same:array1_same_ind}
arr2 = {name:'', mis:array2_dif, ind_dif:array2_dif_ind, same:arrays_same, ind_same:array2_same_ind}

result = {name:'', arr1:arr1, arr2:arr2}

return, result

END