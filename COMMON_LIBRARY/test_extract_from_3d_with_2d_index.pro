PRO test_extract_from_3D_with_2D_index
  ;3d matrix
  array3D = INDGEN(6,4,5)
  PRINT, array3D
  ;index of a 2 D matrix
  array2D = REFORM(array3D[*,*,0])
  ind = WHERE(array2D GE 20)
  dims = SIZE(array2D, /Dimensions) 
  colrow = Array_Indices(dims, ind, /Dimensions) 
  
  dims = Size(array3D, /DIMENSIONS )
  array3D = Reform(array3D, dims[0]*dims[1], dims[2], /OVERWRITE )
  
  pairIndicies = colrow[0,*] + (colrow[1,*] * dims[0])
  subset = array3D[pairIndicies, *]
  
  PRINT, ''
  help, subset
  PRINT, subset
  
END