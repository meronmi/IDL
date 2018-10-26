FUNCTION repeatArray, array, n
arrOut = [!NULL]
FOR i = 0, n-1 DO arrOut = [arrOut, array]
RETURN, arrOut
END