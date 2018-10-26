FUNCTION numrec_avevar, data, n, ave, var

;  Purpose:
;     To compute the mean and variance of an array.

;  Usage:
;     rc = avevar (data, n, ave, var)

;  Input arguments:
;     * data {floating point array}: The one-dimensional data array.
;     * n {integer}: The dimension of the data array.

;  Output arguments:
;     * ave {floating point}: The average value of the array.
;     * var {floating point}: The variance of the data array.

;  Remarks:
;     This routine is an IDL implementation of the Numerical Recipes subroutine
;     period. Warning: The accumulation loops have been modified to account for
;     the fact that FORTRAN arrays tyically start at index 1 while IDL arrays
;     start at index 0.

;  Initialize the return code:
retcode = 0

;  Initialize the average value, accumulate the sum and compute the average:
ave = 0.0
FOR j = 0, n - 1 DO ave = ave + data [j]
ave = ave / n

;  Initialize the variance value, accumulate the sum of deviations from the
;  mean and compute the variance:
var = 0.0
ep = 0.0
FOR j = 0, n - 1 DO BEGIN
   s = data [j] - ave
   ep = ep + s
   var = var + s * s
ENDFOR
var = (var - ep^2 / n) / (n - 1)

RETURN, retcode

END