PRO test0
;x = FINDGEN(10)*30.0 
x = 10+[0.000000,30.0000,60.0000,88.0000,120.000,150.000,180.000,210.000,240.000,270.000]
y = x*0.0+10.0;[2,2,2,2,0,0,0,0,4,4]
PRINT, x
PRINT, y
TIC
res =  REFORM(irreg_grid_cum(x,y,60))
TOC
PRINT, res
END

FUNCTION irreg_grid_cum, x, y, win
; Thif function compute the the backward cumulation over a windon win
; on a x vector that is irregularly gridded  
; x (irregular) and y (any)
; win: ditance backward (in x units)  

n = N_ELEMENTS(x)
;xcol = CMREPLICATE(FLOAT(x),  n)
xcol = CMREPLICATE(x,  n)
;y = FLOAT(y)
;xrow = TRANSPOSE(xcol)
;PRINT, xcol
;PRINT, '**'
;PRINT, xrow
;xdiff is the distance betwee x elements (note that this  can be save once and for all)
xdiff = (xcol-TRANSPOSE(xcol))
;PRINT, '**'
;PRINT, xdiff
;here I have to take only the zero distance and the negative part up to win
;xdiff = (xdiff LE 0) AND (xdiff GT -win)
;use the sparse representation
sparse_xidiff = SPRSIN((xdiff LE 0) AND (xdiff GE -win))
;PRINT, '**'
;PRINT, sparse_xidiff
;PRINT, y
;PRINT, REFORM(xdiff ## y)

;PRINT, SPRSAX(sparse_xidiff,y)
;RETURN, xdiff ## y
RETURN, SPRSAX(sparse_xidiff,y)
END