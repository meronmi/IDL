;rc=modifiedEXTREMA('max', cor, ind)

Function modifiedEXTREMA, minmax, array, ind, n
;ret code
;0  : normal completion
;10 : no 'min' or 'max' requested
;30 : unrecognized data periodicity

IF (minmax NE 'min') AND (minmax NE 'max') THEN RETURN, 10  
y=array
IF (minmax EQ 'min') THEN y=-y    ;change the sign of y accordingly to work on max only
ind=EXTREMA(y, /MAX_ONLY, NUMBER=n) 
;check that 
;minima or maxima are 'big' enough and are not noise
;Here, they have to be a max in 11 (hg*2+1) elements windows. 
;It is assumed that a gs cannot be smaller than 10 decades
;they are not at the lower or upper borders 
;(when looking for minima in medyear, medyear is replicated 3 times
;they have absolute values gt 0.1 (for maxima, wich is correlation)
; and lt 0.8 for minima (general knowledge)


hw=5  ;half wing 
if (n gt 1) then begin                  ;(if only one, just take it)
  tmp_ind=INTARR(n)                     ;make an array to store good maxima
  for k=0, n-1 do begin
    ;check that it is not ath the borders
    if ((ind[k] lt hw+1) OR (ind[k] gt (N_ELEMENTS(y)-hw-1)))  then begin
      tmp_ind[k]=-99
    endif else begin
      IF (y[ind[k]] LT MAX([[y[ind[k]-hw:ind[k]-1]],[y[ind[k]+1:ind[k]+hw]]])) THEN BEGIN
        tmp_ind[k]=-99
      ENDIF ELSE BEGIN
      ;can be GT or GE because another elements has the same value
        IF (y[ind[k]] GT MAX([[y[ind[k]-hw:ind[k]-1]],[y[ind[k]+1:ind[k]+hw]]])) THEN BEGIN
          ;it's stricly greater, check that is above or below limits
          IF (minmax EQ 'max') THEN IF (y[ind[k]] gt 0.1) THEN tmp_ind[k]=ind[k] ELSE tmp_ind[k]=-99
          IF (minmax EQ 'min') THEN IF (y[ind[k]] gt -0.8) THEN tmp_ind[k]=ind[k] ELSE tmp_ind[k]=-99
        ENDIF ELSE BEGIN
          ;it's GE, there is one or more elemnts that are the same in the window
          ;choose the mid point
          valmax=MAX(y[ind[k]-hw:ind[k]+hw])
          indequal=WHERE(valmax EQ y[ind[k]-hw:ind[k]+hw])
          tmp_ind[k]=FLOOR(MEAN(indequal))+ind[k]-hw   
        ENDELSE
      ENDELSE      
    endelse 
  endfor
  t2=where(tmp_ind gt -1, counts)
  if counts eq 0 then begin
    ;unrecognized data periodicity
    ind=-1
    return, 30 
  endif else begin
    ;check that ther are no repeated elements (because of GE above
    ind=tmp_ind[t2]
    inds=ind(SORT(ind))
    uniqindex=UNIQ(inds)
    ind=inds[uniqindex]
    ;cheche that they are not to far away?   
  endelse
endif
return, 0
End