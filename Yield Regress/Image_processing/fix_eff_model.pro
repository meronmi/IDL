FUNCTION fix_eff_model, x, y, ids, yrs
;COMPUTE FIXED EFFECT MODEL REGRESSION, one slope, many offsets
ord_ids=uniqlist(ids)
ind=SORT(ord_ids)
ord_ids=ord_ids[ind]
n = N_ELEMENTS(ord_ids)
;make the X matrix
ind=SORT(ids)
ids0=ids[ind]
x0=x[ind]
y0=y[ind]
yrs0=yrs[ind]
mat=DBLARR(n+1,N_ELEMENTS(y))
mat[0,*]=x0
FOR i=0, n-1 DO BEGIN
  ind1 = WHERE(ids0 EQ ord_ids[i])
  ind0 = WHERE(ids0 NE ord_ids[i])
  mat[i+1,ind1]=1.0
  mat[i+1,ind0]=0.0
ENDFOR
c = REGRESS(mat,y0,const=b0,/DOUBLE)
;to be checked, what happen to the constant?
RETURN, 0
END