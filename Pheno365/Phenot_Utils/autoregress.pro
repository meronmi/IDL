Pro autoregress
;obsolete
;used to check the result of autoregress
a=[0,  0.241921895599668,  0.469471562785891,  0.669130606358858,  0.829037572555042,  0.939692620785908,  0.994521895368273,  0.99026806874157,   0.927183854566787,  0.809016994374947,  0.642787609686539,  0.438371146789077,  0.207911690817759,  0,  0,  0.25,   0.433012701892219,  0.5,  0.433012701892219,  0.25,   6.1257422745431E-17,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0] 
c=[a,a,a,a,a,a,a,a]
c=c/1.5+0.2


k=N_ELEMENTS(c)
err=randomu(2,k)/10.0

yt=c+err
;yt=err  ;only noise
window, /free, title='data'
plot, yt[0:35]
n=36
x=fltarr(n,k)
for r=0,k-1,n do begin
  for c=0,n-1 do begin
    x[c,r+c]=1
  endfor
endfor
y=x[0:10,0:10]
print, y
coeff=regress(x,yt,const=const,correlation=r, mcorrelation=mr, ftest=f, sigma=sigma)
print, 'const'
print, const
print, 'multicorrelation'
print, mr
print, 'coeff'
print, transpose(coeff)
print, 'r'
print, r
window, /free, title='R'
plot, r^2
window, /free, title='const+coeff'
plot, const+coeff
window, /free, title='b/sigma'
;thi is t
plot, coeff/sigma
Result = 1-(1-2*(1-T_PDF(coeff/sigma, k-n-1))) 
window, /free, title='p-value'
;thi is t
plot, Result


coeffa = TS_COEF( yt, 35 , /DOUBLE) 

window, /free, title='autoreg coeff'
plot, coeffa
  


print, 'finito'


End