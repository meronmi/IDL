pro makechar,x,digits,ch
n=n_elements(x)
ch=strarr(n)
for i=0,n-1 do begin
x1=round(x(i)*10^digits)/10.0^digits
sx=strcompress(string(x1),/remove_all)
 partes = STR_SEP(sx, '.')
 n=n_elements(partes)
 ch(i)=partes(0)
 if (n eq 2) then begin
    decimal = STRMID(partes(1), 0 , digits)  ;cojo ndigits decimales
    ch(i)=ch(i)+'.'+decimal
 endif
 endfor
 end