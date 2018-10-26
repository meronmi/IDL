PRO scatter_color_final, im1,im2,im1_range,im2_range, thh=thh, ngrid=ngrid, $
    xtit=xtit,ytit=ytit,tit=tit, fpng=fpng,   $
    frec_min=frec_min,frec_max=frec_max,elimina=elimina,window=window, $
    charsize=charsize,charthick=charthick, muestra_reg=muestra_reg,winsize=winsize, $
    xmargin=xmargin,ymargin=ymargin,tipo_reg=tipo_reg,stat=stat,posleg=posleg, $
    escribe_stat=escribe_stat,pos_stat=pos_stat

if n_elements(xmargin) le 1 then xmargin=[10,3]
if n_elements(ymargin) le 1 then ymargin=[4,2]

loadct,39

; im1, im2           --> imagenes  (o vectores) de igual dimensiones
; im1_range,im2_range   --> rangos entre los cuales hacemos el scatterplot

; thh vector de 3 o 4 valores: (th_min,th_max,fac_multip, fac_aditivo) ;factor aditivo no es obligatorio
; tambien puede ser matriz, permitiendo umbrales diferentes para im1 e im2:
;    thh(3,2) o thh(4,2) (dependiendo de si hay o no factor_aditivo)

; ngrid :divisiones (resolucion del scatterplot)
; ejes: xtit, ytit, tit
; fpng: nombre del fichero .png en el que guardaremos el plot

; frec_min (no obligatorio) solo pinta valores con frecuencia > frec_min
; frec_max (no obligatorio) satura valores con frecuencia > frec_max
    ; si no pasamos frec_max se calcula automaticamente, sirve como argumento output (si se le pasa null)
    ; el algoritmo automatico se basa en histograma (caida y posterior sucesion de 20 valores bajos)

;stat: devuelve RMS, recta de regresion, r, r^2(en el caso lineal),[A(0),A(1),A(2),rms] en el caso exp y s�lo el RMS en otro caso.

;elimina (si se le pasa como argumento, filtra previamente a hacer stat frec<frec_min

;window (no obligatorio, permite elegir una subventana)


;muestra_reg
; puede ser 1: solo recta 1:1
;           2: solo ajuste
;           3: recta 1:1 + ajuste
;   else/no se le pasa nada:    no hace nada

;posleg: si vale 0 --> no dibuja tabla de color
;      si es vector posicion(x,y) en coordenadas normalizadas 0-1 --> dibuja tabla

;escribe_stat  --> vector cuyos elementos pueden ser un subconjunto de ['ajuste','RMS','r']


frec_min_default=10

if n_elements(xtit) le 0 then xtit=''
if n_elements(ytit) le 0 then ytit=''
if n_elements(tit) le 0 then tit=''
if n_elements(frec_min) le 0 then frec_min=frec_min_default
if n_elements(ngrid) le 0 then ngrid=100
if n_elements(charsize) le 0 then charsize=3.0
if n_elements(window) gt 3 then begin
    im1=im1(window(0):window(1),window(2):window(3))
    im2=im2(window(0):window(1),window(2):window(3))
endif


IF ARG_PRESENT(thh) THEN begin
    siz=size(thh)
    th1=thh(*,0)
    if siz(0) eq 2 then th2=thh(*,1) else th2=thh
    wh=where(im1 gt th1(0) and im1 lt th1(1) and im2 gt th2(0) and im2 lt th2(1))
    im1=im1(wh)
    im2=im2(wh)
    im1=im1*th1(2)
    im2=im2*th2(2)
    if n_elements(th1) eq 4 then im1=im1+th1(3)
    if n_elements(th2) eq 4 then im2=im2+th2(3)
endif

print,histogram(im1*10,min=0,max=60)
print,histogram(im2*10,min=0,max=60)
print,min(im1),min(im2)
print,max(im1),max(im2)
;print,th1
;print,th2
;print,im1_range
;print,im2_range


; INPUTS
; im1,min2: (matrices o vectores)
; im1_range: vector, eg. [0.0001,1] tomaria solo eventos en ese rango
; im2_range: vector, eg. [0.0001,1] tomaria solo eventos en ese rango
; xtit, ytit, tit=tit
; binsize=
; fpng (si se le pasa, guarda en el fichero la grafica)

;INPUTS y OUTPUTS
;frec_min: valores con frecuencia mas baja no se muestran
;frec_max: frecuencia de saturacion (a calcular)

;OUTPUTS
;stat [const,slope,r,r^2,rms]

if n_elements(winsize) gt 0 then begin
  xsize=winsize(0)
  ysize=winsize(1)
endif else begin
    xsize=640 & ysize=480;dimensiones en pixeles de la ventana de representacion
    xsize=1280 & ysize=960;dimensiones en pixeles de la ventana de representacion
endelse

;************************Creacion de leyenda
device,decomposed=0
window,1,xsize=xsize,ysize=ysize

;*****************************************
min1=im1_range(0) & max1=im1_range(1)
min2=im2_range(0) & max2=im2_range(1)

;********************************************
;ngrid=(max1-min1)/bin+1
bin1=(max1-min1)/(ngrid-1.0)
bin2=(max2-min2)/(ngrid-1.0)
hist=hist_2d(im1,im2,bin1=bin1,bin2=bin2,max1=max1,max2=max2,min1=min1,min2=min2)

wh=where(im1 gt im1_range(0) and im1 le im1_range(1) and im2 gt im2_range(0) and im2 le im2_range(1) )


IF n_elements(frec_max) le 0 then begin ;frecuencia que antecede 20 valores bajos seguidos (con suma menor de 10)
  seguidos=20
  menor_que=10 ;la suma de los "seguidos" debe ser menor que este valor
  h=histogram(hist)
  n=n_elements(h)
  ac=lonarr(n)
  for i=20l,n-1 do ac(i)=total(h(i-seguidos:i))
  wh=where(h gt 0)
  ac(wh(0):wh(0)+seguidos)=10
  wh=where(ac lt menor_que)
  frec_max=wh(0)-seguidos
  plot,h,yrange=[0,100]
  print,'frec_max=',frec_max
 ; print,ac(0:100)
endif


IF KEYWORD_SET(elimina) THEN begin
binx=min1+indgen(ngrid)*bin1
biny=min2+indgen(ngrid)*bin2
for i=0,ngrid-2 do begin
for j=0,ngrid-2 do begin
    if hist(i,j) lt frec_min then begin
      wh=where( im1 gt binx(i) and im1 le binx(i+1) and im2 gt biny(j) and im2 le biny(j+1),cont)
      if cont gt 0 then begin
        im1(wh)=im1_range(0)-1
        im2(wh)=im2_range(0)-1
      endif
    endif
endfor
endfor
endif

;out=where(hist gt frec_min)

xx=[0,100]

wh=where(hist le frec_min,cont) & if cont gt 0 then hist(wh)=0
wh=where(hist ge frec_max,cont) & if cont gt 0 then hist(wh)=frec_max

wh=where(im1 gt im1_range(0) and im1 le im1_range(1) and im2 gt im2_range(0) and im2 le im2_range(1) )

;***********************Calculo de valor RMS
rms=sqrt(total(double(im1(wh)-im2(wh))^2/N_elements(im1(wh))))
makechar,rms,3,rmschar
print,'rms ESTOY AQUI...EOEEEE',rms

divx=bin1*findgen(ngrid)
divy=bin2*findgen(ngrid)
xrange=im1_range & yrange=im2_range ;intervalo que se representa...
norm256hist=hist*255/max(hist)
quitonivelesbajos=0
ind=indgen(257)
levels=where(ind gt quitonivelesbajos);para quitar el negro y tener un fondo blanco
;  !p.font=0  &  DEVICE, SET_FONT = 'TIMES*BOLD*20'

;!p.font=-1
contour,norm256hist,divx,divy,/fill,LEVELS=levels,background=255,color=0, $
charsize=charsize,charthick=charthick,xtitle=xtit,ytitle=ytit,title=tit,$
xrange=xrange,yrange=yrange,xmargin=xmargin,ymargin=ymargin,xstyle=1,ystyle=1
xx=(indgen(201)/200.)*10
 ;   oplot,xx,xx,color=6754455 ;nos representa el valor ideal de la pendiente

case tipo_reg of  

'lin':begin
    ;**********************
    ;Zona de ajuste lineal
 ;   slope=regress(im1(wh),im2(wh),const=const,correlation=corr)
    slope=regress(im1(wh),im2(wh),const=const,correlation=corr)
    yy=slope#xx+const;
    makechar,slope,3,slopechar & makechar,const,3,constchar & makechar,corr,3,corrchar
    if const gt 0 then suma='+' else suma=' '
    ajuste='y='+slopechar+'x'+suma+constchar
   ; xyouts,xrange(0)+(xrange(1)-xrange(0))*0.07,yrange(0)+(yrange(1)-yrange(0))*0.88,ajuste,charsize=1.3,color=240
   ; xyouts,xrange(0)+(xrange(1)-xrange(0))*0.07,yrange(0)+(yrange(1)-yrange(0))*0.83,'r='+corrchar,charsize=1.3,color=0
    ;************************

    ;xyouts,xrange(0)+(xrange(1)-xrange(0))*0.07,yrange(0)+(yrange(1)-yrange(0))*0.93,'RMS='+rmschar,charsize=1.3,color=0
    stat=[const,slope,corr,corr^2,rms]
end

'exp_2params':begin
    ;**********************************************************************************
    ;Zona de ajuste de exp

    Y=im2(wh)
    X=im1(wh)
    weights=X*0+1
    weights=Y*0+1
    A=[1.0,-0.55] ;valores iniciales para el ajuste �ptimos para FAPAR
    yfit=MPCURVEFIT(X,Y,weights,A,SIGMA,FUNCTION_NAME='gfunct_exp_2params',ITMAX=50,ITER=ITER)
    yy=A(0)*exp(A(1)*xx)
    makechar,A(0),2,prod & makechar,A(1),2,argexp
    if A(0) gt 0 then suma='+' else suma=''
    ajuste='y='+suma+prod+'exp('+argexp+' X)'
    stat=[A(1),A(0),rms]
end

'exp_3params':begin
    ;**********************************************************************************
    ;Zona de ajuste de exp
    Y=im2(wh)
    X=im1(wh)
    weights=X*0+1
    weights=Y*0+1
    A=[-1.0,-0.55,0.99] ;valores iniciales para el ajuste optimos para FAPAR
    yfit=MPCURVEFIT(X,Y,weights,A,SIGMA,FUNCTION_NAME='gfunct_exp_3params',ITMAX=50,ITER=ITER)

    yy=A(2)+A(0)*exp(A(1)*xx)
    makechar,A(2),2,indep & makechar,A(0),2,prod & makechar,A(1),2,argexp
    if A(0) gt 0 then suma='+' else suma=''
    ajuste='y='+indep+suma+prod+'exp('+argexp+' X)'

    stat=[A(2),A(1),A(0),rms]
end

    'else':begin
    stat=[rms]
    end
endcase

if n_elements(pos_stat) le 0 then begin
  pos_stat=fltarr(2)
  ;pos_stat(0)=0.5
  pos_stat(0)=0.07
  pos_stat(1)=0.92
endif

inix=xrange(0)+(xrange(1)-xrange(0))*pos_stat(0)
iniy=yrange(0)+(yrange(1)-yrange(0))*pos_stat(1)
incy=(yrange(1)-yrange(0))*0.04*(charsize+1)


pos1=[inix,iniy]
pos2=[inix,iniy-incy]
pos3=[inix,iniy-2*incy]   
    

if n_elements(escribe_stat) gt 0 then begin
nstat=n_elements(escribe_stat)
tit=strarr(nstat)
if nstat gt 0 then begin
  for i=0,nstat-1 do begin
    case escribe_stat(i) of
    'r':tit(i)='r='+corrchar
    'RMS':tit(i)='RMS='+rmschar
    'ajuste':tit(i)=ajuste
    endcase
  endfor
endif
wh=where(tit ne '',nstat) ;******************** LOS ESTADÍSTICOS POSICION CAMBIAR DE AQUI ANITA
tit=tit(0:nstat-1)
for i=0,nstat-1 do begin
   if strmid(tit(i),0,2) eq 'y=' then color=240 else color=0
   ;xyouts,inix,iniy-i*incy,tit(i),color=0, charsize=charsize,charthick=2.5
   xyouts,inix,iniy-0.1*(i),tit(i),color=0, charsize=3.0,charthick=3.2
   ;xyouts,inix,iniy-1*(i),tit(i),color=0, charsize=3.0,charthick=3.2
   print,'iiii',iniy-0.005*(i)
endfor
endif

;***************************************************

if n_elements(posleg) eq 2 then begin
  anchura=xsize*0.15 & altura=ysize*0.04
  leyenda4=bytarr(anchura,altura)
  for i=0,anchura-1,1 do leyenda4(i,*)=i*255./anchura
  tv,leyenda4,posleg(0),posleg(1),/norm
  xyouts,posleg(0),posleg(1)-0.05,strcompress(string(min(hist)),/remove_all),color=0,charsize=3.0,charthick=3.0, /norm
  xyouts,posleg(0)+0.14,posleg(1)-0.05,strcompress(string(max(hist)),/remove_all),color=0,charsize=3.0,charthick=3.0, /norm
endif

;xyouts,po1(0),po1(1),strcompress(string(max(hist)),/remove_all),color=0,charsize=charsize,charthick=charthick
;xyouts,po2(0),po2(1),strcompress(string(min(hist)),/remove_all),color=0,charsize=charsize,charthick=charthick

if n_elements(muestra_reg) eq 1 then begin

case muestra_reg of
1: oplot,xx,xx,color=fsc_color('black'),thick=1.5,linestyle=2
2: oplot,xx,yy,color=fsc_color('black'),thick=1.5
3:begin
    oplot,xx,xx,color=fsc_color('black'),thick=1.5,linestyle=2
    oplot,xx,yy,color=fsc_color('black'),thick=2
end
'else':
endcase

endif

;******** guarda la ventana del plot en el fichero fpng
if n_elements(fpng) gt 0 then begin
	 write_jpeg,fpng,tvrd(true=1),true=1,quality=100
endif
  
y=histogram(im1(wh)-im2(wh),min=-3,max=3,binsize=0.1,locations=x)
window,4
plot,x,y
!p.font=-1
end

pro gfunct_exp_3params, x, a, f, pder
; Function + partials dd
; a=[-1.0,1.0,0.5]
     ;  x=findgen(1000)
         bx = exp(a(1) * x)
         f= a(0) * bx + a(2)         ;Evaluate the function
        ; IF N_PARAMS() ge 4 THEN pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(f))]]
    IF N_PARAMS() GE 4 THEN pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(f))]]

 end

pro gfunct_exp_2params, x, a, f, pder
; Function + partials dd
; a=[-1.0,1.0,0.5]
     ;  x=findgen(1000)
         bx = exp(a(1) * x)
         f= a(0) * bx          ;Evaluate the function
        ; IF N_PARAMS() ge 4 THEN pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(f))]]
    IF N_PARAMS() GE 4 THEN pder= [[bx], [a(0) * x * bx]]

 end