;********** Program that makes a scatter plot from vectors (that can be selected from images)
;********** Date 12-1-2015    

pro scatter_plotDataXY_rabat, data1, data2, x_label, y_label, dir_output, data1_range, data2_range, ngrid, frec_max

;************** EDITABLE AREA*****************************************************************************************************************
;*********************************************************************************************************************************************
;********** PLOT PARAMETERS
;xmargin=[1,1]               
;ymargin=[1,1]

xsize=600 & ysize=600    ;dimensions of the window
charsize=1.5;1.
charthick=1.4;1.4;8
tit='';'Scatterplot ' + x_label + ' Vs. ' + y_label 
xtit=x_label
ytit=y_label
xthick=1.5
ythick=1.5
posleg=[0.7,0.15]       ;legend position
;write_stat=['OLSajuste','GMRFajuste', 'RMS','r', 'AC']
write_stat=['GMRFajuste', 'RMS','r', 'AC']
;********** HISTOGRAM PARAMETERS
frec_min= 0                ; only represents the values with a frequency > frec_min
;frec_max= 50000;20000               ; saturate values with frequency > frec_max (if no defined is calculated authomatically)
;frec_max= 200000
;data1_range=[-1.0,1.0]            ;data 1 valid range
;data2_range=[-1.0,1.0]            ;data 2 valid range
fpng=dir_output + '\' + x_label+'VS'+ y_label + '_scatter.jpg'


;********** 1.Introduction of the images and selection of the vectors of interest *************************************************************
;**********************************************************************************************************************************************
;file1='D:\valmeanNDVI'
;file2='D:\valmaxNDVI'

;
;col=1500
;row=900
;im1=fltarr(col,row)
;im2=fltarr(col,row)
;
;openr,lun,file1,/get_lun
;readu,lun,im1
;free_lun,lun
;
;openr,lun,file2,/get_lun
;readu,lun,im2
;free_lun,lun
;
;wh1=where(im1 gt 0.)
;print,n_elements(wh1)
;wh2=where(im2 gt 0.)
;print,n_elements(wh2) ;******** Selection of the data from the image
;
;data1=im1(wh1)
;data2=im2(wh2)

;************2. Calculation of the histogram ****************************************************************************************************
;************************************************************************************************************************************************

;print,histogram(data1*10,min=0,max=60)
;print,histogram(data2*10,min=0,max=60)
print,min(data1),min(data2)
print,max(data1),max(data2)                                                         ;******real min and max valueas

min1=data1_range(0) & max1=data1_range(1)
min2=data2_range(0) & max2=data2_range(1)                                            ;**** min and maxim values according the defined range

bin1=(max1-min1)/(ngrid-1.0)
bin2=(max2-min2)/(ngrid-1.0)
hist=hist_2d(data1,data2,bin1=bin1,bin2=bin2,max1=max1,max2=max2,min1=min1,min2=min2) ;***two dimensional density function (histogram) of two variables.
wh=where(data1 gt data1_range(0) and data1 le data1_range(1) and data2 gt data2_range(0) and data2 le data2_range(1) ) ;values that are in the interval

;*************3.Saturate maximum frequency and eliminate minimum frequency************************************************************************
;*************************************************************************************************************************************************
wh=where(hist le frec_min,cont) & if cont gt 0 then hist(wh)=0      
wh=where(hist ge frec_max,cont) & if cont gt 0 then hist(wh)=frec_max
wh=where(data1 gt data1_range(0) and data1 le data1_range(1) and data2 gt data2_range(0) and data2 le data2_range(1) ) ;values that are in the interval
norm256hist=hist*255/max(hist) ;***** normalization of the density histogram
;mic
norm256histf=hist*255.0/FLOAT(max(hist))
;**********4. Plot of the histogram ***************************************************************************************************************
;**************************************************************************************************************************************************
loadct,39
device,decomposed=0
window,xsize=xsize,ysize=ysize, /FREE

divx=bin1*findgen(ngrid)+min1
divy=bin2*findgen(ngrid)+min2

deleteblack=0
ind=indgen(257)
levels=where(ind gt deleteblack);****** to remove the black and have a white background
contour,norm256hist,divx,divy,/fill,LEVELS=levels,background=255,color=0,$
charthick = charthick, charsize=charsize,xthick=xthick,ythick=ythick,xtitle=xtit,ytitle=ytit,title=tit,xrange=data1_range,yrange=data2_range

xx=[data1_range[0],data1_range[1]]                          ;************** oplot a line 1:1
oplot,xx,xx,color=2,thick=2,linestyle=2       

;************************LEGEND OF THE SCATTER
loadct,39
anchura=xsize*0.15 & altura=ysize*0.045
leyenda4=bytarr(anchura,altura)
for i=0,anchura-1,1 do leyenda4(i,*)=i*255./anchura
tv,leyenda4,posleg(0),posleg(1),/norm
xyouts,posleg(0),posleg(1)-0.02,strcompress(string(min(hist)),/remove_all),color=0,charsize=1.0,charthick=1.0, /norm
xyouts,posleg(0)+0.14,posleg(1)-0.02,strcompress(string(max(hist)),/remove_all),color=0,charsize=1.2,charthick=1.0, /norm

;******************************5. Line of fit *********************************************************************************************************
;******************************************************************************************************************************************************
;slope=regress(data1(wh),data2(wh),const=const,correlation=corr)   ;###wh because the elements of data1 & data2 have to be the same
slope=regress(data1,data2,const=const,correlation=corr)   
yy=slope#xx+const;
makechar,slope,4,slopechar & makechar,const,4,constchar & makechar,corr,3,corrchar
if const gt 0 then suma='+' else suma=''
OLSajuste='OLS y='+slopechar+'x'+suma+constchar
;oplot,xx,slope#xx+const,color=1,thick=2 ;plot of the line of fit
res = gmrf(data1,data2) ;[b0,b1,AC,ACsys,ACuns,d,du,ds]
makechar,res[1],4,Gslopechar
makechar,res[0],4,Gconstchar
makechar,res[2],3,GAC
makechar,res[8],3,dfAC
oplot,xx,res[1]#xx+res[0],color=250,thick=2 ;plot of the line of fit
;IF (data1_range[0] NE 0.0) THEN oplot,[0,0],[-1,1],color=120,thick=1, linestyle=3        ;plot vertical and orizontal origin
;IF (data2_range[0] NE 0.0) THEN oplot,[-1,1],[0,0],color=120,thick=1, linestyle=3        ;plot vertical and orizontal origin
device,decomposed=1
!P.COLOR='808080'x
IF (data1_range[0] NE 0.0) THEN oplot,[0,0],[-1,1],thick=1, linestyle=3        ;plot vertical and orizontal origin
IF (data2_range[0] NE 0.0) THEN oplot,[-1,1],[0,0],thick=1, linestyle=3        ;plot vertical and orizontal origin
GMRFajuste='GMRF y='+Gslopechar+'x'+suma+Gconstchar
;***************************6. Calculation RMS ********************************************************************************************************
;******************************************************************************************************************************************************
rms=sqrt(total(double(data1(wh)-data2(wh))^2/N_elements(data1(wh))))
makechar,rms,3,rmschar
stat=[const,slope,corr,corr^2,rms]

;**************************7. Insert in the plot the statistics ****************************************************************************************
;*******************************************************************************************************************************************************
if n_elements(pos_stat) le 0 then begin
  pos_stat=fltarr(2)
  ;pos_stat(0)=0.5
  pos_stat(0)=0.07
  pos_stat(1)=0.92
endif

inix=data1_range(0)+(data1_range(1)-data1_range(0))*pos_stat(0)
iniy=data2_range(0)+(data2_range(1)-data2_range(0))*pos_stat(1)
incy=(data2_range(1)-data2_range(0))*0.04*(charsize+1)

pos1=[inix,iniy]
pos2=[inix,iniy-incy]
pos3=[inix,iniy-2*incy]  

if n_elements(write_stat) gt 0 then begin
nstat=n_elements(write_stat)
tit=strarr(nstat)
if nstat gt 0 then begin
  for i=0,nstat-1 do begin
    case write_stat(i) of
    'r':tit(i)='r='+corrchar
    'RMS':tit(i)='RMSE='+rmschar
    'OLSajuste':tit(i)=OLSajuste
    'GMRFajuste':tit(i)=GMRFajuste
    'AC':tit(i)='AC='+GAC
    ;'dfAC':tit(i)='dfAC='+dfAC
    endcase
  endfor
endif
wh=where(tit ne '',nstat) 
tit=tit(0:nstat-1)
for i=0,nstat-1 do begin
   ccolor = 0
;   if (strmid(tit(i),0,4) eq 'GMRF') OR (strmid(tit(i),0,2) eq 'AC') then ccolor=250
;   if (strmid(tit(i),0,4) eq 'dfAC') then ccolor=60
   ;xyouts,inix,iniy-0.1*(i),tit(i),color=ccolor, charsize=1.2,charthick=1.4
   ;for profiles
   xyouts,inix,iniy-0.05*(i),tit(i),color=ccolor, charsize=1.2,charthick=1.4
   
   print,'iiii',iniy-0.005*(i)
endfor
endif

;************************ 8. Save image ***************************************************************************************************************
;******************************************************************************************************************************************************
write_jpeg,fpng,tvrd(true=1),true=1,quality=100

end
