;********** Program that makes a scatter plot from vectors (that can be selected from images)
;********** Date 12-1-2015    

pro scatter_plotData

;************** EDITABLE AREA*****************************************************************************************************************
;*********************************************************************************************************************************************
;********** PLOT PARAMETERS
xmargin=[1,1]               
ymargin=[1,1]
ngrid=200                 ;resolution of the plot
xsize=1000 & ysize=650    ;dimensions of the window
charsize=1.
charthick=1
tit='Scatter-plot MaxNDVI vs MinNDVI' 
xtit='MaxNDVI'
ytit='MinNDVI'
xthick=1.5
ythick=1.5
posleg=[0.75,0.15]       ;legend position
write_stat=['ajuste','RMS','r']
;********** HISTOGRAM PARAMETERS
frec_min= 10                ; only represents the values with a frequency > frec_min
frec_max= 300               ; saturate values with frequency > frec_max (if no defined is calculated authomatically)
data1_range=[0,1]            ;data 1 valid range
data2_range=[0,1]            ;data 2 valid range


;********** 1.Introduction of the images and selection of the vectors of interest *************************************************************
;**********************************************************************************************************************************************
file1='D:\valmeanNDVI'
file2='D:\valmaxNDVI'
fpng='D:\scatterMean_INDVI.jpg'

col=1500
row=900
im1=fltarr(col,row)
im2=fltarr(col,row)

openr,lun,file1,/get_lun
readu,lun,im1
free_lun,lun

openr,lun,file2,/get_lun
readu,lun,im2
free_lun,lun

wh1=where(im1 gt 0.)
print,n_elements(wh1)
wh2=where(im2 gt 0.)
print,n_elements(wh2) ;******** Selection of the data from the image

data1=im1(wh1)
data2=im2(wh2)

;************2. Calculation of the histogram ****************************************************************************************************
;************************************************************************************************************************************************

print,histogram(data1*10,min=0,max=60)
print,histogram(data2*10,min=0,max=60)
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

;**********4. Plot of the histogram ***************************************************************************************************************
;**************************************************************************************************************************************************
loadct,39
device,decomposed=0
window,1,xsize=xsize,ysize=ysize

divx=bin1*findgen(ngrid)
divy=bin2*findgen(ngrid)

deleteblack=0
ind=indgen(257)
levels=where(ind gt deleteblack);****** to remove the black and have a white background
contour,norm256hist,divx,divy,/fill,LEVELS=levels,background=255,color=0,$
charsize=charsize,xthick=xthick,ythick=ythick,xtitle=xtit,ytitle=ytit,title=tit,xrange=data1_range,yrange=data2_range

xx=[0,100]                          ;************** oplot a line 1:1
oplot,xx,xx,color=2,thick=2,linestyle=2       

;************************LEGEND OF THE SCATTER
loadct,39
anchura=xsize*0.15 & altura=ysize*0.04
leyenda4=bytarr(anchura,altura)
for i=0,anchura-1,1 do leyenda4(i,*)=i*255./anchura
tv,leyenda4,posleg(0),posleg(1),/norm
xyouts,posleg(0),posleg(1)-0.05,strcompress(string(min(hist)),/remove_all),color=0,charsize=1.0,charthick=1.0, /norm
xyouts,posleg(0)+0.14,posleg(1)-0.05,strcompress(string(max(hist)),/remove_all),color=0,charsize=1.0,charthick=1.0, /norm

;******************************5. Line of fit *********************************************************************************************************
;******************************************************************************************************************************************************
slope=regress(data1(wh),data2(wh),const=const,correlation=corr)   ;###wh because the elements of data1 & data2 have to be the same
yy=slope#xx+const;
makechar,slope,3,slopechar & makechar,const,3,constchar & makechar,corr,3,corrchar
if const gt 0 then suma='+' else suma=' '
ajuste='y='+slopechar+'x'+suma+constchar
oplot,xx,slope#xx+const,color=1,thick=2 ;plot of the line of fit

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
    'RMS':tit(i)='RMS='+rmschar
    'ajuste':tit(i)=ajuste
    endcase
  endfor
endif
wh=where(tit ne '',nstat) 
tit=tit(0:nstat-1)
for i=0,nstat-1 do begin
   if strmid(tit(i),0,2) eq 'y=' then color=240 else color=0
   ;xyouts,inix,iniy-i*incy,tit(i),color=0, charsize=charsize,charthick=2.5
   xyouts,inix,iniy-0.1*(i),tit(i),color=0, charsize=1.0,charthick=1.2
   ;xyouts,inix,iniy-1*(i),tit(i),color=0, charsize=3.0,charthick=3.2
   print,'iiii',iniy-0.005*(i)
endfor
endif

;************************ 8. Save image ***************************************************************************************************************
;******************************************************************************************************************************************************
write_jpeg,fpng,tvrd(true=1),true=1,quality=100

end
