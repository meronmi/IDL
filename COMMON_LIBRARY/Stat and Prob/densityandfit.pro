;********** Program that makes a scatter plot from vectors (that can be selected from images)
;********** Date 12-1-2015

pro DensityAndFit, data1, data2, x_label, y_label, dir_output, data1_range, data2_range, ngrid, frec_max

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
  fpng=dir_output + '\' + x_label+'VS'+ y_label + '_scatter.png'


    
  ;************2. Calculation of the histogram ****************************************************************************************************
  ;************************************************************************************************************************************************

  print,'Min of x and y'
  print,min(data1),min(data2)
  print,'Max of x and y'
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


  ;**********4. Plot of the histogram ***************************************************************************************************************
  ;**************************************************************************************************************************************************


  divx=bin1*findgen(ngrid)+min1
  divy=bin2*findgen(ngrid)+min2

  ghc = CONTOUR(hist,divx,divy,/fill,N_LEVELS=30, RGB_TABLE=72, $
    xtitle=xtit,ytitle=ytit, XRANGE=data1_range, YRANGE=data2_range, $
    DIMENSIONS=[600,600], MARGIN=[0.15,0.2,0.1,0.15])
  cb = COLORBAR(TITLE='Density',MAJOR=5, TAPER=1, POSITION=[0.2,0.08,0.8,0.12])

  xx=[data1_range[0],data1_range[1]]                          ;************** oplot a line 1:1
  gh11 = PLOT(xx, xx, LINESTYLE='--',OVERPLOT=1, NAME='1:1')
  
  ;******************************5. Line of fit *********************************************************************************************************
  ;******************************************************************************************************************************************************
  ;slope=regress(data1(wh),data2(wh),const=const,correlation=corr)   ;###wh because the elements of data1 & data2 have to be the same
  slope=REGRESS(data1,data2,const=const,correlation=corr)
  yy=slope#xx+const
  ghOLS = PLOT(xx, yy, LINESTYLE='-', OVERPLOT=1, NAME='OLS')
  ;makechar,slope,4,slopechar & makechar,const,4,constchar & makechar,corr,3,corrchar
  ;if const gt 0 then suma='+' else suma=''
  ;OLSajuste='OLS y='+slopechar+'x'+suma+constchar
  ;oplot,xx,slope#xx+const,color=1,thick=2 ;plot of the line of fit
  res = gmrf(data1,data2) ;[b0,b1,AC,ACsys,ACuns,d,du,ds]
  ;makechar,res[1],4,Gslopechar
  ;makechar,res[0],4,Gconstchar
  ;makechar,res[2],3,GAC
  ;makechar,res[8],3,dfAC
  ghGMRF = PLOT(xx, res[1]#xx+res[0], LINESTYLE='-', COLOR='blue', OVERPLOT=1, NAME='GMRF')
  ;plot vertical and orizontal origin
  IF (data1_range[0] NE 0.0) THEN ghORY =  PLOT([0,0], xx, LINESTYLE=':', COLOR='black', OVERPLOT=1)
  IF (data2_range[0] NE 0.0) THEN ghORY =  PLOT(xx, [0,0], LINESTYLE=':', COLOR='black', OVERPLOT=1)
  ghLeg = LEGEND(TARGET=[gh11,ghOLS,ghGMRF], POSITION=[0.25,1.0], HORIZONTAL_ALIGNMENT=0.5, $
  SHADOW=0, THICK=0, TRANSPARENCY=100)
    ;GMRFajuste='GMRF y='+Gslopechar+'x'+suma+Gconstchar
  ;***************************6. Calculation RMS ********************************************************************************************************
  ;******************************************************************************************************************************************************
  ;rms=sqrt(total(double(data1(wh)-data2(wh))^2/N_elements(data1(wh))))
  rms=sqrt(total(double(data1-data2)^2/N_elements(data1)))
  hT1 = TEXT(0.6,0.96,'OLS $R^{2}$ ='+STRTRIM(corr^2,2), TARGET=ghc)
  hT1 = TEXT(0.6,0.92,'OLS RMSE='+STRTRIM(rms^2,2), TARGET=ghc)
  ;makechar,rms,3,rmschar
  ;stat=[const,slope,corr,corr^2,rms]

;  ;**************************7. Insert in the plot the statistics ****************************************************************************************
;  ;*******************************************************************************************************************************************************
;  if n_elements(pos_stat) le 0 then begin
;    pos_stat=fltarr(2)
;    ;pos_stat(0)=0.5
;    pos_stat(0)=0.07
;    pos_stat(1)=0.92
;  endif
;
;  inix=data1_range(0)+(data1_range(1)-data1_range(0))*pos_stat(0)
;  iniy=data2_range(0)+(data2_range(1)-data2_range(0))*pos_stat(1)
;  incy=(data2_range(1)-data2_range(0))*0.04*(charsize+1)
;
;  pos1=[inix,iniy]
;  pos2=[inix,iniy-incy]
;  pos3=[inix,iniy-2*incy]
;
;  if n_elements(write_stat) gt 0 then begin
;    nstat=n_elements(write_stat)
;    tit=strarr(nstat)
;    if nstat gt 0 then begin
;      for i=0,nstat-1 do begin
;        case write_stat(i) of
;          'r':tit(i)='r='+corrchar
;          'RMS':tit(i)='RMSE='+rmschar
;          'OLSajuste':tit(i)=OLSajuste
;          'GMRFajuste':tit(i)=GMRFajuste
;          'AC':tit(i)='AC='+GAC
;          ;'dfAC':tit(i)='dfAC='+dfAC
;        endcase
;      endfor
;    endif
;    wh=where(tit ne '',nstat)
;    tit=tit(0:nstat-1)
;    for i=0,nstat-1 do begin
;      ccolor = 0
;      ;   if (strmid(tit(i),0,4) eq 'GMRF') OR (strmid(tit(i),0,2) eq 'AC') then ccolor=250
;      ;   if (strmid(tit(i),0,4) eq 'dfAC') then ccolor=60
;      ;xyouts,inix,iniy-0.1*(i),tit(i),color=ccolor, charsize=1.2,charthick=1.4
;      ;for profiles
;      xyouts,inix,iniy-0.05*(i),tit(i),color=ccolor, charsize=1.2,charthick=1.4
;
;      print,'iiii',iniy-0.005*(i)
;    endfor
;  endif

  ;************************ 8. Save image ***************************************************************************************************************
  ;******************************************************************************************************************************************************
  ghc.save, fpng, RESOLUTION=300;, /TRANSPARENT
  ;write_jpeg,fpng,tvrd(true=1),true=1,quality=100

end
