;********** Program that makes a scatter plot from vectors (that can be selected from images)
;********** Date 12-1-2015

pro DensityAndFit_log_scale, data01, data02, x_label, y_label, dir_output, data1_range, data2_range, ngrid, nlevels, TITLE=title, BIN1=bin1in, $
  DOFIT = dofit, LOGFIT=logfit, $
  OLS_STAT = ols_stat, $ ;variable where to store OLS stats
  DOGRMF= dogmrf, FILESUFFIX = filesuffix, NOWIN=nowin, RGBTAB = rgbtab, SIMPLE = simple, DOLOG = dolog, PLOT1TO1 = plot1to1, $
  RANGE_IN = range_in, $ ;used with simple graph, set the min max values to be used (min max stretch), 2 elements vector
  RANGEINLOW = rangeinlow, $ ; ;used with simple graph, set only the min value (to one normally)
  RANGEOUT = rangeout, $; used with simple graph, store the used range for z, 2 elements vector
  SAVECSV = savecsv, $ ;1 to save a csv with data
  TOTALN = totaln, $ ;total number of obs to be used in the csv (made for area and pheno)
  WEIGHT = weight ;1D array of the same dimension as data1 and data2 which holds the weighted values associated with each V1 and V2 element.


  ;make a copy of input
  data1 = data01
  data2 = data02
  ; NOWIN suppres the window bur save the graph
  ; SIMPLE makes a simple density scatter without interpolating
  platform = !version.os_family
  ;  Set the screen device and the directory separator:
  IF (platform EQ 'unix') THEN BEGIN
    screen = 'X'
    dirsep = '/'
    main_dir = '/eos/jeodpp/data/users/mmeroni/data'
  ENDIF ELSE BEGIN
    screen = 'Win'
    dirsep = '\'
    main_dir = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE'
  ENDELSE

  ;************** EDITABLE AREA*****************************************************************************************************************
  ;********** PLOT PARAMETERS

  IF ((N_ELEMENTS(RANGEIN) GT 0) AND (N_ELEMENTS(RANGEINLOW) GT 0)) THEN BEGIN
    PRINTF, 'DensityAndFit_log_scale.pro, conficting def of rangein and rangelow, the program will stop'
    STOP
  ENDIF
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
  ;frec_min= 0                ; only represents the values with a frequency > frec_min
  IF (N_ELEMENTS(filesuffix) GT 0) THEN  fpng=dir_output + dirsep + x_label+'VS'+ y_label + filesuffix + '_scatter.png' ELSE $
    fpng=dir_output + dirsep + x_label+'_VS_'+ y_label + '_scatter.png'



  ;************2. Calculation of the histogram ****************************************************************************************************
  ;************************************************************************************************************************************************
  IF ~(KEYWORD_SET(weight)) THEN weight = FLTARR(N_ELEMENTS(data1))*0.0 + 1
  ;make sure that data do not contain NAN (this not treated by regress)
  indFin = WHERE((FINITE(data1) AND FINITE(data2)), countFin)
  IF (countFin NE N_ELEMENTS(data1)) THEN BEGIN
    data1 = data1[indfin]
    data2 = data2[indfin]
    weight = weight[indfin]
  ENDIF
  ;  print,'Min of x and y'
  ;  print,min(data1),min(data2)
  ;  print,'Max of x and y'
  ;  print,max(data1),max(data2)                                                         ;******real min and max valueas

  min1=data1_range(0) & max1=data1_range(1)
  min2=data2_range(0) & max2=data2_range(1)                                            ;**** min and maxim values according the defined range

  bin1=DOUBLE(max1-min1)/DOUBLE(ngrid)
  IF (N_ELEMENTS(bin1in) GT 0) THEN bin1 = bin1in
  bin2=DOUBLE(max2-min2)/DOUBLE(ngrid)
  hist=hist2d_mic(data1,data2,weight,BINSIZE1=bin1,BINSIZE2=bin2,max1=max1,max2=max2,min1=min1,min2=min2, OBIN1=Obin1, OBIN2=Obin2, BINEDGE1=-1, BINEDGE2=-1) ;***two dimensional density function (histogram) of two variables.
  ;in idl the max1 and max2 are placed in an additional bin because IDL works with >= and < to get the number of obs
  ;so I flip the last row and last colum on the previous ones
  hist2 = hist[0:-2,0:-2]
  hist2[*,-1] = hist2[*,-1] + hist[*,-1]
  hist2[-1,*] = hist2[-1,*] + hist[-1,*]
  hist = hist2
  hist2 = 0
  IF KEYWORD_SET(savecsv) THEN BEGIN
    dlmtr = ','
    IF (N_ELEMENTS(filesuffix) GT 0) THEN  fncsv=dir_output + '\' + filesuffix + '_scatter.csv' ELSE $
      fncsv=dir_output + '\' + x_label+'VS'+ y_label + '_scatter.csv'
    OPENW, lun, fncsv, /GET_LUN
    IF (N_ELEMENTS(totaln) GT 0) THEN PRINTF, lun, 'Total area (pixels) = ' + dlmtr +  STRING(totaln) + dlmtr + 'Total area (km2) = ' +dlmtr + STRING(TOTAL(hist))
    PRINTF, lun, dlmtr + dlmtr + xtit
    PRINTF, lun, dlmtr + dlmtr + STRJOIN('X'+STRTRIM(INDGEN((max1-min1+1)/bin1)+min1,2), dlmtr)
    tmptit = ytit
    FOR i = 0, N_ELEMENTS(hist[0,*])-1 DO BEGIN
      IF (i GT 0) THEN tmptit = ''
      PRINTF, lun, tmptit + dlmtr + 'Y'+STRTRIM((max2-i),2) + dlmtr + STRJOIN(hist[*,N_ELEMENTS(hist[0,*])-1-i], dlmtr)
    ENDFOR
    FREE_LUN, lun
  ENDIF
  ;test for cloudiness, make mean value of y by x step
  ;  sztest = SIZE(hist)
  ;  meanyval = hist*REBIN(REVERSE(TRANSPOSE(obin2+bin2/2.0)), sztest[1], sztest[2])
  ;  meanyval = TOTAL(meanyval, 2)/TOTAL(hist, 2)
  ;  htest = PLOT(Obin1+bin1/2.0, meanyval)
  !except=0 ; avoid issuing error when argument is 0
  IF KEYWORD_SET(dolog) THEN BEGIN
    hist = ALOG(hist)
    cbtitle = 'Density (ln(n))'
  ENDIF ELSE BEGIN
    ;make 0 occurrence non visible (se it to nan)
    ind = WHERE(hist EQ 0, count)
    IF (count GT 0) THEN BEGIN
      hist = FLOAT(hist)
      hist[ind] = -!VALUES.F_INFINITY
      cbtitle = 'Density (n)'
    ENDIF
  ENDELSE

  clear = CHECK_MATH() ;set back to normal
  !except=1
  wh=where(data1 gt data1_range(0) and data1 le data1_range(1) and data2 gt data2_range(0) and data2 le data2_range(1) ) ;values that are in the interval

  ;*************3.Saturate maximum frequency and eliminate minimum frequency************************************************************************
  ;*************************************************************************************************************************************************
  ;  wh=where(hist le frec_min,cont) & if cont gt 0 then hist(wh)=0
  ;  wh=where(hist ge frec_max,cont) & if cont gt 0 then hist(wh)=frec_max
  ;  wh=where(data1 gt data1_range(0) and data1 le data1_range(1) and data2 gt data2_range(0) and data2 le data2_range(1) ) ;values that are in the interval


  ;**********4. Plot of the histogram ***************************************************************************************************************
  ;**************************************************************************************************************************************************



  cvalue = (FINDGEN(nlevels)+1)*(MAX(hist) - MIN(HIST,/NAN)) / FLOAT(nlevels)  + MIN(HIST,/NAN)
  IF (N_ELEMENTS(rgbtab) EQ 0) THEN rgbtab = 72 ;default color table
  tmp = ''
  IF (N_ELEMENTS(title) GT 0) THEN tmp = title
  IF KEYWORD_SET(nowin) THEN buf = 1 ELSE buf = 0
  IF KEYWORD_SET(simple) THEN BEGIN
    IF (N_ELEMENTS(range_in) GT 0) THEN ghc = IMAGE(hist, POSITION=[0.2,0.25,0.8,0.85] , AXIS_STYLE=0, RGB_TABLE=rgbtab, DIMENSIONS=[600,600], BUFFER = buf, MIN_VALUE = range_in[0], MAX_VALUE = range_in[1])
    IF (N_ELEMENTS(rangeinlow) GT 0) THEN ghc = IMAGE(hist, POSITION=[0.2,0.25,0.8,0.85] , AXIS_STYLE=0, RGB_TABLE=rgbtab, DIMENSIONS=[600,600], BUFFER = buf, MIN_VALUE = rangeinlow)
    IF ((N_ELEMENTS(range_in) EQ 0) AND (N_ELEMENTS(rangeinlow) EQ 0)) THEN ghc = IMAGE(hist, POSITION=[0.2,0.25,0.8,0.85] , AXIS_STYLE=0, RGB_TABLE=rgbtab, DIMENSIONS=[600,600], BUFFER = buf)

    ;new with hist2d_mic
    ghp = PLOT([0,1], POSITION=[0.2,0.25,0.8,0.85], AXIS_STYLE=2, XRANGE=[min1,max1], YRANGE=[min2,max2], DIMENSIONS=[600,600],XTICKDIR=1, YTICKDIR=1, XTITLE=xtit, YTITLE=ytit, FONT_SIZE = 12, /NODATA, /CURRENT)
    t = TEXT(0.5,0.88, tmp, FONT_STYLE = 1, ALIGNMENT=0.5)
  ENDIF ELSE BEGIN
    ;ghc = CONTOUR(hist,divx,divy,/fill,C_VALUE=cvalue, RGB_TABLE=rgbtab, $
    ghc = CONTOUR(hist,Obin1+bin1/2.0,Obin2+bin2/2.0,/fill,C_VALUE=cvalue, RGB_TABLE=rgbtab, $
      xtitle=xtit,ytitle=ytit, XRANGE=data1_range, YRANGE=data2_range, $
      DIMENSIONS=[600,600], MARGIN=[0.15,0.2,0.1,0.15], TITLE = tmp, BUFFER = buf, FONT_SIZE = 12)
  ENDELSE


  IF KEYWORD_SET(dolog) THEN BEGIN
    cb = COLORBAR(TITLE=cbtitle,MAJOR=5, TAPER=1, POSITION=[0.2,0.08,0.8,0.12])
  ENDIF ELSE BEGIN
    cb = COLORBAR(TITLE=cbtitle,MAJOR=5, TAPER=1, POSITION=[0.2,0.08,0.8,0.12], TICKFORMAT= '(I)')
  ENDELSE
  IF (N_ELEMENTS(range_in) GT 0) THEN RANGEOUT = range_in
  IF (N_ELEMENTS(rangeinlow) GT 0) THEN RANGEOUT = [rangeinlow, MAX(hist, /NAN)]
  IF ((N_ELEMENTS(range_in) EQ 0) AND (N_ELEMENTS(rangeinlow) EQ 0)) THEN RANGEOUT = [MIN(hist, /NAN), MAX(hist, /NAN)]

  legendTargets = !NULL

  xx=[data1_range[0],data1_range[1]]                          ;************** oplot a line 1:1
  IF KEYWORD_SET(plot1to1) THEN BEGIN
    gh11 = PLOT(xx, xx, LINESTYLE='--',OVERPLOT=1, NAME='1:1')
    legendTargets =[legendTargets, gh11]
  ENDIF
  ;******************************5. Line of fit *********************************************************************************************************
  ;******************************************************************************************************************************************************
  IF KEYWORD_SET(dofit) THEN BEGIN
    ;slope=regress(data1(wh),data2(wh),const=const,correlation=corr)   ;###wh because the elements of data1 & data2 have to be the same
    slope=REGRESS(data1,data2,const=const,correlation=corr)
    p = linregstat(data1,data2)
    p = p[3]
    yy=slope#xx+const
    ghOLS = PLOT(xx, yy, LINESTYLE='-', OVERPLOT=1, NAME='OLS')
    legendTargets =[legendTargets, ghOLS]
    IF (N_ELEMENTS(ols_stat) GT 0) THEN BEGIN
      ols_stat = CREATE_STRUCT('slope',0.0,'offset',0.0, 'corr', 0.0 )
      ols_stat.slope = slope
      ols_stat.offset = const
      ols_stat.corr = corr
    ENDIF
    ;logfit
    IF (KEYWORD_SET(logfit) GT 0) THEN BEGIN
      slopeln=REGRESS(ALOG(data1),data2,const=constln,correlation=corrln)
      x4ln = FINDGEN(100)*(MAX(data1)-MIN(data1))/100.0 + MIN(data1)
      ghLNFIT = PLOT(x4ln, slopeln[0]*ALOG(x4ln)+constln, LINESTYLE='--', OVERPLOT=1, NAME='OLS-LOG')
    ENDIF
    IF KEYWORD_SET(dogmrf) THEN BEGIN
      res = gmrf(data1,data2) ;[b0,b1,AC,ACsys,ACuns,d,du,ds]
      ghGMRF = PLOT(xx, res[1]#xx+res[0], LINESTYLE='-', COLOR='blue', OVERPLOT=1, NAME='GMRF')
      legendTargets =[legendTargets, ghGMRF]
    END
    ;plot vertical and orizontal origin
    IF (data1_range[0] NE 0.0) THEN ghORY =  PLOT([0,0], xx, LINESTYLE=':', COLOR='black', OVERPLOT=1)
    IF (data2_range[0] NE 0.0) THEN ghORY =  PLOT(xx, [0,0], LINESTYLE=':', COLOR='black', OVERPLOT=1)
    ghLeg = LEGEND(TARGET=legendTargets, POSITION=[0.1,1.0], HORIZONTAL_ALIGNMENT=0.5, SHADOW=0, THICK=0, TRANSPARENCY=100)

    ;GMRFajuste='GMRF y='+Gslopechar+'x'+suma+Gconstchar
    ;***************************6. Calculation RMS ********************************************************************************************************
    ;******************************************************************************************************************************************************
    ;rms=sqrt(total(double(data1(wh)-data2(wh))^2/N_elements(data1(wh))))
    rms=sqrt(total(double(data1-data2)^2/N_elements(data1)))
    hT1 = TEXT(0.25,0.96,'OLS $R^{2}$='+STRING(corr^2, FORMAT='(f6.4)') + ', r='+STRING(corr, FORMAT='(f7.4)')+', P = ' +STRING(p, FORMAT='(f6.4)'), TARGET=ghc, FONT_SIZE=10)
      ;hT2 = TEXT(0.6,0.94,'RMSE='+STRTRIM(STRING(rms^2, FORMAT='(f10.5)'),2), TARGET=ghc, FONT_SIZE=10)
      hT3 = TEXT(0.25,0.93,'OLS gain, offset='+STRING(slope, FORMAT='(f8.4)')+', '+STRTRIM(STRING(const, FORMAT='(f10.5)'),2), TARGET=ghc, FONT_SIZE=10)
    IF (KEYWORD_SET(logfit) GT 0) THEN BEGIN
      hT4 = TEXT(0.65,0.96,'LN $R^{2}$ ='+STRING(corrln^2, FORMAT='(f6.4)'), TARGET=ghc, FONT_SIZE=10)
        hT5 = TEXT(0.65,0.93,'LN gain, offset='+STRING(slopeln, FORMAT='(f6.4)')+','+STRTRIM(STRING(constln, FORMAT='(f10.5)'),2), TARGET=ghc, FONT_SIZE=10)
    ENDIF
  ENDIF ;KEYWORD_SET

  ;************************ 8. Save image ***************************************************************************************************************
  ;******************************************************************************************************************************************************
  ghc.save, fpng, RESOLUTION=300;, /TRANSPARENT
  ghc.close


end
