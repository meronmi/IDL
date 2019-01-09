PRO G_make_compare_product
;here I select a spatial window of the global thinned images and I plot over dekd for different products

;image settings
frstyear = 2003         ;note: the stack file MUST start at dek 1 of that year
x0 = -180.004464        ;these info come from the hdr of the stack file
y0 = 75.0044643
xyres = 0.1875
writeLetters = 0      ;add A), B) etc to figures
;USER SETTINGS

caseStudy = 'EastEur2012';'HoA2010'
freetext = caseStudy    ;a text that will be used in the output png files
useSave = 0  ;!!!BE CARE WITH THIS
CASE caseStudy OF
    'EastEur2012': BEGIN
      deks2map = [17,20,23] & year2map = 2012 ;only 3 deks
      sULcorner = 1019 & lULcorner = 110 ;sample and line
      ns = 200 &  nl = 80
    END
    'Baltic2014': BEGIN
      deks2map = [2,5,8] & year2map = 2014 ;only 3 deks
      sULcorner = 1068 & lULcorner = 90 ;sample and line
      ns = 50 &  nl = 30
    END
    'Turk2017': BEGIN
      deks2map = [11,14,17] & year2map = 2017 ;only 3 deks
      sULcorner = 1099 & lULcorner = 175 ;sample and line
      ns = 55 &  nl = 40
    END
    'France2016': BEGIN
      deks2map = [14,16,20] & year2map = 2016 ;only 3 deks
      sULcorner = 949 & lULcorner = 128 ;sample and line
      ns = 55 &  nl = 30
    END
    'DryCorr2016': BEGIN
      deks2map = [12,15,18] & year2map = 2016 ;only 3 deks
      sULcorner = 465 & lULcorner = 300 ;sample and line
      ns = 65 &  nl = 65
    END
    'CALIF2013': BEGIN
      deks2map = [32,33,34] & year2map = 2013 ;only 3 deks
      sULcorner = 295 & lULcorner = 160 ;sample and line
      ns = 80 &  nl = 80
    END
    'SPAIN2005': BEGIN
      deks2map = [7,10,13] & year2map = 2005 ;only 3 deks
      sULcorner = 910 & lULcorner = 160 ;sample and line
      ns = 60 &  nl = 50
    END
    'SYR2018': BEGIN
      deks2map = [1,4,7] & year2map = 2018 ;only 3 deks
      sULcorner = 1100 & lULcorner = 200 ;sample and line
      ns = 100 &  nl = 50
    END
    'AFG2018': BEGIN
      deks2map = [1,4,7] & year2map = 2018 ;only 3 deks
      sULcorner = 1270 & lULcorner = 190 ;sample and line
      ns = 80 &  nl = 50
    END
    'E2016': BEGIN
      deks2map = [9,12,15] & year2map = 2016 ;only 3 deks 
      sULcorner = 1450 & lULcorner = 250 ;sample and line
      ns = 100 &  nl = 100
    END
    'SA2017': BEGIN
      deks2map = [10,11,12] ;only 3
      sULcorner = 1040 ;sample
      lULcorner = 540 ;line
      ns = 100 &     nl = 50
      year2map = 2017
    END
    'SA2017b': BEGIN
      deks2map = [13,16,19] ;only 3
      sULcorner = 1040 ;sample
      lULcorner = 540 ;line
      ns = 100 &     nl = 50
      year2map = 2017
    END
    'WA2009': BEGIN
      deks2map = [27,30,33] ;only 3
      sULcorner = 869 ;sample
      lULcorner = 308 ;line
      ns = 200 &     nl = 75
      year2map = 2009
    END
    'HoA2010': BEGIN 
      deks2map = [20,23,26] ;only 3
      sULcorner = 1090 ;sample 
      lULcorner = 315 ;line
      ns = 148 &     nl = 114
      year2map = 2010
    END
    'HoA2017': BEGIN
      deks2map = [9,12,15] ;only 3
      sULcorner = 1090 ;sample
      lULcorner = 315 ;line
      ns = 148 & nl = 114
      year2map = 2017
    END
    'HoA2018': BEGIN
      deks2map = [9,12,15] ;only 3
      sULcorner = 1090 ;sample
      lULcorner = 315 ;line
      ns = 148 & nl = 114
      year2map = 2018
    END
ENDCASE

;end of USER SETTINGS


startup = C_start_up()
;read the files in the list data
nProducts = N_ELEMENTS(startup.z_fn)
IF (useSave NE 1) THEN BEGIN
  data0 = FLTARR(ns, nl, N_ELEMENTS(deks2map)) * !VALUES.F_NAN
  init = 1
  FOR i = 0, nProducts-1 DO BEGIN
    PRINT, 'Reading ' + startup.ts_name_short[i]
    fn = startup.z_fn[i]
    tmp = ReadEnviWithHdr(fn)
    ts0 = tmp[sULcorner:sULcorner+ns-1,lULcorner:lULcorner+nl-1,*]
    ;fill data for the dekads of interest
    FOR d = 0,  N_ELEMENTS(deks2map)-1 DO BEGIN
      indBand = ((year2map-frstyear) * 36 + deks2map[d]) - 1
      data0[*,*,d]=tmp[sULcorner:sULcorner+ns-1,lULcorner:lULcorner+nl-1,indBand]
    ENDFOR
    IF (init EQ 1) THEN BEGIN
      data = LIST(data0)
      data0[*,*,*] = !VALUES.F_NAN
      ts = LIST(ts0)
      ts0[*,*,*] = !VALUES.F_NAN
      init = 0
    ENDIF ELSE BEGIN
      data.Add, data0
      data0[*,*,*] = !VALUES.F_NAN
      ts.Add, ts0
      ts0[*,*,*] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
  SAVE, /ALL, FILENAME = startup.out_path + '\dataList.sav' 
ENDIF ELSE BEGIN
  RESTORE, startup.out_path + '\dataList.sav'
ENDELSE

IF (N_ELEMENTS(deks2map)) NE 3 THEN STOP 
x_coord_min_max= [x0+sULcorner*xyres, x0+(sULcorner+ns-1)*xyres];[22.67,22.67+ns*0.00892857143]
y_coord_min_max= [y0-lULcorner*xyres, y0-(lULcorner+nl-1)*xyres];[16.08-nl*0.00892857143,16.08]

;here below mostly copied from my pro "figures_for_paper"
mxs = data.Map(Lambda(array: MAX(array,/NAN)))
mns = data.Map(Lambda(array: MIN(array,/NAN)))
data_min_max = [MIN(mns.ToArray(),/NAN), MAX(mxs.ToArray(),/NAN)]



dms = [1400,1000]
axs_stl = 2
pos0 = [0.1,0.7,0.3,0.9];[0.1,0.1,0.3,0.3]  ;[X1, Y1, X2, Y2]
dx = 0.225
dy = 0.25

FOR i= 0, 1 DO BEGIN 
  CASE i OF
    0:BEGIN
        ;plot z-score 
        rgbtab = 72; clorblind friendly red-yellow-blue
        data_min_max = [-3.5,3.5]
        fn_out = startup.out_path + '\'+freetext+'_comparsion_z-score.png'
      END
    1:BEGIN
        ;plot classified z-score
        data = data.Map('zreclass')
        data_min_max = [0,6]
        discreteCTB = ['#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4'];REVERSE(['#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'])
        tickname  = ['Ext. bad','Very bad','Mod. bad','Normal','Mod. good','Very good','Ext. good']
        fn_out = startup.out_path + '\'+freetext+'_comparsion_z-score_classes.png'
      END
  ENDCASE
  pos = pos0
  w1 = IMAGE(REVERSE(data[0,*,*,0],2), DIMENSIONS=dms,  RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, $ ;LAYOUT=[4,4,6],
    MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0)
  
  xax = AXIS('X', LOCATION='bottom', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [x_coord_min_max[0],xyres],TITLE='Deg. E') ;AXIS_RANGE=x_coord_min_max,
  yax = AXIS('Y', LOCATION='left', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [y_coord_min_max[0],xyres],TITLE='Deg. N')

  cbx = 0.8 & cby = 0.72 & cbdx = 0.02 & cbdy = 0.15
  CASE i OF
    0:BEGIN
        cb = COLORBAR(TARGET=w1, ORIENTATION=1, POSITION=[cbx,cby,cbx+cbdx,cby+cbdy], TAPER=0, TICKLAYOUT=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
        txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01,'Z-score', ALIGNMENT=0.5)
    END
    1:BEGIN
        cb = COLORBAR(ORIENTATION=1, POSITION=[cbx,cby,cbx+cbdx,cby+cbdy], RGB_TABLE=discreteCTB,  TAPER=0, TICKLAYOUT=1, TICKNAME=tickname, BORDER=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
        txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01,'classes', ALIGNMENT=0.5)
        txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01+0.02,'Z-score', ALIGNMENT=0.5)
    END
  ENDCASE
  
  pos = pos + [dx, 0, dx, 0] ;[X1, Y1, X2, Y2]
  w2 = IMAGE(REVERSE(data[0,*,*,1],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT) ;LAYOUT=[4,4,6+1],
  pos = pos + [dx, 0, dx, 0]
;  discreteCTB = ['red','red','red','black','black','black','black','white'];REVERSE(['#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'])
;  a = REFORM(data[0,*,*,2])
;  ind = WHERE(~FINITE(a))
;  a[ind]=7
;  a= BYTE(a)
;  w3 = IMAGE(REVERSE(a,2), RGB_TABLE=discreteCTB, POSITION=pos, AXIS_STYLE = axs_stl, XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
;  discreteCTB = ['red','black']
;  tickname  = ['Yes','No']
;  cb.delete
;  cb = COLORBAR(ORIENTATION=1, POSITION=[cbx,cby,cbx+cbdx,cby+cbdy], RGB_TABLE=discreteCTB,  TAPER=0, TICKLAYOUT=1, TICKNAME=tickname, BORDER=1, FONT_SIZE= 12) 
  w3 = IMAGE(REVERSE(data[0,*,*,2],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  
  pos = pos0 + [0, -dy, 0, -dy]
  w4 = IMAGE(REVERSE(data[1,*,*,0],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w5 = IMAGE(REVERSE(data[1,*,*,1],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w6 = IMAGE(REVERSE(data[1,*,*,2],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  pos = pos0 + [0, -2*dy, 0, -2*dy]
  w7 = IMAGE(REVERSE(data[2,*,*,0],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w8 = IMAGE(REVERSE(data[2,*,*,1],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w9 = IMAGE(REVERSE(data[2,*,*,2],2), RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  
  xstep0 = 0.195
  ypos = 0.925
  ;write dek
  yr_suffix = '/'+STRTRIM(year2map,2)
  txt = TEXT(xstep0,ypos,'Dekad ' + STRTRIM(deks2map[0],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xstep0+dx*1,ypos,'Dekad '+ STRTRIM(deks2map[1],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xstep0+dx*2,ypos,'Dekad '+ STRTRIM(deks2map[2],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  ystep0 = 0.795
  xpos = 0.025
  ;write Product Name
  txt = TEXT(xpos,ystep0,startup.ts_name_short[0], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xpos,ystep0-dy*1,startup.ts_name_short[1], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xpos,ystep0-dy*2,startup.ts_name_short[2], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  
  ;write A), B) etc
  ypos = 0.875
  xstep0 = 0.195
  IF (writeLetters EQ 1) THEN BEGIN
    txt = TEXT(xstep0,ypos,'A)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*1,ypos,'B)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*2,ypos,'C)', ALIGNMENT=0, FONT_SIZE=14)
  
    txt = TEXT(xstep0,ypos-dy,'D)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*1,ypos-dy,'E)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*2,ypos-dy,'F)', ALIGNMENT=0, FONT_SIZE=14)
  
    txt = TEXT(xstep0,ypos-2*dy,'G)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*1,ypos-2*dy,'H)', ALIGNMENT=0, FONT_SIZE=14)
    txt = TEXT(xstep0+dx*2,ypos-2*dy,'I)', ALIGNMENT=0, FONT_SIZE=14)
  ENDIF
  w1.save, fn_out, RESOLUTION = 600
  
  
  ;Profile analysis
  IF (i EQ 0) THEN BEGIN
    dms = [1400,1000]
    axs_stl = 2
    posPro = [0.05,0.75,0.25,0.95];[0.1,0.1,0.3,0.3]  ;[X1, Y1, X2, Y2]
    dx = 0.225
    dy = 0.25
    rgbtabbw = 0 ;0 BLACK, 255 WHITE
    ;show image and extracted points 
    tmp = REFORM(data[0,*,*,0])
    indFin = WHERE(FINITE(tmp))
    tmp = BYTE(tmp) * 0 + 255
    tmp[indFin] = 180
    w1 = IMAGE(REVERSE(tmp,2), DIMENSIONS=dms,  RGB_TABLE=rgbtabbw, POSITION=posPro, AXIS_STYLE = axs_stl, $ ;LAYOUT=[4,4,6],
      MIN_VALUE = 0, MAX_VALUE = 255, XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0)
    xax = AXIS('X', LOCATION='bottom', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [x_coord_min_max[0],xyres],TITLE='Deg. E') ;AXIS_RANGE=x_coord_min_max,
    yax = AXIS('Y', LOCATION='left', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [y_coord_min_max[0],xyres],TITLE='Deg. N')
    ;extract random until I get 3 good points
    collectedPoints = 0
    sampleP = !NULL
    lineP = !NULL
    rndFractSeries= RANDOMU(SYSTIME(/SECONDS),1000)
    cSeries = 0
    WHILE collectedPoints LT 3 DO BEGIN
      rndFract= [rndFractSeries[cSeries],rndFractSeries[cSeries+1]]
      cSeries = cSeries + 2
      tentativeS = FLOOR(ns*rndFract[0])
      tentativeL = FLOOR(nl*rndFract[0])
      ;see if it exists (it is not nan, water)
      IF (tmp[tentativeS, tentativeL] LT 200) THEN BEGIN
        sampleP = [sampleP, tentativeS] 
        lineP = [lineP, tentativeL] 
        collectedPoints = collectedPoints + 1
        vrtal = 0 ;aligment to have always the number there (also at borders)
        al = 0
        IF (tentativeL LT 5) THEN vrtal = 1
        IF (ns-tentativeS LT 5) THEN al = 1
        th = TEXT(tentativeS, nl-tentativeL, STRTRIM(collectedPoints,2),ALIGNMENT=al,VERTICAL_ALIGNMENT = vrtal,/DATA,/CURRENT)
      ENDIF
    ENDWHILE
    ;read the ndvi/fapar products
    init = 1
    FOR p = 0, nProducts-1 DO BEGIN
      PRINT, 'Reading ' + startup.ts_name_short[p]
      fn = startup.bil_fn[p]
      tmp = ReadBilWithHdr(fn)
      ;scale it
      tmp = scale_image(tmp, MINVAL = startup.minval[p], MAXVAL = startup.maxval[p], GAIN = startup.gain[p], OFFSET = startup.offset[p])
      ts_original0 = tmp[sULcorner:sULcorner+ns-1,lULcorner:lULcorner+nl-1,*]
      IF (init EQ 1) THEN BEGIN
        ts_original = LIST(ts_original0)
        ts_original0[*,*,*] = !VALUES.F_NAN
        init = 0
      ENDIF ELSE BEGIN
        ts_original.Add, ts_original0
        ts_original0[*,*,*] = !VALUES.F_NAN
      ENDELSE
    ENDFOR
    ;make 3 times the graph (ndvi and anomaly)
    smalldy = 0.125
    bigdy = 0.205
    posPro = [0.3,0.825,0.975,0.95] ; [0.05,0.75,0.25,0.95];  ;[X1, Y1, X2, Y2]
    FOR c = 0, 2 DO BEGIN 
      ;plot original data
;      xrng =  [0,N_ELEMENTS(ts_original[0,sampleP[c],lineP[c],*])-1]
;      yrng = [MIN([ts_original[0,sampleP[c],lineP[c],*],ts_original[1,sampleP[c],lineP[c],*],ts_original[2,sampleP[c],lineP[c],*]],/NAN),MAX([ts_original[0,sampleP[c],lineP[c],*],ts_original[1,sampleP[c],lineP[c],*],ts_original[2,sampleP[c],lineP[c],*]],/NAN)]
;      ph0 = PLOT(ts_original[0,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[0], XRANGE = xrng, YRANGE = yrng, YMINOR=0, YTICKDIR=1, YTITLE='ND,FAPAR',XSHOWTEXT=0, /CURRENT)
;      ph1 = PLOT(ts_original[1,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[1], COLOR='r', /OVERPLOT)
;      ph2 = PLOT(ts_original[2,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[2], COLOR='b', /OVERPLOT)
;      th = TEXT(posPro[0], posPro[3]+0.02, 'Point '+ STRTRIM(c+1,2), FONT_SIZE=12, /CURRENT)
;      th = TEXT(posPro[0], posPro[3]+0.005, 's,l (0 start)= '+ STRTRIM(sULcorner+sampleP[c],2) + ', ' + STRTRIM(lULcorner +lineP[c],2), FONT_SIZE=9, /CURRENT)
;      IF (c EQ 0) THEN lh = LEGEND(TARGET=[ph0,ph1,ph2], POSITION = [(posPro[0]+posPro[2])/2.0,posPro[3]+0.05], ORIENTATION=1, HORIZONTAL_ALIGNMENT='CENTER', LINESTYLE='')
;      ;move one down for z plot
;      posPro = posPro + [0,-smalldy,0,-smalldy]
;      yrng = [MIN([ts[0,sampleP[c],lineP[c],*],ts[1,sampleP[c],lineP[c],*],ts[2,sampleP[c],lineP[c],*]],/NAN),MAX([ts[0,sampleP[c],lineP[c],*],ts[1,sampleP[c],lineP[c],*],ts[2,sampleP[c],lineP[c],*]],/NAN)]
;      ph0 = PLOT(ts[0,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[0], XRANGE = xrng, YRANGE = yrng, YTICKDIR=1, YTITLE='Z-score',/CURRENT)
;      dummy = LABEL_DATE(DATE_FORMAT=['%D/%N/%Y'])
;      a_x =AXIS('X', TARGET=ph0, LOCATION=yrng[0]-(yrng[1]-yrng[0])/5.5, COORD_TRANSFORM=[JULDAY(1,1,frstyear), 10], TICKFORMAT = 'LABEL_DATE');, TICKNAME=strlabel)
;      ph1 = PLOT(ts[1,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[1], COLOR='r', /OVERPLOT)
;      ph2 = PLOT(ts[2,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[2], COLOR='b', /OVERPLOT)
      
      ;xrng =  [0,N_ELEMENTS(ts_original[0,sampleP[c],lineP[c],*])-1]
      tt = [1]
      yy = [frstyear]
      FOR t = 1, N_ELEMENTS(ts_original[0,sampleP[c],lineP[c],*])-1 DO BEGIN
        IF tt[t-1] LT 36 THEN BEGIN
          tt = [tt,tt[t-1]+1]
          yy = [yy, yy[-1]] 
        ENDIF ELSE BEGIN
          tt = [tt,1]
          yy = [yy, yy[-1]+1]
        ENDELSE
      ENDFOR
      jds = tt*0.0
      FOR t =0, N_ELEMENTS(tt)-1 DO jds[t] = DOY_YEAR2JD(DekYear2centralDOY(tt[t], yy[t]), yy[t])
        
      xrng =  [jds[0],jds[-1]]
      xmajo = 16
      xmino = 12   
      yrng = [MIN([ts_original[0,sampleP[c],lineP[c],*],ts_original[1,sampleP[c],lineP[c],*],ts_original[2,sampleP[c],lineP[c],*]],/NAN),MAX([ts_original[0,sampleP[c],lineP[c],*],ts_original[1,sampleP[c],lineP[c],*],ts_original[2,sampleP[c],lineP[c],*]],/NAN)]
      ph0 = PLOT(jds, ts_original[0,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[0], $ 
                 XMAJOR=xmajo, XMINOR=xmino, XRANGE = xrng, YRANGE = yrng, YMINOR=0, YTICKDIR=1, YTITLE='ND,FAPAR',XSHOWTEXT=0, /CURRENT)
      ph1 = PLOT(jds, ts_original[1,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[1], COLOR='r', /OVERPLOT)
      ph2 = PLOT(jds, ts_original[2,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[2], COLOR='b', /OVERPLOT)
      th = TEXT(posPro[0], posPro[3]+0.02, 'Point '+ STRTRIM(c+1,2), FONT_SIZE=12, /CURRENT)
      th = TEXT(posPro[0], posPro[3]+0.005, 's,l (0 start)= '+ STRTRIM(sULcorner+sampleP[c],2) + ', ' + STRTRIM(lULcorner +lineP[c],2), FONT_SIZE=9, /CURRENT)
      IF (c EQ 0) THEN lh = LEGEND(TARGET=[ph0,ph1,ph2], POSITION = [(posPro[0]+posPro[2])/2.0,posPro[3]+0.05], ORIENTATION=1, HORIZONTAL_ALIGNMENT='CENTER', LINESTYLE='')
      ;move one down for z plot
      posPro = posPro + [0,-smalldy,0,-smalldy]
      yrng = [MIN([ts[0,sampleP[c],lineP[c],*],ts[1,sampleP[c],lineP[c],*],ts[2,sampleP[c],lineP[c],*]],/NAN),MAX([ts[0,sampleP[c],lineP[c],*],ts[1,sampleP[c],lineP[c],*],ts[2,sampleP[c],lineP[c],*]],/NAN)]
      dummy = LABEL_DATE(DATE_FORMAT=['%D/%N/%Z'])
      ph0 = PLOT(jds, ts[0,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[0], $
                 XMAJOR=xmajo, XMINOR=xmino, XRANGE = xrng, YRANGE = yrng, YTICKDIR=1, YTITLE='Z-score', XTICKFORMAT='LABEL_DATE', /CURRENT)
;      dummy = LABEL_DATE(DATE_FORMAT=['%D/%N/%Y'])
;      a_x =AXIS('X', TARGET=ph0, LOCATION=yrng[0]-(yrng[1]-yrng[0])/5.5, COORD_TRANSFORM=[JULDAY(1,1,frstyear), 10], TICKFORMAT = 'LABEL_DATE');, TICKNAME=strlabel)
      ph0line = PLOT([jds[0],jds[-1]],[0,0], POSITION=posPro, LINESTYLE='--', /OVERPLOT)
      ph1 = PLOT(jds,ts[1,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[1], COLOR='r', /OVERPLOT)
      ph2 = PLOT(jds,ts[2,sampleP[c],lineP[c],*], POSITION=posPro, NAME = startup.ts_name_short[2], COLOR='b', /OVERPLOT)
      ;move down for next plot
      posPro = posPro + [0,-bigdy,0,-bigdy]
    ENDFOR
    w1.save, startup.out_path + '\'+freetext+'_random_profiles.png', RESOLUTION = 600
  ENDIF ;profile analysis
  ;LAG analysis 
  IF (i EQ 0) THEN BEGIN
    ;data[0-1-2:source, bk, fap, nd3,*,*,0-1-2:deks]
    ;the lag is applied to X (keep Y and move x of lag)
    lag = [-5,-4,-3,-2,-1,0,1,2,3,4,5]
    ;simple example
;    lag = [-3,-2,-1,0,1,2,3]
;    x = [0,1,2,3,2.5,2,1.5,1,0.5,0,1,2,3]
;    y = [3,2.5,2,1.5,1,0.5,0,1,2,3,2.5,2,1.5]
;    h1 = PLOT(x)
;    h2 = PLOT(y, color='r', /OVERPLOT)
;    res = C_CORRELATE(x,y,lag)
;SAVE, /ALL, FILENAME = startup.out_path + '\dataList2.sav'
;startup = C_start_up()
;RESTORE, startup.out_path + '\dataList2.sav'
    FOR comp = 0, 2 DO BEGIN
       pos0_lag = [0.05,0.7,0.395,0.95]
      CASE comp OF 
        0: BEGIN
          indX = 2  ;0 BK, 1 CFAP, 2 ND3, lag is exressed on X
          indY = 0
          pos_lag =  pos0_lag
          END
        1: BEGIN
            indX = 1  ;0 BK, 1 CFAP, 2 ND3, lag is exressed on X
            indY = 0
            pos_lag =  pos0_lag - [0,dy_lag,0,dy_lag]
          END
        2: BEGIN
            indX = 2  ;0 BK, 1 CFAP, 2 ND3, lag is exressed on X
            indY = 1
            pos_lag =  pos0_lag - 2*[0,dy_lag,0,dy_lag]
          END
      ENDCASE
      ;compute the lag and correlation between X and Y
      bestLag = FLTARR(ns,nl)
      bestCorr = FLTARR(ns,nl)
      FOR ss = 0, ns-1 DO BEGIN
        FOR ll = 0, nl-1 DO BEGIN
          ;IF ss EQ 147 THEN STOP
          x = REFORM(ts[indX,ss,ll,*])
          y = REFORM(ts[indY,ss,ll,*])
          indFin=WHERE(FINITE(x) AND FINITE(y),cF)
          IF (cF GT 0) THEN BEGIN
            ;this is done to avoid NAN error but it is not optimal (if the lag is big what happens at the border of the lag?)
            tmp = C_CORRELATE(x[indFin],y[indFin],lag)
            ;        h1 = PLOT(ts[sourceSub,ss,ll,*])
            ;        h2 = PLOT(ts[0,ss,ll,*], color='r', /OVERPLOT)
            maxr = MAX(tmp, maxpos, /NAN)
            res = CHECK_MATH()
            IF (res GT 0) THEN STOP
          ENDIF ELSE maxr = !VALUES.F_NAN
          IF FINITE(maxr) THEN BEGIN
            bestLag[ss,ll] = lag[maxpos]
            bestCorr[ss,ll] = tmp[maxpos]
          ENDIF ELSE BEGIN
            bestLag[ss,ll] = !VALUES.F_NAN
            bestCorr[ss,ll] = !VALUES.F_NAN
          ENDELSE
        ENDFOR
      ENDFOR ;ss
      
      ;figure settings
      rgtab = 72
      dms_lag = [700,1000]
      dx_lag = 0.45
      dy_lag = 0.25
      
      
      ;plot it  
      data_min_max_lag = [lag[0],lag[-1]]
      IF (comp EQ 0) THEN crrnt = 0 ELSE crrnt = 1
      w1 = IMAGE(REVERSE(bestLag,2), DIMENSIONS=dms_lag,  RGB_TABLE=rgbtab, POSITION=pos_lag, AXIS_STYLE = axs_stl, $ ;LAYOUT=[4,4,6],
        MIN_VALUE = data_min_max_lag[0], MAX_VALUE = data_min_max_lag[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, $
        TITLE = startup.ts_name_short[indX] + ' vs. ' + startup.ts_name_short[indY], CURRENT=crrnt)
      posLegend_lag = [pos_lag[2]+0.025,pos_lag[1]+0.075,pos_lag[2]+0.05,pos_lag[1]+0.175] ;[X1, Y1, X2, Y2]
      cb = COLORBAR(TARGET=w1, ORIENTATION=1, POSITION=posLegend_lag, TAPER=0, TICKLAYOUT=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
      txt = TEXT((posLegend_lag[0]+posLegend_lag[2])/2,posLegend_lag[3]+0.025,'Lag of', ALIGNMENT=0.5)
      txt = TEXT((posLegend_lag[0]+posLegend_lag[2])/2,posLegend_lag[3]+0.01, startup.ts_name_short[indX], ALIGNMENT=0.5)

      pos_lag = pos_lag + [dx_lag, 0, dx_lag, 0] ;move one right
      data_min_max_lag = [-1,1]
      w2 = IMAGE(REVERSE(bestCorr,2), DIMENSIONS=dms_lag,  RGB_TABLE=rgbtab, POSITION=pos_lag, AXIS_STYLE = axs_stl, $ ;LAYOUT=[4,4,6],
        MIN_VALUE = data_min_max_lag[0], MAX_VALUE = data_min_max_lag[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, $
        TITLE = startup.ts_name_short[indX] + ' vs. ' + startup.ts_name_short[indY], /CURRENT)
      posLegend_lag = posLegend_lag + [dx_lag+0.05, 0, dx_lag+0.05, 0]
      cb = COLORBAR(TARGET=w2, ORIENTATION=1, POSITION=posLegend_lag, TAPER=0, TICKLAYOUT=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
      txt = TEXT((posLegend_lag[0]+posLegend_lag[2])/2,posLegend_lag[3]+0.025,'Correlation', ALIGNMENT=0.5)
      ;PRINT, 'qui..'
    ENDFOR
    w1.save, startup.out_path + '\'+freetext+'_lag_analysis.png', RESOLUTION = 600
  ENDIF
  
  IF (i EQ 1) THEN BEGIN ;I have reclassified data
    ;compute the mismatch between two images of classes
    classes = [0,1,2,3,4,5,6]
    dms = [1400,250]
    w1 = WINDOW(DIMENSIONS=dms)
    marg = [0.25, 0.1, 0.05, 0.15] ; [left, bottom, right, top]
    pos0 = [0.05,0.1,0.25,0.8];[0.1,0.1,0.3,0.3]  ;[X1, Y1, X2, Y2]
    dx = 0.275
    dy = 0.45
    fs_axis = 8
    fs_legend = 12

    FOR tt = 0, 2 DO BEGIN
      res01 = Mismatch_betwee_A_and_B_with_C_classes(data[0,*,*,tt], data[1,*,*,tt], classes)
      res02 = Mismatch_betwee_A_and_B_with_C_classes(data[0,*,*,tt], data[2,*,*,tt], classes)
      res12 = Mismatch_betwee_A_and_B_with_C_classes(data[1,*,*,tt], data[2,*,*,tt], classes)
      a = [res01.rank_a,res02.rank_a,res12.rank_a]
      mm = a + [res01.rank_mm,res02.rank_mm,res12.rank_mm]
      m = mm + [res01.rank_m,res02.rank_m,res12.rank_m]
      sm = m + [res01.rank_sm,res02.rank_sm,res12.rank_sm]
      um = sm + [res01.rank_um,res02.rank_um,res12.rank_um]
      pos = pos0 + [tt*dx, 0, tt*dx, 0] ;move one right
      b1 = BARPLOT([0,1,2],a, POSITION=pos,  FILL_COLOR='#2c7bb6', YRANGE=[0,100], XRANGE=[-0.6,2.6], NAME = 'A', $
        MARGIN = marg, XTICKNAME= [STRING(startup.ts_name_short[1] +'-'+ startup.ts_name_short[0]), $
        STRING(startup.ts_name_short[2] +'-'+ startup.ts_name_short[0]), $
        STRING(startup.ts_name_short[2] +'-'+ startup.ts_name_short[1])], $
        TITLE='Multicat. agreement',YTITLE='%',FONT_SIZE = fs_axis, XTICKINTERVAL=1, XMINOR=0,/CURRENT)
      ;text = TEXT(0.55, 0.95, 'Multi-category agreement, ' + variable + 'NxHf', ALIGNMENT = 0.5, FONT_SIZE = 18, /CURRENT)
      b2 = BARPLOT([0,1,2],mm, BOTTOM_VALUES=a, FILL_COLOR='#abd9e9', /OVERPLOT, NAME = 'MM')
      b3 = BARPLOT([0,1,2],m, BOTTOM_VALUES=mm, FILL_COLOR='#ffffbf', /OVERPLOT, NAME = 'M')
      b4 = BARPLOT([0,1,2],sm, BOTTOM_VALUES=m, FILL_COLOR='#fdae61', /OVERPLOT, NAME = 'SM')
      b5 = BARPLOT([0,1,2],um, BOTTOM_VALUES=sm, FILL_COLOR='#d7191c', /OVERPLOT, NAME = 'UM')
    ENDFOR
    ghl = LEGEND(TARGET=[b1, b2, b3, b4, b5], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.875,0.825], ORIENTATION = 0, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)

    ;show the dekad they refer to
    xstep0 = 0.15
    ypos = 0.915
    txt = TEXT(xstep0,ypos,'Dekad ' + STRTRIM(deks2map[0],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
    txt = TEXT(xstep0+dx*1,ypos,'Dekad '+ STRTRIM(deks2map[1],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
    txt = TEXT(xstep0+dx*2,ypos,'Dekad '+ STRTRIM(deks2map[2],2) + yr_suffix, ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
    ;print,'sono qui!'
    w1.save, startup.out_path + '\'+freetext+'_comparsion_agreement.png', RESOLUTION = 600
  ENDIF
ENDFOR
windowdeleteall
END

