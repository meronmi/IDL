PRO scatter4SOFI
;fn = 'X:\Active Projects\FAO SOFI\pheno_copy_of_z\COUNTRY ANALYS P10\AAA_summary_table_v3_only_africa.csv'
;fn = 'X:\Active Projects\FAO SOFI\pheno_copy_of_z\COUNTRY ANALYS P10\AAA_summary_table_v3.csv'
fn = 'X:\Active Projects\FAO SOFI\pheno_copy_of_z\COUNTRY ANALYS P10\AAA_summary_table_v4_only_africa.csv'
dir = FILE_DIRNAME(fn)
t = READ_CSV(fn, RECORD_START = 3)
;help, t
tags = TAG_NAMES(t)
t = rename_tags(t, tags, ['country','mask','area','season','gslSig','gslSigPos','gslSigNeg','gslSigPosTS','gslSigNegTS', $
                                                           'sosSig','sosSigPos','sosSigNeg','sosSigPosTS','sosSigNegTS'])
;help, t                                                    
masktypes = ['Croplands','Rangelands']

;decide what to plot
x =  t.gslSigPos;t.sosSigPos
xtit = 'GSL, % area with sig. increase trend';'SOS, % with sig. delay trend'
y = t.gslSigNeg
ytit = 'GSL, % area with sig. reduction trend'
;size of the sign
szRed = ABS(t.gslSigNegTS);t.sosSigPosTS
szGreen = ABS(t.gslSigPosTS);t.sosSigPosTS
maxSz = MAX([szRed, szGreen])
;depend on the type of graph
;            Zambia,Uganda, Tunisia, Somalia,Philippines,Nicaragua, Morocco,Mongolia, Malawi,    Lybia,   Kenya, Honduras,Guatemala,  Ethiopia,El Salvador,Egypt,    Bangladesh,  Algeria
locations= ['center','center','center','top',  'center', 'bottom' , 'bottom', 'center', 'right', 'left', 'top', 'center', 'center', 'center', 'top',    'center', 'center', 'center']
locations = 'center'

;tmp = SORT(t.area)
;ind = UNIQ(t.area[SORT(t.area)])
;t.area[tmp[ind]]

area2color = [['NA','Sandy Brown'], $
              ['WA','Orchid'], $
              ['CA','Forest Green'], $
              ['EA','Gold'], $
              ['SA','Red']]
              
  

FOR s = 1, 2 DO BEGIN
  FOR m = 0, 1 DO BEGIN
    
    ;select those relevant
    ind = WHERE((t.season EQ s) AND (t.mask EQ masktypes[m]), count)
    IF ~((s EQ 1) AND (m EQ 0)) THEN locations = MAKE_ARRAY(count, /STRING, VALUE='center')
    ;xr = [0,MAX([x[ind],y[ind]])+2]  ;,y[ind]
    xr = [0,MAX([x[ind],y[ind]])]  ;,y[ind]
    ;round it to next 5 interval
    xr[1] = CEIL(xr[1]/5.0)*5 
    ;yr = [0,MAX(y[ind])+2]
    yr = xr
    xydim = [900,700]
   
    squaredim = 650
    xrel = squaredim/FLOAT(xydim[0])  ;for a plot of about squaredim
    yrel = squaredim/FLOAT(xydim[1])  ;for a plot of about squaredim
    strTitle = masktypes[m] + ', season ' + STRTRIM(s,2)
    strTitle = strTitle.CapWords()
    mycolors = STRARR(N_ELEMENTS(t.area[ind]))
    FOR i = 0, N_ELEMENTS(mycolors)-1 DO BEGIN
      indc = WHERE(area2color[0,*] EQ t.area[ind[i]])
      mycolors[i] = area2color[1, indc]
    ENDFOR
    ghb = BUBBLEPLOT(x[ind], y[ind], MAGNITUDE = szRed[ind], LABELS =t.country[ind], XRANGE = xr, YRANGE = yr, $
      XTITLE = xtit, YTITLE = ytit, TITLE = strTitle, DIMENSIONS=xydim, $
      LINECOLOR='black', FILLED=1, COLOR = mycolors, $
      MAX_VALUE = maxSz*2, LABEL_FONT_SIZE=10, LABEL_POSITION=locations, EXPONENT=0.5, $;.5
      XTICKFONT_SIZE = 12, YTICKFONT_SIZE = 12, $
      LAYOUT=[2,1,1], POSITION=[0.1,0.1,xrel,yrel], SIZING=0.75)
    ghb.TITLE.FONT_SIZE = 16
    gh = PLOT([0,100],[0,100], /OVERPLOT)
    ghb['axis2'].TICKLEN = 0
    ghb['axis3'].TICKLEN = 0
;    ghb = BUBBLEPLOT(x[ind], y[ind], MAGNITUDE = szGreen[ind], LABELS =t.country[ind], XRANGE = xr, YRANGE = yr, $
;      LINECOLOR='dark green', COLOR='light grey', FILLED=0, MAX_VALUE = maxSz, /OVERPLOT, EXPONENT=.5)
    minMag = MIN(szRed[ind])
    maxMag = MAX(szRed[ind])
    nsample = 4
    x2 = INTARR(nsample);[50, 50, 50, 50];, 50, 50, 50, 50]
    y2 = 1.3*INDGEN(nsample)+1;1*[1,2,4,6];,8,10,13,16]
    bSizes = minMag + INDGEN(4) * (maxMag-minMag)/FLOAT(nsample-1)
    bLabels = STRING(-bSizes, FORMAT='(F6.3)')
    myKey = BUBBLEPLOT(x2, y2, MAGNITUDE=bSizes, $
      EXPONENT=.5, /CURRENT, AXIS_STYLE=0, LABELS=bLabels, $
      FILLED=0, LABEL_FONT_SIZE=8, LINECOLOR='black', $
      FONT_SIZE=10, FONT_STYLE='italic', $
      LABEL_POSITION='center', LAYOUT=[2,1,2], $
      MAX_VALUE=ghb.MAX_VALUE, SIZING=ghb.SIZING, $ ;
      POSITION=[xrel,0.1,0.95,0.8], YRANGE=[0,10])
    nsample = 5
    x2a = INTARR(nsample);[50, 50, 50, 50];, 50, 50, 50, 50]
    y2a = (INDGEN(nsample)+1)*0.5;1*[1,2,4,6];,8,10,13,16]
    bLabels = REFORM(area2color[0,*])
    bcolors = REFORM(area2color[1,*])
    myKey2 = BUBBLEPLOT(x2a, y2a+5, MAGNITUDE=y2a*0+10, $
      EXPONENT=.5, /CURRENT, AXIS_STYLE=0,  $ ;LABELS=bLabels,
      COLOR = bcolors, FILLED=1, LABEL_FONT_SIZE=8,  $
      FONT_SIZE=10, FONT_STYLE='italic', $
      LABEL_POSITION='right', LAYOUT=[2,1,2], $
      MAX_VALUE=40, SIZING=ghb.SIZING, $ ;
      POSITION=[xrel-0.12,0.1,0.95,0.95], YRANGE=[0,8])  
    gText = TEXT(0.2, MAX(y2a[0]+4.92), TARGET=myKey2, 'Northern Africa', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(0.2, MAX(y2a[1]+4.92), TARGET=myKey2, 'Western Africa', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(0.2, MAX(y2a[2]+4.92), TARGET=myKey2, 'Central Africa', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(0.2, MAX(y2a[3]+4.92), TARGET=myKey2, 'Eastern Africa', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(0.2, MAX(y2a[4]+4.92), TARGET=myKey2, 'Southern Africa', /DATA, COLOR='black', FONT_SIZE=12)
    
    gText = TEXT(-0.8, MAX(y2)+2.0, TARGET=myKey, 'Area of the bubble: ', /DATA, COLOR='black', FONT_SIZE=12, FONT_STYLE = 1)
    gText = TEXT(-0.8, MAX(y2)+1.6, TARGET=myKey, 'mean Theil-Sen slope', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(-0.8, MAX(y2)+1.2, TARGET=myKey, 'of ' + masktypes[m]+ ' with', /DATA, COLOR='black', FONT_SIZE=12)
    gText = TEXT(-0.8, MAX(y2)+0.8, TARGET=myKey, 'reduction trend', /DATA, COLOR='black', FONT_SIZE=12)
    ghb.save, dir + '\' + masktypes[m] + '_season' + STRTRIM(s,2) + '.png', RESOLUTION=300
    PRINT, 'debug'
  ENDFOR ;mask
ENDFOR ;seas



END