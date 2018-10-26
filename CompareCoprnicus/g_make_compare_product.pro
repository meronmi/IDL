PRO G_make_compare_product
;here I select a spatial window of the global thinned images and I plot over dekd for different products

;USER SETTINGS
useSave = 1
deks2map = [27,30,33] ;only 3
sULcorner = 1090 ;sample
lULcorner = 315 ;line
ns = 148
nl = 114
year2map = 2010
frstyear = 2003         ;note: the stack file MUST start at dek 1 of that year
x0 = -180.004464        ;these info come from the hdr of the stack file
y0 = 75.0044643
xyres = 0.1875
freetext = 'HoA2010'    ;a text that will be used in the output png files
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
    ;fill data for the dekads of interest
    FOR d = 0,  N_ELEMENTS(deks2map)-1 DO BEGIN
      indBand = ((year2map-frstyear) * 36 + deks2map[d]) - 1
      data0[*,*,d]=tmp[sULcorner:sULcorner+ns-1,lULcorner:lULcorner+nl-1,indBand]
    ENDFOR
    IF (init EQ 1) THEN BEGIN
      data = LIST(data0)
      data0[*,*,*] = !VALUES.F_NAN
      init = 0
    ENDIF ELSE BEGIN
      data.Add, data0
      data0[*,*,*] = !VALUES.F_NAN
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
      END
    1:BEGIN
        ;plot classified z-score
        data = data.Map('zreclass')
        data_min_max = [0,6]
        discreteCTB = ['#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4'];REVERSE(['#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'])
        tickname  = ['Ext. bad','Very bad','Mod. bad','Normal','Mod. good','Very good','Ext. good']
      END
  ENDCASE
  pos = pos0
  w1 = IMAGE(REVERSE(data[0,*,*,0],2), DIMENSIONS=dms, LAYOUT=[4,4,6], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, $
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
  w2 = IMAGE(REVERSE(data[0,*,*,1],2), LAYOUT=[4,4,6+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w3 = IMAGE(REVERSE(data[0,*,*,2],2), LAYOUT=[4,4,6+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  
  pos = pos0 + [0, -dy, 0, -dy]
  w4 = IMAGE(REVERSE(data[1,*,*,0],2), LAYOUT=[4,4,9+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w5 = IMAGE(REVERSE(data[1,*,*,1],2), LAYOUT=[4,4,9+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w6 = IMAGE(REVERSE(data[1,*,*,2],2), LAYOUT=[4,4,9+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  pos = pos0 + [0, -2*dy, 0, -2*dy]
  w7 = IMAGE(REVERSE(data[2,*,*,0],2), LAYOUT=[4,4,13+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w8 = IMAGE(REVERSE(data[2,*,*,1],2), LAYOUT=[4,4,13+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  pos = pos + [dx, 0, dx, 0]
  w9 = IMAGE(REVERSE(data[2,*,*,2],2), LAYOUT=[4,4,13+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
  
  
  xstep0 = 0.195
  ypos = 0.925
  ;write dek
  txt = TEXT(xstep0,ypos,'Dekad ' + STRTRIM(deks2map[0],2), ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xstep0+dx*1,ypos,'Dekad '+ STRTRIM(deks2map[1],2), ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xstep0+dx*2,ypos,'Dekad '+ STRTRIM(deks2map[2],2), ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
  ystep0 = 0.795
  xpos = 0.025
  ;write Product Name
  txt = TEXT(xpos,ystep0,startup.ts_name_short[0], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xpos,ystep0-dy*1,startup.ts_name_short[1], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  txt = TEXT(xpos,ystep0-dy*2,startup.ts_name_short[2], ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
  
  ;write A), B) etc
  ypos = 0.875
  xstep0 = 0.195
  txt = TEXT(xstep0,ypos,'A)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*1,ypos,'B)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*2,ypos,'C)', ALIGNMENT=0, FONT_SIZE=14)

  txt = TEXT(xstep0,ypos-dy,'D)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*1,ypos-dy,'E)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*2,ypos-dy,'F)', ALIGNMENT=0, FONT_SIZE=14)

  txt = TEXT(xstep0,ypos-2*dy,'G)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*1,ypos-2*dy,'H)', ALIGNMENT=0, FONT_SIZE=14)
  txt = TEXT(xstep0+dx*2,ypos-2*dy,'I)', ALIGNMENT=0, FONT_SIZE=14)
  w1.save, startup.out_path + '\'+freetext+'_comparsion_z-score.png', RESOLUTION = 600
  IF (i EQ 1) THEN BEGIN
    ;compute the mismatch between two images of classes
    classes = [0,1,2,3,4,5,6]
    dms = [1400,500]
    w1 = WINDOW(DIMENSIONS=dms)
    marg = [0.25, 0.1, 0.05, 0.15] ; [left, bottom, right, top]
    fs_axis = 8
    fs_legend = 12
    ;bk-cfapar
    FOR tt = 0, 2 DO BEGIN
      res = Mismatch_betwee_A_and_B_with_C_classes(data[0,*,*,tt], data[1,*,*,tt], classes)
      a = [res.rank_a,0]
      mm = a + [res.rank_mm,0]
      m = mm + [res.rank_m,0]
      sm = m + [res.rank_sm,0]
      um = sm + [res.rank_um,0]
      b1 = BARPLOT([0,1],a, LAYOUT=[4,2,tt+1],  FILL_COLOR='#2c7bb6', YRANGE=[0,100], XRANGE=[-0.6,1.6], NAME = 'A', $
        MARGIN = marg, XTICKNAME= [STRING(startup.ts_name_short[1] +' vs. '+ startup.ts_name_short[0]),'-'], $
        TITLE='Multicat. agreement',YTITLE='%',FONT_SIZE = fs_axis, XTICKINTERVAL=1, XMINOR=0,/CURRENT)
      ;text = TEXT(0.55, 0.95, 'Multi-category agreement, ' + variable + 'NxHf', ALIGNMENT = 0.5, FONT_SIZE = 18, /CURRENT)
      b2 = BARPLOT([0,1],mm, BOTTOM_VALUES=a, FILL_COLOR='#abd9e9', /OVERPLOT, NAME = 'MM')
      b3 = BARPLOT([0,1],m, BOTTOM_VALUES=mm, FILL_COLOR='#ffffbf', /OVERPLOT, NAME = 'M')
      b4 = BARPLOT([0,1],sm, BOTTOM_VALUES=m, FILL_COLOR='#fdae61', /OVERPLOT, NAME = 'SM')
      b5 = BARPLOT([0,1],um, BOTTOM_VALUES=sm, FILL_COLOR='#d7191c', /OVERPLOT, NAME = 'UM') 
      ;print,'zuzzu'
    ENDFOR
    ghl = LEGEND(TARGET=[b1, b2, b3, b4, b5], SHADOW = 0, TRANSPARENCY = 100, POSITION = [0.82,0.9495], ORIENTATION = 0, HORIZONTAL_ALIGNMENT = 'CENTER', VERTICAL_ALIGNMENT = 1, FONT_SIZE = fs_legend)
    ;bk-ndv3 and cfapar-bk
    FOR tt = 0, 2 DO BEGIN
      res02 = Mismatch_betwee_A_and_B_with_C_classes(data[0,*,*,tt], data[2,*,*,tt], classes)
      res12 = Mismatch_betwee_A_and_B_with_C_classes(data[1,*,*,tt], data[2,*,*,tt], classes)
      a = [res02.rank_a,res12.rank_a]
      mm = a + [res02.rank_mm,res12.rank_mm]
      m = mm + [res02.rank_m,res12.rank_m]
      sm = m + [res02.rank_sm,res12.rank_sm]
      um = sm + [res02.rank_um,res12.rank_um]
      b1 = BARPLOT([0,1],a, LAYOUT=[4,2,4+tt+1],  FILL_COLOR='#2c7bb6', YRANGE=[0,100], XRANGE=[-0.6,1.6], NAME = 'A', $
        MARGIN = marg, XTICKNAME= [STRING(startup.ts_name_short[2] +' vs. '+ startup.ts_name_short[0]),STRING(startup.ts_name_short[2] +' vs. '+ startup.ts_name_short[1])], $
        TITLE='Multicat. agreement',YTITLE='%',FONT_SIZE = fs_axis, XTICKINTERVAL=1, XMINOR=0,/CURRENT)
      ;text = TEXT(0.55, 0.95, 'Multi-category agreement, ' + variable + 'NxHf', ALIGNMENT = 0.5, FONT_SIZE = 18, /CURRENT)
      b2 = BARPLOT([0,1],mm, BOTTOM_VALUES=a, FILL_COLOR='#abd9e9', /OVERPLOT, NAME = 'MM')
      b3 = BARPLOT([0,1],m, BOTTOM_VALUES=mm, FILL_COLOR='#ffffbf', /OVERPLOT, NAME = 'M')
      b4 = BARPLOT([0,1],sm, BOTTOM_VALUES=m, FILL_COLOR='#fdae61', /OVERPLOT, NAME = 'SM')
      b5 = BARPLOT([0,1],um, BOTTOM_VALUES=sm, FILL_COLOR='#d7191c', /OVERPLOT, NAME = 'UM')
    ENDFOR
    print,'zuzzu'
  ENDIF
  w1.save, startup.out_path + '\'+freetext+'_comparsion_agreement', RESOLUTION = 600
  ;LAG analysis 
  
ENDFOR





END