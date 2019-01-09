;FUNCTION zreclass, data
;  dataout = data*!VALUES.F_NAN
;  indFin = WHERE(FINITE(data))
;  
;  ind = WHERE((data[indFin] LE -2), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 0
;  
;  ind = WHERE((data[indFin] LE -1.5) AND (data[indFin] GT -2), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 1
;  
;  ind = WHERE((data[indFin] LE -1) AND (data[indFin] GT -1.5), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 2
;  
;  ind = WHERE((data[indFin] LT 1) AND (data[indFin] GT -1), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 3
;  
;  ind = WHERE((data[indFin] LT 1.5) AND (data[indFin] GE 1), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 4
;  
;  ind = WHERE((data[indFin] LT 2) AND (data[indFin] GE 1.5), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 5
;  
;  ind = WHERE((data[indFin] GE 2), count)
;  IF (count GT 0) THEN dataout[indFin[ind]] = 6
;  
;  return, dataout
;END

PRO figures_for_paper_prepare_data
;bil file ofz anaomaly are present, name like zN0L0_2003-2016_bsq
;z images for HoA end of 2010
dir_in = 'D:\HoA\Z2'
ns = 3301
nl = 2501
prefix = 'z'
suffix = '__2003-2016_HoA_bsq.img'
NhfHf_computed = 0
option = ['x'] ; ['x', 'f'], compute x only as it was shown to be better for multicat ranked agreement, if modified, data has to change to include the index for the two versions
deks2map = [27,30,33]
year2map = 2010
frstyear = 2003
;store the deks and products here
stages2analyse = [0,2,4,5]
data0 = FLTARR(ns, nl, N_ELEMENTS(deks2map)) * !VALUES.F_NAN

init = 1
 
FOR ii = 0, N_ELEMENTS(stages2analyse)-1 DO BEGIN
  i = stages2analyse[ii]
  strLstage = STRTRIM(i,2) 
  strNstage = STRTRIM(i,2)
  IF (strLstage EQ '5') THEN strLstage = 'f'
  IF (strNstage EQ '5') THEN strNstage = 'f'
  PRINT, 'Now computing for N->' + strNstage + ' , H->' + strLstage
  fn = dir_in + '\' + prefix + 'N' +  strNstage + 'L' + strLstage + suffix
  tmp = ReadEnviWithHdr(fn)
  ;fill data for the dekads of interest
  FOR d = 0,  N_ELEMENTS(deks2map)-1 DO BEGIN
    indBand = ((year2map-frstyear) * 36 + deks2map[d]) - 1
    data0[*,*,d]=tmp[*,*,indBand] 
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

;compute NfHf


SAVE, data, FILENAME = dir_in + '\dataList.sav' 
END

PRO figures_for_paper
dir_in = 'D:\HoA\Z2'
RESTORE, dir_in + '\dataList.sav'

ns = 3301
nl = 2501
x_coord_min_max= [22.67,22.67+ns*0.00892857143]
y_coord_min_max= [16.08-nl*0.00892857143,16.08]

;test data
;data = FINDGEN(ns, nl)
;data = data/MAX(data)-0.5
;data_min_max = [-1, 1]
mxs = data.Map(Lambda(array: MAX(array,/NAN)))
mns = data.Map(Lambda(array: MIN(array,/NAN)))
data_min_max = [MIN(mns.ToArray(),/NAN), MAX(mxs.ToArray(),/NAN)]

;plot z-score stages2analyse = [0,2,4,5]
rgbtab = 72; clorblind friendly red-yellow-blue
dms = [1400,1000]
axs_stl = 2
pos0 = [0.1,0.7,0.3,0.9];[0.1,0.1,0.3,0.3]  ;[X1, Y1, X2, Y2]
pos = pos0
stage = 0
w1 = IMAGE(REVERSE(data[stage,*,*,0],2), DIMENSIONS=dms, LAYOUT=[4,4,6], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, $
           MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0)
xax = AXIS('X', LOCATION='bottom', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [x_coord_min_max[0],0.00892857143],TITLE='Deg. E') ;AXIS_RANGE=x_coord_min_max,
yax = AXIS('Y', LOCATION='left', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [y_coord_min_max[0],0.00892857143],TITLE='Deg. N')
cbx = 0.8 & cby = 0.72 & cbdx = 0.02 & cbdy = 0.15
cb = COLORBAR(TARGET=w1, ORIENTATION=1, POSITION=[cbx,cby,cbx+cbdx,cby+cbdy], TAPER=0, TICKLAYOUT=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01,'Z-score', ALIGNMENT=0.5) 
dx = 0.225
dy = 0.25
pos = pos + [dx, 0, dx, 0] ;[X1, Y1, X2, Y2]
w2 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,6+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w3 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,6+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

stage = 1
pos = pos0 + [0, -dy, 0, -dy]
w4 = IMAGE(REVERSE(data[stage,*,*,0],2), LAYOUT=[4,4,9+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w5 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,9+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w6 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,9+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

stage = 3
pos = pos0 + [0, -2*dy, 0, -2*dy]
w7 = IMAGE(REVERSE(data[stage,*,*,0],2), LAYOUT=[4,4,13+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w8 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,13+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w9 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,13+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

step = dx
xstep0 = 0.195
ypos = 0.925
;write dek
txt = TEXT(xstep0,ypos,'Dekad 27', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xstep0+step*1,ypos,'Dekad 30', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xstep0+step*2,ypos,'Deakad 33', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
ystep0 = 0.795
xpos = 0.025
step = dy
;write c stage
txt = TEXT(xpos,ystep0,'C0', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xpos,ystep0-step*1,'C2', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xpos,ystep0-step*2,'CF', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')

w1.save, 'D:\HoA\HoA_z-score_new.png', RESOLUTION = 600


;plot classified z-score
;w = WINDOW(DIMENSIONS=[400,600])
discreteCTB = ['#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4'];REVERSE(['#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'])
tickname  = ['Ext. bad','Very bad','Mod. bad','Normal','Mod. good','Very good','Ext. good']
;reclassify data
;test data
;data = FLTARR(ns, nl)
;data = zreclass(data)
;FOR i = 0, 6 DO data[i*200:(i+1)*200,*] = i
data = data.Map('zreclass')
data_min_max = [0,6]

;mrgn = [0.2,0.1,0.05,0.1] ;[left, bottom, right, top]
;dms = [1200,800]
;w1 = IMAGE(REVERSE(data[0,*,*,0],2), DIMENSIONS=dms, LAYOUT=[3,3,1], RGB_TABLE=rgbtab, MARGIN=mrgn, AXIS_STYLE = axs_stl, MIN_VALUE = 0, MAX_VALUE = 6,  XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0)
pos = pos0
stage = 0
w1 = IMAGE(REVERSE(data[stage,*,*,0],2), DIMENSIONS=dms, LAYOUT=[4,4,6], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, $
  MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0)
xax = AXIS('X', LOCATION='bottom', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [x_coord_min_max[0],0.00892857143],TITLE='Deg. E') ;AXIS_RANGE=x_coord_min_max,
yax = AXIS('Y', LOCATION='left', TICKDIR=1, MINOR=0, COORD_TRANSFORM = [y_coord_min_max[0],0.00892857143],TITLE='Deg. N')
;cb = COLORBAR(ORIENTATION=1, POSITION=[0.365,0.77,0.385,0.93],  RGB_TABLE=discreteCTB,  TAPER=0, TICKLAYOUT=1, TICKNAME=tickname, BORDER=1) ;postion :[X1, Y1, X2, Y2],
;txt = TEXT(0.3725,0.96,'Z-score', ALIGNMENT=0.5)
cb = COLORBAR(ORIENTATION=1, POSITION=[cbx,cby,cbx+cbdx,cby+cbdy], RGB_TABLE=discreteCTB,  TAPER=0, TICKLAYOUT=1, TICKNAME=tickname, BORDER=1) ;postion :[X1, Y1, X2, Y2],, POSITION=[x1,y1,x1+dx,y1+dy]
txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01,'classes', ALIGNMENT=0.5)
txt = TEXT(cbx+cbdx/2.0,cby+cbdy+0.01+0.02,'Z-score', ALIGNMENT=0.5)
pos = pos + [dx, 0, dx, 0] ;[X1, Y1, X2, Y2]
w2 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,6+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w3 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,6+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

stage=1
pos = pos0 + [0, -dy, 0, -dy]
w4 = IMAGE(REVERSE(data[stage,*,*,0],2), LAYOUT=[4,4,9+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w5 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,9+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w6 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,9+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

stage = 3
pos = pos0 + [0, -2*dy, 0, -2*dy]
w7 = IMAGE(REVERSE(data[stage,*,*,0],2), LAYOUT=[4,4,13+1], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w8 = IMAGE(REVERSE(data[stage,*,*,1],2), LAYOUT=[4,4,13+2], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)
pos = pos + [dx, 0, dx, 0]
w9 = IMAGE(REVERSE(data[stage,*,*,2],2), LAYOUT=[4,4,13+3], RGB_TABLE=rgbtab, POSITION=pos, AXIS_STYLE = axs_stl, MIN_VALUE = data_min_max[0], MAX_VALUE = data_min_max[1], XMAJOR=0, XMINOR=0, YMINOR=0, YMAJOR=0, /CURRENT)

step = dx
;write dek
txt = TEXT(xstep0,ypos,'Dekad 27', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xstep0+step*1,ypos,'Dekad 30', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xstep0+step*2,ypos,'Deakad 33', ALIGNMENT=0.5, FONT_SIZE=14);, FONT_STYLE='Bold')

;write A), B) etc
ypos = 0.875
xstep0 = 0.11
txt = TEXT(xstep0,ypos,'A)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*1,ypos,'B)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*2,ypos,'C)', ALIGNMENT=0, FONT_SIZE=14)

txt = TEXT(xstep0,ypos-DY,'D)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*1,ypos-dy,'E)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*2,ypos-dy,'F)', ALIGNMENT=0, FONT_SIZE=14)

txt = TEXT(xstep0,ypos-2*DY,'G)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*1,ypos-2*dy,'H)', ALIGNMENT=0, FONT_SIZE=14)
txt = TEXT(xstep0+step*2,ypos-2*dy,'I)', ALIGNMENT=0, FONT_SIZE=14)


step = dy
;write c stage
txt = TEXT(xpos,ystep0,'C0', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xpos,ystep0-step*1,'C2', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
txt = TEXT(xpos,ystep0-step*2,'CF', ALIGNMENT=0, FONT_SIZE=14);, FONT_STYLE='Bold')
w1.save, 'D:\HoA\HoA_z-score_classes_new.png', RESOLUTION = 600
PRINT, 'ok'


END