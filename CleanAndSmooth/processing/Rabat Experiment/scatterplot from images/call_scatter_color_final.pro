pro call_scatter_color_final

file1='N:\2012TESIS\CAPITULO_3\EFT_2011\SOM2013\Patterns\NDVI_Ano00_09\valmaxNDVI'
file2='N:\2012TESIS\CAPITULO_3\EFT_2011\SOM2013\Patterns\NDVI_Ano00_09\valminNDVI'
fpng='N:\2012TESIS\CAPITULO_3\EFT_2011\SOM2013\Patterns\NDVI_Ano00_09\scatterMean_INDVI.jpg'

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


;*************** ZONA EDITABLE

ngrid=200
charsize=3.3
charthick=3.3
thh=[0,1,1]
im1_range=[0,1]
im2_range=[0,1]
tipo_reg='lin'
xtit='Mean value'
ytit='INDVI'
muestra_reg=3
escribe_stat=['ajuste','RMS','r']
tic=['0.0','1.5','3.0','4.5','6.0'] ;*****leyenda, posicion
dim_leg=[100,1] & posleg=[0.7,0.25] 
;********************************************* programa

scatter_color_final, im1,im2,im1_range,im2_range, thh=thh, ngrid=ngrid, $
    xtit=xtit,ytit=ytit,tit=tit, fpng=fpng,   $
    frec_min=frec_min,frec_max=frec_max,elimina=elimina,window=window, $
    charsize=charsize,charthick=charthick, muestra_reg=muestra_reg,winsize=winsize, $
    xmargin=xmargin,ymargin=ymargin,tipo_reg=tipo_reg,stat=stat,posleg=posleg, $
    escribe_stat=escribe_stat,pos_stat=pos_stat

print,'Ho finito'
end