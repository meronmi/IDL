PRO plor_specs, targets
symbolSize = 1.2
lineThickness = 2
textFontSize = 15
lfs = 8
lp1 = [1380, 0.6]
lp = [1620, 0.6]
xr = [400, 1050.0]
xtit = 'Wl (nm)'
yr = [0.0, 0.6]
ytit = 'HCRF (-)'
;load rainbow colortable 
ctb = COLORTABLE(13, NCOLORS = N_ELEMENTS(targets.target))
;modify it
ctb[0, *] = [0, 0, 0]
ctb[1, *] = [0, 0, 0]
ctb[2, *] = [0, 0, 0]
;cactus and aloe
ctb[11, *] = [230, 230, 0]
ctb[12, *] = [0, 255, 0]
ctb[13, *] = [0, 255, 0]
ctb[14, *] = [0, 255, 0]


ind = SORT(targets.target)
indGroup1 = [0,1,2,11,12,13,14]
indGroup2 = [3,4,5,6,7,8,9,10,15,16,17,18,19,20,21,22,23,24,25,26]
;plot soil and cactus

ci = 0
ls = 0
hh = PLOT(targets.wl, targets.mean_ref[ind[indGroup1[0]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
  XTITLE=xtit, YTITLE=ytit, DIMENSIONS=[1000,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, $
  NAME=targets.target[ind[indGroup1[0]]] + '_'+ STRTRIM(targets.target_id[ind[indGroup1[0]]],2), MARGIN=[0.1,0.1,0.33,0.1], THICK = 2)
ci ++
ls ++
;h = PLOT(targets.wl, targets.mean_ref[0,*], LINESTYLE = '-', COLOR='black', XRANGE=xr, YRANGE=yr, $
;  XTITLE=xtit, YTITLE=ytit, DIMENSIONS=[800,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, NAME=targets.target[0])
FOR i = 1, N_ELEMENTS(indGroup1)-1 DO BEGIN
  h1 = PLOT(targets.wl, targets.mean_ref[ind[indGroup1[i]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
    XTITLE=xtit, YTITLE=ytit, FONT_SIZE=fs, NAME=targets.target[ind[indGroup1[i]]]+ '_' + STRTRIM(targets.target_id[ind[indGroup1[i]]],2), $
    /OVERPLOT, THICK = 2)
  hh = [hh, h1]
  ci ++
  ls ++
  IF ls EQ 4 THEN ls = 0 
ENDFOR
!null = LEGEND(target=[hh], /AUTO_TEXT_COLOR, FONT_SIZE = lfs, POSITION=lp1, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, VERTICAL_SPACING= 0.01, /DATA)
 
 
;plot all acacias and prosopis  
;ctb = COLORTABLE(13, NCOLORS = N_ELEMENTS(indGroup2))
ctb1_t = COLORTABLE([[0,0,55],[0,0,255]], NCOLORS = 8)  ;80
ctb1 = ctb1_t * 0
ind1 = SORT(targets.mean_ref[ind[indGroup2[0:7]], 500])  ;sort by nir refelectance
FOR j = 0, N_ELEMENTS(ind1)-1 DO ctb1[ind1[j],*] = ctb1_t[j,*]
ctb2_t = COLORTABLE([[0,250,0],[0,50,0]], NCOLORS = 12) ;75
ctb2 = ctb2_t * 0
ind2 = SORT(targets.mean_ref[ind[indGroup2[8:19]], 500])  ;sort by nir refelectance
FOR j = 0, N_ELEMENTS(ind2)-1 DO ctb2[ind2[j],*] = ctb2_t[j,*]
ctb = [ctb1,ctb2]



ci = 0
ls = 0
;print, targets.mean_ref[ind[indGroup2[0]],500], REFORM(ctb[ci,*]) 
hh = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[0]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
  XTITLE=xtit, YTITLE=ytit, DIMENSIONS=[1000,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, $
  NAME=targets.target[ind[indGroup2[0]]] + '_'+ STRTRIM(targets.target_id[ind[indGroup2[0]]],2), MARGIN=[0.1,0.1,0.33,0.1], THICK = 2)

ci ++


FOR i = 1, N_ELEMENTS(indGroup2)-1 DO BEGIN
  IF i GE 8 THEN ls = 2
  ;print, targets.mean_ref[ind[indGroup2[i]],500], REFORM(ctb[ci,*]) 
  h1 = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[i]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
    XTITLE=xtit, YTITLE=ytit, FONT_SIZE=fs, NAME=targets.target[ind[indGroup2[i]]]+ '_' + STRTRIM(targets.target_id[ind[indGroup2[i]]],2), $
    /OVERPLOT, THICK = 2)
  hh = [hh, h1]
  ci ++
ENDFOR
!null = LEGEND(target=[hh], /AUTO_TEXT_COLOR, FONT_SIZE = lfs, POSITION=lp, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, VERTICAL_SPACING= 0.01, /DATA)

PRINT, 'fatto due'


yr = [0.0, 0.4]
lp = [1600, 0.4]
;;plot only the similar acacia and prosopis
;indGroup2 = [4,5,6,7,8,9,10,15,16,17,18,19,20,21,22,23,24,25,26]
sim = [4,5,6,7,8,9,13]
ctb1 = COLORTABLE([[0,0,35],[0,0,255]], NCOLORS = 4)  ;80
ctb2 = COLORTABLE([[0,250,0],[0,35,0]], NCOLORS = 3) ;75
ctb = [ctb1,ctb2]

ci = 0
ls= 0
hh = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[sim[0]]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
  XTITLE=xtit, YTITLE=ytit, DIMENSIONS=[1000,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, $
  NAME=targets.target[ind[indGroup2[sim[0]]]] + '_'+ STRTRIM(targets.target_id[ind[indGroup2[sim[0]]]],2) + 'NDVI='+STRTRIM(targets.ndvi[ind[indGroup2[sim[0]]]],2), MARGIN=[0.1,0.1,0.33,0.1], THICK = 2)




FOR i = 1, N_ELEMENTS(sim)-1 DO BEGIN
  IF i GE 4 THEN ls = 2
  ci ++
  ;print, targets.mean_ref[ind[indGroup2[i]],500], REFORM(ctb[ci,*])
  h1 = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[sim[i]]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
    XTITLE=xtit, YTITLE=ytit, FONT_SIZE=fs, $
    NAME=targets.target[ind[indGroup2[sim[i]]]]+ '_' + STRTRIM(targets.target_id[ind[indGroup2[sim[i]]]],2)+ 'NDVI='+STRTRIM(targets.ndvi[ind[indGroup2[sim[i]]]],2), $
    /OVERPLOT, THICK = 2)
  hh = [hh, h1]
ENDFOR
!null = LEGEND(target=[hh], /AUTO_TEXT_COLOR, FONT_SIZE = lfs, POSITION=lp, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, VERTICAL_SPACING= 0.01, /DATA)
  
PRINT, 'fatto tre'

;plot those that rare different
yr = [0.0, 0.6]
lp = [1620, 0.6]
;;plot only the diverse acacia and prosopis
;indGroup2 = [4,5,6,7,8,9,10,15,16,17,18,19,20,21,22,23,24,25,26]
;sim = [4,5,6,7,8,9,13]
sim = [1,2,3,10,11,12,14,15,16,17,18,19]
ctb1 = COLORTABLE([[0,0,35],[0,0,255]], NCOLORS = 3)  ;80
ctb2 = COLORTABLE([[0,250,0],[0,35,0]], NCOLORS = 12) ;75
ctb = [ctb1,ctb2]

ci = 0
ls= 0
hh = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[sim[0]]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
  XTITLE=xtit, YTITLE=ytit, DIMENSIONS=[1000,800], SYM_SIZE=symbolSize,  FONT_SIZE=fs, $
  NAME=targets.target[ind[indGroup2[sim[0]]]] + '_'+ STRTRIM(targets.target_id[ind[indGroup2[sim[0]]]],2), MARGIN=[0.1,0.1,0.33,0.1], THICK = 2)




FOR i = 1, N_ELEMENTS(sim)-1 DO BEGIN
  IF i GE 3 THEN ls = 2
  ci ++
  ;print, targets.mean_ref[ind[indGroup2[i]],500], REFORM(ctb[ci,*])
  h1 = PLOT(targets.wl, targets.mean_ref[ind[indGroup2[sim[i]]],*], LINESTYLE = ls, COLOR=REFORM(ctb[ci,*]), XRANGE=xr, YRANGE=yr, $
    XTITLE=xtit, YTITLE=ytit, FONT_SIZE=fs, $
    NAME=targets.target[ind[indGroup2[sim[i]]]]+ '_' + STRTRIM(targets.target_id[ind[indGroup2[sim[i]]]],2), $
    /OVERPLOT, THICK = 2)
  hh = [hh, h1]
ENDFOR
!null = LEGEND(target=[hh], /AUTO_TEXT_COLOR, FONT_SIZE = lfs, POSITION=lp, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.05, TRANSPARENCY=100, VERTICAL_SPACING= 0.01, /DATA)  
  
PRINT, 'finished'
END

PRO Spec_som

fn = 'S:\Actions\FOODSEC\projects\Prosopis\MISSION_02_2015\SPEC MEAS\all_specs.csv'
;tmp =  READ_CSV(fn, TABLE_HEADER=hdr,  N_TABLE_HEADER=6, MISSING_VALUE=-9999)
;read info
tmp =  READ_CSV(fn, NUM_RECORDS=6, RECORD_START=0, MISSING_VALUE=-9999);, TABLE_HEADER=hdr,  N_TABLE_HEADER=6)
tmp2 =  READ_CSV(fn, RECORD_START=6, MISSING_VALUE=-9999);, TABLE_HEADER=hdr,  N_TABLE_HEADER=6)
data = { $
  day : INTARR(N_TAGS(tmp)-1), $
  target : STRARR(N_TAGS(tmp)-1), $
  target_id : INTARR(N_TAGS(tmp)-1), $
  description : STRARR(N_TAGS(tmp)-1), $
  id : INTARR(N_TAGS(tmp)-1), $
  fn : STRARR(N_TAGS(tmp)-1), $
  wl : FINDGEN(751) + 325, $
  ref : FLTARR(N_TAGS(tmp)-1, 751) $
}
FOR i = 1, N_TAGS(tmp)-1 DO BEGIN
  data.day[i-1] = tmp.(i)[0]
  data.target[i-1] = tmp.(i)[1]
  data.target_id[i-1] = tmp.(i)[2]
  data.description[i-1] = tmp.(i)[3]
  data.id[i-1] = tmp.(i)[4]
  data.fn[i-1] = tmp.(i)[5]
  data.ref[i-1, *] = tmp2.(i)
ENDFOR

;find unique targets ids and fill the mean and sd for eah of them
uniq_target_ids = data.target_id[UNIQ(data.target_id, SORT(data.target_id))]
n_uniq = N_ELEMENTS(uniq_target_ids)
targets = { $
  day : INTARR(n_uniq), $
  target : STRARR(n_uniq), $
  target_id : INTARR(n_uniq), $
  description : STRARR(n_uniq), $
  wl : FINDGEN(751) + 325, $
  mean_ref : FLTARR(n_uniq, 751), $
  sd_ref : FLTARR(n_uniq, 751), $
  ndvi: FLTARR(n_uniq) $
}
FOR i = 0, n_uniq - 1 DO BEGIN
  ind = WHERE(data.target_id EQ uniq_target_ids[i])
  targets.day[i] = data.day[ind[0]]
  targets.target[i] = data.target[ind[0]]
  targets.target_id[i] = data.target_id[ind[0]]
  targets.description[i] = data.description[ind[0]]
  targets.mean_ref[i,*] = MEAN(data.ref[ind, *], DIMENSION = 1)
  targets.sd_ref[i,*] = STDDEV(data.ref[ind, *], DIMENSION = 1)
  red =  MEAN(targets.mean_ref[i,351:360])
  nir = MEAN(targets.mean_ref[i,451:460])
  targets.ndvi[i] = (nir-red)/(nir+red) 
ENDFOR

plor_specs, targets



END