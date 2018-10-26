PRO biomass_stats

fname = 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\SENEGAL_IDL_FORMAT_2014-01-14.csv'
tmp =  READ_CSV(fname, $
                HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN)
PRINT, hdr
;load data into a strcture with the following fields:
;ID,Site,XUTM,YUTM,Year,HB,TB
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7'], $
                        ['ID','Site','XUTM','YUTM','Year','HB','TB'])

;********************************************************************************
;overall boxplot
tmp = [TRANSPOSE(data.hb),TRANSPOSE(data.TB)] 
res = CREATEBOXPLOTDATA(tmp)
gh0 = BOXPLOT(res, TITLE = 'Biomass', YTITLE='kg/ha', XRANGE =[-0.5, 1.5]) 
gh0.XTICKNAME = ['','Herb','','Tree','']
gh0.XMINOR = 0
gh0.XTICKLEN = 0
idList = uniqlist(data.ID)
yearList = uniqlist(data.Year)

;********************************************************************************
;boxplot of herb biomass by site
tmp = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  ind = WHERE(data.ID EQ idList[i])
  tmp[i,*] = data.HB[ind]
ENDFOR
;check where i don't have at least 5 elements (cannot compute boxplot)
fin = TOTAL(FINITE(tmp), 2)
indKeep = WHERE(fin GE 5)
indRemove = WHERE(fin LT 5)
IF (indRemove[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs'
  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs are not boxplotted, sites:'
  PRINT, idList[indRemove]
  idList2 = idList[indKeep]
  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
  tmp2 = tmp[indKeep,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  idList2 = idList
  tmp2 = tmp
ENDELSE
res = CREATEBOXPLOTDATA(tmp2)
gh1 = BOXPLOT(res, TITLE = 'Herb Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(idList2)])
gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh1.XTICKFONT_SIZE = 10
gh1.XMINOR = 0
gh1.XTICKLEN = 0
;********************************************************************************
;boxplot of herb biomass by year
tmp = FLTARR(N_ELEMENTS(yearList),N_ELEMENTS(idList))
FOR i = 0, N_ELEMENTS(yearList)-1 DO BEGIN
  ind = WHERE(data.Year EQ yearList[i])
  tmp[i,*] = data.HB[ind]
ENDFOR
;check where i don't have at least 5 elements (cannot compute boxplot)
fin = TOTAL(FINITE(tmp), 2)
indKeep = WHERE(fin GE 5)
indRemove = WHERE(fin LT 5)
IF (indRemove[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' years containing less than 5 obs'
  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' years containing less than 5 obs are not boxplotted, sites:'
  PRINT, yearList[indRemove]
  yearList2 = yearList[indKeep]
  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(idList))
  tmp2 = tmp[indKeep,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  yearList2 = yearList
  tmp2 = tmp
ENDELSE
res = CREATEBOXPLOTDATA(tmp2)
gh2 = BOXPLOT(res, TITLE = 'Herb Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(yearList2)])
gh2.XTICKNAME = [' ',STRTRIM([yearList2],2),' ']
gh2.XTICKFONT_SIZE = 10
gh2.XMINOR = 0
gh2.XTICKLEN = 0

;********************************************************************************
;boxplot of tree biomass by site
tmp = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  ind = WHERE(data.ID EQ idList[i])
  tmp[i,*] = data.TB[ind]
ENDFOR
;check where i don't have at least 5 elements (cannot compute boxplot)
fin = TOTAL(FINITE(tmp), 2)
indKeep = WHERE(fin GE 5)
indRemove = WHERE(fin LT 5)
IF (indRemove[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs'
  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' sites containing less than 5 obs are not boxplotted, sites:'
  PRINT, idList[indRemove]
  idList2 = idList[indKeep]
  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(yearList))
  tmp2 = tmp[indKeep,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  idList2 = idList
  tmp2 = tmp
ENDELSE
res = CREATEBOXPLOTDATA(tmp2)
gh3 = BOXPLOT(res, TITLE = 'Tree Leaf Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(idList2)])
gh3.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh3.XTICKFONT_SIZE = 10
gh3.XMINOR = 0
gh3.XTICKLEN = 0
;********************************************************************************
;boxplot of tree biomass by year
tmp = FLTARR(N_ELEMENTS(yearList),N_ELEMENTS(idList))
FOR i = 0, N_ELEMENTS(yearList)-1 DO BEGIN
  ind = WHERE(data.Year EQ yearList[i])
  tmp[i,*] = data.TB[ind]
ENDFOR
;check where i don't have at least 5 elements (cannot compute boxplot)
fin = TOTAL(FINITE(tmp), 2)
indKeep = WHERE(fin GE 5)
indRemove = WHERE(fin LT 5)
IF (indRemove[0] NE -1) THEN BEGIN
  tmpTitle = ', ' + STRTRIM(N_ELEMENTS(indRemove),2)+' years containing less than 5 obs'
  PRINT, STRTRIM(N_ELEMENTS(indRemove),2)+' years containing less than 5 obs are not boxplotted, sites:'
  PRINT, yearList[indRemove]
  yearList2 = yearList[indKeep]
  tmp2 = FLTARR(N_ELEMENTS(indKeep),N_ELEMENTS(idList))
  tmp2 = tmp[indKeep,*]
ENDIF ELSE BEGIN
  tmpTitle = ''
  yearList2 = yearList
  tmp2 = tmp
ENDELSE
res = CREATEBOXPLOTDATA(tmp2)
gh3 = BOXPLOT(res, TITLE = 'Tree Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(yearList2)])
gh3.XTICKNAME = [' ',STRTRIM([yearList2],2),' ']
gh3.XTICKFONT_SIZE = 10
gh3.XMINOR = 0
gh3.XTICKLEN = 0

;scatter plot herb Vs tree
gh3 =  plot(REFORM(data.HB), REFORM(data.TB), FONT_SIZE=12, FONT_STYLE=1, ASPECT_RATIO=1, $
  LINESTYLE="none", SYMBOL="o", XTITLE='Herb Biomass', YTITLE='Tree Leaf Biomass');,  XRANGE=xrange, YRANGE=yrange)
gh3.SYM_SIZE = 0.75
gh3.SYM_FILLED=1
END