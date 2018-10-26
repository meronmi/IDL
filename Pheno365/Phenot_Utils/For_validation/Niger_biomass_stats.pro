PRO Niger_biomass_stats

fname = 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\NIGER_IDL_FORMAT_2014-03-05.csv'
tmp =  READ_CSV(fname, $
                HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
;load data into a structure with the following fields:
;ID, SiteID, Location, Longitude, Latitude, Biomass, Year, Month, Day 
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7','FIELD8','FIELD9'], $ ; if you have more than 9 fields, name as follows: FIELD01, FIELD02, ...
                        ['ID','SiteID','Location','Longitude','Latitude','Biomass','Year','Month','Day'])
FOR i=3,5 DO BEGIN
  ind = WHERE(data.(i) EQ -9999, count)
  IF (count GT 0) THEN data.(i)[ind] = !VALUES.F_NAN
ENDFOR
                     
;********************************************************************************
;overall boxplot
tmp = [TRANSPOSE(data.Biomass)]
res = CREATEBOXPLOTDATA(tmp)
gh0 = BOXPLOT(res, TITLE = 'Biomass', YTITLE='kg/ha')  ; Program caused arithmetic error: Floating overflow
;gh0.XTICKNAME = ['','','Niger','','']
gh0.XMINOR = 0
gh0.XTICKLEN = 0
idList = uniqlist(data.ID)
yearList = uniqlist(data.Year)

;********************************************************************************
;boxplot of biomass by site
tmp = FLTARR(N_ELEMENTS(idList),N_ELEMENTS(yearList))
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  ind = WHERE(data.ID EQ idList[i])
  tmp[i,*] = data.Biomass[ind]
ENDFOR

 
;check where I don't have at least 5 elements (cannot compute boxplot)
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
gh1 = BOXPLOT(res, TITLE = 'Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(idList2)])
gh1.XTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh1.XTICKFONT_SIZE = 10
gh1.XMINOR = 0
gh1.XTICKLEN = 0
gh1.FILL_COLOR = 'forest green'
gh1.THICK = 2


; horizontal diplay of boxplots
res = CREATEBOXPLOTDATA(tmp2)
gh1b = BOXPLOT(res, TITLE = 'Biomass' + tmpTitle, /HORIZONTAL, xTITLE='kg/ha', YRANGE =[-1, N_ELEMENTS(idList2)])
gh1b.YTICKNAME = [' ',STRTRIM([idList2],2),' ']
gh1b.YTICKFONT_SIZE = 10
gh1b.YMINOR = 0
gh1b.YTICKLEN = 0
gh1b.FILL_COLOR = 'forest green'
gh1b.THICK = 2

;********************************************************************************
;boxplot of biomass by year
tmp = FLTARR(N_ELEMENTS(yearList),N_ELEMENTS(idList))
FOR i = 0, N_ELEMENTS(yearList)-1 DO BEGIN
  ind = WHERE(data.Year EQ yearList[i])
  tmp[i,*] = data.Biomass[ind]
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
gh2 = BOXPLOT(res, TITLE = 'Biomass' + tmpTitle, YTITLE='kg/ha', XRANGE =[-1, N_ELEMENTS(yearList2)])
gh2.XTICKNAME = [' ',STRTRIM([yearList2],2),' ']
gh2.XTICKFONT_SIZE = 10
gh2.XMINOR = 0
gh2.XTICKLEN = 0
gh2.YSUBTICKLEN = 0.25
gh2.FILL_COLOR = 'forest green'
gh2.THICK = 2

; horizontal diplay of boxplots
res = CREATEBOXPLOTDATA(tmp2)
gh2b = BOXPLOT(res, TITLE = 'Biomass' + tmpTitle, /HORIZONTAL, XTITLE='kg/ha', YRANGE =[-1, N_ELEMENTS(yearList2)])
gh2b.YTICKNAME = [' ',STRTRIM([yearList2],2),' ']
gh2b.YTICKFONT_SIZE = 10
gh2b.YMINOR = 0
gh2b.YTICKLEN = 0
gh2b.YSUBTICKLEN = 0.25
gh2b.FILL_COLOR = 'forest green'
gh2b.THICK = 2


;********************************************************************************


END