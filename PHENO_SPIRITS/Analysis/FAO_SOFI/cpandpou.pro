PRO CPandPOU
fn = 'X:\Active Projects\FAO SOFI\Andrea\asap4IDL.csv'
fnout = 'X:\Active Projects\FAO SOFI\Andrea\asapListFromIDL.csv'
OPENW, lun, fnout, /GET_LUN
PRINTF, lun, 'Country,MaxWarn_avgCP0,MaxWarn_avgCP1,MaxWarn_avgCP1-MaxWarn_avgCP0'
x = READ_CSV(fn, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN);RECORD_START = 3)
;help, t
tags = TAG_NAMES(x)
t = rename_tags(x, tags, hdr)

;help, t, /STRUCTURES    

INDICES = MULTISORT(t.country, t.year)     
FOR i = 0, N_TAGS(t)-1 DO t.(i) =t.(i)[INDICES] 
;ind = where(t.country EQ 'Bhutan')
;PRINT, t.maxwarning[ind]
uniqCountryList = t.country[UNIQ(t.country)]
deltaslope2BA = !NULL
asapw = !NULL
FOR r = 0, N_ELEMENTS(uniqCountryList)-1 DO BEGIN
  ;locate the country records
  lines = WHERE(t.country EQ uniqCountryList[r]) 
  ;get name of country
  name = t.country[lines[0]]
  ;get the cp and warn values
  cp = t.CP[lines]
  mw = t.maxwarning[lines]
  indFin = WHERE(FINITE(mw), countFin)
  ;some new compuatations
  indFinPou = WHERE(FINITE(t.pou[lines]), countFinPou)
  indFinAsapW = WHERE(FINITE(t.WARNINGS_CROP[lines]), countFinAsapW)
  IF ((countFinPou EQ 13) AND (countFinAsapW EQ 14)) THEN BEGIN
    FOR j = 2, 10 DO BEGIN
      ;store asap warning 
      asapw =  [asapw,t.WARNINGS_CROP[lines[indFinAsapW[j]]]]
      ;store POU delta in mean slope 2 after minus slope 2 before
      ;1compute the two deltas before
      deltabef =  MEAN([t.pou[lines[indFinAsapW[j-1]]] - t.pou[lines[indFinAsapW[j-2]]], t.pou[lines[indFinAsapW[j]]] - t.pou[lines[indFinAsapW[j-1]]]])
      deltaaft =  MEAN([t.pou[lines[indFinAsapW[j+2]]] - t.pou[lines[indFinAsapW[j+1]]], t.pou[lines[indFinAsapW[j+1]]] - t.pou[lines[indFinAsapW[j]]]])
      deltaslope2BA=[deltaslope2BA, deltaaft-deltabef]
    ENDFOR
  ENDIF ELSE BEGIN
    PRINT, name + '       not analysed by delta
  ENDELSE
  ;see where cp is 1 and where it is 0
  indCP1 = WHERE(cp EQ 1, countCP1)
  indCP0 = WHERE(cp EQ 0, countCP0)
  IF ((countCP1 GT 0) OR (countFin EQ 0)) THEN BEGIN
    avgCP0 = MEAN(mw[indCP0])
    avgCP1 = MEAN(mw[indCP1])
    PRINT, name +','+STRTRIM(avgCP0,2) +','+STRTRIM(avgCP1,2) +','+STRTRIM(avgCP1-avgCP0,2)
    PRINTF, lun, name +','+STRTRIM(avgCP0,2) +','+STRTRIM(avgCP1,2) +','+STRTRIM(avgCP1-avgCP0,2) 
  ENDIF ELSE BEGIN
    PRINT, name +','+'no cp1 or not asap'
     PRINTF, lun, name +','+'no cp1 or not asap'
  ENDELSE
  PRINT, ''
ENDFOR
FREE_LUN, lun
h = PLOT(asapw, deltaslope2BA, LINESTYLE='', SYMBOL='+', yrange=[-0.02,0.02])
h1 = PLOT([0,100],[0,0], LINESTYLE='-', OVERPLOT = 1, COLOR='red')
PRINT, 'FINITO'
END