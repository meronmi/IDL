PRO map_2biggest_anomalies_and_year
;get one anomaly type (can be gsl or sos) and per pixel and per season:
; - find the two largest absolute value, save the value and the year it happended 

target_anomaly = 'l';'s,l'
lookfor = 'largestNegative' ;'largestNegative, largestPositive, largestAbsolute'

fn_anomaly = 'F:\Pheno_year_by_year\Pheno\consolidated_results\'
fn_anomaly = fn_anomaly + 'BBB_' + target_anomaly + '_adj_anomX100_' ;examle of a file nameBBB_m_adj_anomX100_1.bil
fn_anomaly_suffix = '.bil'
fn_gsl = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_l_adj_avgx100_'
fn_gsl_suffix = '.bil'
fn_progDek = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_adj_anom_prog_dek_from_first_year_minus1_'
fn_progDek_suffix = '.bil'

validRange = [-36*100,36*100]
outPath = 'F:\Pheno_year_by_year\Pheno\consolidated_results\2LargestDev'
dirsep = '\'
exclude2003 = 1
expressPrctOfAvg = 1 ;deviations as % of average length of season 
minNumberOfdata = 4

IF (exclude2003 EQ 1) THEN  frstyear = 2004 ELSE frstyear = 2003
IF (N_ELEMENTS(validRange) NE 2) THEN STOP
IF (validRange[1] LE validRange[0]) THEN STOP

FOR seas = 1, 2 DO BEGIN
  ; check and open the bil files
  fnameY = fn_anomaly + STRTRIM(seas,2) + fn_anomaly_suffix
  IF (STRTRIM(read_info('interleave', remove_ext_from_fn(fnameY) + '.hdr'),2) NE 'bil') THEN STOP
  IF FILE_TEST(fnameY) EQ 0 THEN STOP
  dt = FIX(read_info('data type',  remove_ext_from_fn(fnameY) + '.hdr'))
  ns = LONG(read_info('samples',  remove_ext_from_fn(fnameY) + '.hdr'))
  nl = LONG(read_info('lines',  remove_ext_from_fn(fnameY) + '.hdr'))
  nb = FIX(read_info('bands',  remove_ext_from_fn(fnameY) + '.hdr'))
  ;open the pheno var
  OPENR, lunY, fnameY, /GET_LUN
  assoY = ASSOC(lunY, MAKE_ARRAY(ns,nb, TYPE=dt))
  ;open its associated time
  fnameTT = fn_progDek + STRTRIM(seas,2) + fn_progDek_suffix 
  OPENR, lunTT, fnameTT, /GET_LUN
  dtTT = FIX(read_info('data type',  remove_ext_from_fn(fnameTT) + '.hdr'))
  assoTT = ASSOC(lunTT, MAKE_ARRAY(ns,nb, TYPE=dtTT))
  ;open the avg length if needed
  IF (expressPrctOfAvg EQ 1) THEN BEGIN
    fnL = fn_gsl + STRTRIM(seas,2) + fn_gsl_suffix
    dtL = FIX(read_info('data type',  remove_ext_from_fn(fnL) + '.hdr'))
    OPENR, lunGSL, fnL, /GET_LUN
    gslBSQ = MAKE_ARRAY(ns,nl, TYPE=dtL)
    READU, lunGSL, gslBSQ
    FREE_LUN, lunGSL
    indNaN = WHERE(((gslBSQ LT validRange[0]) OR (gslBSQ GT validRange[1])), countNaN,  NCOMPLEMENT = countFIN)
    IF (countNaN GT 0) THEN BEGIN
      gslBSQ = FLOAT(gslBSQ)
      gslBSQ[indNaN] = !VALUES.F_NAN
    ENDIF
  ENDIF
  
  
  ; open outputs
  FILE_MKDIR, outPath
  strlookfor = lookfor
  IF (exclude2003 EQ 1) THEN strlookfor = lookfor + '_2003_excluded'
  IF (expressPrctOfAvg EQ 1) THEN strlookfor = strlookfor + '_PrctDev_'
  fnameDev1 = outPath+dirsep+target_anomaly+'_seas'+STRTRIM(seas,2)+    '_n1' + strlookfor + 'Dev.img'
  fnameDev2 = outPath+dirsep+target_anomaly+'_seas'+STRTRIM(seas,2)+    '_n2' + strlookfor + 'Dev.img'
  fnameYear1 = outPath+dirsep+target_anomaly+'_seas'+STRTRIM(seas,2)+   '_n1' + strlookfor + 'DevYear.img'
  fnameYear2 = outPath+dirsep+target_anomaly+'_seas'+STRTRIM(seas,2)+   '_n2' + strlookfor + 'DevYear.img'
  
  OPENW, lunDev1, fnameDev1, /GET_LUN
  OPENW, lunDev2, fnameDev2, /GET_LUN
  OPENW, lunYear1, fnameYear1, /GET_LUN
  OPENW, lunYear2, fnameYear2, /GET_LUN
   
  ; Loop on the image lines
  FOR line = 0, nl-1, 1L DO BEGIN
    IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
    yline=float(assoY[line])
    tline = float(assoTT[line])
    IF (exclude2003 EQ 1) THEN BEGIN
      yline = yline[*,1:-1]
      tline = tline[*,1:-1]
    ENDIF
  
    dev1line = FLTARR(ns) -9999
    dev2line = FLTARR(ns) -9999
    year1line = INTARR(ns) -9999
    year2line = INTARR(ns) -9999
    
    indNaN = WHERE(((yline LT validRange[0]) OR (yline GT validRange[1])), countNaN,  NCOMPLEMENT = countFIN)
    IF (countNaN GT 0) THEN yline[indNaN] = !VALUES.F_NAN
    ;exclude those that have all nan from further processing
    finitePerColumn = TOTAL(FINITE(yline), 2)
    indColumnWithFinite = WHERE(finitePerColumn GT minNumberOfdata, countColumnWithFinite)
    IF (countColumnWithFinite GT 0) THEN BEGIN  
      finyline = yline[indColumnWithFinite,*]
      fintline = tline[indColumnWithFinite,*]
      ;lookfor = 'largestNegative' ;'largestPositive, largestAbsolute'
      CASE lookfor OF
        'largestNegative': valMax = MIN(finyline/100.0, indMax, DIMENSION = 2, /NAN)     
        'largestPositive': valMax = MAX(finyline/100.0, indMax, DIMENSION = 2, /NAN)
        'largestAbsolute': valMax = MAX(finyline/100.0, indMax, DIMENSION = 2, /NAN, /ABSOLUTE)   
        ELSE: STOP
      ENDCASE      
;      posMax = ARRAY_INDICES(finyline, indMax) 
;      ;posMax1 is 2D, each line is a record indicating the position of the max (colum, line), 
;      ;;so the second element can be used to index TT
;      ; that has the progressive dek of the event from 2002 (1 is first dek of 2002)
;      yearMax = posMax[1,*] + frstyear
      ;get year from progressive dks from 2002
      yearMax = FLOOR((fintline[indMax]-1) / 36.0) + 2002
      IF ((MAX(yearMax) GT 2016) OR (MIN(yearMax) LT 2003)) THEN STOP
;      IF (line EQ 7753-1) THEN BEGIN
;        PRINT, 'debug'
;      ENDIF
      IF (expressPrctOfAvg EQ 1) THEN BEGIN
        valMax = valMax / (FLOAT(gslBSQ[indColumnWithFinite,line])/100.0) * 100
      ENDIF
      dev1line[indColumnWithFinite] = valMax
      year1line[indColumnWithFinite] = FIX(REFORM(yearMax))
      
      ;first was identified, remove it and find the second, if the second is the same of the previous means that the two are equal, set it as -6666
      finylineTmp = finyline
      finylineTmp[indMax] = !VALUES.F_NAN
         CASE lookfor OF
        'largestNegative': valMax2 = MIN(finylineTmp/100.0, indMax2, DIMENSION = 2, /NAN)     
        'largestPositive': valMax2 = MAX(finylineTmp/100.0, indMax2, DIMENSION = 2, /NAN)
        'largestAbsolute': valMax2 = MAX(finylineTmp/100.0, indMax2, DIMENSION = 2, /NAN, /ABSOLUTE)   
        ELSE: STOP
      ENDCASE
;      posMax2 = ARRAY_INDICES(finylineTmp, indMax2)   
;      ;posMax1 is 2D, each line is a record indicating the position of the max (colum, line), so the second element (column) is the year
;      yearMax2 = posMax2[1,*] + frstyear
      ;get year from progressive dks from 2002
      yearMax2 = FLOOR((fintline[indMax2]-1) / 36.0) + 2002
      IF ((MAX(yearMax2) GT 2016) OR (MIN(yearMax2) LT 2003)) THEN STOP
      IF (expressPrctOfAvg EQ 1) THEN BEGIN
        valMax2 = valMax2 / (FLOAT(gslBSQ[indColumnWithFinite,line])/100.0) * 100
      ENDIF
      dev2line[indColumnWithFinite] = valMax2
      year2line[indColumnWithFinite] =  FIX(REFORM(yearMax2))
      
      ;check the new max or min is not equal to the previous one
      indDuplic = WHERE(valMax-valMax2 EQ 0, countDuplic)
      IF (countDuplic GT 0) THEN BEGIN
        dev1line[indColumnWithFinite[indDuplic]] = -666
        dev2line[indColumnWithFinite[indDuplic]] = -666
        year1line[indColumnWithFinite[indDuplic]] = -666
        year2line[indColumnWithFinite[indDuplic]] = -666
      ENDIF
    ENDIF ;IF (countColumnWithFinite GT 0) THEN BEGIN
    WRITEU, lunDev1, dev1line
    WRITEU, lunDev2, dev2line
    WRITEU, lunYear1, year1line
    WRITEU, lunYear2, year2line
  ENDFOR
  FREE_LUN, lunDev1
  FREE_LUN, lunDev2
  FREE_LUN, lunYear1
  FREE_LUN, lunYear1
  FREE_LUN, lunY
  FREE_LUN, lunTT
  
  ;write hdrs
  
  tmp =[fnameDev1, fnameDev2, fnameYear1, fnameYear2]
  mapinfo = read_info('map info',  remove_ext_from_fn(fnameY) + '.hdr')
  FOR i = 0, 1 DO res = write_envi_hdr(remove_ext_from_fn(tmp[i])+'.hdr', ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
  FOR i = 2, 3 DO res = write_envi_hdr(remove_ext_from_fn(tmp[i])+'.hdr', ns, nl, 2, INTERLEAVE='bsq', MAPINFO=mapinfo)
ENDFOR ;FOR seas = 1, 2 DO BEGIN



END