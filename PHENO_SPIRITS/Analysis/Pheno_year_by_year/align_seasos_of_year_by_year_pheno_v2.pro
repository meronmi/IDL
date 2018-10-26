PRO align_seasos_of_year_by_year_pheno_v2
debug = 0
skip_lines = 2400;4000 2400 takes mongolia
sl = [5271,2400]
maxShift = 6    ;after this an anamoly is unreliable and removed

;determine ngspy and time breakpoints between season using avg pheno (37<=m<=72 by definition)

;In pheno year by year s1 and s2 have little credibility as a pure monomodal season with m ranging in the period 33-3 (for example) may end up being season 1 with
;sos 25 (m=1) and sos 62 (m=71).
;
;so get avg pheno and breapoints referring always to the central year (as m is always central in the avg, by def).
;to define breakpoints I just need kk0, s1 and 2 e1 and 2.
pheno_dir_LTA = 'F:\AVG_Pheno_ASAP2-0'
ngspy_lta = ReadEnviWithHdr(pheno_dir_LTA + '\' + 'phenoKK0.img')
ind1 = WHERE((ngspy_lta GE 100) AND (ngspy_lta LT 200), count)
ind2 = WHERE((ngspy_lta GE 200) AND (ngspy_lta LT 250), count)
ngspy_LTA = ngspy_LTA * 0
ngspy_LTA[ind1] = 1
ngspy_LTA[ind2] = 2 
ind1 = 0
ind2 = 0
s1_lta = ReadEnviWithHdr(pheno_dir_LTA + '\' + 'phenos1.img')
s2_lta = ReadEnviWithHdr(pheno_dir_LTA + '\' + 'phenos2.img')
e1_lta = ReadEnviWithHdr(pheno_dir_LTA + '\' + 'phenoe1.img')
e2_lta = ReadEnviWithHdr(pheno_dir_LTA + '\' + 'phenoe2.img')
;so, with avg pheno I have two case ngspy = 1 and ngspy =2

;this is used to scale y
ym_gain_offset = [0.0048, -0.2]

;a bit of file naming for the pheo year by year 
varGenericError4ym = 254b    ;ym uses the full range 1-250, I cannot add flags
;instead all pheno data are used up to 108, so I can use values above
valNoSeasonalityLTA = 200b   ;no seasonality in lta
valNoSeasonalityYbY = 201b   ;seasonality in lta but not found in year by year pheno
valOneSeasonLTA = 202b       ;LTA has only one season for this pixel
valUnreliableAligment = 203b 
valEmptyLineDebug = 210b
valEmptyLine = 250b         ;value used to init anomalies
valEmptyProgress = -999
;F:\Pheno_year_by_year\Pheno
dir = 'F:\Pheno_year_by_year\Pheno\'
fns = CREATE_STRUCT('k',dir + 'BBB_' +'kk0', $
                    's',dir + 'BBB_' +'s', $
                    'm',dir + 'BBB_' +'m', $
                    'e',dir + 'BBB_' +'e', $
                    'l',dir + 'BBB_' +'l', $
                    'y',dir + 'BBB_' +'ym')
ns = LONG(read_info('samples', dir + '\' + FILE_BASENAME(fns.k, '.bil')+'.hdr'))
nl = LONG(read_info('lines', dir + '\' + FILE_BASENAME(fns.k, '.bil')+'.hdr'))
nb = LONG(read_info('bands', dir + '\' + FILE_BASENAME(fns.k, '.bil')+'.hdr'))
mapinfo = read_info('map info',  dir + '\' + FILE_BASENAME(fns.k, '.bil')+'.hdr')
dt = 1

OPENR, lunk, fns.k+'.bil' , /GET_LUN
OPENR, luns1, fns.s+'1.bil' , /GET_LUN
OPENR, luns2, fns.s+'2.bil' , /GET_LUN
OPENR, lunm1, fns.m+'1.bil' , /GET_LUN
OPENR, lunm2, fns.m+'2.bil' , /GET_LUN
OPENR, lune1, fns.e+'1.bil' , /GET_LUN
OPENR, lune2, fns.e+'2.bil' , /GET_LUN
OPENR, lunl1, fns.l+'1.bil' , /GET_LUN
OPENR, lunl2, fns.l+'2.bil' , /GET_LUN
OPENR, luny1, fns.y+'1.bil' , /GET_LUN
OPENR, luny2, fns.y+'2.bil' , /GET_LUN

;in writing
;anomalies 
anomStr = '_adj_anomX100_'
luns1w = 50 & OPENW, luns1w, fns.s+anomStr+'1.bil' 
luns2w = 51 & OPENW, luns2w, fns.s+anomStr+'2.bil' 
lunm1w = 52 & OPENW, lunm1w, fns.m+anomStr+'1.bil' 
lunm2w = 53 & OPENW, lunm2w, fns.m+anomStr+'2.bil' 
lune1w = 54 & OPENW, lune1w, fns.e+anomStr+'1.bil' 
lune2w = 55 & OPENW, lune2w, fns.e+anomStr+'2.bil' 
lunl1w = 56 & OPENW, lunl1w, fns.l+anomStr+'1.bil' 
lunl2w = 57 & OPENW, lunl2w, fns.l+anomStr+'2.bil' 
luny1w = 58 & OPENW, luny1w, fns.y+anomStr+'1.bil' 
luny2w = 59 & OPENW, luny2w, fns.y+anomStr+'2.bil'

;means
avgStr = '_adj_avgx100_'
luns1mw = 60 & OPENW, luns1mw, fns.s+avgStr+'1.bil'
luns2mw = 61 & OPENW, luns2mw, fns.s+avgStr+'2.bil'
lunm1mw = 62 & OPENW, lunm1mw, fns.m+avgStr+'1.bil'
lunm2mw = 63 & OPENW, lunm2mw, fns.m+avgStr+'2.bil'
lune1mw = 64 & OPENW, lune1mw, fns.e+avgStr+'1.bil'
lune2mw = 65 & OPENW, lune2mw, fns.e+avgStr+'2.bil'
lunl1mw = 66 & OPENW, lunl1mw, fns.l+avgStr+'1.bil'
lunl2mw = 67 & OPENW, lunl2mw, fns.l+avgStr+'2.bil'
luny1mw = 68 & OPENW, luny1mw, fns.y+avgStr+'1.bil'
luny2mw = 69 & OPENW, luny2mw, fns.y+avgStr+'2.bil'
;time (progressive deks from dek 1 of year-1) at which the anomaly is occurring
lunPD1w = 80 & OPENW, lunPD1w, dir +'BBB_adj_anom_prog_dek_from_first_year_minus1_'+'1.bil'
lunPD2w = 81 & OPENW, lunPD2w, dir +'BBB_adj_anom_prog_dek_from_first_year_minus1_'+'2.bil'
lunUnrel1w = 82 & OPENW, lunUnrel1w, dir +'BBB_Unreliable_map_1.bil'
lunUnrel2w = 83 & OPENW, lunUnrel2w, dir +'BBB_Unreliable_map_2.bil'


;assoc lines l..
lk = ASSOC(lunk, MAKE_ARRAY(ns, nb, TYPE = dt))
ls1 = ASSOC(luns1, MAKE_ARRAY(ns, nb, TYPE = dt))
ls2 = ASSOC(luns2, MAKE_ARRAY(ns, nb, TYPE = dt))
lm1 = ASSOC(lunm1, MAKE_ARRAY(ns, nb, TYPE = dt))
lm2 = ASSOC(lunm2, MAKE_ARRAY(ns, nb, TYPE = dt))
le1 = ASSOC(lune1, MAKE_ARRAY(ns, nb, TYPE = dt))
le2 = ASSOC(lune2, MAKE_ARRAY(ns, nb, TYPE = dt))
ll1 = ASSOC(lunl1, MAKE_ARRAY(ns, nb, TYPE = dt))
ll2 = ASSOC(lunl2, MAKE_ARRAY(ns, nb, TYPE = dt))
ly1 = ASSOC(luny1, MAKE_ARRAY(ns, nb, TYPE = dt))
ly2 = ASSOC(luny2, MAKE_ARRAY(ns, nb, TYPE = dt))


cUnreliable1 = 0L     ;count unreliable alignment season 1
cUnreliable2 = 0L     ;count unreliable alignment season 2
;now start the loop
FOR j = 0, nl-1 DO BEGIN
  IF (debug EQ 1) THEN j = LONG(sl[1])
  IF ((j MOD 1000.0) EQ 0) THEN BEGIN
    PRINT, 'line ' + STRTRIM(j,2) + ' ', systime()
  ENDIF
  IF (skip_lines GT 0) THEN BEGIN  
    empty_lineUnrel = MAKE_ARRAY(ns, TYPE = 1) + valEmptyLineDebug
    empty_lineProgress = MAKE_ARRAY(ns, nb, TYPE = 2) + valEmptyProgress
    empty_lineAnom = MAKE_ARRAY(ns, nb, TYPE = 2) + FIX(valEmptyLineDebug*100)
    empty_lineAvg = MAKE_ARRAY(ns, TYPE = 2) + FIX(valEmptyLineDebug*100)
    
    ;move skip_lines ahed quicly
    WHILE (j LT skip_lines) DO BEGIN  
      
      WRITEU, luns1w, empty_lineAnom
      WRITEU, luns2w, empty_lineAnom
      WRITEU, lunm1w, empty_lineAnom
      WRITEU, lunm2w, empty_lineAnom
      WRITEU, lune1w, empty_lineAnom
      WRITEU, lune2w, empty_lineAnom
      WRITEU, lunl1w, empty_lineAnom
      WRITEU, lunl2w, empty_lineAnom
      WRITEU, luny1w, empty_lineAnom
      WRITEU, luny2w, empty_lineAnom
      
      WRITEU, lunPD1w, empty_lineProgress
      WRITEU, lunPD2w, empty_lineProgress
      
      WRITEU, luns1mw, empty_lineAvg
      WRITEU, luns2mw, empty_lineAvg
      WRITEU, lunm1mw, empty_lineAvg
      WRITEU, lunm2mw, empty_lineAvg
      WRITEU, lune1mw, empty_lineAvg
      WRITEU, lune2mw, empty_lineAvg
      WRITEU, lunl1mw, empty_lineAvg
      WRITEU, lunl2mw, empty_lineAvg
      WRITEU, luny1mw, empty_lineAvg
      WRITEU, luny2mw, empty_lineAvg

      WRITEU, lunUnrel1w, empty_lineUnrel
      WRITEU, lunUnrel2w, empty_lineUnrel   
      IF ((j MOD 1000.0) EQ 0) THEN BEGIN
        PRINT, 'line ' + STRTRIM(j,2) + ' ', systime()
      ENDIF 
      j = j + 1L
    ENDWHILE
  ENDIF

  ;read the pheno year by year
  ngspy = lk[j]
  ind1 = WHERE((ngspy GE 100) AND (ngspy LT 200), count)
  ind2 = WHERE((ngspy GE 200) AND (ngspy LT 250), count)
  ngspy = ngspy * 0
  ngspy[ind1] = 1
  ngspy[ind2] = 2 
  s1=FLOAT(ls1[j]) & s2=FLOAT(ls2[j])
  m1=FLOAT(lm1[j]) & m2=FLOAT(lm2[j])
  e1=FLOAT(le1[j]) & e2=FLOAT(le2[j])
  l1=FLOAT(ll1[j]) & l2=FLOAT(ll2[j])
  y1=FLOAT(ly1[j]) & y2=FLOAT(ly2[j])
  ;variable for output
  s1o=MAKE_ARRAY(ns, nb) +  valEmptyLine & s2o=MAKE_ARRAY(ns, nb) +  valEmptyLine 
  m1o=MAKE_ARRAY(ns, nb) +  valEmptyLine & m2o=MAKE_ARRAY(ns, nb) +  valEmptyLine
  e1o=MAKE_ARRAY(ns, nb) +  valEmptyLine & e2o=MAKE_ARRAY(ns, nb) +  valEmptyLine
  l1o=MAKE_ARRAY(ns, nb) +  valEmptyLine & l2o=MAKE_ARRAY(ns, nb) +  valEmptyLine
  y1o=MAKE_ARRAY(ns, nb) +  valEmptyLine & y2o=MAKE_ARRAY(ns, nb) +  valEmptyLine  
  ;averages to be saved
  l_s_avg =  MAKE_ARRAY(ns, 2, TYPE = 4) + valNoSeasonalityLTA
  l_m_avg =  MAKE_ARRAY(ns, 2, TYPE = 4) + valNoSeasonalityLTA
  l_e_avg =  MAKE_ARRAY(ns, 2, TYPE = 4) + valNoSeasonalityLTA
  l_l_avg =  MAKE_ARRAY(ns, 2, TYPE = 4) + valNoSeasonalityLTA
  l_y_avg =  MAKE_ARRAY(ns, 2, TYPE = 4) + valNoSeasonalityLTA
  
  ;progressive dek of anomaly to be saved
  pd1 = MAKE_ARRAY(ns, nb, TYPE = 2) + valEmptyProgress
  pd2 = MAKE_ARRAY(ns, nb, TYPE = 2) + valEmptyProgress
  
  unrel1 = MAKE_ARRAY(ns, TYPE = 1)
  unrel2 = MAKE_ARRAY(ns, TYPE = 1)
  
  FOR i = 0, ns-1 DO BEGIN
    keepAnalyzingSeas1 = 1
    keepAnalyzingSeas2 = 1
    flagToUse = 0  
    IF (debug EQ 1) THEN BEGIN
      i = LONG(sl[0])
      debug=0
    ENDIF
    
    IF (ngspy_LTA[i,j] EQ 0) THEN BEGIN
      keepAnalyzingSeas1 = 0
      keepAnalyzingSeas2 = 0
      flagToUse = valNoSeasonalityLTA
    ENDIF
    
    IF (keepAnalyzingSeas1 EQ 1) THEN BEGIN
      ;case ngspy_lta = 1  or 2 (ngspy_LTA[i,j] EQ 2)
      ;define brakpoint for the seasons (start and end ober the 1-108 range from LTA)
      bp_seas1 = [s1_lta[i,j], e1_lta[i,j]]
      IF (ngspy_LTA[i,j] EQ 2) THEN bp_seas2 = [s2_lta[i,j], e2_lta[i,j]]
      ;take all events (s1 and s2) in temporal order and classify them as belongin to one or the other. If not exclude them.
      mp = [m1[i,*], m2[i,*]] ;2 coulmn (seas1 and seas2) x nb rows
      indValid = WHERE(mp LE 108, countValid)
      IF (countValid EQ 0) THEN BEGIN
        ;althought lta pheno indicate one season, none is found year by year
        keepAnalyzingSeas1 = 0
        keepAnalyzingSeas2 = 0
        flagToUse = valNoSeasonalityYbY
      ENDIF
    ENDIF
    

    
    IF (keepAnalyzingSeas1 EQ 1) THEN BEGIN
      ;I have values, pontially distrubuted over the two seasons
      ;Express the time of m as number of deks for the year before year 0 in the time series (so in the first year, the first dek will be 37
      mpProgressiveDeks = [mp[0,*]+ INDGEN(N_ELEMENTS(mp[0,*]))*36, mp[1,*]+INDGEN(N_ELEMENTS(mp[1,*]))*36]
      ;order them
      indValidOrdered = indValid[SORT(mpProgressiveDeks[indValid])]
      ;now I have the ones to keep and their cronological order
      sp = [s1[i,*], s2[i,*]]
      ep = [e1[i,*], e2[i,*]]
      lp = [l1[i,*], l2[i,*]]
      yp = ym_gain_offset[0]*[y1[i,*], y2[i,*]]+ym_gain_offset[1]   ;scale y, ym_gain_offset = [0.0048, -0.2]
      bp_seas1_36 = SPIRITS_from_108_to_36_ARRAY(bp_seas1)
      IF (ngspy_LTA[i,j] EQ 2) THEN bp_seas2_36 = SPIRITS_from_108_to_36_ARRAY(bp_seas2)
      ;now check which belongs to the recognised seasons in LTA, keep them and discard the others
      mp_36 = SPIRITS_from_108_to_36_ARRAY(mp)
      ;to find if they are in the breaks I have to separate the two cases I have on breaks 1-36, eos after sos VS sos after eos
      ;for season 1
      IF (bp_seas1_36[1] GT bp_seas1_36[0]) THEN BEGIN
        ;standard case, eos comes after sos
        indOK1 = WHERE((mp_36[indValid] GE bp_seas1_36[0]) AND ((mp_36[indValid] LE bp_seas1_36[1])), countOK1)
      ENDIF ELSE BEGIN
        ;eos combe before sos
        indOK1 = WHERE(((mp_36[indValid] GE bp_seas1_36[0]) AND (mp_36[indValid] LE 36)) OR $
                       ((mp_36[indValid] GE 1) AND (mp_36[indValid] LE bp_seas1_36[1])), countOK1)
      ENDELSE
      IF (countOK1 EQ 0) THEN BEGIN
        keepAnalyzingSeas1 = 0    ;season 1 is unreliable
        flagToUse = valNoSeasonalityYbY
        cUnreliable1 = cUnreliable1 + 1
      ENDIF
    ENDIF
    IF (keepAnalyzingSeas1 EQ 1) THEN BEGIN
      indOK1 = indValid[indOK1]
      indOKordered1 = indOK1[SORT(mpProgressiveDeks[indOK1])] 
      ;now I have the ones to keep and their cronological order     ;however if lta sees 1 season, there might be two and evrything is mised up, including the mean
      ;here we aim at focussing on the more frequent one and discard the other so first find the mean of the more frequent one
      ;idea: make a moving avg on the circular 260 deg doain, with a moving window of 7 (three months), find the "most represented 3-month period and make the avg of it",
      ;esclude those that are more that 6 deks away
      IF (ngspy_LTA[i,j] EQ 1) THEN BEGIN
        ;get the 1-36 index 
        posMax1_36 = max_from_circular_moving_average(mp[indOKordered1], 5)
        ;now I have find which of indOKordered1 are in the range posMax1_36 - 4 to posMax1_36 + 4
        range4mean = [posMax1_36-4, posMax1_36+4]
        ind = WHERE(range4mean LT 1, count)
        IF (count GT 0) THEN range4mean[ind] = 36 + range4mean[ind] 
        ind = WHERE(range4mean GT 36, count)
        IF (count GT 0) THEN range4mean[ind] = range4mean[ind] - 36 
        IF ((range4mean[1]-range4mean[0]) GE 0) THEN BEGIN
          ind = WHERE((mp_36[indOKordered1] GE range4mean[0]) AND (mp_36[indOKordered1] LE range4mean[1]), count)
        ENDIF ELSE BEGIN
          ind = WHERE(((mp_36[indOKordered1] GE range4mean[0]) AND (mp_36[indOKordered1] LE 36)) OR $
            ((mp_36[indOKordered1] GE 1) AND (mp_36[indOKordered1] LE range4mean[1])), count)
        ENDELSE
        ind4avg = indOKordered1[ind]
        m_avg = mean_vec_1_108(mp[ind4avg])
        ;now compute the anomaly of all data
        tmp = anom_between_vec_angles_1_108(mp[indOKordered1], m_avg)
        ind2rem = WHERE(ABS(tmp) GT maxShift, count2rem, COMPLEMENT=ind2keep)
        indOKordered1 = indOKordered1[ind2keep]
      ENDIF
      ;check that so far, those identified as first season are no more than nb
      IF (N_ELEMENTS(indOKordered1) GT nb) THEN BEGIN
        keepAnalyzingSeas1 = 0    ;season 1 is unreliable
        keepAnalyzingSeas2 = 0    ;season two does not exists in LTA
        flagToUse = valUnreliableAligment
        cUnreliable1 = cUnreliable1 + 1
        unrel1[i] = 1
      ENDIF
    ENDIF
    
     
    IF (keepAnalyzingSeas1 EQ 1) THEN BEGIN      
      ;I must save the results in a non ambiguous way, as deviation from the mean that it is save as well,  ;start with mean
      ;s
      l_s_avg[i,0] =  mean_vec_1_108(sp[indOKordered1])
      l_m_avg[i,0] =  mean_vec_1_108(mp[indOKordered1])
      l_e_avg[i,0] =  mean_vec_1_108(ep[indOKordered1])
      l_l_avg[i,0] =  MEAN(lp[indOKordered1])
      l_y_avg[i,0] =  MEAN(yp[indOKordered1])
      ;I am done with averages, now I have to expreess values as anomalies
      ;mpProgressiveDeks is the number of deks of each m event from the year previous to the start
      ;i have checked above if it was LTA monomodal, here cannot be gt nb
      ;still can be gt nb if bimodal          
      ind2write1 = INDGEN(N_ELEMENTS(indOKordered1))
      m1o[i,ind2write1] = anom_between_vec_angles_1_108(mp[indOKordered1], l_m_avg[i,0])
      ;check that I don't have deltas greater than maxShift deks (3 moths) , perhaps remove them as they would be unreliable (it happens when lta says 1 big season and then I find 2 season year by year)
      ind2rem = WHERE(ABS(m1o[i,ind2write1]) GT maxShift, count2rem, COMPLEMENT=ind2keep)
      IF (count2rem GT 0) THEN BEGIN
        ind2write1 = ind2write1[ind2keep]
        indOKordered1 = indOKordered1[ind2keep]
        ;means need to be recomputed
        l_s_avg[i,0] =  mean_vec_1_108(sp[indOKordered1])
        l_m_avg[i,0] =  mean_vec_1_108(mp[indOKordered1])
        l_e_avg[i,0] =  mean_vec_1_108(ep[indOKordered1])
        l_l_avg[i,0] =  MEAN(lp[indOKordered1])
        l_y_avg[i,0] =  MEAN(yp[indOKordered1])
        m1o[i,*] = valEmptyLine
        m1o[i,ind2write1] = anom_between_vec_angles_1_108(mp[indOKordered1], l_m_avg[i,0])
      ENDIF
;      IF (N_ELEMENTS(indOKordered1) GT nb) THEN BEGIN
;        keepAnalyzingSeas1 = 0    ;season 1 is not ok but a season 2, if existing may be ok
;        flagToUse = valUnreliableAligment
;        cUnreliable1 = cUnreliable1 + 1
;        unrel1[i] = 1
;      ENDIF
    ENDIF  
      
    IF (keepAnalyzingSeas1 EQ 1) THEN BEGIN   
      s1o[i,ind2write1] = anom_between_vec_angles_1_108(sp[indOKordered1], l_s_avg[i,0])
      pd1[i,ind2write1] = mpProgressiveDeks[indOKordered1]
      e1o[i,ind2write1] = anom_between_vec_angles_1_108(ep[indOKordered1], l_e_avg[i,0])
      l1o[i,ind2write1] = lp[indOKordered1] - l_l_avg[i,0]
      y1o[i,ind2write1] = yp[indOKordered1] - l_y_avg[i,0]
    ENDIF
    ;for season 2
    IF ((keepAnalyzingSeas2 EQ 1) AND (ngspy_LTA[i,j] EQ 2)) THEN BEGIN  ;go on if it was noy omitted
      IF (bp_seas2_36[1] GT bp_seas2_36[0]) THEN BEGIN
        ;standard case, eos comes after sos
        indOK2 = WHERE((mp_36[indValid] GE bp_seas2_36[0]) AND ((mp_36[indValid] LE bp_seas2_36[1])), countOK2)
      ENDIF ELSE BEGIN
        ;eos combe before sos
        indOK2 = WHERE(((mp_36[indValid] GE bp_seas2_36[0]) AND (mp_36[indValid] LE 36)) OR $
          ((mp_36[indValid] GE 1) AND (mp_36[indValid] LE bp_seas2_36[1])), countOK2)
      ENDELSE
      IF (countOK2 EQ 0) THEN BEGIN
        keepAnalyzingSeas2 = 0    ;season 1 is unreliable
        flagToUse = valNoSeasonalityYbY
        cUnreliable2 = cUnreliable2 + 1
      ENDIF
      indOK2 = indValid[indOK2]
      indOKordered2 = indOK2[SORT(mpProgressiveDeks[indOK2])]
      ;manage the situation that I have more than nb seasons
      IF (N_ELEMENTS(indOKordered2) GT nb) THEN BEGIN
        keepAnalyzingSeas2 = 0    ;season 1 is not ok but a season 2, if existing may be ok
        flagToUse = valUnreliableAligment
        cUnreliable12 = cUnreliable2 + 1
        unrel2[i] = 1
      ENDIF
    ENDIF
    IF ((keepAnalyzingSeas2 EQ 1) AND (ngspy_LTA[i,j] EQ 2)) THEN BEGIN 
      l_s_avg[i,1] =  mean_vec_1_108(sp[indOKordered2])
      l_m_avg[i,1] =  mean_vec_1_108(mp[indOKordered2])
      l_e_avg[i,1] =  mean_vec_1_108(ep[indOKordered2])
      l_l_avg[i,1] =  MEAN(lp[indOKordered2])
      l_y_avg[i,1] =  MEAN(yp[indOKordered2])
      ;s2
      ind2write2 = INDGEN(N_ELEMENTS(indOKordered2))
      m2o[i,ind2write2] = anom_between_vec_angles_1_108(mp[indOKordered2], l_m_avg[i,1])
      ind2rem = WHERE(ABS(m2o[i,ind2write2]) GT maxShift, count2rem, COMPLEMENT=ind2keep)
      IF (count2rem GT 0) THEN BEGIN
        ind2write2 = ind2write2[ind2keep]
        indOKordered2 = indOKordered2[ind2keep]
        ;means recomputed
        l_s_avg[i,1] =  mean_vec_1_108(sp[indOKordered2])
        l_m_avg[i,1] =  mean_vec_1_108(mp[indOKordered2])
        l_e_avg[i,1] =  mean_vec_1_108(ep[indOKordered2])
        l_l_avg[i,1] =  MEAN(lp[indOKordered2])
        l_y_avg[i,1] =  MEAN(yp[indOKordered2])
        m2o[i,ind2write2] = anom_between_vec_angles_1_108(mp[indOKordered2], l_m_avg[i,1])
      ENDIF
    ENDIF   
    
    IF ((keepAnalyzingSeas2 EQ 1) AND (ngspy_LTA[i,j] EQ 2)) THEN BEGIN  ;go on if it was noy omitted
      s2o[i,ind2write2] = anom_between_vec_angles_1_108(sp[indOKordered2], l_s_avg[i,1])
      pd2[i,ind2write2] = mpProgressiveDeks[indOKordered2]
      e2o[i,ind2write2] = anom_between_vec_angles_1_108(ep[indOKordered2], l_e_avg[i,1])
      l2o[i,ind2write2] = lp[indOKordered2] - l_l_avg[i,1]
      y2o[i,ind2write2] = yp[indOKordered2] - l_y_avg[i,1]
    ENDIF
 
    IF (keepAnalyzingSeas1  EQ 0) THEN BEGIN
      ;for some reason it was not analyzed
      s1o[i,*] = flagToUse & m1o[i,*] = flagToUse & e1o[i,*] = flagToUse & l1o[i,*] = flagToUse & y1o[i,*] = varGenericError4ym
      l_s_avg[i,0] = flagToUse
      l_m_avg[i,0] = flagToUse
      l_e_avg[i,0] = flagToUse
      l_l_avg[i,0] = flagToUse
      l_y_avg[i,0] = varGenericError4ym
    ENDIF
    IF (keepAnalyzingSeas2  EQ 0) THEN BEGIN
      ;for some reason it was not analyzed
      s2o[i,*] = flagToUse &  m2o[i,*] = flagToUse & e2o[i,*] = flagToUse & l2o[i,*] = flagToUse & y2o[i,*] = varGenericError4ym
      l_s_avg[i,1] = flagToUse
      l_m_avg[i,1] = flagToUse
      l_e_avg[i,1] = flagToUse
      l_l_avg[i,1] = flagToUse
      l_y_avg[i,1] = varGenericError4ym
    ENDIF    
    ;PRINT, i
  ENDFOR  ;i
  WRITEU, luns1w, FIX(100*s1o)
  WRITEU, luns2w, FIX(100*s2o)
  WRITEU, lunm1w, FIX(100*m1o)
  WRITEU, lunm2w, FIX(100*m2o)
  WRITEU, lune1w, FIX(100*e1o)
  WRITEU, lune2w, FIX(100*e2o)
  WRITEU, lunl1w, FIX(100*l1o)
  WRITEU, lunl2w, FIX(100*l2o)
  WRITEU, luny1w, FIX(100*y1o)
  WRITEU, luny2w, FIX(100*y2o)
  
  WRITEU, luns1mw, FIX(100*l_s_avg[*,0])
  WRITEU, luns2mw, FIX(100*l_s_avg[*,1])
  WRITEU, lunm1mw, FIX(100*l_m_avg[*,0])
  WRITEU, lunm2mw, FIX(100*l_m_avg[*,1])
  WRITEU, lune1mw, FIX(100*l_e_avg[*,0])
  WRITEU, lune2mw, FIX(100*l_e_avg[*,1])
  WRITEU, lunl1mw, FIX(100*l_l_avg[*,0])
  WRITEU, lunl2mw, FIX(100*l_l_avg[*,1])
  WRITEU, luny1mw, FIX(100*l_y_avg[*,0])
  WRITEU, luny2mw, FIX(100*l_y_avg[*,1])
  
  WRITEU, lunPD1w, pd1
  WRITEU, lunPD2w, pd2
  WRITEU, lunUnrel1w, unrel1
  WRITEU, lunUnrel2w, unrel2
ENDFOR ; j
PRINT, 'N unreliable s1 = ', cUnreliable1
PRINT, 'N unreliable s2 = ', cUnreliable2

;case ngspy_lta = 1
;take all events (s1 and s2) and classify them as beloning to s1 or not. If not exclude them.
FREE_LUN, lunk
FREE_LUN, luns1
FREE_LUN, luns2
FREE_LUN, lunm1
FREE_LUN, lunm2
FREE_LUN, lune1
FREE_LUN, lune2
FREE_LUN, lunl1
FREE_LUN, lunl2
FREE_LUN, luny1
FREE_LUN, luny2

FREE_LUN, luns1w
FREE_LUN, luns2w
FREE_LUN, lunm1w
FREE_LUN, lunm2w
FREE_LUN, lune1w
FREE_LUN, lune2w
FREE_LUN, lunl1w
FREE_LUN, lunl2w
FREE_LUN, luny1w
FREE_LUN, luny2w

FREE_LUN, luns1mw
FREE_LUN, luns2mw
FREE_LUN, lunm1mw
FREE_LUN, lunm2mw
FREE_LUN, lune1mw
FREE_LUN, lune2mw
FREE_LUN, lunl1mw
FREE_LUN, lunl2mw
FREE_LUN, luny1mw
FREE_LUN, luny2mw

FREE_LUN, lunPD1w
FREE_LUN, lunPD2w
FREE_LUN, lunUnrel1w
FREE_LUN, lunUnrel2w

res = write_envi_hdr(fns.s+anomStr+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.s+anomStr+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.m+anomStr+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.m+anomStr+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.e+anomStr+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.e+anomStr+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.l+anomStr+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.l+anomStr+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.y+anomStr+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.y+anomStr+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)


res = write_envi_hdr(dir +'BBB_adj_anom_prog_dek_from_first_year_minus1_'+'1.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(dir +'BBB_adj_anom_prog_dek_from_first_year_minus1_'+'2.hdr', ns, nl, 2, NBANDS=nb, INTERLEAVE='bil', MAPINFO=mapinfo)

res = write_envi_hdr(dir +'BBB_Unreliable_map_'+'1.hdr', ns, nl, 1, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(dir +'BBB_Unreliable_map_'+'2.hdr', ns, nl, 1, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)

res = write_envi_hdr(fns.s+avgStr+'1.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.s+avgStr+'2.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.m+avgStr+'1.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.m+avgStr+'2.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.e+avgStr+'1.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.e+avgStr+'2.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.l+avgStr+'1.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.l+avgStr+'2.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.y+avgStr+'1.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)
res = write_envi_hdr(fns.y+avgStr+'2.hdr', ns, nl, 2, NBANDS=1, INTERLEAVE='bil', MAPINFO=mapinfo)


END