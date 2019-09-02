FUNCTION locate_largest_season_in_jd_range_v5, xjd, y, wide_rangejd, narrow_rangejd, indexName
  ; Michele Meroni, June 2019
  ;
  
  ; xjd: is the time interval containing the season of interest. IT MUST contain the full year of interest 
  ;      + a bit of previous (season can start there) and IT MAY contain a bit of the the following year
  ; y: the index (admitted types: NDVI, VH/VV, RVI)
  ; narrow_rangejd: the preferred range, in julian days, of the year of interest where to look for the largest season maximum, if there is no, rangejd_mins will be used
  ; wide_rangejd:   the second option for the max search
  
  ;so the order of prefrence is
  ; 1 Max in narrow_rangejd 
  ; 2 Max in wide_rangejd

  
  ; indexname: a string indicating the index type (possible values: NDVIm, CR, RVI)
  ;
  ; NOTE that minima in rangejd_mins and maxima in rangejd_max will be preferred to those occurring outside (even if they are smaller minima)
  ; 
  ; Method:
  ; 1. compute the smoothed curve (trial and error process to detrmmine the smoothing parameters, that can be different between S1 and S2)
  ; 2. take care of edges
  ; 3. locate relative minima an maxima
  ; 4. for each maximum with existing minima on the two sides, compute the integral Int (the magnitue of a season depend on ampltitude and duration
  ; 5. retrive the two largest integrals: the one in the narrower rangejd_max (NInt) and the one in the lager rangejd_mins (LInt)
  ; 6. Locate the season according to this scheme:
  ;    - NInt refer to the same maximum of LInt, get it
  ;    - LInt > Mult*Nint, this season, despite being in the wide range, is much bigger, take the season associated with LInt
  ;    - LInt <= Mult*Nint, the season in the narrow range is not negiglibly small comapred to the one outside, take the season associate with NInt
  ;    Mult is the multiplier to be set (M = 2 as a start of the trial and error process  
  ; 
  ; Note: in v4 minima away from max are found looking at the minimum causing a significant hole, where ever it happens.
  ;       In v5 this minimum is accepted only if it falls in the wide range or if it outside, only there is no min inside. 
  ;       If not, the hole threshold is reduce in steps (holeMFIReductionStep) and a 
  ;       minimum is searched again. This is looped untile I find a minimum in widerange. 
  ; 
  ; RETURN the left and right subscript of minima and some other stats 


  Mult = 2                                ; integral multiplier, see above 
  
  holeMinFractionalImportance = 0.15      ; the minimum fractional importance of the hole made the minimum, used in findMinAwayFromMax_v3
  holeMFIReductionStep = 0.025            ; the quantity used to reduce holeMinFractionalImportance when the retrieved min falls outside the wide range
                                          ; AND (x 2) the minimum level of holeMinFractionalImportance that is used before giving up looking for a min (holeMFIReductionStep*2)
  
  
  thershold_amplitudeP2P5lim = 0.9        ; fraction of the value of the grow (decay) amplitude used to set the upper (lower) limit of the tanh flex point.
                                          ; i.e. the inflection ponint cannot be closer than this to the maximum
  thershold_amplitudeP2P5fg = 0.5         ; fraction of the value of the grow (decay) amplitude used to set the first guess of the tanh flex point.
                                          ; i.e. it is placed at the time of half-amplitude

  doplot = 0                              ;(debug) 1 for plotting results of the function

  ; the function smooth the y values to get estimates of the minima and maxima, check that at very least 10 obs are available
  IF (N_ELEMENTS(y) LE 10) THEN BEGIN
    season = CREATE_STRUCT('retr', 0, 'code', FIX(0),'msg', '', 'subMax', -99)
    season.retr = -1
    season.code = -100
    season.msg = 'locate_largest_season_in_jd_range: less than 10 data points'
    RETURN, season
  ENDIF

  ;linearly iterpolate y over a regular daily grid
  xjd_reg = FINDGEN(xjd[-1]-xjd[0]+1)+xjd[0]
  y_reg = INTERPOL(y, xjd, xjd_reg)

  CASE indexName OF
    'NDVIm':BEGIN
      maxNDVI = 1.0
      minNDVI = 0
      ;threshold to check that the min-max-min bump is not negiglibly small (range)
      thr = 0.1
      cloud = FLTARR(N_ELEMENTS(y_reg))*0.0
      ;smooth the y
      width_climatology= 75
      width_on_upper_env = 45
      sago_interpol3_for_s2, y_reg, cloud, width_climatology, width_on_upper_env, smNDVI=y_regs1, iMax=100, maxNDVI=maxNDVI, minNDVI=minNDVI
      ;smooth again to remove hi-frequency variations (with one fit only there are a still some up down that are no good for extrema)
      width_climatology= 60
      width_on_upper_env = 30
      sago_interpol3_for_s2, y_regs1, cloud, width_climatology, width_on_upper_env, smNDVI=y_regs2, iMax=100, maxNDVI=maxNDVI, minNDVI=minNDVI
    END
    'CR':BEGIN
      maxNDVI = 10.0d^(10.0/10.0) ;10 db
      minNDVI = 10.0d^(-100.0/10.0)  ;-100dB
      ;threshold to check that the min-max-min bump is not negiglibly small (range)
      thr = 0.075
      width = 45
      sago_interpol3_for_s1, y_reg, width, smNDVI=y_regs1, maxNDVI=maxNDVI, minNDVI=minNDVI ;no upper envelope
      width = 30
      sago_interpol3_for_s1, y_regs1, width, smNDVI=y_regs2, maxNDVI=maxNDVI, minNDVI=minNDVI
    END
    'RVI':BEGIN
      maxNDVI = 4.0  ; by def 
      minNDVI = 0.0  ;-100dB
      ;threshold to check that the min-max-min bump is not negiglibly small (range)
      thr = 0.1
      width = 45
      sago_interpol3_for_s1, y_reg, width, smNDVI=y_regs1, maxNDVI=maxNDVI, minNDVI=minNDVI
      width = 30
      sago_interpol3_for_s1, y_regs1, width, smNDVI=y_regs2, maxNDVI=maxNDVI, minNDVI=minNDVI
    END
    ELSE: STOP
  ENDCASE
    
  ;Here we should have  a resonably smoothed function. Store the smoothed values for the observation points
  ;Find the sub of xjd that are sharee by xjd_reg
  match, LONG(xjd), LONG(xjd_reg), sub_xjd, sub_xjd_reg
  season = CREATE_STRUCT('subLeft', 0, 'subRight', 0, 'subMax', -99, 'xjdLimP2', 0.0, 'xjdLimP5', 0.0, 'xjdFgP2', 0.0, 'xjdFgP5', 0.0, $
    'y_reg_smooth', y_regs2, 'y_obs_smooth', y_regs2[sub_xjd_reg], $
    'ySmoothAtSubLeft', 0.0, 'ySmoothAtSubRight', 0.0, 'ySmoothAtSubMax', 0.0, 'retr', 0, 'code', FIX(0),'msg', '')

  ;locate all max and min using extrema (extrema finds ANY relative max or mean)
  ;Before finding extrema, mirror edges so that monotonic increment will generate a min on the left side and a max on the right side
  ;conversely, a monotonic decrease will generate a max on the left and min on the right
  y_regs2_edged = [REVERSE(y_regs2[1:5]),y_regs2,REVERSE(y_regs2[-6:-2])]
  subRegExtrema = Extrema(y_regs2_edged, signature = sig, number = num)
  ;bring it back to non edged dimensions
  subRegExtrema = subRegExtrema - 5
  ind = WHERE((subRegExtrema GE 0) AND (subRegExtrema LE (N_ELEMENTS(y_regs2)-1)), count)
  IF (count GT 0) THEN BEGIN
    subRegExtrema = subRegExtrema[ind[SORT(ind)]]
    sig = sig[ind[SORT(ind)]]
  ENDIF
  

  
  ;New version based on integrals
  
  ;4. for each maximum with existing minima on the two sides, compute the integral Int (the magnitue of a season depend on ampltitude and duration
  ;subY_OfMax = subRegExtrema[WHERE(sig EQ 1)]
  subRegMaxima = subRegExtrema[WHERE(sig[1:-2] EQ 1, nCandidates) + 1]    ;all maxima, avoiding sub at edges, that would not have one of the two minima
  
  ;check that a max esistis in the wide range at least
  sub_subRegMaxima_inWideRange = WHERE((xjd_reg[subRegMaxima] GE wide_rangejd[0]) AND (xjd_reg[subRegMaxima] LE wide_rangejd[1]), countInWideRange)
  IF (countInWideRange EQ 0) THEN BEGIN
    PRINT, 'No extrema in the specified min period'
    season.retr = -1
    season.code = -101
    season.msg = 'locate_largest_season_in_jd_range: There is no max in the wide range'
    RETURN, season
  ENDIF
  
  IF (countInWideRange NE nCandidates) THEN BEGIN
    ;retain only those happening the the wide range
    subRegMaxima = subRegMaxima[sub_subRegMaxima_inWideRange]
  ENDIF
  
   
  integs = FLTARR(countInWideRange) 
  sub_subRegExtrema_LeftMins = INTARR(countInWideRange)
  sub_subRegExtrema_RightMins = INTARR(countInWideRange)
  
  
  FOR i = 0, countInWideRange-1 DO BEGIN
    minFoundInWideRange = 0
    newHoleMFI = holeMinFractionalImportance
    sub_subRegExtrema_tmp_list = !NULL ;store here the minima found reducing the holeMinFractionalImportance
    WHILE (minFoundInWideRange EQ 0) DO BEGIN
      sub_subRegExtrema_LeftMins[i] = findMinAwayFromMax_v3(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'left', holeMinFractionalImportance)
      ;condition to start looking for a different min: - a min was selected outside the wide range
      ;Note that holeMinFractionalImportance is progressively reduced until a min value. If this is reached and no min is found in the wide range, just take what was found outside
      IF (xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins[i]]] LT wide_rangejd[0]) THEN BEGIN
        newHoleMFI = newHoleMFI - holeMFIReductionStep
        ;the holeMinFractionalImportance was already reduced down to holeMFIReductionStep and no min in wide range was found, keep the one closer to the wide range
        IF (newHoleMFI LT holeMFIReductionStep*2.0) THEN BEGIN
          sub_subRegExtrema_LeftMins[i] = MAX(sub_subRegExtrema_tmp_list) 
          minFoundInWideRange = 1
        ENDIF
        sub_subRegExtrema_tmp = findMinAwayFromMax_v3(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'left', newHoleMFI)
        sub_subRegExtrema_tmp_list = [sub_subRegExtrema_tmp_list, sub_subRegExtrema_tmp]
        IF (xjd_reg[subRegExtrema[sub_subRegExtrema_tmp]] GE wide_rangejd[0]) THEN BEGIN
          minFoundInWideRange = 1
          sub_subRegExtrema_LeftMins[i] = sub_subRegExtrema_tmp
        ENDIF
      ENDIF ELSE BEGIN
        minFoundInWideRange = 1
      ENDELSE
    ENDWHILE
    
    minFoundInWideRange = 0
    newHoleMFI = holeMinFractionalImportance
    sub_subRegExtrema_tmp_list = !NULL ;store here the minima found reducing the holeMinFractionalImportance
    WHILE (minFoundInWideRange EQ 0) DO BEGIN
      sub_subRegExtrema_RightMins[i] = findMinAwayFromMax_v3(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'right', holeMinFractionalImportance)
      IF (xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins[i]]] GT wide_rangejd[1]) THEN BEGIN
        newHoleMFI = newHoleMFI - holeMFIReductionStep
        ;the holeMinFractionalImportance was already reduced down to holeMFIReductionStep and no min in wide range was found, keep the one outside
        IF (newHoleMFI LT holeMFIReductionStep) THEN BEGIN
          sub_subRegExtrema_RightMins[i] = MIN(sub_subRegExtrema_tmp_list) 
          minFoundInWideRange = 1
        ENDIF
        sub_subRegExtrema_tmp = findMinAwayFromMax_v3(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'right', newHoleMFI)
        sub_subRegExtrema_tmp_list = [sub_subRegExtrema_tmp_list, sub_subRegExtrema_tmp]
        IF (xjd_reg[subRegExtrema[sub_subRegExtrema_tmp]] LE wide_rangejd[1]) THEN BEGIN
          minFoundInWideRange = 1
          sub_subRegExtrema_RightMins[i] = sub_subRegExtrema_tmp
        ENDIF
      ENDIF ELSE BEGIN
        minFoundInWideRange = 1
      ENDELSE
    ENDWHILE
    ;now compute the integral of the smoothed curve down to the min val in the min-max-min range
    ys = y_regs2[subRegExtrema[sub_subRegExtrema_LeftMins[i]]:subRegExtrema[sub_subRegExtrema_RightMins[i]]]
    integs[i] = TOTAL(ys) - MIN(ys)   
  ENDFOR
  IF (doplot EQ 1) THEN BEGIN
    o = xjd_reg[0]
    h = PLOT(xjd_reg-o, y_reg)
    h = PLOT(xjd-o, y, SYMBOL='o', LINESTYLE='',/OVERPLOT)
    h = PLOT(xjd_reg-o, y_regs2, LINESTYLE='-',COLOR='r',/OVERPLOT)
    yr = h.YRANGE
    colors = ['blue','red','green','violet', 'dark blue', 'dark red']
    FOR i = 0, countInWideRange-1 DO BEGIN
      h = PLOT([xjd_reg[subRegMaxima[i]],xjd_reg[subRegMaxima[i]]]-o,yr, LINESTYLE='-',COLOR=colors[i],/OVERPLOT)
      h = PLOT([xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins[i]]],xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins[i]]]]-o, yr, LINESTYLE='--',COLOR=colors[i], /OVERPLOT)
      h = PLOT([xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins[i]]],xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins[i]]]]-o, yr, LINESTYLE='--',COLOR=colors[i],/OVERPLOT)
    ENDFOR
  ENDIF  
  ; After this, it may happen that the some (or all) the periods overlap completely because each max is analysed separetely. For example:
  ; period 1: min - max - min - max - min
  ; period 2:             min - max - min 
  ; When one or more than one periods overlap, only the largest must be kept
  IF (countInWideRange GT 1) THEN BEGIN
    subs = INDGEN(countInWideRange)
    sub2check = subs*0+1 ;1 if it was not omitted yet
    sub2keep = !NULL
    FOR i = 0, countInWideRange-1 DO BEGIN
      ;if this period ise entirely within another one, remove it, if not keep it
      ; a) ind of the other elements
      indOthers = WHERE((subs NE i) AND (sub2check EQ 1), countOther)
      IF (countOther EQ 0) THEN BEGIN
        sub2keep = [sub2keep, i]
      ENDIF ELSE BEGIN
        ind = WHERE((subRegExtrema[sub_subRegExtrema_LeftMins[i]] GE subRegExtrema[sub_subRegExtrema_LeftMins[indOthers]]) AND $
                    (subRegExtrema[sub_subRegExtrema_RightMins[i]] LE subRegExtrema[sub_subRegExtrema_RightMins[indOthers]]), count)
        IF (count EQ 0) THEN BEGIN
          sub2keep = [sub2keep, i]
        ENDIF ELSE BEGIN
          ;it has to be removed now. If it has the same boundaries of another one, it would couse both to be removed. Instead I should keep one of the two (or more) that
          ;have different max but same bounding minima. 
          sub2check[i] = 0
       ENDELSE
     ENDELSE
    ENDFOR
    IF N_ELEMENTS(sub2keep) EQ 0 THEN sub2keep = 1 ;they have same boundary, they are equal
    sub_subRegExtrema_LeftMins = sub_subRegExtrema_LeftMins[sub2keep]
    sub_subRegExtrema_RightMins = sub_subRegExtrema_RightMins[sub2keep]
    integs = integs[sub2keep]
    subRegMaxima = subRegMaxima[sub2keep]
  ENDIF
  ; 5. retrive the two largest integrals: the one in the narrower narrow_rangejd  (NInt) and the one happening everywhere in the lager wide_rangejd  (WInt)
  sub_subRegMaxima_inNarrowRange = WHERE((xjd_reg[subRegMaxima] GE narrow_rangejd[0]) AND (xjd_reg[subRegMaxima] LE narrow_rangejd[1]), countInNarrowRange)
  sub_subRegMaxima_inWideRange = WHERE((xjd_reg[subRegMaxima] GE wide_rangejd[0]) AND (xjd_reg[subRegMaxima] LE wide_rangejd[1]), countInWideRange)
  IF (countInNarrowRange GT 0) THEN BEGIN
    NInt = MAX(integs[sub_subRegMaxima_inNarrowRange], sub)
    sub_subRegMaxima_NInt = sub_subRegMaxima_inNarrowRange[sub]
  ENDIF 
  IF (countInWideRange GT 0) THEN BEGIN
    WInt = MAX(integs[sub_subRegMaxima_inWideRange], sub)
    sub_subRegMaxima_WInt = sub_subRegMaxima_inWideRange[sub]
  ENDIF 
  ; select th ebest candidate
  ; case A) both exists and refer to the same (i.e. the largest integral is in the narrow window), take it
  IF ((countInNarrowRange GT 0) AND (countInWideRange GT 0)) THEN BEGIN
    IF (sub_subRegMaxima_NInt EQ sub_subRegMaxima_WInt) THEN sub_subRegMaxima_SelectedMax = sub_subRegMaxima_NInt 
  ENDIF
     
  ; case B) both exists and they are different, take the one in the narrow if WInt<Mult*NInt, if not take the one in the wide 
  IF ((countInNarrowRange GT 0) AND (countInWideRange GT 0)) THEN BEGIN
    IF (sub_subRegMaxima_NInt NE sub_subRegMaxima_WInt) THEN BEGIN
      IF (WInt GE Mult*NInt) THEN sub_subRegMaxima_SelectedMax = sub_subRegMaxima_WInt ELSE sub_subRegMaxima_SelectedMax = sub_subRegMaxima_NInt 
    ENDIF  
  ENDIF
  
  ; case C) there is only one in the wide range, take it
  IF ((countInNarrowRange EQ 0) AND (countInWideRange GT 0)) THEN sub_subRegMaxima_SelectedMax = sub_subRegMaxima_WInt
  
  subReg_SelectedMax = subRegMaxima[sub_subRegMaxima_SelectedMax]
  subReg_MinLeft = subRegExtrema[sub_subRegExtrema_LeftMins[sub_subRegMaxima_SelectedMax]]
  subReg_MinRight = subRegExtrema[sub_subRegExtrema_RightMins[sub_subRegMaxima_SelectedMax]]
  xjdLeft =  xjd_reg[subReg_MinLeft]
  xjdRight =  xjd_reg[subReg_MinRight]
  xjdMax = xjd_reg[subReg_SelectedMax]
  
  ;The max was slected Some checks:
  ;c1: Does it has at least 3 obs in each side? If not, return -101 or -102 (relative maxima too close to data boundaries, left / right)
  IF (subReg_SelectedMax LT 3) THEN BEGIN
    season.retr = -1
    season.code = -201
    season.msg = 'locate_largest_season_in_jd_range: relative maximum in the required period too close to left data boundary (less than 3 obs). The series should have more past data'
    RETURN, season
  ENDIF
  IF (subReg_SelectedMax GT N_ELEMENTS(y_regs2)-4) THEN BEGIN
    season.retr = -1
    season.code = -202
    season.msg = 'locate_largest_season_in_jd_range: relative maximum in the required period too close to right data boundary (less than 3 obs). The series should have more recent data'
    RETURN, season
  ENDIF



  ;Limits for p2 and p5, the inflection points of the left and right hyper tangent
  ;I have the position (and values for left_min-> max->right_min over which I will optimize
  ;Now set limit for p2, the left inflection point that I do not want to be too close to the maximum
  ;upper limit it at the time at wich the fitted curve is 90% of amplitude up
  ythresh = y_regs2[subReg_MinLeft] + thershold_amplitudeP2P5lim*(y_regs2[subReg_SelectedMax]-y_regs2[subReg_MinLeft])
  ind = WHERE(y_regs2[subReg_MinLeft:subReg_SelectedMax] GE ythresh[0])
  xjdLimP2 = xjd_reg[ind[0]+subReg_MinLeft]
  ;now fg
  ythresh = y_regs2[subReg_MinLeft] + thershold_amplitudeP2P5fg *(y_regs2[subReg_SelectedMax]-y_regs2[subReg_MinLeft])
  ind = WHERE(y_regs2[subReg_MinLeft:subReg_SelectedMax] GE ythresh[0])
  xjdFgP2 = xjd_reg[ind[0]+subReg_MinLeft]
  ;Now limit for p5
  ythresh = y_regs2[subReg_MinRight] + thershold_amplitudeP2P5lim*(y_regs2[subReg_SelectedMax]-y_regs2[subReg_MinRight])
  ind = WHERE(y_regs2[subReg_SelectedMax:subReg_MinRight] LE ythresh[0])
  xjdLimP5 = xjd_reg[ind[0]+subReg_SelectedMax]
  ;now fg
  ythresh = y_regs2[subReg_MinRight] + thershold_amplitudeP2P5fg *(y_regs2[subReg_SelectedMax]-y_regs2[subReg_MinRight])
  ind = WHERE(y_regs2[subReg_SelectedMax:subReg_MinRight] LE ythresh[0])
  xjdFgP5 = xjd_reg[ind[0]+subReg_SelectedMax]

  IF (doplot eq 1) THEN BEGIN
    o = xjd_reg[0]
    h = PLOT(xjd_reg-o, y_reg)
    h = PLOT(xjd-o, y, SYMBOL='o', LINESTYLE='',/OVERPLOT)
    h = PLOT(xjd_reg-o, y_regs2, LINESTYLE='-',COLOR='r',/OVERPLOT)
    yr = h.YRANGE
    h = PLOT([xjd_reg[subReg_MinLeft],xjd_reg[subReg_MinLeft]]-o,yr, LINESTYLE='-',COLOR='b',/OVERPLOT)
    h = PLOT([xjd_reg[subReg_SelectedMax],xjd_reg[subReg_SelectedMax]]-o, yr, LINESTYLE='--',COLOR='black', /OVERPLOT)
    h = PLOT([xjd_reg[subReg_MinRight],xjd_reg[subReg_MinRight]]-o, yr, LINESTYLE='-',COLOR='g',/OVERPLOT)
    h = PLOT([xjdLimP2,xjdLimP2]-o, yr, LINESTYLE=':',COLOR='black', /OVERPLOT)
    h = PLOT([xjdLimP5,xjdLimP5]-o, yr, LINESTYLE=':',COLOR='black', /OVERPLOT)
    h = PLOT([xjdFgP2,xjdFgP2]-o, yr, LINESTYLE=':',COLOR='green', /OVERPLOT)
    h = PLOT([xjdFgP5,xjdFgP5]-o, yr, LINESTYLE=':',COLOR='green', /OVERPLOT)
    h.close
  ENDIF
  res = MIN(ABS(xjd-xjdLeft[0]), subLonOriginalData)
  res = MIN(ABS(xjd-xjdRight[0]), subRonOriginalData)
  res = MIN(ABS(xjd-xjdMax), subMaxOnOriginalData)
  season.retr = 1
  season.subLeft=subLonOriginalData
  season.subRight=subRonOriginalData
  season.subMax=subMaxOnOriginalData
  season.ySmoothAtSubLeft=y_regs2[subReg_MinLeft]
  season.ySmoothAtSubRight=y_regs2[subReg_MinRight]
  season.ySmoothAtSubMax=y_regs2[subReg_SelectedMax]
  ;with very few observation it may happen that xjdFgPx, that is set on the smoothed curve, are out of the interval
  ;defined by xjd[subLonOriginalData] and jd[subRonOriginalData] and give bad parameter error in the optimization. Set them to the most
  ;reasonables value (halfway between min and max.
  ;1. Check left side
  IF (xjdFgP2 LE xjd[subLonOriginalData]) THEN BEGIN
    xjdFgP2 = xjd[subLonOriginalData] + (xjd[subMaxOnOriginalData]-xjd[subLonOriginalData])/2.0
    ;now fix the lim if the new first guess is beyond it
    IF (xjdLimP2 LE xjdFgP2) THEN xjdLimP2 = xjd[subMaxOnOriginalData]
  ENDIF
  ;2. Check right side
  IF (xjdFgP5 GE xjd[subRonOriginalData]) THEN BEGIN
    xjdFgP5 = xjd[subMaxOnOriginalData] + (xjd[subRonOriginalData]-xjd[subMaxOnOriginalData])/2.0
    ;now fix the lim if the new first guess is beyond it
    IF (xjdLimP5 LE xjdFgP5) THEN xjdLimP5 = xjd[subMaxOnOriginalData]
  ENDIF

  season.xjdLimP2=xjdLimP2
  season.xjdLimP5=xjdLimP5
  season.xjdFgP2=xjdFgP2
  season.xjdFgP5=xjdFgP5

;  IF (xjdFgP2 LE xjd[subLonOriginalData]) THEN BEGIN
;    xjdFgP2 = xjd[subLonOriginalData] + (xjd[subMaxOnOriginalData]-xjd[subLonOriginalData])/2.0
;    ;now fix the lim if the new first guess is beyond it
;    IF (xjdLimP2 LE xjdFgP2) THEN xjdLimP2 = xjd[subMaxOnOriginalData]
;  ENDIF
  ;chech that the min-max-min bump is not negiglibly small range gt 0.1 for NDVI , see above for other
  
  IF ((season.ySmoothAtSubMax - MIN([season.ySmoothAtSubLeft, season.ySmoothAtSubRight])) LT thr) THEN BEGIN
    season.retr = 0
    season.code = -300
    season.msg = 'The selected bump had a range (on the smoothed curve) smaller than thr'
  ENDIF

  RETURN, season
END