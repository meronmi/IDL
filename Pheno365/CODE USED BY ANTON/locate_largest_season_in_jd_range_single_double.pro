FUNCTION locate_largest_season_in_jd_range_single_double, xjd, y, wide_rangejd, narrow_rangejd, indexName, doubleSeasonAdmitted, yearOfInterst
  ; Michele Meroni, June 2019
  ;
  
  ; xjd: is the time interval containing the season of interest. IT MUST contain the full year of interest 
  ;      + a bit of previous (season can start there) and IT MAY contain a bit of the the following year
  ; y: the index (admitted types: NDVI, VH/VV, RVI)
  ; wide_rangejd:   max can occurr here but those occurring in narrow_rangejd are preferred (unless comapratively small)
  ;                 min occurring here are directly accepted, those occurring outside are accepted if a min inside does not popup reducing the strength of
  ;                 the criterion used to reject a min (holeMinFractionalImportance )
  ; narrow_rangejd: max occuring here is preferred. 
  ; indexname: a string indicating the index type (possible values: NDVIm, CR, RVI)
  ; doubleSeasonAdmitted: set it to 1 to report for two maxima in the range (this is for winter crops that shows two bumbs with a min around Jan)
  
  ;so the order of preference for single season admitted is
  ; 1 Max in narrow_rangejd and mins in  wide_rangejd 
  ; 2 Max in narrow_rangejd and mins also outside wide_rangejd (before accepting this the holeMinFractionalImportance is reduced to see if a min in wide_rangejd appears)
  ; 3 Max in wide_rangejd and mins in wide_rangejd 
  ; 4 Max in wide_rangejd and mins also outside wide_rangejd (before accepting this the holeMinFractionalImportance is reduced to see if a min in wide_rangejd appears) 
  ;
  ; NOTE that minima in rangejd_mins and maxima in rangejd_max will be preferred to those occurring outside (even if they are smaller minima)
  ; 
  ; Method:
  ; 1. compute the smoothed curve (trial and error process to detrmmine the smoothing parameters, that can be different between S1 and S2)
  ; 2. take care of edges
  ; 3. locate relative minima an maxima
  ; 4. for each maximum with existing minima on the two sides, compute the integral Int (the magnitue of a season depend on ampltitude and duration
  ;    To achieve this we can accept minima also outside wide_rangejd, but before opting for it we reduce progressively holeMinFractionalImportance the 
  ;    process of accpeting or rejecting a minima to see if min within wide_rangejd pops up
  ; 5. retrive the two largest integrals: the one in the narrower rangejd_max (NInt) and the one in the lager rangejd_mins (LInt)
  ; 6. Locate the season according to this scheme:
  ;    - NInt refer to the same maximum of LInt, get it
  ;    - LInt > Mult*Nint, this season, despite being in the wide range, is much bigger, take the season associated with LInt
  ;    - LInt <= Mult*Nint, the season in the narrow range is not negiglibly small comapred to the one outside, take the season associate with NInt
  ;    Mult is the multiplier to be set (M = 2 as a start of the trial and error process  
  ; ATTENTION: bimodal winter crop treated but not decsribed here
  ; Note: in v4 minima away from max are found looking at the minimum causing a significant hole, where ever it happens.
  ;       In v5 this minimum is accepted only if it falls in the wide range or if it outside, only there is no min inside. 
  ;       If not, the hole threshold is reduce in steps (holeMFIReductionStep) and a 
  ;       minimum is searched again. This is looped untile I find a minimum in widerange. 
  ; 
  ; RETURN the left and right subscript of minima and some other stats 


  Mult = 2                                ; integral multiplier, see above 
  
  holeMinFractionalImportance = 0.15      ; the fractional importance of the hole made the minimum, used in findMinAwayFromMax
  holeMFIReductionStep = 0.025             ; the quantity used to reduce holeMinFractionalImportance when the retrieved min falls outside the wide range
                                          ; AND (x 2) the minimum level of holeMinFractionalImportance that is used before giving up looking for a min (holeMFIReductionStep*2)
  holeMFIMinimum = 0.1                    ; the final minimum acceptable 
  
  thershold_amplitudeP2P5lim = 0.9        ; fraction of the value of the grow (decay) amplitude used to set the upper (lower) limit of the tanh flex point.
                                          ; i.e. the inflection ponint cannot be closer than this to the maximum
  thershold_amplitudeP2P5fg = 0.5         ; fraction of the value of the grow (decay) amplitude used to set the first guess of the tanh flex point.
                                          ; i.e. it is placed at the time of half-amplitude

  doplot = 0                              ;(debug) 1 for plotting results of the function

  ; the function smooth the y values to get estimates of the minima and maxima, check that at very least 10 obs are available
  IF (N_ELEMENTS(y) LE 10) THEN BEGIN
    season = CREATE_STRUCT('retr', -1, 'code', -100, 'n_seasons', 1, 'subMax', -9999, 'msg', 'locate_largest_season_in_jd_range: less than 10 data points')
    RETURN, season
  ENDIF

  ;linearly iterpolate y over a regular daily grid
  xjd_reg = FINDGEN(xjd[-1]-xjd[0]+1)+xjd[0]
  y_reg = INTERPOL(y, xjd, xjd_reg)
  IF (indexName EQ 'NDVI') THEN indexName = 'NDVIm' ;in oder to accept new format
  CASE indexName OF
    'NDVIm' :BEGIN
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
  season = CREATE_STRUCT('subLeft', [0,0], 'subRight', [0,0], 'subMax', [-9999,-9999], 'xjdLimP2', [0.0,0.0], 'xjdLimP5', [0.0,0.0], 'xjdFgP2', [0.0,0.0], 'xjdFgP5', [0.0,0.0], $
                         'y_reg_smooth', y_regs2, 'y_obs_smooth', y_regs2[sub_xjd_reg], $
                         'ySmoothAtSubLeft', [0.0,0.0], 'ySmoothAtSubRight', [0.0,0.0], 'ySmoothAtSubMax', [0.0,0.0], $
                         'n_seasons', 1, 'retr', [0,0], 'code', FIX([0,0]),'msg', ['',''])
  ;For Dom test on actual min
  y_regs2_only_obs = y_regs2 * !VALUES.F_NAN
  y_regs2_only_obs[sub_xjd_reg] = y[sub_xjd]
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
    ;left min
    minFoundInWideRange = 0
    newHoleMFI = holeMinFractionalImportance
    sub_subRegExtrema_tmp_list = !NULL ;store here the minima found reducing the holeMinFractionalImportance
    WHILE (minFoundInWideRange EQ 0) DO BEGIN
      sub_subRegExtrema_LeftMins[i] = findMinAwayFromMax_v5(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'left', newHoleMFI, y_regs2_only_obs)
      sub_subRegExtrema_tmp_list = [sub_subRegExtrema_tmp_list, sub_subRegExtrema_LeftMins[i]]
      IF (xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins[i]]] LT wide_rangejd[0]) THEN BEGIN
        newHoleMFI = newHoleMFI - holeMFIReductionStep
        ;the holeMinFractionalImportance was already reduced down to holeMFIReductionStep*2 and no min in wide range was found, keep the one closer to the wide range
        IF (newHoleMFI LT holeMFIMinimum) THEN BEGIN
          sub_subRegExtrema_LeftMins[i] = MAX(sub_subRegExtrema_tmp_list)
          minFoundInWideRange = 1
        ENDIF
      ENDIF ELSE BEGIN
        minFoundInWideRange = 1
      ENDELSE
    ENDWHILE
    
    ;now the right
    minFoundInWideRange = 0
    newHoleMFI = holeMinFractionalImportance
    sub_subRegExtrema_tmp_list = !NULL ;store here the minima found reducing the holeMinFractionalImportance
    WHILE (minFoundInWideRange EQ 0) DO BEGIN
      ;sub_subRegExtrema_RightMins[i] = findMinAwayFromMax_v3(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'right', newHoleMFI)
      sub_subRegExtrema_RightMins[i] = findMinAwayFromMax_v5(subRegExtrema, sig, subRegMaxima[i], xjd_reg, y_regs2, 'right', newHoleMFI, y_regs2_only_obs)
      sub_subRegExtrema_tmp_list = [sub_subRegExtrema_tmp_list, sub_subRegExtrema_RightMins[i]]
      IF (xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins[i]]] GT wide_rangejd[1]) THEN BEGIN
        newHoleMFI = newHoleMFI - holeMFIReductionStep
        ;the holeMinFractionalImportance was already reduced down to holeMFIReductionStep and no min in wide range was found, keep the one outside
        IF (newHoleMFI LT holeMFIMinimum) THEN BEGIN
          sub_subRegExtrema_RightMins[i] = MIN(sub_subRegExtrema_tmp_list) 
          minFoundInWideRange = 1
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
  ;Modif 18/7/2019 now I have a set of maxima, remove those that are negiglibly small
  ;chech that the min-max-min bump is not negiglibly small range gt 0.1 for NDVI , see above for other
  sub2keep = WHERE(y_regs2[subRegMaxima] - $
                   MIN([y_regs2[subRegExtrema[sub_subRegExtrema_LeftMins]],y_regs2[subRegExtrema[sub_subRegExtrema_RightMins]]]) GT thr, count2Keep)
  IF (count2Keep EQ 0) THEN BEGIN
    PRINT, 'No extrema in the specified min period'
    season.retr = -1
    season.code = -101
    season.msg = 'locate_largest_season_in_jd_range: There is no max in the wide range'
    RETURN, season
  ENDIF ELSE BEGIN
    sub_subRegExtrema_LeftMins = sub_subRegExtrema_LeftMins[sub2keep]
    sub_subRegExtrema_RightMins = sub_subRegExtrema_RightMins[sub2keep]
    integs = integs[sub2keep]
    subRegMaxima = subRegMaxima[sub2keep]
  ENDELSE
  
  
  ; 5. act differently if a double season is admitted (for winter crops) or not
  countInWinCropRange = 0
  IF (doubleSeasonAdmitted EQ 1) THEN BEGIN
    ; 5a. look how many max I have in the winterCropRange, wide_rangejd[0] to narrow_rangejd[1]
    ;sub_subRegMaxima_inWinCropRange = WHERE((xjd_reg[subRegMaxima] GE wide_rangejd[0]) AND (xjd_reg[subRegMaxima] LE narrow_rangejd[1]), countInWinCropRange)
    ;modification 2/7/2018: the maximum (in radar expecially) can be on the right side dand excluded here
    ;instead of using the max I use here the mid point between the two minima, so if the season is large it will be retained (see ID 54382456)
    ;modification 18/7/2018: if the decrease is very slow the midpoint goes out, consider the maximum as well
    ; 
    xjd_midPoint = 0.5*(xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins]] + xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins]]);subRegExtrema[sub_subRegExtrema_LeftMins[sub_subRegMaxima_SelectedMax]]
    sub_subRegMaxima_inWinCropRange = WHERE($
      ((xjd_midPoint GE wide_rangejd[0]) OR (xjd_reg[subRegMaxima] GE wide_rangejd[0])) AND ((xjd_midPoint LE narrow_rangejd[1]) OR (xjd_reg[subRegMaxima] LE narrow_rangejd[1])), countInWinCropRange)
    sub_subRegMaxima_inNarrowRange = WHERE($
      ((xjd_midPoint GE narrow_rangejd[0]) OR (xjd_reg[subRegMaxima] GE narrow_rangejd[0])) AND ((xjd_midPoint LE narrow_rangejd[1]) OR (xjd_reg[subRegMaxima] LE narrow_rangejd[1])), countInNarrowRange)
  ENDIF
  IF (doubleSeasonAdmitted EQ 1) AND (countInWinCropRange GT 1) THEN BEGIN
    ; a double season is admitted and I actually have at least two max in the winterCropRange, wide_rangejd[0] to narrow_rangejd[1]  
    IF (countInNarrowRange GT 0) THEN BEGIN
      ;here I am loking for a large season with a minor season before, the large seaon has to peak in the narrow range
      ;at least one season is the the narrow range, take the largest there and the on before
      subDescInNarrowRange = REVERSE(SORT(integs[sub_subRegMaxima_inNarrowRange])) ; sort descending
      indTmp = WHERE(sub_subRegMaxima_inWinCropRange EQ sub_subRegMaxima_inNarrowRange[subDescInNarrowRange[0]])
      IF (indTmp GT 0) THEN BEGIN
        ;there is a season before, take it
        sub_subRegMaxima_SelectedMax = sub_subRegMaxima_inWinCropRange[[indTmp,indTmp-1]]
      ENDIF ELSE BEGIN
        ;all the seasons where in the narrow range, take the largest and the one after
        sub_subRegMaxima_SelectedMax = sub_subRegMaxima_inWinCropRange[[indTmp,indTmp+1]]
      ENDELSE
    ENDIF ELSE BEGIN
      ;none is in narrow range, do the best..
      subDesc = REVERSE(SORT(integs[sub_subRegMaxima_inWinCropRange])) ; sort descending
      ;if the largest is not the first, that that and the one before
      ;if the largest is the first, look for the second largest, take that and the one before (that cane be thid first and the largest)
      IF (subDesc[0] NE 0) THEN BEGIN
        sub_subRegMaxima_SelectedMax = sub_subRegMaxima_inWinCropRange[[subDesc[0],subDesc[0]-1]]
      ENDIF ELSE BEGIN
        sub_subRegMaxima_SelectedMax = sub_subRegMaxima_inWinCropRange[[subDesc[1],subDesc[1]-1]]
      ENDELSE
    ENDELSE
    ;sub_subRegMaxima_SelectedMax = sub_subRegMaxima_inWinCropRange[subDesc[0:1]]  ;take the biggest two
    sub_subRegMaxima_SelectedMax = sub_subRegMaxima_SelectedMax[SORT(sub_subRegMaxima_SelectedMax)] ;sort by date
    season.n_seasons = 2
  ENDIF ELSE BEGIN
    ; 5b. retrive the two largest integrals: the one in the narrower narrow_rangejd  (NInt) and the one happening everywhere in the lager wide_rangejd  (WInt)
;    sub_subRegMaxima_inNarrowRange = WHERE((xjd_reg[subRegMaxima] GE narrow_rangejd[0]) AND (xjd_reg[subRegMaxima] LE narrow_rangejd[1]), countInNarrowRange)
;    sub_subRegMaxima_inWideRange = WHERE((xjd_reg[subRegMaxima] GE wide_rangejd[0]) AND (xjd_reg[subRegMaxima] LE wide_rangejd[1]), countInWideRange)
    ;modification 2/7/2018
;    sub_subRegMaxima_inNarrowRange = WHERE((xjd_reg[subRegMaxima] GE narrow_rangejd[0]) AND (xjd_reg[subRegMaxima] LE narrow_rangejd[1]), countInNarrowRange)
;    sub_subRegMaxima_inWideRange = WHERE((xjd_reg[subRegMaxima] GE wide_rangejd[0]) AND (xjd_reg[subRegMaxima] LE wide_rangejd[1]), countInWideRange)
    xjd_midPoint = 0.5*(xjd_reg[subRegExtrema[sub_subRegExtrema_LeftMins]] + xjd_reg[subRegExtrema[sub_subRegExtrema_RightMins]]);subRegExtrema[sub_sub
    sub_subRegMaxima_inNarrowRange = WHERE((xjd_midPoint GE narrow_rangejd[0]) AND ((xjd_midPoint LE narrow_rangejd[1]) OR (xjd_reg[subRegMaxima] LE narrow_rangejd[1])), countInNarrowRange)
    sub_subRegMaxima_inWideRange = WHERE((xjd_midPoint GE wide_rangejd[0]) AND ((xjd_midPoint LE wide_rangejd[1]) OR (xjd_reg[subRegMaxima] LE wide_rangejd[1])), countInWideRange)
    IF (countInNarrowRange GT 0) THEN BEGIN
      NInt = MAX(integs[sub_subRegMaxima_inNarrowRange], sub)
      sub_subRegMaxima_NInt = sub_subRegMaxima_inNarrowRange[sub]
    ENDIF 
    IF (countInWideRange GT 0) THEN BEGIN
      WInt = MAX(integs[sub_subRegMaxima_inWideRange], sub)
      sub_subRegMaxima_WInt = sub_subRegMaxima_inWideRange[sub]
    ENDIF 
    ; select the best candidate
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
  ENDELSE ; IF (doubleSeasonAdmitted EQ 1) DO BEGIN 
  ;TREAT THE CASE WHERE TWO SEASONS ARE SELECTED, here it can be one season or two
  subReg_SelectedMax = subRegMaxima[sub_subRegMaxima_SelectedMax]
  subReg_MinLeft = subRegExtrema[sub_subRegExtrema_LeftMins[sub_subRegMaxima_SelectedMax]]
  subReg_MinRight = subRegExtrema[sub_subRegExtrema_RightMins[sub_subRegMaxima_SelectedMax]]
  xjdLeft =  xjd_reg[subReg_MinLeft]
  xjdRight =  xjd_reg[subReg_MinRight]
  xjdMax = xjd_reg[subReg_SelectedMax]
  
  ;The max was selected, some checks:
  FOR s = 0, season.n_seasons-1 DO BEGIN
  ;c1: Does it has at least 3 obs in each side? If not, return a warning (relative maxima too close to data boundaries, left / right)
    IF (subReg_SelectedMax[s] LT 3) THEN BEGIN
      season.retr[s] = -1
      season.code[s] = -201
      season.msg[s] = 'locate_largest_season_in_jd_range: relative maximum in the required period too close to left data boundary (less than 3 obs). The series should have more past data'
      ;RETURN, season
    ENDIF
    IF (subReg_SelectedMax[s] GT N_ELEMENTS(y_regs2)-4) THEN BEGIN
      season.retr[s] = -1
      season.code[s] = -202
      season.msg[s] = 'locate_largest_season_in_jd_range: relative maximum in the required period too close to right data boundary (less than 3 obs). The series should have more recent data'
      ;RETURN, season
    ENDIF
    IF (season.retr[s] NE -1) THEN BEGIN
      ;Limits for p2 and p5, the inflection points of the left and right hyper tangent
      ;I have the position (and values for left_min-> max->right_min over which I will optimize
      ;Now set limit for p2, the left inflection point that I do not want to be too close to the maximum
      ;upper limit it at the time at wich the fitted curve is 90% of amplitude up
      ythresh = y_regs2[subReg_MinLeft[s]] + thershold_amplitudeP2P5lim*(y_regs2[subReg_SelectedMax[s]]-y_regs2[subReg_MinLeft[s]])
      ind = WHERE(y_regs2[subReg_MinLeft[s]:subReg_SelectedMax[s]] GE ythresh[0])
      season.xjdLimP2[s] = xjd_reg[ind[0]+subReg_MinLeft[s]]
      ;now fg
      ythresh = y_regs2[subReg_MinLeft[s]] + thershold_amplitudeP2P5fg *(y_regs2[subReg_SelectedMax[s]]-y_regs2[subReg_MinLeft[s]])
      ind = WHERE(y_regs2[subReg_MinLeft[s]:subReg_SelectedMax[s]] GE ythresh[0])
      season.xjdFgP2[s] = xjd_reg[ind[0]+subReg_MinLeft[s]]
      ;Now limit for p5
      ythresh = y_regs2[subReg_MinRight[s]] + thershold_amplitudeP2P5lim*(y_regs2[subReg_SelectedMax[s]]-y_regs2[subReg_MinRight[s]])
      ind = WHERE(y_regs2[subReg_SelectedMax[s]:subReg_MinRight[s]] LE ythresh[0])
      season.xjdLimP5[s] = xjd_reg[ind[0]+subReg_SelectedMax[s]]
      ;now fg
      ythresh = y_regs2[subReg_MinRight[s]] + thershold_amplitudeP2P5fg *(y_regs2[subReg_SelectedMax[s]]-y_regs2[subReg_MinRight[s]])
      ind = WHERE(y_regs2[subReg_SelectedMax[s]:subReg_MinRight[s]] LE ythresh[0])
      season.xjdFgP5[s] = xjd_reg[ind[0]+subReg_SelectedMax[s]]
    ENDIF ;IF (season.retr[s] NE -1) THEN BEGIN
    ;res = MIN(ABS(xjd-xjdLeft[0]), subLonOriginalData)
    ;res = MIN(ABS(xjd-xjdRight[0]), subRonOriginalData)
    res = MIN(ABS(xjd-xjdLeft[s]), subLonOriginalData)
    res = MIN(ABS(xjd-xjdRight[s]), subRonOriginalData)
    res = MIN(ABS(xjd-xjdMax[s]), subMaxOnOriginalData)
    season.retr[s] = 1
    season.subLeft[s]=subLonOriginalData
    season.subRight[s]=subRonOriginalData
    season.subMax[s]=subMaxOnOriginalData
    season.ySmoothAtSubLeft[s]=y_regs2[subReg_MinLeft[s]]
    season.ySmoothAtSubRight[s]=y_regs2[subReg_MinRight[s]]
    season.ySmoothAtSubMax[s]=y_regs2[subReg_SelectedMax[s]]
    ;with very few observation it may happen that xjdFgPx, that is set on the smoothed curve, are out of the interval
    ;defined by xjd[subLonOriginalData] and jd[subRonOriginalData] and give bad parameter error in the optimization. Set them to the most
    ;reasonables value (halfway between min and max.
    ;1. Check left side
    IF (season.xjdFgP2[s] LE xjd[subLonOriginalData]) THEN BEGIN
      season.xjdFgP2[s] = xjd[subLonOriginalData] + (xjd[subMaxOnOriginalData]-xjd[subLonOriginalData])/2.0
      ;now fix the lim if the new first guess is beyond it
      IF (season.xjdLimP2[s] LE season.xjdFgP2[s]) THEN season.xjdLimP2[s] = xjd[subMaxOnOriginalData]
    ENDIF
    ;2. Check right side
    IF (season.xjdFgP5[s] GE xjd[subRonOriginalData]) THEN BEGIN
      season.xjdFgP5[s] = xjd[subMaxOnOriginalData] + (xjd[subRonOriginalData]-xjd[subMaxOnOriginalData])/2.0
      ;now fix the lim if the new first guess is beyond it
      IF (season.xjdLimP5[s] GE season.xjdFgP5[s]) THEN season.xjdLimP5[s] = xjd[subMaxOnOriginalData]
    ENDIF
    ;chech that the min-max-min bump is not negiglibly small range gt 0.1 for NDVI , see above for other
    IF ((season.ySmoothAtSubMax[s] - MIN([season.ySmoothAtSubLeft[s], season.ySmoothAtSubRight[s]])) LT thr) THEN BEGIN
      season.retr[s] = 0
      season.code[s] = -300
      season.msg[s] = 'The selected bump had a range (on the smoothed curve) smaller than thr'
    ENDIF
  ENDFOR  ;  FOR s = 0, season.n_seasons-1 DO BEGIN
    
  IF (doplot eq 1) THEN BEGIN
    o = xjd_reg[0]
    h = PLOT(xjd_reg-o, y_reg)
    h = PLOT(xjd-o, y, SYMBOL='o', LINESTYLE='',/OVERPLOT)
    h = PLOT(xjd_reg-o, y_regs2, LINESTYLE='-',COLOR='r',/OVERPLOT)
    yr = h.YRANGE
    FOR s = 0, season.n_seasons-1 DO BEGIN
      h = PLOT([xjd_reg[subReg_MinLeft[s]],xjd_reg[subReg_MinLeft[s]]]-o,yr, LINESTYLE='-',COLOR='b',THICK=2-s,/OVERPLOT)
      h = PLOT([xjd_reg[subReg_SelectedMax[s]],xjd_reg[subReg_SelectedMax[s]]]-o, yr, LINESTYLE='--',COLOR='black', /OVERPLOT)
      h = PLOT([xjd_reg[subReg_MinRight[s]],xjd_reg[subReg_MinRight[s]]]-o, yr, LINESTYLE='-',COLOR='g',THICK=2-s,/OVERPLOT)
      h = PLOT([season.xjdLimP2[s],season.xjdLimP2[s]]-o, yr, LINESTYLE=':',COLOR='black', /OVERPLOT)
      h = PLOT([season.xjdLimP5[s],season.xjdLimP5[s]]-o, yr, LINESTYLE=':',COLOR='black', /OVERPLOT)
      h = PLOT([season.xjdFgP2[s],season.xjdFgP2[s]]-o, yr, LINESTYLE=':',COLOR='green', /OVERPLOT)
      h = PLOT([season.xjdFgP5[s],season.xjdFgP5[s]]-o, yr, LINESTYLE=':',COLOR='green', /OVERPLOT)
    ENDFOR
    h.close
  ENDIF
   
  ;if 2 maxima are found after 1/3 (DOY 60), this is not plausible (no autumn growth followed by vernalization)
  ;it is likely that there is one season followed by a cover crop, take the biggest (hoping is the correct one)
  IF (season.n_seasons EQ 2) THEN BEGIN
    ;check if both max are in the period (use yearOfInterst) DOY_YEAR2JD(60, yearOfInterst)
    IF ((xjd_reg[subReg_SelectedMax[0]] GT DOY_YEAR2JD(60, yearOfInterst)) AND (xjd_reg[subReg_SelectedMax[1]] GT DOY_YEAR2JD(60, yearOfInterst))) THEN BEGIN
    ;IF ((JD2DOY(xjd_reg[subReg_SelectedMax[0]]) GT 60) AND (JD2DOY(xjd_reg[subReg_SelectedMax[1]]) GT 60)) THEN BEGIN
      ;remove the smallest in terms of max y, put the retained in season 1
      res = MIN(y_regs2[subReg_SelectedMax], subMin, SUBSCRIPT_MAX=subMax)
      season.n_seasons = 1
      season.SUBLEFT = [season.SUBLEFT[subMax], 0]
      season.SUBRIGHT = [season.SUBRIGHT[subMax], 0]
      season.SUBMAX = [season.SUBMAX[subMax], 0]
      season.XJDLIMP2 = [season.XJDLIMP2[subMax], 0]
      season.XJDLIMP5 = [season.XJDLIMP5[subMax], 0]
      season.XJDFGP2 = [season.XJDFGP2[subMax], 0]
      season.XJDFGP5 = [season.XJDFGP5[subMax], 0]
      season.YSMOOTHATSUBLEFT = [season.YSMOOTHATSUBLEFT[subMax], 0]
      season.YSMOOTHATSUBRIGHT = [season.YSMOOTHATSUBRIGHT[subMax], 0]
      season.YSMOOTHATSUBMAX = [season.YSMOOTHATSUBMAX[subMax], 0]
      season.RETR = [season.RETR[subMax], 0]
      season.CODE = [season.CODE[subMax], -500]
      season.MSG = [season.MSG[subMax], 'Two bumbs in summer time, only the biggest is retained']
    ENDIF
  ENDIF
  
  RETURN, season
END