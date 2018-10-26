FUNCTION phenot_p4

;  Purpose:
;     to test the error in NRT detection of GS start
;     for every availble year and season, I remove that season and estimate it NRT, 
;     without data coming from that season 
;  Outcome:
;     

;  Usage:
;     rc = phenot_p4()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.
;     1: Season probably incomplete (min after max is identical to max
;        or min is larger than median of FAPAR distribution).
;     2: Processing ended before identifying maxngrowseas growing
;        seasons due to lack of valid data.

;  Remarks:



;  History:


;  Include the necessary common block(s):

@cb_in.comm
@cb_stat.comm
@cb_options.comm
@cb_pdhtf.comm

;total maximum number of season in my data series is -> maxngrowseas, of which some may be missing
;the number of gs per year is -> ngspy
;the number of full solar year is -> n_full_solar_years
;total number of year is  -> n_full_solar_years +1 (the last is the current year which is not complete)

;indseason=indgen(maxngrowseas)  ;array of subscripts 
;gsstrexp=fltarr(maxngrowseas)  ;array for recording the NRT estimation of start of GS 
;gsstrexp2=fltarr(maxngrowseas)  ;array for recording the "exact" NRT estimation of start of GS
indseason=indgen(nyears)  ;array of subscripts 
gsstrexp=fltarr(2, nyears)  ;array for recording the NRT estimation of start of GS 
gsstrexp2=fltarr(2, nyears)  ;array for recording the "exact" NRT estimation of start of GS
;for every gs, determine NRT the start of gs
;in NRT I have the fapar record until decade i and I have to say if i is start or not 

;define winlen based on number of seasons
winlen = floor(10 / ngspy)  ;10 if one gs , 5 if two


;n year of GSs (1 or 2) retrieved by phenot
n=N_ELEMENTS(gsstrt[0,*])
;Loop over the expected numebr of GS in a year (can be 1 or 2)
;(perform the analysis on the first GS and the second, if any
FOR c = 0, ngspy-1 DO BEGIN
  ;  Loop over all years for which a season has bee found
  FOR y=0, n-1 DO BEGIN  
    
;FOR k=0, maxngrowseas-1 DO BEGIN
    ;retrieve the threshold without using the season k
    ind=where(indseason ne y, count)
    gsstrthresh=min(gsstrtdelta[c, ind], /NAN)
    ;control
    if gsstrthresh lt 0.0001 then stop
    ;extract the relevant data going from the previous end of GS (if any, for the first recod go back to 0)
    ;special case for the first year where I start searching at 0
    
    
    ;determine position of previous and next seasons 
    if ngspy eq 1 then begin
      ypGS=y-1  ; year of previous GS
      cpGS=0    ; season number of previous GS
      ynGS=y+1  ; year of next GS
      cnGS=0    ; season number of next GS
    endif else begin  
      if (c eq 0) then begin
        ypGS=y-1
        cpGS=1
        ynGS=y  ; year of next GS
        cnGS=1    ; season number of next GS
      endif else begin
        ypGS=y
        cpGS=0
        ynGS=y+1  ; year of next GS
        cnGS=0    ; season number of next GS
      endelse
    endelse
    if (y eq 0) then ind0=0 else ind0=gsstop[cpGS, ypGS]
    if (ynGS gt n-1) then ind1=N_ELEMENTS(fapar)-1 else ind1=gsstrt[cnGS, ynGS] 
        
    data=fapar[ind0:ind1] ; fapar data
    dec=ind0+indgen(N_ELEMENTS(data)) ;decades
    ;start the analysis
    
     
    i=0+(winlen-1)  ;so I have winlen elements [0, 1, .. winlen-1] 
    exit_loop=0
    REPEAT BEGIN
      ;  Collect the valid values to be used for the model fitting:
      good_ind = WHERE((data [i-(winlen-1):i] GE ibel) $     ;X, ibel is ignore below  
               AND (data [i-(winlen-1):i] LE iabo), ngood)            ;iabo is ignore above
      ;goodfi = good_ind +ind0
      goodf = data [i-(winlen-1)+good_ind]                                       ;Y
      goodfi       = dec[0]+ (i-(winlen-1)) + good_ind
      nbad = winlen - ngood
      ;here I may check for no data and for some the existence variability
      ;....
      ;optimize the expenential
      ;y=a0+a1*(exp(a2*x)) ;we may consider x - a3
      ;fg=[0.0, max(goodf)-min(goodf), 0.05] ;do better here!!!
      fg=[0.1, double(goodf[0])] ;do better here!!!
      weights = FLTARR(N_ELEMENTS(goodf))*0.0+1.0
      ;p = MPFITFUN('exp4mpfit', double(goodfi), double(goodf), double(weights)*0.0, $
      p = MPFITFUN('exp24mpfit', double(goodfi), double(goodf), double(weights)*0.0, $
                  double(fg), weights=double(weights), $
                  STATUS = status, YFIT=yfit, BESTNORM=BESTNORM, FTOL=1.D-3, $
                  PERROR=PERROR, ERRMSG=error, ITMAX=500, /AUTODERIVATIVE, /QUIET) 
      ;check if the threshold was exceeded, if yes advance
      delta=yfit[N_ELEMENTS(yfit)-1]- yfit[0]
  
      if (delta gt gsstrthresh) then begin  ;SOS was found, proceed to next gs (exit the loop)
        ;print, p
        window, /free, TITLE='NRT-GS Year '+strtrim(y+1, 2)+'Season '+strtrim(c+1, 2)
        plot, goodfi, goodf, psym=1, xrange=[goodfi[0]-1, goodfi[N_elements(goodfi)-1]+1]
        oplot, goodfi, yfit, color=3000
        ;start set to last element of the window
        gsstrexp[c, y]=goodfi[N_ELEMENTS(goodfi)-1]
        ;start set to the the firt occurrence of an increase exceeding delta 
        fmin=min(yfit, ind_min)
        ind_str=where(yfit[ind_min:N_ELEMENTS(yfit)-1] - fmin gt gsstrthresh, count_gt_thresh)
        if count_gt_thresh gt 1 then ind_str=ind_str[0]
        gsstrexp2[c, y]=goodfi[ind_str]
        exit_loop=1
        ;if (k+1 eq 21) then stop
      endif else begin
        i=i+1   ;move on
        ;check that the next i is still in the window
        if (i gt N_ELEMENTS(data)-1) then begin
          gsstrexp[c, y]=!VALUES.F_NAN
          exit_loop=1
        endif
      endelse 
    ENDREP UNTIL ( exit_loop eq 1)
  ENDFOR
  print, 'window lenght'
  print, winlen
  
  print, 'Statistics for GS n. '+strtrim(c,2)
  print, 'GS start of exp (NRT)'
  print, fix(gsstrexp[c,*]), FORMAT='(100(i5,x))'
  print, 'GS start of pdhtf'
  print, gsstrt[c,*], FORMAT='(100(i5,x))'
  print, 'GS number'
  print, indseason+1, FORMAT='(100(i5,x))'
  print, 'delta exp - pdhtf (based on fit)'
  print,  fix(gsstrexp[c,*]-gsstrt[c,*]), FORMAT='(100(i5,x))'
  print, 'delta2 exp - pdhtf (based on actual increase)'
  print,  fix(gsstrexp2[c,*]-gsstrt[c,*]), FORMAT='(100(i5,x))'
  print, 'Delay in detecting delta2'
  print,  fix(gsstrexp2[c,*]-gsstrexp[c,*]), FORMAT='(100(i5,x))'
  print, 'RMSE'
  print,  sqrt(mean((gsstrt[c,*]-gsstrexp[c,*])^2))
  print, 'mean error'
  print,  mean(gsstrt[c,*]-gsstrexp[c,*])
  print, 'mean error2'
  print,  mean(gsstrt[c,*]-gsstrexp2[c,*])
ENDFOR 


retcode=0  
RETURN, retcode

END