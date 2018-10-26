FUNCTION Pheno_indicator_builder2
@cb_io.comm

;Difference between Pheno_indicator_builder and  Pheno_indicator_builder2;
;Pheno_indicator_builder2 resample the time axis to get more precise results.
;With Pheno_indicator_builder2 the time is days instead of dekad
           
;Builds all the require pheno indicators form the 9 DHTM parameters:
;pdhtf_base 
;pdhtf_amp1
;pdhtf_sft1 
;pdhtf_slo1 
;pdhtf_amp2 
;pdhtf_sft2 
;pdhtf_slo2 
;gsinistart
;gsiniend
;which are all stored as multi-band (multi-year) bil images
;
;THEORY: accumulated values are computed as summation of the modeled 
;fapar between stargs and stopgs. 
;Start and end of the growing season as the dates at
;which the simulated signal grows beyond or decays below the first
;and last simulated value by more than fract_thresh of the appropriate
;amplitude.
;The subjectively set parameters are then the two thresholds used:
;- the one used in the growth phase (ftg Fractional Threshold Grow)
;- the one used in the decay phase (ftd Fractional Threshold Decay)
;plus:
;-all the possible interval in the growing phase (1-10, 1-20..)
;-all the possible interval in the decay phase (1-10, 1-20..)

;If we explore n interval for each ft whe we'll get 2*n^2 indicators (the '2*
;comes from the fact that all the acc are computed with and withou substraction of 
;the baseline

;USER DEFINED SETTINGS
;root directory where input and output data are stored
base_path = 'K:\Tunisia'
;prefix for data source
prefix = 'MarsFAPAR_'
;prefix = 'G2_' 
;relative path to directory storing Remote Sensing data
;bp 
;rs_product_dir = 'K:\Tunisia\BP_data\BIL\pheno_products\REALIGN_ON_eos'
;marsop
rs_product_dir = 'VGT_data\bil\DIR_RECOMPOSED_UppEnv_17_feb\REALIGN_ON_eos'

;rs_product_dir = 'K:\Tunisia\BP_data\BIL\pheno_products\REALIGN_ON_eos'

ns = 494                      ; number of samples
nl = 842                      ; number of lines
nb = 15                       ; number of years  from 1997-1998

path=base_path
pheno_path=path + '\'+ rs_product_dir

;I/O:
;ret = set_IO()
path = pheno_path ;(from common bloc io, as ns, nl, nb)
;This naming convention comes from phenot and should not be changed:
base_par_fn = ['p0','p1','p2','p3','p4','p5','p6','t0','t1']
;if usign realigne parameters
;base_par_fn = 'A20-1997_'+base_par_fn
base_par_fn = 'A1-1997_'+base_par_fn



;PARAMETERS
resampling = 10               ;factor to resample dekads to get more precise results
dt=1/DOUBLE(resampling)       ;for integral computation
gs=1                          ;GS to be analysed (can be 1 or 2)
;fractional thresholds ing growing or decay fase
ftg=[1,5,10,20,30,40,50,60,70,80,90,95,99]
ftd=[1,5,10,20,30,40,50,60,70,80,90,95,99]
;ftg=[1,99]
;ftd=[1,99]



;#############################################################################

n_prod=N_ELEMENTS(ftg)*N_ELEMENTS(ftd)*2+N_ELEMENTS(ftg)*N_ELEMENTS(ftg)*2+N_ELEMENTS(ftd)*N_ELEMENTS(ftd)*2
c_prod=0              


STARTTIME = SYSTIME(1)
;A) OPEN INPUT FILES
IF (gs EQ 1) THEN base_par_fn = base_par_fn +'1'
IF (gs EQ 2) THEN base_par_fn = base_par_fn +'2'
;array to store the lun
luna= LONARR(N_ELEMENTS(base_par_fn))

FOR i=0, N_ELEMENTS(luna)-1 DO BEGIN
  OPENR, lun, path+'\'+base_par_fn[i], /GET_LUN & luna[i]=lun
ENDFOR
p0assvar = ASSOC(luna[0], FLTARR(ns,nb)) & p1assvar = ASSOC(luna[1], FLTARR(ns,nb))
p2assvar = ASSOC(luna[2], FLTARR(ns,nb)) & p3assvar = ASSOC(luna[3], FLTARR(ns,nb))
p4assvar = ASSOC(luna[4], FLTARR(ns,nb)) & p5assvar = ASSOC(luna[5], FLTARR(ns,nb))
p6assvar = ASSOC(luna[6], FLTARR(ns,nb)) & t0assvar = ASSOC(luna[7], FLTARR(ns,nb))
t1assvar = ASSOC(luna[8], FLTARR(ns,nb))

all_fn_string=''    ;this string will contain the list of output files 



;B) LOOP fg and fd
FOR g=0, N_ELEMENTS(ftg)-1 DO BEGIN
  FOR d=0, N_ELEMENTS(ftd)-1 DO BEGIN
    FOR b=0, 1 DO BEGIN ;baseline sub or not
      ;C) OPEN OUTPUT (for the couple [fg,fd])
      fname=prefix+'g'+strtrim(ftg[g],2)+'d'+strtrim(ftd[d],2)+'b'+strtrim(b,2)
      pheno_out_fn=path+'\'+fname
      all_fn_string=all_fn_string+','+fname
      OPENW, W1, pheno_out_fn, /GET_LUN
      c_prod=c_prod+1
      PRINT, 'Processing '+strtrim(FIX(c_prod/FLOAT(n_prod)*100),2)+' %'
      ;variable to store the line output
      pheno_line = FLTARR(ns,nb) 
      ;D) LOOP ON (i,j) pixels
      FOR line=0, nl-1, 1L DO BEGIN   ;loop on lines
        p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
        p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
        p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
        p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
        t1line=float(t1assvar[line])
        FOR sample=0, ns-1 DO BEGIN ;loop on samples 
          ;E) LOOP ON years and fill the output
          FOR y=0, nb-1 DO BEGIN
            ;process the year if not NAN and season failure (put 0 in this case)
            IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN 
              IF (p0line[sample,y] NE -999) THEN BEGIN 
                ;compute the fitted curve
                pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
                            p3line[sample,y], p4line[sample,y], p5line[sample,y], $
                            p6line[sample,y]]
                pheno_line[sample,y] = compute_acc(pdhtf_pars, t0line[sample,y], t1line[sample,y], resampling, $
                                                   ftg[g], ftd[d], b)
              ENDIF ELSE BEGIN ;is -999
                pheno_line[sample,y]=0.0
              ENDELSE
            ENDIF ELSE BEGIN
              ;fill output with NaN
              pheno_line[sample,y]=!VALUES.F_NAN
            ENDELSE
          ENDFOR  ;years
        ENDFOR  ;sample
        ;F) write the output
        WRITEU, W1, pheno_line
      ENDFOR  ;line
      FREE_LUN, W1
    ENDFOR  ;b  
  ENDFOR  ;d
ENDFOR  ;g

;C) LOOP in growth period only
ft = ftg
FOR g=0, N_ELEMENTS(ft)-1 DO BEGIN
  FOR d=0, N_ELEMENTS(ft)-1 DO BEGIN
    IF (ft[g] LT ft[d]) THEN BEGIN
      FOR b=0, 1 DO BEGIN ;baseline sub or not
        ;C) OPEN OUTPUT (for the couple [fg,fd])
        fname=prefix+'g'+strtrim(ft[g],2)+'g'+strtrim(ft[d],2)+'b'+strtrim(b,2)
        pheno_out_fn=path+'\'+fname
        all_fn_string=all_fn_string+','+fname
        OPENW, W1, pheno_out_fn, /GET_LUN
        c_prod=c_prod+1
        PRINT, 'Processing '+strtrim(FIX(c_prod/FLOAT(n_prod)*100),2)+' %'
        ;variable to store the line output
        pheno_line = FLTARR(ns,nb) 
        ;D) LOOP ON (i,j) pixels
        FOR line=0, nl-1, 1L DO BEGIN 
          p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
          p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
          p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
          p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
          t1line=float(t1assvar[line])
          FOR sample=0, ns-1 DO BEGIN
            ;E) LOOP ON years and fill the output
            FOR y=0, nb-1 DO BEGIN
              ;process the year if not NAN and season failure (put 0 in this case)
              IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN 
                IF (p0line[sample,y] NE -999) THEN BEGIN 
                  ;compute the fitted curve
                  pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
                              p3line[sample,y], p4line[sample,y], p5line[sample,y], $
                              p6line[sample,y]]
                  pheno_line[sample,y] = compute_acc_grow_only(pdhtf_pars, t0line[sample,y], t1line[sample,y], resampling, $
                                                     ft[g], ft[d], b)
                ENDIF ELSE BEGIN ;is -999
                  pheno_line[sample,y]=0.0
                ENDELSE
              ENDIF ELSE BEGIN
                ;fill output with NaN
                pheno_line[sample,y]=!VALUES.F_NAN
              ENDELSE
            ENDFOR  ;years
          ENDFOR  ;sample
          ;F) write the output
          WRITEU, W1, pheno_line
        ENDFOR  ;line
        FREE_LUN, W1
      ENDFOR  ;b
    ENDIF ; IF (ft[g] NE ft[d]) THEN BEGIN 
  ENDFOR  ;d
ENDFOR  ;g

;C) LOOP in decay period only
ft = ftd
FOR g=0, N_ELEMENTS(ft)-1 DO BEGIN
  FOR d=0, N_ELEMENTS(ft)-1 DO BEGIN
    IF (ft[g] LT ft[d]) THEN BEGIN
      FOR b=0, 1 DO BEGIN ;baseline sub or not
        ;C) OPEN OUTPUT (for the couple [fg,fd])
        fname=prefix+'d'+strtrim(ft[g],2)+'d'+strtrim(ft[d],2)+'b'+strtrim(b,2)
        pheno_out_fn=path+'\'+fname
        all_fn_string=all_fn_string+','+fname
        OPENW, W1, pheno_out_fn, /GET_LUN
        c_prod=c_prod+1
        PRINT, 'Processing '+strtrim(FIX(c_prod/FLOAT(n_prod)*100),2)+' %'
        ;variable to store the line output
        pheno_line = FLTARR(ns,nb) 
        ;D) LOOP ON (i,j) pixels
        FOR line=0, nl-1, 1L DO BEGIN 
          p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
          p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
          p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
          p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
          t1line=float(t1assvar[line])
          FOR sample=0, ns-1 DO BEGIN
            ;E) LOOP ON years and fill the output
            FOR y=0, nb-1 DO BEGIN
              ;process the year if not NAN and season failure (put 0 in this case)
              IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN 
                IF (p0line[sample,y] NE -999) THEN BEGIN 
                  ;compute the fitted curve
                  pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
                              p3line[sample,y], p4line[sample,y], p5line[sample,y], $
                              p6line[sample,y]]
                  pheno_line[sample,y] = compute_acc_dec_only(pdhtf_pars, t0line[sample,y], t1line[sample,y], resampling, $
                                                     ft[g], ft[d], b)
                ENDIF ELSE BEGIN ;is -999
                  pheno_line[sample,y]=0.0
                ENDELSE
              ENDIF ELSE BEGIN
                ;fill output with NaN
                pheno_line[sample,y]=!VALUES.F_NAN
              ENDELSE
            ENDFOR  ;years
          ENDFOR  ;sample
          ;F) write the output
          WRITEU, W1, pheno_line
        ENDFOR  ;line
        FREE_LUN, W1
      ENDFOR  ;b
    ENDIF ; IF (ft[g] NE ft[d]) THEN BEGIN 
  ENDFOR  ;d
ENDFOR  ;g



CLOSE, /ALL
PRINT, all_fn_string
; Evaluation of processing time
ELAPSED_TIME = SYSTIME(1) - STARTTIME
HOURS =  FLOOR(ELAPSED_TIME / (60*60))
MINUTES = FLOOR((ELAPSED_TIME MOD (60*60))/ 60)
SECS = FLOOR((ELAPSED_TIME - HOURS*60*60-MINUTES*60))
PRINT, 'OVERALL PROCESSING TOOK :'+STRCOMPRESS(HOURS)+' HOURS, ' +STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
RETURN, 0
END