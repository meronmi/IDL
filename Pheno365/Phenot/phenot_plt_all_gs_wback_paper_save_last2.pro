FUNCTION phenot_plt_all_gs_wback_paper_save_last2
@cb_options.comm
;  Purpose:
;     To plot the details of a all growing season.

;  Outcome:
;     A plot is generate on screen to show a single growing season,
;     together with lines showing the inistart/iniend as well as the
;     gsstart/gsstop events.

;  Usage:
;    rc = phenot_plt_gs ()

;  Input parameters: None.
;  Output parameters: None.

;  Return values:
;     0: Normal completion.

;  Remarks: None.

;  History:
;     This is an updated version of Version: 6.0, Release: 2.0, of the routine
;     gs6t_plt_diag_gs.pro developed on 2008-03-03.
;     Modified by Michele 4 march 2011 and 18 april to use the [c,y] format

;  Include the necessary common block(s):
@cb_os.comm
@cb_job.comm
@cb_in.comm
@cb_stat.comm
@cb_frst.comm
@cb_pdhtf.comm

;  Initialize the return code:
retcode = 0
;for paper save the info of the last two seasons
mymat=FLTARR(5,100) ;dek, data, fitting, info (sos, eos)
myhdr=['dekfrom0_10_98','data','fit','sos','eos']

;MM+
   wintit = 'Overall Functioning'
   screen_dims = GET_SCREEN_SIZE(RESOLUTION = resolution)
   xs = FIX(screen_dims [0] / 100) * 100
   ys = FIX(xs * 0.4)
   CASE period OF
     1: tunit = '[days]'
     10: tunit = '[dekads]'
     30: tunit = '[months]'
     ELSE: tunit = '[unknown]'
   ENDCASE
   
   ;if i eq 0 then WINDOW, 9, XSIZE = xs, YSIZE = ys, TITLE = wintit else $
   WINDOW, /FREE, XSIZE = xs, YSIZE = ys, TITLE = wintit
;  Create the frame and labels:
   x2time=indgen(N_ELEMENTS(julian1))
;paper
lastind=N_ELEMENTS(x2time)-1
mymat[0,*]=x2time[lastind-99:lastind]
   PLOT, $
      x2time, $
      products [*, 0], $
      /NODATA, $
      XMARGIN = [10, 5], $
      XRANGE = [x2time[0],x2time[N_ELEMENTS(x2time)-1]], $
      XSTYLE = 1, $
      ;XTICKUNITS = ['Month', 'Year'], $
      ;XTICKFORMAT = 'LABEL_DATE', $
      XTICKLEN = 0.02, $
      XTICKINTERVAL = 36, $ ;MM: it was 10
      XMINOR=5, $
      XTITLE = 'Time ' + tunit, $
      YMARGIN = [8, 4], $
      YRANGE = [ibel, iabo], $
      YSTYLE = 1, $
      YTICKLEN = 0.02 * ys / xs, $
      YTITLE = 'fAPAR', $
      BACKGROUND = 16777215, COLOR = 0 

      
;      TITLE = site_name + ' [' + $
;         STRTRIM(STRING(site_lat), 2) + ', ' + $
;         STRTRIM(STRING(site_lon), 2) + ']'

;  Plot the original time series as a function of the first
;  day of the recording periods:
;  WARNING: If the PSYM value is changed here, update also the
;  label in the bottom left corner of the graph!
;  PSYM = 1 for '+'
;  PSYM = 2 for '*'
;paper
mymat[1,*]=products[lastind-99:lastind,0]
mymat[2,*]=fitted[lastind-99:lastind]
   OPLOT, $
      x2time, $
      products [*, 0], $
      PSYM = 1, $
      SYMSIZE = 0.75, $
      MIN_VALUE = ibel, $
      MAX_VALUE = iabo, $
      COLOR=0
    
  ; Add a vertical white line to indicate the solar year
  FOR i=0, n_full_solar_years do OPLOT, $
        [i*36+xoffst[0],i*36+xoffst[0]], $
        [0.0, 1.0], LINESTYLE = 0, $
        COLOR=0
     
  grey=100L + 256*100L + (256^2)*100L
  ;now plot each gs
  ;paper 
  cc=0
  FOR y=0, n_full_solar_years DO BEGIN  
    FOR c = 0, ngspy-1 DO BEGIN
      ;paper
      IF (y GE n_full_solar_years-1) THEN BEGIN
        mymat[3,cc]= gsstrt [c,y]
        mymat[4,cc]= gsstop [c,y]
        cc=cc+1
      ENDIF
      clr=LONG(y*10+1)*1500+LONG(c)*100
      ;  Set the time limits for the plot (current setting only
      ;  useful for decadal data):
  ;    from_per = MAX([inper_inistart[c,y] - 5, 0])
  ;    to_per = MIN([inper_iniend[c,y] + 5, npts - 1])
      from_per =gsinistartDek[c,y]
      to_per = gsiniendDek[c,y]
      ;replot original data with a different color to check everithing ok
  ;    OPLOT, $
  ;      periods [from_per:to_per], $
  ;      fapar [from_per:to_per], $
  ;      PSYM = 1, color=clr
      ;  If the inversion worked, superimpose the simulated time series (continuous line):
        
        IF (gsflag[c,y] EQ 10) THEN BEGIN
          fittedcurve=fltarr(N_ELEMENTS(periods [from_per:to_per]))
          j=0
          ;build the fitted curve
          FOR k = from_per, to_per DO BEGIN
            param=[pdhtf_base[c,y], pdhtf_amp1[c,y], pdhtf_sft1[c,y], pdhtf_slo1[c,y], pdhtf_amp2[c,y], $
              pdhtf_sft2[c,y], pdhtf_slo2[c,y]]
            pdhtf, k, param, f
            fittedcurve[j]=f
            j=j+1
          ENDFOR
          OPLOT, $
            periods [from_per:to_per], $
            fittedcurve, $
            ;MIN_VALUE = 0.0, $
            ;MAX_VALUE = 1.0, $
            LINESTYLE = 0, color=clr, THICK=1.5
        ENDIF
        ;  Add a vertical line to indicate actual inistart:
;        OPLOT, $
;          [from_per,from_per], $
;          [0.0, 1.0], $
;          LINESTYLE = 3, COLOR=grey ;color=clr
;        ;  Add a vertical line to indicate actual iniend:
;        OPLOT, $
;          [to_per,to_per], $
;          [0.0, 1.0], $
;          LINESTYLE = 3, COLOR=grey;color=clr
        ;  If the inversion worked, add a vertical line to indicate the timing of the first
        ;  shift (inflexion point of the first hyperbolic tangent):
        IF (gsflag[c,y] EQ 10) THEN BEGIN
;           OPLOT, $
;              [pdhtf_sft1 [c,y], pdhtf_sft1 [c,y]], $
;              [0.0, 1.0], $
;              LINESTYLE = 3, color=clr      
;        ;  Add a vertical line to indicate the timing of the second
;        ;  shift (inflexion point of the second hyperbolic tangent):
;           OPLOT, $
;              [pdhtf_sft2 [c,y], pdhtf_sft2 [c,y]], $
;              [0.0, 1.0], $
;              LINESTYLE = 4, color=clr
        ;  Add a vertical line to indicate start of growing season:
           OPLOT, $
              [gsstrt [c,y], gsstrt [c,y]], $
              [0.0, 1.0], $
              LINESTYLE = 1, color=clr, THICK=1.5
        ;  Add a vertical line to indicate end of growing season:
           OPLOT, $
              [gsstop [c,y], gsstop [c,y]], $
              [0.0, 1.0], $
              LINESTYLE = 1, color=clr, THICK=1.5
        ENDIF
    ENDFOR
  ENDFOR
 ;MM-

SET_PLOT, screen
; paper
WRITE_CSV, 'Q:\HoA\SPIRITS_HOA\MTA and bil files\pheno.csv', mymat, HEADER = myhdr
RETURN, retcode

END