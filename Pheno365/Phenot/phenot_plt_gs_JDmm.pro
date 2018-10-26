FUNCTION phenot_plt_gs_JDmm, c, y

;  Purpose:
;     To plot the details of a particular growing season.

;  Outcome:
;     A plot is generate on screen to show a single growing season,
;     together with lines showing the inistart/iniend as well as the
;     gsstart/gsstop events.

;  Usage:
;    rc = phenot_plt_gs (y, stat)

;  Input parameters:
;     - c {integer}: gs index (can be 1 or 2)
;     - y {integer}: Solar year index


;  Output parameters: None.

;  Return values:
;     0: Normal completion.

;  Remarks: None.

;  History:
;     This is an updated version of Version: 6.0, Release: 2.0, of the routine
;     gs6t_plt_diag_gs.pro developed on 2008-03-03.
;     Modified by Michele, 18 april 2011, to work with [c,y] indexes

;  Include the necessary common block(s):
@cb_os.comm
@cb_job.comm
@cb_in.comm
@cb_stat.comm
@cb_frst.comm
@cb_pdhtf.comm

;  Initialize the return code:
retcode = 0

stat= gsflag[c,y] ;only 10 indicates success
;  Set the time limits for the plot (current setting only
;  useful for decadal data):
inistart = gsinistartDek[c,y]
inistartJD = acq[gsinistartDek[c,y]] 
iniend = gsiniendDek[c,y]
iniendJD = acq[gsiniendDek[c,y]]
from_per = MAX([inistart - 5, 0])
;to_per = MIN([iniend + 5, npts - 1])
to_per = MIN([iniend + 5, npts - 1, N_ELEMENTS(periods)-1])

;  Set the plot title, depending on whether the inversion worked or not:
IF (stat EQ 10) THEN BEGIN
   ptitle = 'GS #' + STRTRIM(STRING(numgs+1), 2) + $
      ' for ' + site_name + ' [' + $
      STRTRIM(STRING(site_lat), 2) + ', ' + $
      STRTRIM(STRING(site_lon), 2) + ']'
ENDIF ELSE BEGIN
   ptitle = 'Non-GS #' + STRTRIM(STRING(numgs+1), 2) + $
      ' for ' + site_name + ' [' + $
      STRTRIM(STRING(site_lat), 2) + ', ' + $
      STRTRIM(STRING(site_lon), 2) + ']'
ENDELSE

;MM+ WINDOW, /FREE, TITLE='GS ' + STRTRIM(STRING(y), 2)
WINDOW, /FREE, TITLE='GS ' + STRTRIM(STRING(numgs+1), 2)
;  Plot the raw product (star symbols):
PLOT, $
   ;periods [from_per:to_per], $
   LONG(acq [from_per:to_per]), XTICKFORMAT='(I10)', $
   fapar [from_per:to_per], $
   MIN_VALUE = 0.0, $
   MAX_VALUE = 1.0, $
   YRANGE = [0.0, 1.0], $
   PSYM = 1, $
   XTITLE = 'Julian Days', $
;   YTITLE = prod_fields [0] + ', smooth ' + $
;      prod_fields [0] + ' and fitted ' + prod_fields [0], $
   YTITLE = prod_fields [0] + ' and fitted ' + prod_fields [0], $
   TITLE = ptitle, $
   BACKGROUND = 16777215, COLOR = 0 

;;  Superimpose the smoothed time series (long dashes):
;OPLOT, $
;   periods [inistart:iniend], $
;   smoothfapar [inistart:iniend], $
;   MIN_VALUE = 0.0, $
;   MAX_VALUE = 1.0, $
;   LINESTYLE = 5

;  If the inversion worked, superimpose the simulated time series (continuous line):
IF (stat EQ 10) THEN BEGIN
   OPLOT, $
      ;periods [inistart:iniend], $
      acq [inistart:iniend], $
      fitted [inistart:iniend], $
      MIN_VALUE = 0.0, $
      MAX_VALUE = 1.0, $
      LINESTYLE = 0, COLOR = 0 
ENDIF

;  Add a vertical line to indicate actual inistart:
OPLOT, $
   ;[inistart, inistart], $
   [inistartJD, inistartJD], $
   [0.0, 1.0], $
   LINESTYLE = 1, COLOR = 0
XYOUTS, $
   ;inistart + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
   inistartJD + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
   (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
   'inistart', $
   ORIENTATION = 90.0, $
   ALIGNMENT = 1.0, COLOR = 0

;  Add a vertical line to indicate actual iniend:
OPLOT, $
   ;[iniend, iniend], $
   [iniendJD, iniendJD], $
   [0.0, 1.0], $
   LINESTYLE = 1, COLOR = 0
XYOUTS, $
   ;iniend - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
   iniendJD - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
   (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
   'iniend', $
   ORIENTATION = 90.0, $
   ALIGNMENT = 1.0, COLOR = 0

;  If the inversion worked, add a vertical line to indicate the timing of the first
;  shift (inflexion point of the first hyperbolic tangent):
IF (stat EQ 10) THEN BEGIN
   OPLOT, $
      [pdhtf_sft1 [c,y], pdhtf_sft1 [c,y]], $
      [0.0, 1.0], $
      LINESTYLE = 3, COLOR = 0
   XYOUTS, $
      pdhtf_sft1 [c,y] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
      'shift_1', $
      ORIENTATION = 90.0, $
      ALIGNMENT = 1.0, COLOR = 0

;  Add a vertical line to indicate the timing of the second
;  shift (inflexion point of the second hyperbolic tangent):
   OPLOT, $
      [pdhtf_sft2 [c,y], pdhtf_sft2 [c,y]], $
      [0.0, 1.0], $
      LINESTYLE = 3, COLOR = 0
   XYOUTS, $
      pdhtf_sft2 [c,y] - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
      'shift_2', $
      ORIENTATION = 90.0, $
      ALIGNMENT = 1.0, COLOR = 0

;  Add a vertical line to indicate start of growing season:
   OPLOT, $
      [gsstrt [c,y], gsstrt [c,y]], $
      [0.0, 1.0], $
      LINESTYLE = 0, COLOR = 0
   XYOUTS, $
      gsstrt [c,y] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
      'GS stsrt', $
      ORIENTATION = 90.0, $
      ALIGNMENT = 1.0, COLOR = 0

;  Add a vertical line to indicate end of growing season:
   OPLOT, $
      [gsstop [c,y], gsstop [c,y]], $
      [0.0, 1.0], $
      LINESTYLE = 0, COLOR = 0
   XYOUTS, $
      gsstop [c,y] - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
      'GS end', $
      ORIENTATION = 90.0, $
      ALIGNMENT = 1.0, COLOR = 0
ENDIF

;  Save the same plot in an Encapsulated PostScript graphics file in
;  the output directory. First, assemble the file name:
;fn = STRMID(data_fname, 0, STRLEN(data_fname) - 4)
;SET_PLOT, 'PS'
;DEVICE, /ENCAPSULATED, FILENAME = run_dir + fn + '_gs-' + $
;   STRTRIM(STRING(y), 2) + '.eps'
;!P.MULTI = [0, 0, 0, 0, 0]
;PLOT, $
;   periods [inistart:iniend], $
;   fapar [inistart:iniend], $
;   MIN_VALUE = 0.0, $
;   MAX_VALUE = 1.0, $
;   YRANGE = [0.0, 1.0], $
;   PSYM = 2, $
;   XTITLE = 'Periods (from start of analyzed time series)', $
;;   YTITLE = prod_fields [0] + ', smooth ' + $
;;      prod_fields [0] + ' and fitted ' + prod_fields [0], $
;   YTITLE = prod_fields [0] + ' and fitted ' + prod_fields [0], $
;   TITLE = ptitle
;
;;;  Superimpose the smoothed time series (long dashes):
;;OPLOT, $
;;   periods [inistart:iniend], $
;;   smoothfapar [inistart:iniend], $
;;   MIN_VALUE = 0.0, $
;;   MAX_VALUE = 1.0, $
;;   LINESTYLE = 5
;
;;  If the inversion worked, superimpose the simulated time series (continuous line):
;IF (stat EQ 10) THEN BEGIN
;   OPLOT, $
;      periods [inistart:iniend], $
;      fitted [inistart:iniend], $
;      MIN_VALUE = 0.0, $
;      MAX_VALUE = 1.0, $
;      LINESTYLE = 0
;ENDIF
;
;;  Add a vertical line to indicate actual inistart:
;OPLOT, $
;   [inistart, inistart], $
;   [0.0, 1.0], $
;   LINESTYLE = 1
;XYOUTS, $
;   inistart + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;   (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;   'inistart', $
;   ORIENTATION = 90.0, $
;   ALIGNMENT = 1.0
;
;;  Add a vertical line to indicate actual iniend:
;OPLOT, $
;   [iniend, iniend], $
;   [0.0, 1.0], $
;   LINESTYLE = 1
;XYOUTS, $
;   iniend - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;   (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;   'iniend', $
;   ORIENTATION = 90.0, $
;   ALIGNMENT = 1.0
;
;;  Add a vertical line to indicate the timing of the first
;;  shift (inflexion point of the first hyperbolic tangent):
;IF (stat EQ 10) THEN BEGIN
;   OPLOT, $
;      [pdhtf_sft1 [c,y], pdhtf_sft1 [c,y]], $
;      [0.0, 1.0], $
;      LINESTYLE = 3
;   XYOUTS, $
;      pdhtf_sft1 [c,y] - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;      'shift_1', $
;   ORIENTATION = 90.0, $
;   ALIGNMENT = 1.0
;
;;  Add a vertical line to indicate the timing of the second
;;  shift (inflexion point of the second hyperbolic tangent):
;   OPLOT, $
;      [pdhtf_sft2 [c,y], pdhtf_sft2 [c,y]], $
;      [0.0, 1.0], $
;      LINESTYLE = 3
;   XYOUTS, $
;      pdhtf_sft2 [c,y] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;      'shift_2', $
;      ORIENTATION = 90.0, $
;      ALIGNMENT = 1.0
;
;;  Add a vertical line to indicate start of growing season:
;   OPLOT, $
;      [gsstrt [c,y], gsstrt [c,y]], $
;      [0.0, 1.0], $
;      LINESTYLE = 0
;   XYOUTS, $
;      gsstrt [c,y] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;      'GS stsrt', $
;      ORIENTATION = 90.0, $
;      ALIGNMENT = 1.0
;
;;  Add a vertical line to indicate end of growing season:
;   OPLOT, $
;      [gsstop [c,y], gsstop [c,y]], $
;      [0.0, 1.0], $
;      LINESTYLE = 0
;   XYOUTS, $
;      gsstop [c,y] - (!X.CRANGE [1] - !X.CRANGE [0]) * 0.01, $
;      (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.97, $
;      'GS end', $
;      ORIENTATION = 90.0, $
;      ALIGNMENT = 1.0
;ENDIF
;
;DEVICE, /CLOSE
;SET_PLOT, screen

RETURN, retcode

END