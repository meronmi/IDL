FUNCTION phenot_plt_fapar

;  Purpose:
;     To plot the original time series (products).

;  Outcome:
;     The FAPAR graph is generated on screen and a file containing the same
;     graph is created in the output directory.

;  Usage:
;     rc = phenot_plt_fapar()

;  Input parameters: None.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.

;  Remarks: None.

;  History:
;     Version 1.0: This routine is an updated version of 'gs6t_plt_fapar.pro',
;        ported to IDL 7.0 on 24 September 2008.
;     Version 1.1: Port under Suse Linux (starting February 2010).

;  Include the necessary common block(s):
@cb_os.comm
@cb_job.comm
@cb_in.comm

;  Initialize the return code:
retcode = 0

;  Set the name of the time units:
CASE period OF
   1: tunit = '[days]'
   10: tunit = '[dekads]'
   30: tunit = '[months]'
   ELSE: tunit = '[unknown]'
ENDCASE

;  Generate an array of Julian dates for the time axis of the graph. Include
;  one more day on either end to ensure they are fully taken into account
;  (Julian days start at local noon) and to include the necessary tick marks
;  and labels:
fst_day = JULDAY(1, 1, fst_year) - 1
lst_day = JULDAY(12, 31, lst_year) + 1

;  The following statement is currently specific to plot data on a 10-day interval:
xtime = TIMEGEN(START = fst_day, FINAL = lst_day, DAYS = [1, 11, 21])

;  Get information on the current screen and set the dimensions of the graphics
;  windows accordingly:
screen_dims = GET_SCREEN_SIZE(RESOLUTION = resolution)
SET_PLOT, screen
xs = FIX(screen_dims [0] / 100) * 100
ys = FIX(xs * 0.4)
!P.MULTI = [0, 0, 0, 0, 0]

;  Set the format of the X axis labels:
rc = LABEL_DATE(DATE_FORMAT = ['%M', '%Y'])

;  Loop over the products available:
nprods = ncols - 9
FOR i = 0, nprods - 1 DO BEGIN
   SET_PLOT, screen
   wintit = 'Product ' + STRTRIM(STRING(i), 2)
   WINDOW, /FREE, XSIZE = xs, YSIZE = ys, TITLE = wintit

;  Create the frame and labels:
   PLOT, $
      xtime, $
      products [*, i], $
      /NODATA, $
      XMARGIN = [10, 5], $
      XRANGE = [fst_day, lst_day], $
      XSTYLE = 1, $
      XTICKUNITS = ['Month', 'Year'], $
      XTICKFORMAT = 'LABEL_DATE', $
      XTICKLEN = 0.02, $
      XTICKINTERVAL = 3, $
      XTITLE = 'Time ' + tunit, $
      YMARGIN = [8, 4], $
      YRANGE = [ignore_below [i], ignore_above [i]], $
      YSTYLE = 1, $
      YTICKLEN = 0.02 * ys / xs, $
      YTITLE = prod_fields [i], $
      TITLE = site_name + ' [' + $
         STRTRIM(STRING(site_lat), 2) + ', ' + $
         STRTRIM(STRING(site_lon), 2) + ']'

;  Plot the original time series as a function of the first
;  day of the recording periods:
;  WARNING: If the PSYM value is changed here, update also the
;  label in the bottom left corner of the graph!
;  PSYM = 1 for '+'
;  PSYM = 2 for '*'
   OPLOT, $
      julian1, $
      products [*, i], $
      PSYM = 1, $
      SYMSIZE = 0.5, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i]

;  Superimpose a plot of the smoothed time series:
   CASE period OF
      1: BEGIN
            width = 7
         END
      10: BEGIN
            width = 5
         END
      30: BEGIN
            width = 3
         END
   ENDCASE

   smoothts = products [*, i]
   rc = smoothmv(products [*, i], ignore_below [i], ignore_above [i], width, $
      smoothts)
   OPLOT, $
      julian1, $
      smoothts, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i], $
      LINESTYLE = 0

;  Add the spatial averaging scheme in the top left corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.92, $
      'Cell: ' + STRTRIM(STRING(cell_size), 2), $
      ALIGNMENT = 0.0

;  Add the sensor name in the top right corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.98, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.92, $
      sensor, $
      ALIGNMENT = 1.0

;  Add a descriptor of the symbols used in the lower left corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.08, $
      'Data points: +', $
      ALIGNMENT = 0.0

;  Add a descriptor of the curve superimposed in the lower right corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.98, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.08, $
      'Smooth signal: from routine smoothmv', $
      ALIGNMENT = 1.0

;  ===============================================
;MM+
   wintit = 'Dek_Product ' + STRTRIM(STRING(i), 2)
   
   ;if i eq 0 then WINDOW, 9, XSIZE = xs, YSIZE = ys, TITLE = wintit else $
   WINDOW, /FREE, XSIZE = xs, YSIZE = ys, TITLE = wintit
;  Create the frame and labels:
   x2time=indgen(N_ELEMENTS(julian1))
   PLOT, $
      x2time, $
      products [*, i], $
      /NODATA, $
      XMARGIN = [10, 5], $
      XRANGE = [x2time[0],x2time[N_ELEMENTS(x2time)-1]], $
      XSTYLE = 1, $
      COLOR=30000, $
      ;XTICKUNITS = ['Month', 'Year'], $
      ;XTICKFORMAT = 'LABEL_DATE', $
      XTICKLEN = 0.02, $
      XTICKINTERVAL = 10, $
      XTITLE = 'Time ' + tunit, $
      YMARGIN = [8, 4], $
      YRANGE = [ignore_below [i], ignore_above [i]], $
      YSTYLE = 1, $
      YTICKLEN = 0.02 * ys / xs, $
      YTITLE = prod_fields [i], $
      TITLE = site_name + ' [' + $
         STRTRIM(STRING(site_lat), 2) + ', ' + $
         STRTRIM(STRING(site_lon), 2) + ']'

;  Plot the original time series as a function of the first
;  day of the recording periods:
;  WARNING: If the PSYM value is changed here, update also the
;  label in the bottom left corner of the graph!
;  PSYM = 1 for '+'
;  PSYM = 2 for '*'
   OPLOT, $
      x2time, $
      products [*, i], $
      PSYM = 1, $
      SYMSIZE = 0.5, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i]

;  Superimpose a plot of the smoothed time series:
   CASE period OF
      1: BEGIN
            width = 7
         END
      10: BEGIN
            width = 5
         END
      30: BEGIN
            width = 3
         END
   ENDCASE

   smoothts = products [*, i]
   rc = smoothmv(products [*, i], ignore_below [i], ignore_above [i], width, $
      smoothts)
   OPLOT, $
      x2time, $
      smoothts, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i], $
      LINESTYLE = 0
;MM-
;  Save the same plot in an Encapsulated PostScript graphics file in
;  the output directory. First, assemble the file name:
   fn = 'raw-data'
   SET_PLOT, 'PS'
   DEVICE, $
      FILENAME = run_dir + fn + '_p' + STRTRIM(STRING(i), 2) + '.eps', $
      /ENCAPSULATED, $
      XSIZE = 21, $
      YSIZE = 0.4 * 21
      !P.MULTI = [0, 0, 0, 0, 0]

;  Set the format of the X axis labels:
   rc = LABEL_DATE(DATE_FORMAT = ['%M', '%Y'])

;  Create the frame and labels:
   PLOT, $
      xtime, $
      products [*, i], $
      /NODATA, $
      XMARGIN = [10, 5], $
      XRANGE = [fst_day, lst_day], $
      XSTYLE = 1, $
      XCHARSIZE = 0.5, $
      XTICKUNITS = ['Month','Year'], $
      XTICKFORMAT = 'LABEL_DATE', $
      XTICKLEN = 0.02, $
      XTICKINTERVAL = 3, $
      XTITLE = 'Time', $
      YMARGIN = [8, 4], $
      YRANGE = [ignore_below [i], ignore_above [i]], $
      YSTYLE = 1, $
      YCHARSIZE = 0.8, $
      YTICKLEN = 0.02 * ys / xs, $
      YTITLE = prod_fields [i], $
      TITLE = site_name + ' [' + $
         STRTRIM(STRING(site_lat), 2) + ', ' + $
         STRTRIM(STRING(site_lon), 2) + ']'

;  Plot the original time series as a function of the first
;  day of the recording periods:
   OPLOT, $
      julian1, $
      products [*, i], $
      PSYM = 1, $
      SYMSIZE = 0.5, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i]

;  Superimpose a plot of the smoothed FAPAR time series:
   CASE period OF
      1: BEGIN
            width = 7
         END
      10: BEGIN
            width = 5
         END
      30: BEGIN
            width = 3
         END
   ENDCASE
   rc = smoothmv(products [*, i], ignore_below [i], ignore_above [i], width, $
      smoothts)
   OPLOT, $
      julian1, $
      smoothts, $
      MIN_VALUE = ignore_below [i], $
      MAX_VALUE = ignore_above [i], $
      LINESTYLE = 0

;  Add the spatial averaging scheme in the top left corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.02, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.92, $
      'Cell: ' + STRTRIM(STRING(cell_size), 2), $
      CHARSIZE = 0.6, $
      ALIGNMENT = 0.0

;  Add the sensor name in the top right corner:
   XYOUTS, $
      !X.CRANGE [0] + (!X.CRANGE [1] - !X.CRANGE [0]) * 0.98, $
      !Y.CRANGE [0] + (!Y.CRANGE [1] - !Y.CRANGE [0]) * 0.92, $
      sensor, $
      CHARSIZE = 0.6, $
      ALIGNMENT = 1.0

   DEVICE, /CLOSE
ENDFOR
SET_PLOT, screen

RETURN, retcode

END