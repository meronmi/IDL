Function phenot_funct, y, yJulDay
;  Purpose:
;     To launch phenot on a vector y (time profile)
;     no data has to be NaN
;  Usage:
;     re= phenot_funct()

;  Input parameters: None.
;     y: the time series array
;     npts: the number of bands (dekads) in the time profile
;     ign_b: value below which ignore data (typically 0.0)
;     ign_a: value above which ignore data (for fapar typically 1.0)
;     fst_year: first year of the anyasys (e.g. 2001)
;     lst_year: last year of the anyasys (e.g. 2010)

;  Return values:
;     ret: structure containing all relevant info
;     -1: an error was encountered
;  Example:
;     
;  History:
;     Version 1.0: created by MM
;     Version 1.1: MM, requires the acq vector containing the Hulian day of acquisition
;common blocks
@cb_in.comm
@cb_job.comm
@cb_options.comm
@cb_os.comm   ;use for plotting, if required
@cb_pdhtf.comm
;NaN are not handled by MV code. set them to -999 which is handled (is below ibel)
miss = -999
fapar = y
acq = yJulDay
;  Define the array that will contain the optimally estimated FAPAR
;  values during the various growing seasons:
fitted = FLTARR(npts)
nan_ind=where(finite(fapar) eq 0, count_nan)
if (count_nan gt 0) then fapar[nan_ind]=miss
;convert all ibel iabo to NaN for use of standard IDL routines
nanfapar=fapar
ind_nan=where((fapar lt ibel) or (fapar gt iabo), count_nan)
if (count_nan gt 0) then nanfapar[ind_nan]=!VALUES.F_NAN
empty_season = 0
MNGS = 3    ;minimum number of occurrence of a growing season to say that it exists 
            ;
;-------------------------------------------------------------------------------------------------------
;THIS PART IS TO ALLOW PLOTTING OF PHENO ANALYS, IT IS USED FOR DEBUG, SET pltres to 1 if PLOT REQUESTED
;pltres=0
IF (pltres EQ 1) THEN BEGIN
  wantplotgs=1
  site_name='Sample'
  site_lat='X'
  site_lon='Y'
  screen = 'Win'
  dirsep = '\'
  data_fname='tmp'
  run_dir='D:\Users\meronmi\Documents\IDL\MV Pheno\Products'
  prod_fields=['000','000']
  products=FLTARR(N_ELEMENTS(y),2)
  products[*,0]=fapar
  
END
;-------------------------------------------------------------------------------------------------------




;run the analysis
rc2 = phenot_p2()

; error codes:
; 10 More than 40% of the values in the input record are invalid
; 20 [95th - 5th] percentile range is too low, less than faprangeminthresh
; 30 unrecognized data periodicity
; 40 anomalous number of gs was found (lt 1 or gt 2)
;if rc2 eq 0 then rc3 = phenot_p3_mm()
;IF (rc2 EQ 0) OR (rc2 EQ 1) then rc3 = phenot_p3_mm()
IF (rc2 EQ 0) OR (rc2 EQ 1) THEN BEGIN
  rc3 =  phenot_p3_pdhtf_mm()
  IF (rc3 EQ 10) THEN BEGIN
    ;lombrat found 2 GS, but when actually fitting one (two) was never found (empty_season > 0)
    ;rerun stat again by forcing ngspy = 1 (do not use lomb anymore, just set it
    rc2 = phenot_p2()
    IF (rc2 EQ 0) OR (rc2 EQ 1) THEN BEGIN
      rc3 =  phenot_p3_pdhtf_mm()
      ;debug
      IF (rc3 EQ 10) THEN STOP
    ENDIF
  ENDIF
ENDIF
if (pltres eq 1) THEN BEGIN
  IF (rc2 EQ 0) OR (rc2 EQ 1) then BEGIN
    rc=phenot_plt_all_gsJD_wback()
    ;FOR PAPER: 
    ;rc=phenot_plt_all_gs_wback_paper_save_last2()
    CELL_SIZE='1000 m'
    sensor='VGT'
    IGNORE_BELOW=ibel
    IGNORE_ABOVE=iabo
    ncols=10
    rc = phenot_plt_fapar_wback() 
  ENDIF ELSE BEGIN
    ;rc = phenot_plt_fapar_wback() 
    PRINT, 'Phenot2 could not come to normal completion, nothing more to bet ploted'
  ENDELSE
END
RETURN, rc2
End