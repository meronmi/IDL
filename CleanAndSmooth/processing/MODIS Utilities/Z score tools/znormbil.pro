FUNCTION ZNORMBIL, fanme, ns, nl, nb, periodicity, frstPeriod, frstPavgsd, lstPavgsd, noDataValue, minVal, maxVal, minVar, min5prct
COMPILE_OPT idl2
;!EXCEPT = 0



;  Purpose:
;     Compute the Z score of time series. Mean and SD are computed using only data in the specified range.
;     This range may be limited to the years having a complete set of observations to avoid that the meand and sd
;     of different periods are extracted using different number of data
;
;  Restrictions:
;     - Input file must be an integer bil (NDVI scaled to -10,000, 10,000);
;     - Periodicity restrcited to the following periods: 8 (MODIS), 15 (GIMMS-AVHRR), 16 (MODIS) , 10 (SPOT, eMODIS);
;     - No misssing bands allowed.

;  Outcome:
;     Save the historical mean (*_avg), the historical SD (*_sd) and a Z-score file (*_Z).
;     Additionally, save some diagnostics in a separate file (*_dia)
;       0: correct computation of all stats
;       200: all values were NoData
;       +1: (warning), at least one value lower than minVal or greater than maxVal was included in the Z computation
;       +10: (warning), the overall variability (95-5 percentile) was smaller than minVar
;       +100: (warning), the 5 percentile was lower than min5prct. For example: set min5prct = 5 to get those 
;                        pixels having at least a 5% value < near zero, likely water pixels in BOKU dataset, and water (having 0) in after Chen smoothing)
;       Thus, for example a value of 111 indicates that all the three conditions above hold.
;       Any value GT than 0 may strongly indicate a problem.

;  Usage:
;     rc = ZNORMBIL (fanme, ns, nl, nb, periodicity, frstPeriod, frstPavgsd, lstPavgsd, noDataValue, minVal, maxVal, minVar, min5prct)

; Example:
;     1. MODIS TERRA (21 is the first band of 2001, 296 is the last of 2012)
;     print, ZNORMBIL ('K:\ILRI\MODIS_C5\bil_files\MODIS_C5_Terra_NDVIx10000_bil_cl_Sm_Ch', 3808, 4658, 301, 16, 4, 21, 296, -999, 0, 10000, 500, 5)
;     2. MODIS TERRA BOKU (as above 21 is the first band of 2001, 296 is the last of 2012)
;     print, ZNORMBIL ('K:\ILRI\BOKU\bil files\BOKU_C5_NDVIx10000_T_sm_bil', 4208, 5148, 301, 16, 4, 21, 296, -999, 0, 10000, 500, 5)
;     3. MODIS TERRA & AQUA BOKU. Using only data from 2002 when both are present (X is the first band of 2003, Y is the last of 2012)
;     print, ZNORMBIL ('K:\ILRI\BOKU\bil files\BOKU_C5_NDVIx10000_TA_sm_bil_from_2002', 4208, 5148, 301, 8, 4, 21, 296, -999, 0, 10000, 500, 5)
;     
;  Input parameters:
;     fname {string}: The full path input bil file (can be NDVI; FAPAR, etc).
;     ns {integer}: number of samples in the input file.
;     nl {integer}: number of lines in the input file.
;     nb {integer}: number of bands in the input file.
;     periodicity {integer}: compositying period, can assume the following values:
;       8 - MODIS Terra and Aqua,   46 periods in a year
;       16 - MODIS one sensor,      23 periods in a year
;       15 - GIMMS archive,         24 periods in a yera
;       10 - Dekadal SPOT or MODIS  36 periods in a year       
;     frstPeriod {integer}: Ordinal period number of the first date (e.g., 10 for SPOT VGT starting at dek 10 in 1998)
;     frstPavgsd {integer}: Ordinal band number of the first image to be used in computing mean and sd (First band is 1) 
;     lstPavgsd {integer}: Ordinal band number of the first image to be used in computing mean and sd (First band is 1)
;     noDataValue {integer}: Value  reserved to no data. No data are simply ignored. 
;     minVal {integer}: Minimum index value, Threshold value for diagnostics
;     maxVal {integer}: Maximum index value, Threshold value for diagnostics
;     minVar {integer}: Minimum 95-5 percentile variability, Threshold value for diagnostics
;     min5prct {integer}: Minimum 5 percentile, Threshold value for diagnostics

;  Return values:
;     0: Normal completion.
;     1: File not found
;     2: Periodicity not allowed
;     3: First period not matching periodicity
;     
;  History:
;     Version: 1.0, Michele Meroni 15 April 2013
;   

t0 = SYSTIME(1)

;File specifications, open input and output files

; Input 
IF FILE_TEST(fanme) EQ 0 THEN RETURN, 1
OPENR, lun_in, fanme, /GET_LUN
line_in = ASSOC(lun_in, INTARR(ns,nb))
; Output
; Hist average
fname_avg = fanme + '_avg'
IF FILE_TEST(fname_avg) eq 1 THEN FILE_DELETE, fname_avg
OPENW, lun_avg, fname_avg, /GET_LUN, /APPEND
; Hist sd
fname_sd = fanme + '_sd'
IF FILE_TEST(fname_sd) eq 1 THEN FILE_DELETE, fname_sd
OPENW, lun_sd, fname_sd, /GET_LUN, /APPEND
;Diagnostic file
fname_dia = fanme + '_dia'
IF FILE_TEST(fname_dia) eq 1 THEN FILE_DELETE, fname_dia
OPENW, lun_dia, fname_dia, /GET_LUN, /APPEND
;Z-score
fname_z = fanme + '_Z'
IF FILE_TEST(fname_z) eq 1 THEN FILE_DELETE, fname_z
OPENW, lun_z, fname_z, /GET_LUN, /APPEND

;define the array containing the ordinal number of the each period

ord_per = INTARR(nb)
CASE periodicity OF                         ;assign periods per year
  8:  ppy = 46                                        
  16: ppy = 23
  15: ppy = 24
  10: ppy = 36
  ELSE: RETURN, 2                           ;unrecognized periodicity
ENDCASE
IF (frstPeriod GT ppy) THEN RETURN, 3       ;wrong frstPeriod 
ord_per[0] = frstPeriod
FOR i = 1, nb-1, 1L DO $
    IF ((ord_per[i-1]) LT ppy) THEN ord_per[i] = ord_per[i-1] + 1 ELSE ord_per[i] = 1 ;starts from the second period
PRINT, 'Summary of input data'
PRINT, 'Periodicity = ' + STRTRIM(periodicity,2) + ' days per year, ' + STRTRIM(ppy,2) + ' periods per year'
PRINT, 'First period in the time series  = ' + STRTRIM(frstPeriod,2)
PRINT, 'Last period in the time series = ' + STRTRIM(ord_per[nb-1],2)
PRINT, 'Period range for mean and sd computation'
PRINT, 'First band = ' + STRTRIM(frstPavgsd,2) + ' corresponding to ordinal period = ' + STRTRIM(ord_per[frstPavgsd-1],2)
PRINT, 'Last band = ' + STRTRIM(lstPavgsd,2) + ' corresponding to ordinal period = ' + STRTRIM(ord_per[lstPavgsd-1],2)
IF (ord_per[frstPavgsd-1] NE 1) OR (ord_per[lstPavgsd-1] NE ppy) THEN $
  PRINT, '*** WARNING: The first (or last) band used for AVG and SD computation does not correspond to the first (or last) band of a calendar year' 


;Loop on lines
FOR l=0, nl-1, 1L DO BEGIN
  ;var initialization, clean line and set it to NaN
  line_avg = FLTARR(ns,ppy)*!VALUES.F_NAN      
  line_sd =  FLTARR(ns,ppy)*!VALUES.F_NAN      
  line_dia = BYTARR(ns)*0                         ;here set it to correct computation and modify afterwards       
  
  ;load the line, INTARR(ns,nb), and replace noData with floating NaN (to use /NAN keyword afterwards)
  line_data = FLOAT(line_in[l])                   
  indNaN = WHERE(line_data EQ noDataValue, countNaN)
  IF (countNaN GT 0) THEN line_data[indNaN] = !VALUES.F_NAN
    
  ;Compute and save diagnostics (on all values, not only the period for hist avg and sd)
  
  indFin = WHERE(FINITE(line_data), countFin)
  IF (countFin EQ 0) THEN BEGIN                   ;treat the case where they are all NaN 
    line_dia[*] = 200
  ENDIF ELSE BEGIN    
    IF (countNaN GT 0) THEN BEGIN                 ;check for columns with all NaN
      zeros = BYTARR(ns,nb)*0
      zeros[indNaN] = 1
      ind = WHERE(TOTAL(zeros, 2) EQ nb, count)   ;all bands for these samples are NaN
      IF (count GT 0) THEN line_dia[ind] = 200
    ENDIF  
    ;check for values outside range
    zeros = BYTARR(ns,nb)*0
    ind = WHERE((line_data[indFin] LT minVal) OR (line_data[indFin] GT maxVal), count)
    IF (count GT 0) THEN zeros[indFin[ind]] = 1 
    ind = WHERE(TOTAL(zeros, 2) GE 1, count)
    IF (count GT 0) THEN line_dia[ind] = line_dia[ind] + 1
    ;check for overall variability and 5th percentile
    FOR s = 0, ns-1, 1L DO BEGIN
      IF (line_dia[s] NE 200) THEN BEGIN 
        ind = WHERE(FINITE(line_data[s,*]), count)
        data = line_data[s,ind]
        data = data[SORT(data)]
        pc5  = data[5 * N_ELEMENTS(data) / 100]
        pc95  = data[95 * N_ELEMENTS(data) / 100]
        IF (pc95-pc5 LT minVar) THEN line_dia[s] = line_dia[s] + 10
        IF (pc5 LT min5prct) THEN line_dia[s] = line_dia[s] + 100
      ENDIF
    ENDFOR  ;s 
  ENDELSE
  WRITEU, lun_dia, BYTE(line_dia)
  
  ;Compute and save AVG and SD per period using the prescribed period  
  
  FOR p=1, ppy DO BEGIN
    ;identify, for period p, which bands should be considered using ord_per
    ind = WHERE(ord_per EQ p)
    ind = ind[WHERE((ind GE frstPavgsd-1) AND (ind LE lstPavgsd-1))]
    ;avoid sending to mean all NAN values (if all are NaN, avg and sd are already NaN
    indFin = WHERE(line_dia NE 200, countFin)
    IF countFin GT 0 THEN BEGIN 
      tmp = line_data[indFin, *]
      line_avg[indFin,p-1] = MEAN(tmp[*, ind], DIMENSION=2, /DOUBLE, /NAN)
      line_sd[indFin,p-1] = STDDEV(tmp[*, ind], DIMENSION=2, /DOUBLE, /NAN)
    ENDIF
  ENDFOR
  WRITEU, lun_avg, line_avg
  WRITEU, lun_sd, line_sd
  
  ;Compute the z-scores looping on bands
  
  ;var initialization, clean line and set it to NaN
  line_z = FLTARR(ns,nb) * !VALUES.F_NAN
  FOR b=0, nb-1, 1L DO BEGIN
    ;avoid divisions by zero: water pixel may have sd = 0 because all values are set to 0, assign NaN to this Z
    ind0 = WHERE(line_sd[*,ord_per[b]-1] EQ 0, count0)
    IF (count0 GT 0) THEN BEGIN
      line_z[ind0,b] = !VALUES.F_NAN 
      ind = WHERE(line_sd[*,ord_per[b]-1] NE 0, count)
      IF (count GT 0) THEN line_z[ind,b] =  (line_data[ind,b] - line_avg[ind,ord_per[b]-1]) / line_sd[ind,ord_per[b]-1]
    ENDIF ELSE BEGIN
      line_z[*,b] = (line_data[*,b] - line_avg[*,ord_per[b]-1]) / line_sd[*,ord_per[b]-1]
    ENDELSE 
  ENDFOR  ;b
  WRITEU, lun_z, line_z
  
  ;JUNK = CHECK_MATH()
  ;IF JUNK GT 0 THEN STOP
ENDFOR  ;l


; Write hdrs
fname=fname_avg+'.hdr'
OPENW, Wh, fname, /GET_LUN
PRINTF,Wh,'ENVI'
PRINTF,Wh,'description = {Historical average considerin period '+ STRTRIM(frstPavgsd, 2)+' to '+STRTRIM(lstPavgsd,2)+'}'
PRINTF,Wh,'samples ='+STRCOMPRESS(ns)
PRINTF,Wh,'lines   ='+STRCOMPRESS(nl)
PRINTF,Wh,'bands   ='+STRCOMPRESS(ppy)
PRINTF,Wh,'header offset = 0'
PRINTF,Wh,'file type = ENVI Standard'
PRINTF,Wh,'data type = 4'
PRINTF,Wh,'interleave = bil'
PRINTF,Wh,'byte order = 0'
FREE_LUN, Wh

fname=fname_sd+'.hdr'
OPENW, Wh, fname, /GET_LUN
PRINTF,Wh,'ENVI'
PRINTF,Wh,'description = {Historical sd considering period '+ STRTRIM(frstPavgsd, 2)+' to '+STRTRIM(lstPavgsd,2)+'}'
PRINTF,Wh,'samples ='+STRCOMPRESS(ns)
PRINTF,Wh,'lines   ='+STRCOMPRESS(nl)
PRINTF,Wh,'bands   ='+STRCOMPRESS(ppy)
PRINTF,Wh,'header offset = 0'
PRINTF,Wh,'file type = ENVI Standard'
PRINTF,Wh,'data type = 4'
PRINTF,Wh,'interleave = bil'
PRINTF,Wh,'byte order = 0'
FREE_LUN, Wh

fname=fname_dia+'.hdr'
OPENW, Wh, fname, /GET_LUN
PRINTF,Wh,'ENVI'
PRINTF,Wh,'description = {Deiagnostics. 0: correct, 200: all NoData, +1: at least one LT minVal or GT maxVal, +10: 95-5 percentile LT minVar, +100: 5 percentile LT min5prct}'
PRINTF,Wh,'samples ='+STRCOMPRESS(ns)
PRINTF,Wh,'lines   ='+STRCOMPRESS(nl)
PRINTF,Wh,'bands   ='+STRCOMPRESS(1)
PRINTF,Wh,'header offset = 0'
PRINTF,Wh,'file type = ENVI Standard'
PRINTF,Wh,'data type = 1'
PRINTF,Wh,'interleave = bsq'
PRINTF,Wh,'byte order = 0'
FREE_LUN, Wh
CLOSE, /ALL

fname=fname_z+'.hdr'
OPENW, Wh, fname, /GET_LUN
PRINTF,Wh,'ENVI'
PRINTF,Wh,'description = {Z-scores}'
PRINTF,Wh,'samples ='+STRCOMPRESS(ns)
PRINTF,Wh,'lines   ='+STRCOMPRESS(nl)
PRINTF,Wh,'bands   ='+STRCOMPRESS(nb)
PRINTF,Wh,'header offset = 0'
PRINTF,Wh,'file type = ENVI Standard'
PRINTF,Wh,'data type = 4'
PRINTF,Wh,'interleave = bil'
PRINTF,Wh,'byte order = 0'
FREE_LUN, Wh
CLOSE, /ALL

; Evaluation of processing time
PRINT, 'Processing time (min): '+ STRTRIM(FIX(SYSTIME(1) - t0) / 60, 2)

RETURN, 0
END