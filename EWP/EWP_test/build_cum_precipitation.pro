FUNCTION cum_forward, x
res = REFORM(x * !VALUES.F_NAN)
FOR i = 0, (N_ELEMENTS(x)-1)-2 DO BEGIN
  res[i] = TOTAL(x[i:i+2])
ENDFOR
RETURN, res
END


PRO build_cum_precipitation
fn_rain = 'E:\WA\EWP\TAMSAT ROI WA\AAA_Tamsat_89-13_bil'
fn__base_out = 'E:\WA\EWP\TAMSAT_dek_monthly_sum\sumP
n_dek_array = 3 ;array of cumulation periods
ns = 1867
nl = 348
nb = 900
;Tamsat has missing data
nb_tamsat = 890
Years = [1989, 2013]
Dekads = [1, 36]


;open the rain and t
OPENR, R1, fn_rain, /GET_LUN
line_ass_rain = ASSOC(R1, INTARR(ns,nb_tamsat))
rfilled =  INTARR(ns,nb) * !VALUES.F_NAN
timeArrayRain = extractTimeFromHdr(fn_rain + '.hdr', 'tamsat_'); 3 columns (YYYY, TT, number of days in a dkad, nb rows
;find where missing
posok = !Null
posmiss = !Null
dekmiss = !Null
c = -1

FOR y = Years[0], Years[1] DO BEGIN
  CASE y OF
    Years[0]: BEGIN
      d_start = Dekads[0]
      d_stop = 36
      END
    Years[1]: BEGIN
      d_start = 1
      d_stop = Dekads[1]
      END
    ELSE: BEGIN
      d_start = 1
      d_stop = 36
      END
  ENDCASE
  FOR d = d_start, d_stop DO BEGIN
    c = c + 1
    ind = WHERE((timeArrayRain[0,*] EQ y) AND (timeArrayRain[1,*] EQ d), count_match)
    IF (count_match NE 0) THEN posok = [posok, c] 
    IF (count_match EQ 0) THEN BEGIN
      PRINT, 'missing ' + STRTRIM(y,2) + '; ' + STRTRIM(d,2)
      posmiss = [posmiss, c]
      dekmiss = [dekmiss, d]
    ENDIF
  ENDFOR
ENDFOR
;print, posok  ;posok gives the position (in the array with all require data) of the available data 
;print, posmiss



;open output
n = N_ELEMENTS(n_dek_array)
;line ewp
sumP = FLTARR(ns,nb,n)*!VALUES.F_NAN
;files bil for output
lunstack = !NULL
FOR h = 0, N_ELEMENTS(n_dek_array)-1 DO BEGIN
  fn_out = fn__base_out + '_length' + STRTRIM(FIX(n_dek_array[h]),2) + '_bil'
  OPENW, lun, fn_out, /GET_LUN
  lunstack = [lunstack, lun]
ENDFOR
TIC
;loop on all lines
FOR l = 0, nl-1 DO BEGIN
  IF ((l MOD 10) EQ 0) THEN BEGIN
    TOC
    PRINT, 'Line: ' + STRTRIM(l,2)
    TIC
  ENDIF
  rfilled[*,*] = !VALUES.F_NAN
  r = FLOAT(line_ass_rain[l]) ;rain
 
  ;scale and treat NaN
  indNan = WHERE((r GT 2000) OR (r LT 0), count_NaN)
  IF (count_NaN GT 0) THEN r[indNan] = !VALUES.F_NAN
  ;now fill the gaps using long term average
  ;compute LTA
  lta_line = FLTARR(ns,36)*!VALUES.F_NAN
  FOR tt = 1, 36 DO BEGIN
    ;timeArrayRain[1] is the dekad number
    ind = WHERE (timeArrayRain[1,*] EQ tt)
    ;to avoi making the mean on all NAN that rise an error
    rf = FINITE(r[*,ind])
    rf = TOTAL(rf,2)
    indCol = WHERE(rf GT 0)
    tmp = r[indcol,*]
    lta_line[indcol,tt-1] = MEAN(tmp[*,ind], /NAN,DIMENSION = 2)
;    PRINT, max(lta_line[*,tt-1], /NAN)
;    PRINT, 'debug'
  ENDFOR
  rfilled[*,posok]= r
  FOR m = 0, N_ELEMENTS(posmiss)-1 DO BEGIN
    rfilled[*,posmiss[m]] = lta_line[*,dekmiss[m]-1] 
  ENDFOR
  r = rfilled
  
  ;loop on samples
  FOR s = 0, ns-1 DO BEGIN 
    rr = r[s,*]
    IF ((TOTAL(FINITE(rr)) EQ N_ELEMENTS(rr))) THEN BEGIN 
      ;loop on different half_life
      FOR h = 0, N_ELEMENTS(n_dek_array)-1 DO BEGIN
        sumP[s,*,h] = cum_forward(rr)
      ENDFOR 
    ENDIF ELSE sumP[s,*,*] = !VALUES.F_NAN
  ENDFOR ;s
  FOR h = 0, N_ELEMENTS(n_dek_array)-1 DO BEGIN
    WRITEU, lunstack[h], sumP[*,*,h]
  ENDFOR
ENDFOR ;l

;here I have to write the hdr

FOR h = 0, N_ELEMENTS(n_dek_array)-1 DO BEGIN
  fn_out = fn__base_out + '_length' + STRTRIM(FIX(n_dek_array[h]),2) + '_bil'
  OPENW, lun, fn_out + '.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  PRINTF, lun, 'bands = ' +  STRTRIM(nb, 2)
  PRINTF, lun, 'interleave = bil'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun
  FREE_LUN, lunstack[h]
ENDFOR

END
