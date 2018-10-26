

PRO build_ewp_images, half_life, option_weight

;SPI was computed on Tamsat resolution and resampled to VGT after
;here we do the same, we resample Tav at Tamsat, we compute EWP and Standardized EWP,
;only after we resample at VGT resolution
;same time span 1984-2013 used for SPI computation
;as minimum Tav is from 1989 we do 1989 - now

;option_weight = 'const';'const';'exp' ;'const' ;to make exp weightin or constant weight

;be care, I could have missing images, expetially in tamsat.
;what do I do? intepolate?

fn_rain = 'E:\WA\EWP\TAMSAT ROI WA\AAA_Tamsat_89-13_bil'
CASE option_weight OF
  'exp': fn__base_out = 'E:\WA\EWP\EWP_images\ewp'
  'const': fn__base_out = 'E:\WA\EWP\EWP_images\buttami_cwp'
  ELSE: STOP
ENDCASE
;fn_tav = 'E:\WA\EWP\ECMWF_Tav_resampled2TAMSAT\AAA_int_tav_89-13_bil'
fn_tav = 'E:\WA\EWP\ECMWF_Tav_res_invdist1_2_TAMSAT\AAA_int_tav_89-13_InvDistRes_bil'
ns = 1867
nl = 348
nb = 900
;Tamsat has missing data
nb_tamsat = 890
Years = [1989, 2013]
Dekads = [1, 36]


;half_life = 800;half_life_define_all()     ;half life in degree days, in steps of 10 days of 30 deg
pcap = 100.0


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

;waiting for Gabor

OPENR, R2, fn_tav, /GET_LUN
line_ass_tav = ASSOC(R2, FLTARR(ns,nb))
timeArrayTav = extractTimeFromHdr(fn_tav + '.hdr', 'tav'); 3 columns (YYYY, TT, number of days in a dkad, nb rows

;open output
n = N_ELEMENTS(half_life)
;line ewp
ewp = FLTARR(ns,nb,n)*!VALUES.F_NAN
;files bil for output
lunstack = !NULL
FOR h = 0, N_ELEMENTS(half_life)-1 DO BEGIN
  fn_out = fn__base_out + '_hl' + STRTRIM(FIX(half_life[h]),2) + '_bil'
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
    rfilled[*,posmiss[m]] = ROUND(lta_line[*,dekmiss[m]-1]) 
  ENDFOR
  r = rfilled
  
;  ;now fill the gap by interpolation
;  rfilled[*,posok]= r
;  rfilled2 = rfilled
;  FOR vv = 0, N_ELEMENTS(rfilled[*,0])-1 DO rfilled2[vv,*] = INTERPOL(rfilled[vv,*], nb, /NAN)
;  ;first values interpolated can be negative
;  indFin = WHERE(FINITE(rfilled2))
;  ind = WHERE(rfilled2[indFin] LT 0, countneg)
;  IF (countneg GT 0) THEN rfilled2[indFin[ind]] = 0.0
;  ;copy back where data were present
;  rfilled2[*,posok] = r
;  r = rfilled2
;  count_NaN  = 0

  t = FLOAT(line_ass_tav[l]) ;tav
  indNan = WHERE((t GT 100.0) OR (t LT -100.0), count_NaN)
  IF (count_NaN GT 0) THEN t[indNan] = !VALUES.F_NAN
  count_NaN  = 0
  
  ;loop on samples
  FOR s = 0, ns-1 DO BEGIN 
    ;IF ((s MOD 100) EQ 0) THEN PRINT, 'Sample: ' + STRTRIM(s,2)
    IF ((s EQ 94) AND (l EQ 0)) THEN BEGIN
      PRINT, 'Debug'
    ENDIF
    rr = r[s,*]
    tt = t[s,*]
    ;note that tt is the daily Tavarage fof a giben dekad, so it must be multiplyed by the number of days
    tt = tt * timeArrayTav[2,*]
    IF (TOTAL(FINITE(tt)) EQ N_ELEMENTS(tt)) AND ((TOTAL(FINITE(rr)) EQ N_ELEMENTS(rr))) THEN BEGIN 
      ;loop on different half_life
      FOR h = 0, N_ELEMENTS(half_life)-1 DO BEGIN
;        PRINT, 'debug'
;        TIC
;        ewp[s,*,h] = BackwardExpWeightedMean_FAST (0, tt, rr, half_life[h], pcap)
;        TOC
;        PRINT, 'debug'
;        TIC
        ewp[s,*,h] = BackwardExpWeightedMean_FAST2(0, tt, rr, half_life[h], pcap, option_weight)
;        TOC
;        PRINT, 'debug'
      ENDFOR 
    ENDIF ELSE ewp[s,*,*] = !VALUES.F_NAN
  ENDFOR ;s
  FOR h = 0, N_ELEMENTS(half_life)-1 DO BEGIN
    WRITEU, lunstack[h], ewp[*,*,h]
  ENDFOR
ENDFOR ;l

;here I have to write the hdr

FOR h = 0, N_ELEMENTS(half_life)-1 DO BEGIN
  fn_out = fn__base_out + '_hl' + STRTRIM(FIX(half_life[h]),2) + '_bil' 
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

