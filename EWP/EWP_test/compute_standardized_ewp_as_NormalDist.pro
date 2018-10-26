PRO compute_standardized_ewp_as_NormalDist, fn_base_out, fn_ewp_in, half_life
;mask_dir = 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip'
;fn_intersection = 'eco_crop_pasture' ;1 is grasslan, 2 is crop
;fn_rain = 'E:\WA\EWP\TAMSAT_resampled2VGT\AAA_tamsat_rain_9816_1336_bil'

;fn_base_out = 'E:\WA\EWP\EWP_images\Nstandardized_ewp_hl'
;fn_ewp_in = 'E:\WA\EWP\EWP_images\ewp_hl'
suffix_in = '_bil'
;half_life = 1500;half_life_define_all()
ns = 1867
nl = 348
nb = 900
Years = [1999, 2013]
Dekads = [1, 36]
;note: ewp can use data in the past so it is unreliable at the beginning of the series (in 89)
skip_this_dek_at_beginning = 36

;retrieve dekad TT from original ddata
fn_tav = 'E:\WA\EWP\ECMWF_Tav_res_invdist1_2_TAMSAT\AAA_int_tav_89-13_InvDistRes_bil'
timeArray = extractTimeFromHdr(fn_tav + '.hdr', 'tav') ; 3 columns (YYYY, TT, number of days in a dkad, nb rows



FOR h = 0, N_ELEMENTS(half_life)-1 DO BEGIN 
  PRINT, 'Halflife: ' + STRTRIM(h,2) + ' of ' + STRTRIM(N_ELEMENTS(half_life),2) 
  ;open one of the ewp files (bsq)
  OPENR, lun, fn_ewp_in + STRTRIM(FIX(half_life[h]),2) + suffix_in, /GET_LUN
  line_ass_ewp = ASSOC(lun, FLTARR(ns,nb))
  
  
  standard_ewp = FLTARR(ns,nl,36*15); the output is from 1999 to 2013
  
  ;compute the standardization  
  ;loop on all lines
  TIC
  FOR l = 0, nl-1 DO BEGIN
    IF ((l MOD 10) EQ 0) THEN BEGIN
      TOC
      PRINT, 'Line: ' + STRTRIM(l,2)
      TIC
    ENDIF
    ewp = FLOAT(line_ass_ewp[l]) ;ewp
    FOR s = 0, ns-1 DO BEGIN
      IF (s EQ 100) AND (l EQ 141) THEN BEGIN
        PRINT, 'Debug'
      ENDIF
      FOR TT = 1, 36 DO BEGIN
        ;find all the images of a given dekad
        Ttmp = REFORM(timeArray[1, skip_this_dek_at_beginning: *])
        indTT = WHERE(Ttmp EQ TT)
        indTT = indTT + skip_this_dek_at_beginning
        tmp = NDIST_Zscore(ewp[s,indTT], MIN_POSOBS = 3)
        ;here I shuold have all the years
        ;take last 15
        tmp = tmp[-15:-1]
        iind = INDGEN(15)*36+TT-1
        standard_ewp[s,l,iind] = tmp
      ENDFOR ;TT
    ENDFOR; s
  ENDFOR ;l
  FREE_LUN, lun


  ;write the the big multiband bil file  
  OPENW, lunw, fn_base_out +  STRTRIM(FIX(half_life[h]),2), /GET_LUN
  FOR l = 0, nl-1 DO BEGIN
    WRITEU, lunw, standard_ewp[*,l,*] ;its a bsq
  ENDFOR
  FREE_LUN, lunw
  ;write the hdr
  OPENW, lun, fn_base_out +  STRTRIM(FIX(half_life[h]),2) + '.hdr', /GET_LUN
  PRINTF, lun, 'ENVI'
  PRINTF, lun, 'file type = ENVI standard'
  PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
  PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
  bands = 15*36
  PRINTF, lun, 'bands = ' + STRTRIM(bands,2)
  PRINTF, lun, 'interleave = bil'
  PRINTF, lun, 'data type = 4'
  FREE_LUN, lun

  write_spirits_format = 0
  IF (write_spirits_format) THEN BEGIN    
    ;now write single bands file in spirits format for reampling
    i = -1
    FOR yy = Years[0], Years[1] DO BEGIN
      FOR d = Dekads[0], Dekads[1] DO BEGIN
        i = i + 1
        ;form YYYYTT label
        label = STRTRIM(yy,2)
        IF (d LT 10) THEN label = label + '0' + STRTRIM(d,2)  ELSE label = label + STRTRIM(d,2)  
        ;write file
        OPENW, lunw, fn_base_out +  STRTRIM(FIX(half_life[h]),2) + '_' + label, /GET_LUN
        tmp = standard_ewp[s,l,i]
        ind = WHERE(~FINITE(tmp), countNaN)
        IF (countNaN GT 0) THEN tmp[ind] = -9999
        WRITEU, lunw, tmp ;its a bsq
        FREE_LUN, lunw
        ;write hdr
        OPENW, lunw, fn_base_out +  STRTRIM(FIX(half_life[h]),2) + '_' + label + '.hdr', /GET_LUN
        PRINTF, lun, 'ENVI'
        PRINTF, lun, 'file type = ENVI standard'
        PRINTF, lun, 'data type = 4'
        PRINTF, lun, 'values = {SEWP, -, -10000, 10000, , , 0, 1.0}'
        PRINTF, lun, 'samples = ' + STRTRIM(ns, 2)
        PRINTF, lun, 'lines = ' + STRTRIM(nl, 2)
        PRINTF, lun, 'bands = ' + STRTRIM(1,2)
        PRINTF, lun, 'interleave = bsq'
        PRINTF, lun, 'flags = {-9999 = no data}'
        
        FREE_LUN, lun
      ENDFOR
    ENDFOR ;yy
  ENDIF
ENDFOR ;h

  
  
  

PRINT, 'Finished'

END