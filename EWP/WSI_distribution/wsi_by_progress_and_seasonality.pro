PRO script_ritardato
;  PRINT, 'test'
;  today = SYSTIME(/JULIAN)
;  ok = 0
;  REPEAT BEGIN
;    IF SYSTIME(/JULIAN) GT today + 2 THEN ok = 1 
;    WAIT, 60*60
;  ENDREP UNTIL (ok EQ 1)
;
;  PRINT, make_bil_from_meta('Y:\meteo\GWSI\v5_25prctRoot\Africa_ROI\AAA_C1_91-16.mta', 'X:\WSI_v5_25root\WSI_v5_1991-2016C01_bil')
;  PRINT, make_bil_from_meta('Y:\meteo\GWSI\v5_25prctRoot\Africa_ROI\AAA_C2_91-16.mta', 'X:\WSI_v5_25root\WSI_v5_1991-2016C02_bil')
  wsi_by_progress_and_seasonality, '\WSI_v6_1991-2016C01_bil', 'C01'
  res = mean_wsi_by_progress_and_seasonality('C01')
  wsi_by_progress_and_seasonality, '\WSI_v6_1991-2016C02_bil', 'C02'
  res = mean_wsi_by_progress_and_seasonality('C02')
END

PRO run_it
 
  ;wsi_by_progress_and_seasonality, '\WSI_a1991-2016C02_bil', 'C02'
  ;wsi_by_progress_and_seasonality, '\WSI_a1991-2016C01_bil', 'C01'
  ;wsi_by_progress_and_seasonality, '\WSI_v4_1991-2016C02_bil', 'C02'
  wsi_by_progress_and_seasonality, '\WSI_v4_1991-2016C01_bil', 'C01'
  res = mean_wsi_by_progress_and_seasonality('C01')
END

PRO wsi_by_progress_and_seasonality, file, suffix
;this program reads a stack file of WSI and extract WSI value at the progresses
;0 25 50 75 and 100%, so 5 files and 25 bands (has I have 25 years of obs)

;stack file, must be 25 year times 36 dekads, starting dek 1 of 1991 and
;ending dek 36 of 2015, or different
;it must be a bil file
no_data_out = 252
run = 'ROI_Africav6'; 'westafrica'
CASE run OF
  'westafrica': BEGIN
    dir_in = 'Y:\meteo\GWSI\westafrica\wsi_distribution'
    dir_out = 'Y:\meteo\GWSI\westafrica\wsi_distribution'
    fn_modality = 'Y:\meteo\GWSI\westafrica\wsi_distribution\pheno stuff\vt_KK0_wa.img'
    wsi_stack_name = dir_in + '\wsi1991-2015_bil'
    ns = 5265
    nl = 1681
    nyears = 25
    ndeks = 36 
    nb = nyears * ndeks  
  END
  'ROI_Africa': BEGIN
    dir_in = 'X:\WSI_v3\STACKS'
    dir_out = 'X:\WSI_v3\STACKS\WSI_by_prog'
    fn_modality = 'Q:\GWSI\v3\ROI_Africa\KK\a_KK.img'
    ;'Y:\meteo\GWSI\westafrica\wsi_distribution\pheno stuff\vt_KK0_wa.img'
    wsi_stack_name = dir_in + file ;'\WSI_a1991-2016C02_bil'
    ns = 8177
    nl = 8233
    nyears = 26
    ndeks = 36
    nb = nyears * ndeks
  END
  'ROI_Africav4': BEGIN
    dir_in = 'X:\WSI_v4_2017_05_04\STACKS'
    dir_out = 'X:\WSI_v4_2017_05_04\STACKS\WSI_by_prog'
    fn_modality = 'Q:\GWSI\v3\ROI_Africa\KK\a_KK.img'
    ;'Y:\meteo\GWSI\westafrica\wsi_distribution\pheno stuff\vt_KK0_wa.img'
    wsi_stack_name = dir_in + file ;'\WSI_a1991-2016C02_bil'
    ns = 8177
    nl = 8233
    nyears = 26
    ndeks = 36
    nb = nyears * ndeks
  END
  'ROI_Africav5': BEGIN
    dir_in = 'X:\WSI_v5_25root'
    dir_out = 'X:\WSI_v5_25root\WSI_by_prog'
    fn_modality = 'Q:\GWSI\v3\ROI_Africa\KK\a_KK.img'
    ;'Y:\meteo\GWSI\westafrica\wsi_distribution\pheno stuff\vt_KK0_wa.img'
    wsi_stack_name = dir_in + file ;'\WSI_a1991-2016C02_bil'
    ns = 8177
    nl = 8233
    nyears = 26
    ndeks = 36
    nb = nyears * ndeks
  END
  'ROI_Africav6': BEGIN
    dir_in = 'X:\WSI_v6'
    dir_out = 'X:\WSI_v6\WSI_by_prog'
    fn_modality = 'Q:\GWSI\v3\ROI_Africa\KK\a_KK.img'
    ;'Y:\meteo\GWSI\westafrica\wsi_distribution\pheno stuff\vt_KK0_wa.img'
    wsi_stack_name = dir_in + file ;'\WSI_a1991-2016C02_bil'
    ns = 8177
    nl = 8233
    nyears = 26
    ndeks = 36
    nb = nyears * ndeks
  END
ENDCASE

FILE_MKDIR, dir_out
;make an array with dekad number
d = !NULL
FOR i = 0, nyears-1 DO d = [d, INDGEN(36)+1]

;open the modality file
modality = BYTARR(ns,nl)
OPENR, lun, fn_modality, /GET_LUN
READU, lun, modality
FREE_LUN, lun

;Run it trice for: monomodal, bimodal_first, bimodal_second
seasNames = ['mono','bi1','bi2'] 
FOR seas = 0, 2 DO BEGIN 
  ;prepare outputs
  prog = [0, 25, 50, 75, 100]
  fns_out = dir_out + '\'+seasNames[seas] + '_wsi_at_prog_' + STRTRIM(prog,2) + '_' + suffix
  ;out_mat = FLTARR(ns, nl, nyears, N_ELEMENTS(prog)) * !VALUES.F_NAN  
  out_mat =  MAKE_ARRAY(ns, nl, nyears, N_ELEMENTS(prog), /BYTE, VALUE = 252); BYTARR(ns, nl, nyears, N_ELEMENTS(prog))*no_data_out; * 0
   
  ;read and process the bil
  OPENR, lun, wsi_stack_name, /GET_LUN
  line_ass = ASSOC(lun, BYTARR(ns,nb))
  chunck10perct = ROUND(nl/10.0)
  FOR l = 0, nl-1 DO BEGIN
    IF (l MOD chunck10perct EQ 0) THEN BEGIN
      PRINT, SYSTIME()
      PRINT, 'Done: ' + STRTRIM(l/chunck10perct*10,2) + '%'
    ENDIF
    ;IF (ROUND(l/FLOAT(nl)*100) MOD 5 EQ 0) THEN PRINT, nl, l, ROUND(l/FLOAT(nl)*100)
    ;tmp = FLOAT(line_ass[l])
    tmp = line_ass[l]
    ;here i have the line, go through the pixels
    FOR s = 0, ns-1 DO BEGIN
      ;consider only mono modal at first cycle, then bi-first and then bi-second
      IF ((seas EQ 0) AND ((modality[s,l] GE 100) AND (modality[s,l] LT 200))) OR $
         ((seas GT 0) AND ((modality[s,l] GE 200) AND (modality[s,l] LT 250))) $
        THEN BEGIN
        ts = REFORM(tmp[s,*])
        ind = WHERE((ts GE 0.0) AND (ts LE 100.0), count)
        IF (count GT 0) THEN BEGIN
          ;values are present, see where the season is
          ;find the min dek, do not look at the first year that may be not complete
          ;take the full second year plus the last dek of the first (to check 
          ;the case where the season starts at dek1)
          tsy2 = ts[ndeks-1:2*ndeks-1]
          onoff = ((tsy2 GE 0.0) AND (tsy2 LE 100.0) )
          dy2 = d[ndeks-1:2*ndeks-1]
          ;proceed left to right to identify breakpoints
          foundStart = 0
          foundStop = 0
          dekstart = 0
          dekstart2 = 0
          dekstop = 0
          dekstop2 = 0
          active = 0
          FOR dd = 1, ndeks DO BEGIN
            ;is this point a start?
            IF ((onoff[dd]) AND (~onoff[dd-1])) THEN BEGIN
              IF foundStart EQ 0 THEN BEGIN
                dekstart = dy2[dd]
              ENDIF ELSE BEGIN
                dekstart2 = dy2[dd]
              ENDELSE
              foundStart = foundStart + 1
            ENDIF
            ;is this point a stop?
            IF ((~onoff[dd]) AND (onoff[dd-1])) THEN BEGIN
              IF foundStop EQ 0 THEN BEGIN
                dekstop = dy2[dd]-1
              ENDIF ELSE BEGIN
                dekstop2 = dy2[dd]-1
              ENDELSE
              foundStop = foundStop + 1
            ENDIF
          ENDFOR
          IF (foundStart GT 2) OR (foundStop GT 2) THEN STOP
          check = 0
          ;organize the four possible values
          ;monomodal case
          IF (dekstart2 EQ 0) THEN BEGIN
            IF (dekstop GT dekstart) THEN BEGIN
              ;not crossing 1
              length = dekstop - dekstart + 1
              check = 1       
            ENDIF
            IF (dekstop LT dekstart) THEN BEGIN
              ;crossing 1
              length = dekstop + (36 - dekstart + 1) 
              check = 1
            ENDIF
          ENDIF ELSE BEGIN
            ;bimodal case seas i snow 1 for the first bi-modal, and 2 for the second 
            ;find out the required start
            IF (seas EQ 1) THEN biDekStart = MIN([dekstart, dekstart2])
            IF (seas EQ 2) THEN biDekStart = MAX([dekstart, dekstart2])
            ;now find its stop
            ;is thre a stop bigger than biDekStart? It's that, and not crossing the year
            biDekStop = 0
            IF (dekstop GT biDekStart) THEN BEGIN
              biDekStop = dekstop 
            ENDIF ELSE BEGIN
              IF (dekstop2 GT biDekStart) THEN biDekStop = dekstop2
            ENDELSE
            ;no could not find it, must be crossing the year, so the stop is the smallest
            IF (biDekStop EQ 0) THEN biDekStop = MIN([dekstop, dekstop])
            dekstart = biDekStart
            dekstop = biDekStop
            IF (dekstop GT dekstart) THEN BEGIN
              ;not crossing 1
              length = dekstop - dekstart + 1
              check = 1
            ENDIF
            IF (dekstop LT dekstart) THEN BEGIN
              ;crossing 1
              length = dekstop + (36 - dekstart + 1)
              check = 1
            ENDIF
          ENDELSE
          IF (check EQ 0) THEN STOP
          ;find the first occurrence of start
          ind = WHERE(d GE dekstart)
          year1prog0_ind = MIN(ind)
          
          
          ;now get all values at the various progresses
          prog = [0, 25, 50, 75, 100]
          steps = FIX(ROUND(length * prog/100.0))  -1
          steps[0]=0 
          ;find the index of prog
          FOR p = 0, N_ELEMENTS(prog)-1 DO BEGIN
            first_ind = year1prog0_ind + steps[p]
            inds = INDGEN(nyears)*ndeks + first_ind
            indvalid = WHERE(inds LE nb)
            inds = inds[indvalid]
            n = N_ELEMENTS(inds)
            out_mat[s,l,0:n-1,p] = ts[inds]  
          ENDFOR
           
        ENDIF
      ENDIF
    ENDFOR
  ENDFOR
  
  ;make ouput bsq
  
  FOR f = 0, N_ELEMENTS(prog)-1 DO BEGIN
    OPENW, lun, fns_out[f], /GET_LUN
    dat = out_mat[*,*,*,f]
    WRITEU, lun, dat
    FREE_LUN, lun
    res = write_envi_hdr(fns_out[f], ns, nl, 1, NBANDS=nyears, INTERLEAVE=intrlv)
  ENDFOR
ENDFOR ;seas
END
