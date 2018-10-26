PRO build_significant_variables
;user def parameters for 4 files
base_path='X:\WA corr analyis GIS\sahel resuts'  ;'X:\WA corr analyis GIS\results'
;fnames = ['len\len1dltXvsacc1Y_Pval_gain.img','sos\sos1dltXvsacc1Y_Pval_gain.img',$
;         'eos\eos1dltXvsacc1Y_Pval_gain.img','maxv\maxv1dltXvsacc1Y_Pval_gain.img']
fnames = ['len\len1VSacc1_Pval_gain.img','sos\sos1dltVSacc1_Pval_gain.img',$
         'eos\eos1dltVSacc1_Pval_gain.img','maxv\maxv1VSacc1_Pval_gain.img']
var = ['lem','sos','eos','peak']

ns = 7841;4716
nl = 1458;1233

;********************************************************
;open and read the p-value files
mat = FLTARR(ns, nl, 4)
tmp = FLTARR(ns, nl)
FOR i= 0, 3 DO BEGIN
  OPENR, lun, base_path+'\'+fnames[i], /GET_LUN
  READU, lun, tmp 
  mat[*, *, i] = tmp
  FREE_LUN, lun
ENDFOR

;make computations, see xls file for legend
matout = BYTARR(ns, nl) 
FOR s = 0, ns-1 DO BEGIN
  FOR l = 0, nl-1 DO BEGIN
    p_array = REFORM(mat[s, l, *])
    ind_fin = WHERE(FINITE(p_array), count_fin)
    IF (count_fin EQ 4) THEN BEGIN 
      sig_array = WHERE(p_array LT 0.05, count_sign)
      CASE count_sign OF
        0: matout[s, l] = 0     ;none
        1: matout[s, l] = REFORM(sig_array)+1 ; 1 LEN, 2 SOS, 3 EOS, 4 PEAK
        2: BEGIN
          IF ARRAY_EQUAL(sig_array, [0,1]) THEN matout[s, l] = 5 ; LEN + SOS
          IF ARRAY_EQUAL(sig_array, [0,2]) THEN matout[s, l] = 6 ; LEN + EOS
          IF ARRAY_EQUAL(sig_array, [0,3]) THEN matout[s, l] = 7 ; LEN + PEAK
          IF ARRAY_EQUAL(sig_array, [1,2]) THEN matout[s, l] = 8 ; SOS + EOS
          IF ARRAY_EQUAL(sig_array, [1,3]) THEN matout[s, l] = 9 ; SOS + PEAK
          IF ARRAY_EQUAL(sig_array, [2,3]) THEN matout[s, l] = 10 ; EOS + PEAK
        END
        3: BEGIN
          IF ARRAY_EQUAL(sig_array, [0,1,2]) THEN matout[s, l] = 11 ; LEN + SOS + EOS
          IF ARRAY_EQUAL(sig_array, [0,1,3]) THEN matout[s, l] = 12 ; LEN + SOS + Peak
          IF ARRAY_EQUAL(sig_array, [1,2,3]) THEN matout[s, l] = 13 ; SOS + EOS + PEAK
          IF ARRAY_EQUAL(sig_array, [0,2,3]) THEN matout[s, l] = 14 ; LEN + EOS + PEAK
        END
        4: matout[s, l] = 15    ;all
        ELSE: STOP
      ENDCASE
    ENDIF ELSE matout[s, l] = 255
  ENDFOR
ENDFOR

;open for output
OPENW, lun, base_path+'\unique_P_map', /GET_LUN
WRITEU, lun, matout
FREE_LUN, lun
;write hdr
OPENW, lun,base_path+'\unique_P_map.hdr', /GET_LUN
PRINTF,lun,'ENVI'
PRINTF,lun,'description = {unique p - value}'
PRINTF,lun,'samples ='+STRCOMPRESS(ns)
PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
PRINTF,lun,'bands   ='+STRCOMPRESS(1)
PRINTF,lun,'header offset = 0'
PRINTF,lun,'file type = ENVI Standard'
PRINTF,lun,'data type = 1'
PRINTF,lun,'interleave = bsq'
PRINTF,lun,'byte order = 0'
FREE_LUN, lun
END