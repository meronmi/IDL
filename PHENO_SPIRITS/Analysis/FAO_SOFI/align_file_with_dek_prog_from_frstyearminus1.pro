PRO align_file_with_dek_prog_from_frstYearMinus1
;sos
;fn_to_be_aligned = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_s_adj_anomX100_1.bil'
;gsl
fn_to_be_aligned = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_l_adj_anomX100_1.bil'
fn_out = remove_ext_from_fn(fn_to_be_aligned)+'_bandAligned2years.bil'
fn_for_aligment = 'F:\Pheno_year_by_year\Pheno\consolidated_results\BBB_adj_anom_prog_dek_from_first_year_minus1_1.bil

dt1 = FIX(read_info('data type',  remove_ext_from_fn(fn_to_be_aligned) + '.hdr'))
dt2 = FIX(read_info('data type',  remove_ext_from_fn(fn_for_aligment) + '.hdr'))
ns = LONG(read_info('samples',  remove_ext_from_fn(fn_to_be_aligned) + '.hdr'))
nl = LONG(read_info('lines',  remove_ext_from_fn(fn_to_be_aligned) + '.hdr'))
nb = FIX(read_info('bands',  remove_ext_from_fn(fn_to_be_aligned) + '.hdr'))

OPENR, lunTarget, fn_to_be_aligned, /GET_LUN
assoIN = ASSOC(lunTarget, MAKE_ARRAY(ns,nb, TYPE=dt1))
OPENR, lunTime, fn_for_aligment, /GET_LUN
assoTime = ASSOC(lunTime, MAKE_ARRAY(ns,nb, TYPE=dt2))
OPENW, lunOut, fn_out, /GET_LUN

;loop on line
FOR line = 0, nl-1, 1L DO BEGIN
  IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
  lineIn = assoIN[line]
  lineTime = assoTime[line]
  lineOut = lineTime * 0 + 9999 
  ind = WHERE(lineTime GT 0, count)
  IF (count GT 0) THEN BEGIN
    FOR sample = 0, ns-1 DO BEGIN
      zt = lineTime[sample,*]
      ind = WHERE(zt GT 0, count)
      IF (count GT 0) THEN BEGIN
        zin = lineIn[sample,*]
        zty =  FLOOR((zt[ind]-1) / 36.0) + 2002
        ztsub = FIX(zty - 2003)
        lineOut[sample,ztsub] = zin[ind]
      ENDIF
    ENDFOR
  ENDIF
  WRITEU, lunOut, lineOut 
ENDFOR

FREE_LUN, lunTarget
FREE_LUN, lunTime
FREE_LUN, lunOut
mapinfo = read_info('map info',  remove_ext_from_fn(fn_to_be_aligned) + '.hdr')
res = write_envi_hdr(remove_ext_from_fn(fn_out), ns, nl, dt1, NBANDS=14, INTERLEAVE='bil', $
  MAPINFO=mapinfo)
END