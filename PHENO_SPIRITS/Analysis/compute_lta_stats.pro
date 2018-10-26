PRO compute_LTA_stats
;compute max, min and range of an LTA. This stats are used by pheno as preliminary tests to exclude pixels
;from SPF
;===============================================================================
;Preliminary elimination of pixels without seasonality (Ymax and Ymin are computed over the 36 dekads [37-72] of the central year)
;FEN0Max  = 0.180   If Ymax        < FEN0Max => No seasonality (deserts)
;FEN0Min  = 0.80    ***************[0.75]If Ymin        > FEN0Min => No seasonality (evergreen)
;FEN0Rng  = 0.04    ***************[0.075] If (Ymax-Ymin) < FEN0Rng => No seasonality (variability too low)
;-------------------------------------------------------------------------------

;lta MUST be a scaled bil (physical units, no data as NAN)
fn_lta = '\\ies\d5\asap\boku\V1\pheno\LTA2002-2016\AAA_OF_LTA2002_2016_bil_sc'
dt = FIX(read_info('data type',  remove_ext_from_fn(fn_lta) + '.hdr'))
IF (dt NE 4) THEN STOPO
ns = LONG(read_info('samples',  remove_ext_from_fn(fn_lta) + '.hdr'))
nl = LONG(read_info('lines',  remove_ext_from_fn(fn_lta) + '.hdr'))
nb = FIX(read_info('bands',  remove_ext_from_fn(fn_lta) + '.hdr'))
IF (nb NE 36) THEN STOP
;open the LTA
OPENR, lun, fn_lta, /GET_LUN
asso = ASSOC(lun, MAKE_ARRAY(ns,nb, TYPE=dt))
fn_base_out = fn_lta
STRREPLACE, fn_base_out, '_bil', '' 
OPENW, lunMin, fn_base_out + '_min', /GET_LUN
OPENW, lunMax, fn_base_out + '_max', /GET_LUN
OPENW, lunRange, fn_base_out + '_range', /GET_LUN
OPENW, lunExcl, fn_base_out + '_excluded_by_SPF_initial_test_max_range', /GET_LUN

FOR line = 0, nl-1, 1L DO BEGIN
  IF ((line MOD (nl/10)) EQ 0) THEN PRINT, 'Processing, '+string(line/float(nl)*100)+'% at '+string(SYSTIME(0))
  x = asso[line]
  minx = MIN(x, DIMENSION=2, MAX=maxx, /NAN)
  WRITEU, lunMin, minx
  WRITEU, lunMax, maxx
  WRITEU, lunrange, maxx-minx
  tmp = minx * 0
  indFin = WHERE(FINITE(minx))
  ind = WHERE((maxx[indFin] LT 0.18) OR (maxx[indFin]-minx[indFin]) LT 0.04, count)
  IF (count GT 0) THEN tmp[indFin[ind]] = 1
  WRITEU, lunExcl, tmp
ENDFOR  

mapinfo = read_info('map info',  remove_ext_from_fn(fn_lta) + '.hdr')
res = write_envi_hdr(fn_base_out + '_min'+'.hdr', ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
res = write_envi_hdr(fn_base_out + '_max'+'.hdr', ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
res = write_envi_hdr(fn_base_out + '_range'+'.hdr', ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)
res = write_envi_hdr(fn_base_out + '_excluded_by_SPF_initial_test_max_range'+'.hdr', ns, nl, 4, INTERLEAVE='bsq', MAPINFO=mapinfo)

END