PRO map_pix_95_5_prct_range
;18/09/2018
;utility for pheno. maps the 95 minus 5 percentile of the bil file used to compute pheno

fn =  'Z:\LTDR\LTDR10day_MVC_NDVIstack_V5screened_v3_sc'
dir = FILE_DIRNAME(fn)
fn = FILE_BASENAME(fn, '.bil')


hdr_fn = dir + '\' + fn + '.hdr'

ns = LONG(read_info('samples', hdr_fn))
nl = LONG(read_info('lines', hdr_fn))
nb = LONG(read_info('bands', hdr_fn))
dt = FIX(read_info('data type', hdr_fn))
mapinfo =''
interleave  = STRTRIM(read_info('interleave', hdr_fn),2)
IF (interleave NE 'bil') THEN BEGIN
  PRINT, 'ReadBilWithHdr.pro. Error, ' + fn + ' is not bil, check hdr.'
ENDIF

OPENR, lun, dir + '\' + fn, /GET_LUN
fn_out = fn + ' _95_5_prct_range' 
OPENW, lunW, dir + '\' + fn_out, /GET_LUN
line = ASSOC(lun, MAKE_ARRAY(ns, nb, TYPE = dt))
minnumval = LONG(nb * 0.6)
FOR i = 0, nl-1 DO BEGIN
  IF ((i MOD 100) EQ 0) THEN PRINT, i
  y = line[i]
  lineRange = FLTARR(ns)*!VALUES.F_NAN
  FOR j = 0, ns-1 DO BEGIN
    tmp = y[j,*]
    indFin = WHERE(FINITE(tmp),countFin)
    ;check that I have more than 60% of values
    
    IF (countFin GE minnumval) THEN BEGIN
      rc05 = prctl(0.05, tmp[indFin], 0, 0.0, 1.0, 0, y05pctl)
      rc95 = prctl(0.95, tmp[indFin], 0, 0.0, 1.0, 0, y95pctl)
      if ((rc05 NE 0) OR (rc95 NE 0)) THEN STOP
      lineRange[j] = y95pctl - y05pctl
    ENDIF
  ENDFOR
  WRITEU, lunW, lineRange
ENDFOR
FREE_LUN, lun
FREE_LUN, lunW
res = write_envi_hdr(dir + '\' + fn_out, ns, nl, dt,  MAPINFO=mapinfo)

END
