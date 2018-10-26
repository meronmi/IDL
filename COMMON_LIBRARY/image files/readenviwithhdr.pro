FUNCTION ReadEnviWithHdr, fn

IF (!version.os_family EQ 'unix') THEN dirsep = '/' ELSE dirsep = '\'

fn_base = FILE_BASENAME(fn, '.img')
fn_base = FILE_BASENAME(fn_base, '.bil')
dir = FILE_DIRNAME(fn)

hdr_fn = dir + dirsep + fn_base + '.hdr'

ns = LONG(read_info('samples', hdr_fn))
nl = LONG(read_info('lines', hdr_fn))
nb = LONG(read_info('bands', hdr_fn))
dt = FIX(read_info('data type', hdr_fn))
mapinfo = read_info('map info',hdr_fn)
;projectioninfo = read_info('projection info', hdr_fn)
CASE dt OF
  1: data = BYTARR(ns,nl,nb)
  2: data = INTARR(ns,nl,nb)
  3: data = LONARR(ns,nl,nb)
  4: data = FLTARR(ns,nl,nb)
  5: data = DBLARR(ns,nl,nb)
  ELSE: STOP
ENDCASE
data = REFORM(data)
OPENR, lun, fn, /GET_LUN
READU, lun, data
FREE_LUN, lun
RETURN, data


END