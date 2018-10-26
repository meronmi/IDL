FUNCTION ReadBilWithHdr, fn
fn_base = FILE_BASENAME(fn, '.bil')
fn_base = FILE_BASENAME(fn_base, '.img')
dir = FILE_DIRNAME(fn)

hdr_fn = dir + '\' + fn_base + '.hdr'

ns = LONG(read_info('samples', hdr_fn))
nl = LONG(read_info('lines', hdr_fn))
nb = LONG(read_info('bands', hdr_fn))
dt = FIX(read_info('data type', hdr_fn))
mapinfo = read_info('map info',hdr_fn)
interleave  = STRTRIM(read_info('interleave', hdr_fn),2)
IF (interleave NE 'bil') THEN BEGIN
  PRINT, 'ReadBilWithHdr.pro. Error, ' + fn + ' is not bil, check hdr.'
  STOP
ENDIF
;projectioninfo = read_info('projection info', hdr_fn)
data = REFORM(MAKE_ARRAY(ns,nl,nb, TYPE = dt))



OPENR, lun, fn, /GET_LUN
line = ASSOC(lun, MAKE_ARRAY(ns, nb, TYPE = dt))
FOR i = 0, nl-1 DO BEGIN
  data[*,i,*] = line[i]
ENDFOR
FREE_LUN, lun
RETURN, data


END