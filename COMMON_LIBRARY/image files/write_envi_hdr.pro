FUNCTION write_envi_hdr, fn, nsamples, nlines, dt, DIR = dir, NBANDS=nbands, INTERLEAVE=intrlv, $
         MAPINFO=mapinfo, COORDINFO=coordinfo, PROJECTIONINFO=projectioninfo, $
         FREE_TEXT = free_text, FLAGS = flags, BAND_NAMES=band_names
         
;Usage:
;res =write_envi_hdr(filename, nsamples, nlines, datatype, DIR = dir, NBANDS=nbands, INTERLEAVE=intrlv, MAPINFO=mapinfo, COORDINFO=coordinfo) 


;if a dir was provided
IF KEYWORD_SET(dir) THEN fn = dir + '\' + fn
;check if the filename has already ".hdr" extension
tmp = STRSPLIT(fn, '.', /EXTRACT)
IF N_ELEMENTS(tmp) GT 1 THEN BEGIN
  IF tmp[1] NE 'hdr' THEN BEGIN
    PRINT, 'ERROR: write_envi_hdr.pro called with wrong extention'
    PRINT, tmp[1]
    STOP
  ENDIF
ENDIF ELSE BEGIN
  fn = fn +'.hdr'
ENDELSE
intl = 'bsq'
IF KEYWORD_SET(intrlv) THEN intl = intrlv
OPENW, lun, fn, /GET_LUN
PRINTF, lun, 'ENVI'
PRINTF, lun, 'samples = ' + STRTRIM(nsamples,2)
PRINTF, lun, 'lines = ' + STRTRIM(nlines,2)
IF KEYWORD_SET(nbands) THEN PRINTF, lun, 'bands = ', + STRTRIM(nbands,2) $
  ELSE PRINTF, lun, 'bands = 1'
PRINTF, lun, 'data type  = ' + STRTRIM(dt,2)
PRINTF, lun, 'file type = ENVI Standard'
PRINTF, lun, 'interleave = ', +STRTRIM(intl,2)
PRINTF, lun, 'byte order = 0'
IF KEYWORD_SET(mapinfo) THEN PRINTF, lun, 'map info = ' + mapinfo
IF KEYWORD_SET(coordinfo) THEN PRINTF, lun, 'coordinate system string = ' + coordinfo
IF KEYWORD_SET(projectioninfo) THEN PRINTF, lun, 'projection info  = ' + projectioninfo
IF KEYWORD_SET(free_text) THEN PRINTF, lun, STRTRIM(free_text,2)
IF KEYWORD_SET(flags) THEN PRINTF, lun, 'flags  = {' + flags + '}'
IF KEYWORD_SET(BAND_NAMES) THEN BEGIN
  PRINTF, lun, 'band names = {'
  FOR n = 0, N_ELEMENTS(band_names)- 1 DO BEGIN
    IF (n NE N_ELEMENTS(band_names)- 1) THEN BEGIN
      PRINTF, lun, band_names[n] + ','
    ENDIF ELSE BEGIN
      PRINTF, lun, band_names[n] + '}'
    ENDELSE
  ENDFOR
ENDIF
FREE_LUN, lun
RETURN, 0
END