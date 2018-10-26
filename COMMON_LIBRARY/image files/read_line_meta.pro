PRO test_meta
fn_meta = '\\ies\d5\asap\Copernicus_soil_moisture\DATA\swi_040.mta'
info = READ_META(fn_meta)

TIC
;lineProfiles = READ_LINE_META(info, 0)
OPENW, lunW, '\\ies\d5\asap\Copernicus_soil_moisture\DATA\swi_040_test_bil', /GET_LUN
FOR l = 0, info.nl -1 do begin
  lineProfiles = READ_LINE_META(info, l)
  WRITEU, lunW, lineProfiles
ENDFOR
TOC
res = write_envi_hdr( '\\ies\d5\asap\Copernicus_soil_moisture\DATA\swi_040_test_bil.hdr', info.ns, info.nl, info.dt, NBANDS=info.nb, INTERLEAVE='bil')
;PRINT, info
PRINT, 'xx'
END




FUNCTION READ_LINE_META, info, line
;read a given line of all files listed in info structure
;line starts at 0 

;Calling sequence
;1. First use READ_META that reads the metafile and store the relvant information in a stucture
;info = READ_META('\\ies\d5\asap\Copernicus_soil_moisture\DATA\swi_040.mta')
;2. Second read the z profile of the desired line using READ_LINE_META
;lineProfiles = READ_LINE_META(info, 15)  ;here it reads line 15 from all the files

;File info such as number of columns, lines, bands, datatype are stored in the structure returned by READ_META

z = MAKE_ARRAY(info.ns, info.nb, TYPE = info.dt)
z1 = MAKE_ARRAY(info.ns, TYPE = info.dt)
;size in bytes of one element 
CASE info.dt OF
  1: sz = 1
  2: sz = 2
  3: sz = 4
  4: sz = 4
  5: sz = 8
  ELSE: STOP
ENDCASE
pos = info.ns * line * sz
FOR f = 0, info.nb-1 DO BEGIN
  ;TIC
  ;this is very slow:z[*,f] = READ_BINARY(info.filenames[f], DATASTART = info.ns * line, DATA_TYPE = info.dt, DATA_DIMS=info.ns) 
  OPENR, lun, info.filenames[f], /GET_LUN
  POINT_LUN, lun, pos
  READU, lun, z1
  z[*,f] = z1
  ;z[0:info.ns-1,f] = z1
  FREE_LUN, lun 
;  TOC
ENDFOR
RETURN, z
END