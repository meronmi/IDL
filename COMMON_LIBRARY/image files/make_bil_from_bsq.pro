PRO make_all_in_a_dir_test
  dir_in = 'D:\ARC\2018 11 15 rasters';'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI
  files_in = FILE_SEARCH(dir_in,'*.img')
  FOR i = 0, N_ELEMENTS(files_in)-1 DO  BEGIN
    PRINT, make_bil_from_bsq(files_in[i])
  ENDFOR
END


FUNCTION make_bil_from_bsq2, fn_bsq
  ;remove_ext_from_fn(fnameY)
  fn_base = FILE_BASENAME(fn_bsq) ;this is with extension
  fn_base = remove_ext_from_fn(fn_base) ;this is without extension
  fn_dir = FILE_DIRNAME(fn_bsq)

  samples = STRTRIM(read_info('samples', fn_dir+'\'+fn_base+'.hdr'),2)
  lines = STRTRIM(read_info('lines', fn_dir+'\'+fn_base+'.hdr'),2)
  bands = STRTRIM(read_info('bands', fn_dir+'\'+fn_base+'.hdr'),2)
  dt = STRTRIM(read_info('data type', fn_dir+'\'+fn_base+'.hdr'),2)
  mapinfo = STRTRIM(read_info('map info', fn_dir+'\'+fn_base+'.hdr'),2)
  band_names = STRTRIM(read_info('band names', fn_dir+'\'+fn_base+'.hdr'),2)
  data = ReadEnviWithHdr(fn_bsq)

  OPENW, lunout, fn_dir+'\'+fn_base+'_FROM2002_bil', /GET_LUN
  FOR i = 0, lines-1 DO BEGIN
    WRITEU, lunout, data[*,i,-17:-1]
  ENDFOR
  FREE_LUN, lunout
  tmp = '{2002_endWSI, 2003_endWSI, 2004_endWSI, 2005_endWSI, 2006_endWSI, 2007_endWSI, 2008_endWSI, 2009_endWSI, 2010_endWSI, 2011_endWSI, 2012_endWSI, 2013_endWSI, 2014_endWSI, 2015_endWSI, 2016_endWSI, 2017_endWSI, 2018_endWSI}}'
  ret = write_envi_hdr(fn_dir+'\'+fn_base+'_FROM2002_bil', samples, lines, dt, NBANDS=17, INTERLEAVE='bil', MAPINFO=mapinfo, BAND_NAMES=tmp)
  RETURN, ret
END

FUNCTION make_bil_from_bsq, fn_bsq
;remove_ext_from_fn(fnameY)
fn_base = FILE_BASENAME(fn_bsq) ;this is with extension
fn_base = remove_ext_from_fn(fn_base) ;this is without extension
fn_dir = FILE_DIRNAME(fn_bsq)

samples = STRTRIM(read_info('samples', fn_dir+'\'+fn_base+'.hdr'),2)
lines = STRTRIM(read_info('lines', fn_dir+'\'+fn_base+'.hdr'),2)
bands = STRTRIM(read_info('bands', fn_dir+'\'+fn_base+'.hdr'),2)
dt = STRTRIM(read_info('data type', fn_dir+'\'+fn_base+'.hdr'),2)
mapinfo = STRTRIM(read_info('map info', fn_dir+'\'+fn_base+'.hdr'),2)
band_names = STRTRIM(read_info('band names', fn_dir+'\'+fn_base+'.hdr'),2)
data = ReadEnviWithHdr(fn_bsq)

OPENW, lunout, fn_dir+'\'+fn_base+'_bil', /GET_LUN
FOR i = 0, lines-1 DO BEGIN
  WRITEU, lunout, data[*,i,*]
ENDFOR
FREE_LUN, lunout
ret = write_envi_hdr(fn_dir+'\'+fn_base+'_bil', samples, lines, dt, NBANDS=bands, INTERLEAVE='bil', MAPINFO=mapinfo, BAND_NAMES=band_names)
RETURN, ret
END