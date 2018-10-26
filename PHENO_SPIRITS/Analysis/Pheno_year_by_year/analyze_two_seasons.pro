;PRINT, make_bil_from_meta('\\ies\d5\asap\Pheno_year_by_year\Pheno\BBB_meta_kk0.mta', '\\ies\d5\asap\Pheno_year_by_year\Pheno\BBB_kk0.bil')
PRO analyze_two_seasons
dir = '\\ies\d5\asap\Pheno_year_by_year\Pheno\'
fn = dir + 'BBB_kk0.bil'
;ngspy = ReadBilWithHdr(fn)
fn_base = FILE_BASENAME(fn, '.bil')
hdr_fn = dir + '\' + fn_base + '.hdr'
ns = LONG(read_info('samples', hdr_fn))
nl = LONG(read_info('lines', hdr_fn))
nb = LONG(read_info('bands', hdr_fn))
dt = FIX(read_info('data type', hdr_fn))
mapinfo = read_info('map info',hdr_fn)
interleave  = STRTRIM(read_info('interleave', hdr_fn),2)
IF (interleave NE 'bil') THEN BEGIN
  PRINT, 'ReadBilWithHdr.pro. Error, ' + fn + ' is not bil, check hdr.'
ENDIF

nbimodal = BYTARR(ns, nl)

OPENR, lun, fn, /GET_LUN

line = ASSOC(lun, MAKE_ARRAY(ns, nb, TYPE = dt))
FOR i = 0, nl-1 DO BEGIN
  IF ((i MOD 1000.0) EQ 0) THEN PRINT, i  
  data = line[i]
  ind = WHERE((data GE 200) AND (data LT 250), count)
  tmp = data[*,0]
  indW1D = WHERE((tmp GE 250), countN)
  data[*,*] = 0
  IF (count GT 0) THEN data[ind] = 1
  ;to set 255 for water
  nbimodal[*,i] = TOTAL(data, 2)
  IF (countN GT 0) THEN nbimodal[indW1D,i] = 255 
ENDFOR
FREE_LUN, lun


;ind = WHERE((ngspy GE 100) AND (ngspy LT 200), count) 
;ngspy[ind] = 1
;ind = WHERE((ngspy GE 200) AND (ngspy LT 250), count)
;ngspy[ind] = 2
;ind = WHERE((ngspy LT 100) OR (ngspy GE 250), count)
;ngspy[ind] = 0
;maxNgspy = MAX(ngspy, DIMENSION = 3)
;
;;set the two as 1 and rest to 0 to get the total number of years with two seasons
;ind = WHERE(ngspy EQ 2, count, COMPLEMENT=indN2)
;ngspy[ind] = 1
;ngspy[indN2] = 0
;indN2 = 0
;
;n2seasons = BYTE(TOTAL(ngspy, 3))



res = write_envi_img(nbimodal, dir+'BBB_n_bimodal_years'+'.img')
res = write_envi_hdr(dir+'BBB_n_bimodal_years'+'.hdr', ns, nl, 1, INTERLEAVE='bsq')
 


 

END 