PRO pheno_europe
yr_lbl = INDGEN(17)+1999
ny = N_ELEMENTS(yr_lbl)
dir = 'S:\Actions\FOODSEC\projects\Pheno_Europe\RES'
ns = 5407
nl = 4650
dt = 1
valNaN = 251
;s1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;s2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;l1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;l2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;ya1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;ya2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
ratio_max_on_min_ya = FLTARR(ns,nl,ny) * !VALUES.F_NAN
s_main_ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;s_sub_ts  = FLTARR(ns,nl,ny) * !VALUES.F_NAN 
ya_main_ts  = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;ya_sub_ts  = FLTARR(ns,nl,ny) * !VALUES.F_NAN
len_main_ts  = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;len_sub_ts  = FLTARR(ns,nl,ny) * !VALUES.F_NAN
n_bimodal = FLTARR(ns, nl) * !VALUES.F_NAN
first_is_main = BYTARR(ns, nl, ny) * 0B;boolean when there are two seasons and first is main 
FOR i = 0, N_ELEMENTS(yr_lbl)-1 DO BEGIN
  ngspy = BYTARR(ns,nl)
  ya1 = BYTARR(ns,nl)
  ya2 = BYTARR(ns,nl)
  s1 = BYTARR(ns,nl)
  s2 = BYTARR(ns,nl)
  l1 = BYTARR(ns,nl)
  l2 = BYTARR(ns,nl)
  base = dir + '\ph' + STRTRIM(yr_lbl[i],2)
  OPENR, lun, base + 'KK0.img', /GET_LUN & READU, lun, ngspy & FREE_LUN, lun
  OPENR, lun, base + 'ya1.img', /GET_LUN & READU, lun, ya1 & FREE_LUN, lun
  OPENR, lun, base + 'ya2.img', /GET_LUN & READU, lun, ya2 & FREE_LUN, lun
  OPENR, lun, base + 's1.img', /GET_LUN & READU, lun, s1 & FREE_LUN, lun
  OPENR, lun, base + 's2.img', /GET_LUN & READU, lun, s2 & FREE_LUN, lun
  OPENR, lun, base + 'L1.img', /GET_LUN & READU, lun, l1 & FREE_LUN, lun
  OPENR, lun, base + 'L2.img', /GET_LUN & READU, lun, l2 & FREE_LUN, lun
  ;ind = WHERE(s1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(s1[ind])
  ;s1ts[*,*,i] = tmp
  ;ind = WHERE(s2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(s2[ind]) 
  ;s2ts[*,*,i] = tmp
  ;ind = WHERE(l1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(l1[ind])
  ;l1ts[*,*,i] = tmp
  ;ind = WHERE(l2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(l2[ind])
  ;l2ts[*,*,i] = tmp
  ;ind = WHERE(ya1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(ya1[ind])
  ;ya1ts[*,*,i] = tmp
  ;ind = WHERE(ya2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(ya2[ind])
  ;ya2ts[*,*,i] = tmp
  indMono = WHERE((ngspy GE 100) AND (ngspy LT 200), countMono)
  indBi = WHERE((ngspy GE 200) AND (ngspy LT 245), countBi)
  n_bimodal[indBi] = n_bimodal[indBi]  + 1 
  FOR j = 0, countBi -1 DO BEGIN
    indArr = ARRAY_INDICES(ngspy, indBi[j])
    res = MAX([ya1[indBi[j]],ya2[indBi[j]]], indMax,  SUBSCRIPT_MIN=indMin)
    ratio_max_on_min_ya[indArr[0], indArr[1], i] = MAX([ya1[indBi[j]],ya2[indBi[j]]]) / FLOAT(MIN([ya1[indBi[j]],ya2[indBi[j]]]))
    IF (indMax EQ 0) THEN first_is_main[indArr[0], indArr[1], i] = 1
    tmp = [s1[indBi[j]],s2[indBi[j]]]
    s_main_ts[indArr[0], indArr[1], i] = tmp[indMax]
    ;s_sub_ts[indArr[0], indArr[1], i]  = tmp[indMin]
    tmp = [ya1[indBi[j]],ya2[indBi[j]]]
    ya_main_ts[indArr[0], indArr[1], i]  = tmp[indMax]
    ;ya_sub_ts[indArr[0], indArr[1], i]  = tmp[indMin]
    tmp = [l1[indBi[j]],l2[indBi[j]]]
    len_main_ts[indArr[0], indArr[1], i]  = tmp[indMax]
    ;len_sub_ts[indArr[0], indArr[1], i]  = tmp[indMin] 
  ENDFOR
ENDFOR
;save the number of bimodal occurrences
OPENW, lun, dir + '\AAA_ph_n_bimodal99-15.img', /GET_LUN
WRITEU, lun, n_bimodal
FREE_LUN, lun
mapinfo = read_info('map info', dir + '\ph' + STRTRIM(yr_lbl[0],2) + 'KK0.hdr')
coordinfo = read_info('projection info', dir + '\ph' + STRTRIM(yr_lbl[0],2) + 'KK0.hdr')
res = write_envi_hdr('AAA_ph_n_bimodal99-15.hdr', ns, nl, 1, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)

;Spcifically for the bimodals when there are two seasons
;save the fraction of occurrences of (there are two season) and (the biggest is the first)
res = TOTAL(first_is_main,3)/FLOAT(n_bimodal)
OPENW, lun, dir + '\AAA_ph_fract_of_the_big_is_first.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_fract_of_the_big_is_first.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
;save the ratio mean NDVI over the season BIG/SMALL
res = MEAN(res,DIMENSION= 3, /NAN)
OPENW, lun, dir + '\AAA_ph_mean_seas_NDVI_big_on_small.img', /GET_LUN
WRITEU, lun, ratio_max_on_min_ya & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_mean_seas_NDVI_big_on_small.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
;save the mean SOS of the big one
res = MEAN(s_main_ts,DIMENSION= 3, /NAN)
OPENW, lun, dir + '\AAA_ph_mean_SOS_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_mean_SOS_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
res = STDDEV(s_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_sd_SOS_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_sd_SOS_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
;save the mean LEN of the big one
res = MEAN(len_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_mean_LEN_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_mean_LEN_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
res = STDDEV(len_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_sd_LEN_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_sd_LEN_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, COORDINFO=coordinfo)
END
