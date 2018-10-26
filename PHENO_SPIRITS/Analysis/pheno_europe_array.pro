PRO pheno_europe_array
yr_lbl = INDGEN(17)+1999
ny = N_ELEMENTS(yr_lbl)
dir = 'S:\Actions\FOODSEC\projects\Pheno_Europe\RES'


;dir = 'S:\Actions\FOODSEC\projects\Pheno_Europe\RES'
ns = 5407
nl = 4650
dt = 1
valNaN = 251
;s1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;s2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;l1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;l2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;y1ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
;y2ts = FLTARR(ns,nl,ny) * !VALUES.F_NAN
ratio_max_on_min_yatmax = FLTARR(ns,nl,ny) * !VALUES.F_NAN
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
  y1 = BYTARR(ns,nl)
  y2 = BYTARR(ns,nl)
  s1 = BYTARR(ns,nl)
  s2 = BYTARR(ns,nl)
  l1 = BYTARR(ns,nl)
  l2 = BYTARR(ns,nl)
  base = dir + '\ph' + STRTRIM(yr_lbl[i],2)
  OPENR, lun, base + 'KK0.img', /GET_LUN & READU, lun, ngspy & FREE_LUN, lun
  OPENR, lun, base + 'ym1.img', /GET_LUN & READU, lun, y1 & FREE_LUN, lun
  indNaN = WHERE(y1 GT 250, countNaN)
  y1 = (FLOAT(y1) * 0.004) - 0.08
  IF (countNaN GT 0) THEN y1[indNaN]=!VALUES.F_NAN 
  OPENR, lun, base + 'ym2.img', /GET_LUN & READU, lun, y2 & FREE_LUN, lun
  indNaN = WHERE(y2 GT 250, countNaN)
  y2 = (FLOAT(y2) * 0.004) - 0.08
  IF (countNaN GT 0) THEN y2[indNaN]=!VALUES.F_NAN
  OPENR, lun, base + 's1.img', /GET_LUN & READU, lun, s1 & FREE_LUN, lun
  OPENR, lun, base + 's2.img', /GET_LUN & READU, lun, s2 & FREE_LUN, lun
  OPENR, lun, base + 'L1.img', /GET_LUN & READU, lun, l1 & FREE_LUN, lun
  OPENR, lun, base + 'L2.img', /GET_LUN & READU, lun, l2 & FREE_LUN, lun
  ysum1 = y1; * l1
  ysum2 = y2; * l2
  
  ;ind = WHERE(s1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(s1[ind])
  ;s1ts[*,*,i] = tmp
  ;ind = WHERE(s2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(s2[ind]) 
  ;s2ts[*,*,i] = tmp
  ;ind = WHERE(l1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(l1[ind])
  ;l1ts[*,*,i] = tmp
  ;ind = WHERE(l2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(l2[ind])
  ;l2ts[*,*,i] = tmp
  ;ind = WHERE(y1 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(y1[ind])
  ;y1ts[*,*,i] = tmp
  ;ind = WHERE(y2 LT valNaN) & tmp = FLOAT(ns,nl) & tmp[ind] = FLOAT(y2[ind])
  ;y2ts[*,*,i] = tmp
  ;use of k replace bu direct use of s1 and s2 as k was found to be unreliable (K=200 and one season only)
  
  ;indMono = WHERE((ngspy GE 100) AND (ngspy LT 200), countMono)
  ;indBi = WHERE((ngspy GE 200) AND (ngspy LT 245), countBi)
  indBi = WHERE((s1 LE 108) AND (s2 LE 108), countBi)
  ;initialize n_bimodal if needed (unless adding 1 to NaN Gives Nan)
  ind = WHERE(~FINITE(n_bimodal[indBi]), count)
  IF (count GT 0) THEN n_bimodal[indBi[ind]]=0.0
  n_bimodal[indBi] = n_bimodal[indBi]  + 1 
  
  ;be care it happens that sesonality is 200 but y1 and y2 are flagged over 251
  diffYsum = FLOAT(ysum1[indBi]) - FLOAT(ysum2[indBi])
  ind_indBi_first_is_main = WHERE(diffYsum GE 0)
  ind_indBi_secon_is_main = WHERE(diffYsum LT 0)
  
  var = FLTARR(ns,nl) * !VALUES.F_NAN
  var[indBi[ind_indBi_first_is_main]] = ysum1[indBi[ind_indBi_first_is_main]]/FLOAT(ysum2[indBi[ind_indBi_first_is_main]])
  var[indBi[ind_indBi_secon_is_main]] = ysum2[indBi[ind_indBi_secon_is_main]]/FLOAT(ysum1[indBi[ind_indBi_secon_is_main]])
  ratio_max_on_min_yatmax[*,*,i] = var
  
  var = BYTARR(ns,nl) * 0B
  var[indBi[ind_indBi_first_is_main]] = 1
  first_is_main[*,*, i] = 1
  
  var = FLTARR(ns,nl) * !VALUES.F_NAN
  var[indBi[ind_indBi_first_is_main]] = s1[indBi[ind_indBi_first_is_main]]
  var[indBi[ind_indBi_secon_is_main]] = s2[indBi[ind_indBi_secon_is_main]]
  s_main_ts[*, *, i] = var
   
  var = FLTARR(ns,nl) * !VALUES.F_NAN
  var[indBi[ind_indBi_first_is_main]] = y1[indBi[ind_indBi_first_is_main]]
  var[indBi[ind_indBi_secon_is_main]] = y2[indBi[ind_indBi_secon_is_main]]
  ya_main_ts[*, *, i]  = var
  
  var = FLTARR(ns,nl) * !VALUES.F_NAN
  var[indBi[ind_indBi_first_is_main]] = l1[indBi[ind_indBi_first_is_main]]
  var[indBi[ind_indBi_secon_is_main]] = l2[indBi[ind_indBi_secon_is_main]]
  len_main_ts[*, *, i]  = var
ENDFOR
;save the number of bimodal occurrences
OPENW, lun, dir + '\AAA_ph_n_bimodal99-15.img', /GET_LUN
WRITEU, lun, n_bimodal
FREE_LUN, lun
mapinfo = read_info('map info', dir + '\ph' + STRTRIM(yr_lbl[0],2) + 'KK0.hdr')
projectioninfo = read_info('projection info', dir + '\ph' + STRTRIM(yr_lbl[0],2) + 'KK0.hdr')
res = write_envi_hdr('AAA_ph_n_bimodal99-15.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)

;Spcifically for the bimodals when there are two seasons
;save the fraction of occurrences of (there are two season) and (the biggest is the first)
res = TOTAL(first_is_main,3)/FLOAT(n_bimodal)
OPENW, lun, dir + '\AAA_ph_fract_of_the_big_is_first.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_fract_of_the_big_is_first.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
;save the ratio mean NDVI over the season BIG/SMALL
res = MEAN(ratio_max_on_min_yatmax,DIMENSION= 3, /NAN)
OPENW, lun, dir + '\AAA_ph_peak_seas_NDVI_big_on_small.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_peak_seas_NDVI_big_on_small.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
;save the mean SOS of the big one
res = MEAN(s_main_ts,DIMENSION= 3, /NAN)
OPENW, lun, dir + '\AAA_ph_mean_SOS_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_mean_SOS_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
res = STDDEV(s_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_sd_SOS_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_sd_SOS_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
;save the mean LEN of the big one
res = MEAN(len_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_mean_LEN_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_mean_LEN_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
res = STDDEV(len_main_ts,DIMENSION=3, /NAN)
OPENW, lun, dir + '\AAA_ph_sd_LEN_of_BIG.img', /GET_LUN
WRITEU, lun, res & FREE_LUN, lun
res = write_envi_hdr('AAA_ph_sd_LEN_of_BIG.hdr', ns, nl, 4, DIR = dir, MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
END
