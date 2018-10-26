PRO run_sub_s1
dir_pheno = 'E:\WA MODIS\Phenology\SPRITS-ASAP\Dek1-36'
pheno_events = define_pheno_events() ;this mast be guiven in temporal order (sos first etc)
pheno_fn = ['OF_LTAs1_1_36.img','OF_LTAmg1_1_36.img','OF_LTAm1_1_36.img','OF_LTAsen1_1_36.img','OF_LTAe1_1_36.img']
;NDVI
start_year = 2002
start_dek = 09
end_year = 2016
end_dek = 35
fn_refrence = 'BOKU_OF_WA_bil_sc'
dir_stack = 'E:\WA MODIS\WA ROI NDVI\bil files'
ret = B0_sub_of_pheno_events(start_year, start_dek, end_year, end_dek, dir_stack, dir_pheno, pheno_events, pheno_fn, fn_refrence)
;ECMWF rad and tav
start_year = 1989
start_dek = 1
fn_refrence = 'ECMWF_rad_bil'
dir_stack = 'E:\WA MODIS\WA ERA INT'
ret = B0_sub_of_pheno_events(start_year, start_dek, end_year, end_dek, dir_stack, dir_pheno, pheno_events, pheno_fn, fn_refrence)
;tav
fn_refrence = 'ECMWF_tav_bil'
ret = B0_sub_of_pheno_events(start_year, start_dek, end_year, end_dek, dir_stack, dir_pheno, pheno_events, pheno_fn, fn_refrence)
;chirps
start_year = 1981
start_dek = 1
fn_refrence = 'chirps_bil'
dir_stack = 'E:\WA MODIS\WA ROI CHIRPS'
ret = B0_sub_of_pheno_events(start_year, start_dek, end_year, end_dek, dir_stack, dir_pheno, pheno_events, pheno_fn, fn_refrence)
END

FUNCTION define_pheno_events
RETURN, ['sos','mg','tom','sen','eos']
END

FUNCTION B0_sub_of_pheno_events, start_year, start_dek, end_year, end_dek, dir_stack, dir_pheno, pheno_events, pheno_fn, fn_refrence
pheno_unit = 'dekad' ;'doy'

;in a stack file of input with known dek and year of start-end (start_year, start_dek, end_year, end_dek)
;find the subscripts corresponding to the occurrence of all phenological events desired, it only save those season having all events (at the edges of the ts not all events may be present)
;note that different pixels may have different numbers of complete seasons

;get the dimension from the first
hdr_fn = FILE_BASENAME(pheno_fn[0], '.img') +'.hdr'
ns = LONG(read_info('samples', dir_pheno + '\' + hdr_fn))
nl = LONG(read_info('lines', dir_pheno + '\' + hdr_fn))
nb = FIX(read_info('bands', dir_pheno + '\' + hdr_fn))
IF (nb GT 1) THEN STOP
dt = FIX(read_info('data type', dir_pheno + '\' + hdr_fn))
;3d matrix to store dek of pheno events
pheno = MAKE_ARRAY(ns,nl,N_ELEMENTS(pheno_fn), TYPE=dt)
;read the files
FOR i = 0, N_ELEMENTS(pheno_events)-1 DO BEGIN
  pheno[*,*,i] = load_envi_bsq_file(dir_pheno + '\' + pheno_fn[i])
ENDFOR

;transform to dek if required
IF (pheno_unit EQ 'doy') THEN BEGIN
  FOR i = 0, N_ELEMENTS(pheno_events) DO BEGIN
    pheno[*,*,i] = doy2dek(pheno[*,*,i])
  ENDFOR
ENDIF ELSE BEGIN
  ;make a check
  indFin = WHERE(FINITE(pheno))
  ind = WHERE((pheno[indFin] GT 36) AND (pheno[indFin] LT 250), count)
  IF (count GT 0) THEN STOP
ENDELSE

;locate pheno events 
;make it able to store the max possible number of events (one per year, all years)
sub_ph_events = REPLICATE({sub: MAKE_ARRAY(ns,nl,end_year - start_year + 1, TYPE=2)-9999}, N_ELEMENTS(pheno_events))
;make the array of dekads in the stack
n_deks = (36 - start_dek + 1) + 36 * (end_year - start_year + 1 - 2) + end_dek
;check that it is coherent with the reference file
hdr_fn = FILE_BASENAME(fn_refrence, '.img') +'.hdr'
IF (FIX(read_info('bands', dir_stack + '\' + hdr_fn)) NE n_deks) THEN STOP

deks_in_stak = INTARR(n_deks)
deks_in_stak[0] = start_dek
FOR i = 1, n_deks-1 DO BEGIN
  IF (deks_in_stak[i-1] EQ 36) THEN deks_in_stak[i] = 1 ELSE deks_in_stak[i] = deks_in_stak[i-1] + 1
ENDFOR

;for ecah pheno events find the subscripts
;make the pheno var 2d (flatten rows and columns)
s = SIZE(pheno)
pheno = REFORM(pheno, s[1]*s[2],s[3])

;work only with those with finite values
indValid = WHERE((pheno[*,0] GE 1) AND (pheno[*,0] LE 36))
FOR i = 0, N_ELEMENTS(pheno_events)-1 DO BEGIN
  tmp_sub_ph_events = REFORM(sub_ph_events[i].sub)
  ss = SIZE(tmp_sub_ph_events)
  ;flatten it
  tmp_sub_ph_events = REFORM(tmp_sub_ph_events, ss[1]*ss[2],ss[3])
  ;for this particular event find subscript of those pixels having pheno event = 1, then 2 etc
  FOR d = 1, 36 DO BEGIN
    ;say we are selecting those having dek d = 1
    ind_dek_d = WHERE(pheno[indValid,i] EQ d, count_dek_d)    ;here I have the position of these pixels
    IF (count_dek_d GT 0) THEN BEGIN
      ind_dek_d = indValid[ind_dek_d]   ;index them back in the original 2d matrix
      sub_of_dek_d = WHERE(deks_in_stak EQ d) ;find the sub of dekad d (1)  in the stack being processes
      ;now, for each of the pixel having dekad d (1) as this pheno event, assign the stack subsets
      FOR j = 0, N_ELEMENTS(ind_dek_d)-1 DO tmp_sub_ph_events[ind_dek_d[j], 0:N_ELEMENTS(sub_of_dek_d)-1] = sub_of_dek_d
    ENDIF
  ENDFOR
  sub_ph_events[i].sub = REFORM(tmp_sub_ph_events, ss[1], ss[2], ss[3])
ENDFOR
;now all the occurrences of the pheno events are found but with no guarantee on order in subs
;for instance the first sub of eos may be eralier than the first sub of sos (because the stack file was
;beginning some time after the sos associated to that sos
;that is: sos must be < mg < tom < sen < eos
;check it over the whole matrix, if necessary correct it. Correcting the first sub is enough for correcting all
FOR l = 0, nl-1 DO BEGIN
  FOR s = 0, ns-1 DO BEGIN
    tmp = sub_ph_events[*].sub[s,l,0]
    ind = WHERE(tmp[0] GT tmp, count) ; ind are the subscripts to be removed
    IF (count GT 0) THEN BEGIN
      FOR i = 0, count-1 DO BEGIN
        sub_ph_events[ind[i]].sub[s,l,*] = [REFORM(sub_ph_events[ind[i]].sub[s,l,1:*]),-9999] 
      ENDFOR
    ENDIF
  ENDFOR
ENDFOR

;write the output files
FOR i = 0, N_ELEMENTS(pheno_events)-1 DO BEGIN
   ret = write_envi_img(REFORM(sub_ph_events[i].sub), dir_stack + '\' + fn_refrence + '_' + 'sub_of_' +  pheno_events[i] + '.img')
   ret = write_envi_hdr(dir_stack + '\' + fn_refrence + '_' + 'sub_of_' +  pheno_events[i] + '.hdr', ns, nl, 2, NBANDS=ss[3])
ENDFOR

PRINT, 'ended..'
END