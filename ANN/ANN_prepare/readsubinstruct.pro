FUNCTION readSubInStruct, fn, pheno_events, ns, nl 


hdr_fn = FILE_DIRNAME(fn) + '\' + FILE_BASENAME(fn) + '_sub_of_' + pheno_events[0] + '.hdr'
nb = LONG(read_info('bands', hdr_fn))

tmp_MAT = MAKE_ARRAY(ns, nl, nb, TYPE = 2)
Stru = CREATE_STRUCT('sos',tmp_MAT,'mg',tmp_MAT,'tom',tmp_MAT,'sen',tmp_MAT,'eos',tmp_MAT) 

FOR i = 0, N_ELEMENTS(pheno_events)-1 DO BEGIN
  fn0 = FILE_DIRNAME(fn) + '\' + FILE_BASENAME(fn) + '_sub_of_' + pheno_events[i] + '.img'
  OPENR, lun, fn0, /GET_LUN
  READU, lun, tmp_MAT
  Stru.(i) = tmp_MAT
  FREE_LUN, lun
ENDFOR
RETURN, Stru
END