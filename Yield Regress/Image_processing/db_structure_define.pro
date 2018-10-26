FUNCTION db_structure_define, n_record, pheno_products
;define the db to store all data
db = {$
  depts   : {data: intarr(n_record), excluded: bytarr(n_record), lbl: 'Dept_id'}, $
  years   : {data: intarr(n_record), excluded: bytarr(n_record), lbl: 'Year'}, $
  yield   : {data: fltarr(n_record), lbl: 'Yield'}, $
  surf    : {data: fltarr(n_record), lbl: 'Surface'}, $
  ;npix in that dep
  n       : {data: lonarr(n_record), lbl: 'n_dep'}, $
  ;npix in that dep, with non0 crop mask
  ncm     : {data: lonarr(n_record), lbl: 'n_dep_cm'}, $
  ;npix in that dep, with non0 crop mask, over avg_fapar_threshold
  ncm_AFT        : {data: lonarr(n_record), lbl: 'n_dep_cm_GTf_thr'}, $
  ;npix in that dep, with non0 crop mask, over avg_fapar_threshold, over crop fract threshold
  ncm_AFT_ACMT   : {data: lonarr(n_record), lbl: 'n_dep_cm_GTf_thr_GTcm_thr'}, $
  pheno_wavg        : {data: fltarr(N_ELEMENTS(pheno_products), n_record), $
                      lbl: strarr(N_ELEMENTS(pheno_products))}, $
  ;npix in that dep, with non0 crop mask, over avg_fapar_threshold, over crop fract threshold,
  ;which are finite this year
  ncm_AFT_ACMT_FCY  : {data: lonarr(N_ELEMENTS(pheno_products), n_record), $
                      lbl: strarr(N_ELEMENTS(pheno_products))}, $
  ;total wwight for that (dep,year,pheno_product)
  totalw            : {data: lonarr(N_ELEMENTS(pheno_products), n_record), $
                      lbl: strarr(N_ELEMENTS(pheno_products))} $
  }
RETURN, db
END