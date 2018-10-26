PRO read_reg_coeff, rep_yield_fname, RS_product_name, level, depid, b0, b1
;Reads the coefficients for a given product and given level
;Returned values:
; depid: list of department id
; b0: list of intercepts
; b1: list of slopes
 
;EXAMPLE
;read_reg_coeff, 'K:\Tunisia\27bisMar\durum_wheat\rep_yield_durum_wheat_3-30.csv', 'd12', 'All_regional_mods', b0, b1
;read_reg_coeff, 'K:\Tunisia\27bisMar\durum_wheat\rep_yield_durum_wheat_3-30.csv', 'd12', 'All_national_mod', b0, b1

;naming covention
nat_level = 'RSI-glob_Coeff-glob'
reg_level = 'RSI-glob_Coeff-dep'

;check that the level is correct
IF (~(STRMATCH(level, nat_level) OR STRMATCH(level, reg_level))) THEN STOP

data = READ_CSV( rep_yield_fname, HEADER=columns)
ind_prod_field = WHERE(STRMATCH(columns, 'RS_product') EQ 1, count) & IF (count EQ 0) THEN STOP
ind_dept_field = WHERE(STRMATCH(columns, 'Dept_id') EQ 1, count) & IF (count EQ 0) THEN STOP
ind_b0_field = WHERE(STRMATCH(columns, 'b0') EQ 1, count) & IF (count EQ 0) THEN STOP
ind_b1_field = WHERE(STRMATCH(columns, 'b1') EQ 1, count) & IF (count EQ 0) THEN STOP
;extract the indexes of the required products only
ind_prod_records = WHERE(STRMATCH(data.(ind_prod_field), RS_product_name), count) & IF (count EQ 0) THEN STOP

IF STRMATCH(level, nat_level) THEN BEGIN
  ind = WHERE(STRMATCH(data.(ind_dept_field)[ind_prod_records],  nat_level))
ENDIF
IF STRMATCH(level, reg_level) THEN BEGIN
  ind = WHERE( $
      ~STRMATCH(data.(ind_dept_field)[ind_prod_records],  nat_level) AND $
      ~STRMATCH(data.(ind_dept_field)[ind_prod_records],  reg_level))
  PRINT, 'Coefficients found for ', STRTRIM(N_ELEMENTS(ind),2) + ' department'
  b0 = DBLARR(N_ELEMENTS(ind))
  b1 = b0 & depid = INTARR(N_ELEMENTS(ind))  
ENDIF
b0 = data.(ind_b0_field)[ind_prod_records[ind]]
b1 = data.(ind_b1_field)[ind_prod_records[ind]]
depid = data.(ind_dept_field)[ind_prod_records[ind]]
;print, b0
;print, b1
;print, depid
END