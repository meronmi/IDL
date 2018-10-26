PRO MEGA_SCRIPT_FOR_THE_WE
  half_life = half_life_define_all()
  
  ;start with the constant weight
;  option_weight = 'const';'exp' ;'const' ;to make exp weightin or constant weight
;  build_ewp_images, half_life, option_weight  
  ;normalize when terminates
;  fn_base_out = 'E:\WA\EWP\EWP_images\standardized_cwp_hl'
;  fn_ewp_in = 'E:\WA\EWP\EWP_images\cwp_hl'
;  compute_standardized_ewp, fn_base_out, fn_ewp_in, half_life
  ;compute lagged  r when terminates
  ;do for SPI1 first
  lagged_r_TAMSAT_res, 'SPI3'
  lagged_r_TAMSAT_res, 'SPI4'
  lagged_r_TAMSAT_res, 'SPI5'
  lagged_r_TAMSAT_res, 'SPI6'
  lagged_r_TAMSAT_res, 'SPI7'
  lagged_r_TAMSAT_res, 'SPI8'
  lagged_r_TAMSAT_res, 'SPI9'
  lagged_r_TAMSAT_res, 'SPI10'
  lagged_r_TAMSAT_res, 'SPI12'
  
;  ;now for the produced files using the stupid convention
;  hl = 'zCWP' + STRTRIM(half_life,2)
;  FOR i = 0, N_ELEMENTS(hl)-1 DO lagged_r_TAMSAT_res, hl[i]
  
;  ;now with exp back weight (n normalization and after beta norm
;  option_weight = 'exp' ;'const' ;to make exp weightin or constant weight
;  build_ewp_images, half_life, option_weight  
;  ;normalize when terminates
;  fn_base_out = 'E:\WA\EWP\EWP_images\Nstandardized_ewp_hl'
;  fn_ewp_in = 'E:\WA\EWP\EWP_images\ewp_hl'
;  compute_standardized_ewp_as_NormalDist, fn_base_out, fn_ewp_in, half_life
;;  now for the produced files using the stupid convention
;  hl = 'NzEWP' + STRTRIM(half_life,2)
;  FOR i = 0, N_ELEMENTS(hl)-1 DO lagged_r_TAMSAT_res, hl[i]
  
;  ;now beta norm
;  fn_base_out = 'E:\WA\EWP\EWP_images\standardized_ewp_hl'
;  fn_ewp_in = 'E:\WA\EWP\EWP_images\ewp_hl'
;  compute_standardized_ewp, fn_base_out, fn_ewp_in, half_life
;  hl = 'zEWP' + STRTRIM(half_life,2)
;  FOR i = 0, N_ELEMENTS(hl)-1 DO lagged_r_TAMSAT_res, hl[i]
  
END