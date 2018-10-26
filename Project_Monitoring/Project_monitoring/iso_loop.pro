PRO iso_loop
  fn_data = 'X:\ANNE ISO\eM_sNE_2001-15_dekada_stack'
  fn_mask = 'X:\ANNE ISO\mask_outsideNE_nonVegetated_gt380mm_emodis_valid'
  fn_base_out = 'X:\ANNE ISO\ISOclassification'

  e = ENVI()
  ENVI_OPEN_FILE, fn_mask, r_fid=m_fid
  ENVI_OPEN_FILE, fn_data, r_fid=fid
  ENVI_FILE_QUERY, fid, ns=ns, nl=nl, nb=nb, bname=bname
  dims = [-1l, 0, ns-1, 0, nl-1]
  pos = INDGEN(nb)
  m_pos = [0l]

  FOR i = 7, 15 DO BEGIN
    t0 = TIC()
    PRINT, 'Inizio n = ' + STRTRIM(i,2)
    fn_classification = fn_base_out + '_' + STRTRIM(i,2) + 'classes'
    n_iso = i
    min_classes = i
    ENVI_DOIT, 'class_doit', $
      fid=fid, pos=pos, dims=dims, $
      ;/check, o_min=0, o_max=255, $
      out_name=fn_classification, r_fid=r_fid, m_fid=m_fid, $
      method=4, change_thresh=0.01, iso_merge_dist=5, $ ; method 4 is isodata
      iso_min_pixels=47735, min_classes=n_iso, num_classes=n_iso, iterations=50, $
      iso_split_std=1.0, iso_merge_pairs=2, $
      value=0, m_pos=m_pos
    t1 = TOC()
    PRINT, 'Seconds = ' + STRTRIM(t1-t0.time, 2) + ' Hours = ' + STRTRIM((t1-t0.time)/(60.0*60.0), 2)  
    PRINT, 'Fatto n = ' + STRTRIM(i,2)
  ENDFOR
END