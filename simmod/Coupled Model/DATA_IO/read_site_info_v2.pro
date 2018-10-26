FUNCTION read_site_info_v2, site_ini_fn
tmp = READ_CSV(site_ini_fn)
n = N_TAGS(tmp)-1
info = CREATE_STRUCT('rs_fn', '', 'gpp_fn', '', 'modmet_fn', '', 'eddy_dir', '', $
                     'igbp', LIST(), 'site_code', LIST())
info.rs_fn = tmp.(0)[1]
info.gpp_fn = tmp.(1)[1]
info.modmet_fn = tmp.(2)[1]
info.eddy_dir = tmp.(3)[1]

FOR j = 1, N_ELEMENTS(tmp.(4))-1 DO BEGIN
  info.igbp.Add, tmp.(4)[j]
  info.site_code.Add, tmp.(5)[j]
ENDFOR


RETURN, info
END