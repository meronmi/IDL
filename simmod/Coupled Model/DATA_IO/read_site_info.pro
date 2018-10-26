FUNCTION read_site_info, site_ini_fn
tmp = READ_CSV(site_ini_fn)
n = N_TAGS(tmp)-1
base = CREATE_STRUCT('name', '', 'rs_fn', '', 'gpp_fn', '', 'modmet_fn', '', 'eddy_fn', '', $
                     'years', LIST())
info = REPLICATE(base,n)
FOR i = 0, n-1 DO BEGIN
  info[i].name = tmp.(i+1)[0]
  info[i].rs_fn = tmp.(i+1)[1]
  info[i].gpp_fn = tmp.(i+1)[2]
  info[i].modmet_fn = tmp.(i+1)[3]
  info[i].eddy_fn = tmp.(i+1)[4]
  tmp_list = LIST()
  FOR j = 5, N_ELEMENTS(tmp.(i+1))-1 DO $
    IF (tmp.(i+1)[j] NE '') THEN tmp_list.Add, FIX(tmp.(i+1)[j])  
  info[i].years = tmp_list
ENDFOR
RETURN, info
END