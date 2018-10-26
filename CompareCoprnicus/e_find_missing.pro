PRO handler_E_find_missing
startup = C_start_up()
nProducts = N_ELEMENTS(startup.z_fn)
flag_missing=LIST([251],[251],[251,252,253,255])
flag_non_land = [254,254,254]
FOR i = 0, nProducts-1 DO BEGIN
  i = 2 ;test ndvi3
  bil_fn =  startup. stack_path + '\c2003-2018NDVI3.bil' ;non smoothed
  E_find_missing, bil_fn, startup.out_path, flag_missing[i], flag_non_land[i]
ENDFOR
END

PRO E_find_missing, fb_bil, out_path, flag_missing, flag_non_land
; flag_missing: flag for missing vale (255 for modis, 251 for fapar)

fn_base = FILE_BASENAME(fb_bil, '.bil')
dir = FILE_DIRNAME(fb_bil)
hdr_fn = dir + '\' + fn_base + '.hdr'

ns = LONG(read_info('samples', hdr_fn))
nl = LONG(read_info('lines', hdr_fn))
nb = LONG(read_info('bands', hdr_fn))
dt = FIX(read_info('data type', hdr_fn))
mapinfo = read_info('map info',hdr_fn)
interleave  = STRTRIM(read_info('interleave', hdr_fn),2)
IF (interleave NE 'bil') THEN BEGIN
  PRINT, 'Error, ' + fn + ' is not bil, check hdr.'
  STOP
ENDIF

count_missing = LONARR(nb, nl)
count_potential = LONARR(nb, nl)

OPENR, lun, fb_bil, /GET_LUN
line = ASSOC(lun, MAKE_ARRAY(ns, nb, TYPE = dt))
FOR i = 0, nl-1 DO BEGIN
  tmp = line[i]
  nFlags = N_ELEMENTS(flag_missing)
  tmp1 = tmp * 0B
  tmp2 = tmp * 0B
  FOR k=0, nFlags-1 DO BEGIN
    ind = WHERE(tmp EQ flag_missing[k], count)
    IF (count GT 0) THEN BEGIN
      tmp1[ind] = tmp1[ind] + 1
      count_missing[*,i] = TOTAL(tmp1, 1) ;sum over longitude (ns, first dimension)
    ENDIF
    IF (k EQ 0) THEN BEGIN
      ind = WHERE(tmp NE flag_non_land, count)
      IF (count GT 0) THEN BEGIN
        tmp2[ind] = tmp2[ind] + 1
        count_potential[*,i] = TOTAL(tmp2, 1) ;sum over longitude (ns, first dimension)
        tmp2 = 0
      ENDIF
    ENDIF
  ENDFOR
  tmp = 0
  tmp1 = 0
ENDFOR
FREE_LUN, lun
PRINT, '*******************'
PRINT, fb_bil
PRINT, 'total missing = ' + STRTRIM(TOTAL(count_missing),2)
PRINT, 'total % missing = ' + STRTRIM(TOTAL(count_missing)/FLOAT(TOTAL(count_potential))*100.0,2)
count_miss_avg_per_dek = LONARR(36,nl)
count_potential_avg_per_dek = LONARR(36,nl)
nyears = 15
count_miss_by_lat = TOTAL(count_missing, 1)
count_potential_by_lat = TOTAL(count_potential, 1)

FOR i = 0, 36-1 DO BEGIN
  count_miss_avg_per_dek[i,*] = TOTAL(count_missing[i:i+(nyears-1)*36:36,*], 1)
  count_potential_avg_per_dek[i,*] = TOTAL(count_potential[i:i+(nyears-1)*36:36,*], 1)
ENDFOR
;get the lat from mapinfo
tmp = STRSPLIT(mapinfo, "{}", /EXTRACT)
tmp = STRSPLIT(tmp, ",", /EXTRACT)
ymax = tmp[4]
yres = tmp[6]
y = ymax - FINDGEN(nl)*FLOAT(yres) 
x = FINDGEN(36)+1
res = PLOT(y, count_miss_by_lat/FLOAT(count_potential_by_lat)*100.0, XTITLE= 'lat', YTITLE='% missing')
fn_out =  out_path + '\'+FILE_BASENAME(fb_bil,'bil') + '_percent_mising_hovemoller.png' 
ret = hovmoller(count_miss_avg_per_dek/FLOAT(count_potential_avg_per_dek)*100.0, y, x, 'Lat', 'Dek', '% missing', out_path + '\' + fn_base, FN_OUT = fn_out)
print, 'ok'
END