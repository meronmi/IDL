PRO stack_to_single_SPIRITS_files
scale2byte = 1
fn_stack = 'E:\EL NINO Anton\GlobalGIMMS_v1\unfiltered\global_NDVI3gV1.stk'
out_dir = 'E:\EL NINO Anton\GlobalGIMMS_v1\unfiltered\SPIRITS2'
pre = 'gimmsv1_'
suf = ''
ns = 4320
nl = 1658
nb = 828
dt = 2
map_info = '{Geographic Lat/Lon, 1.0000, 1.0000, -179.95833, 82.208333, 0.08333333333333333333, 0.08333333333333333333, WGS-84, units=degrees}'
frst_year = 1981
frst_month = 7
frst_per = 1
lst_year = 2015
lst_month = 12
lst_per = 2

stack = MAKE_ARRAY(ns, nl, nb, TYPE = dt)
OPENR, lun, fn_stack, /GET_LUN
READU, lun, stack
FREE_LUN, lun

;HELP, stack

c = 0
pre_ex = 0
ex_cond = 0
y = frst_year
m = frst_month
p = frst_per
WHILE (ex_cond EQ 0) DO BEGIN
  IF (pre_ex EQ 1) THEN ex_cond = 1
  tmp = stack[*,*,c]
  ;scale to byte and put flags 
  IF (scale2byte) THEN BEGIN
    count_back = 0
    count_missing = 0
    count = 0
    ind_back = WHERE(tmp LE -3000, count_back)
    nd = tmp / 10
    flag = tmp - nd * 10
    ind_missing = WHERE(flag GE 1, count_missing)
    nd_byte = ((nd / 1000.0) + 0.08)/0.004
    ind = WHERE(nd_byte LT 0, count)
    IF (count GT 0) THEN nd_byte[ind] = 0
    ind = WHERE(nd_byte gt 250, count)
    IF (count GT 0) THEN nd_byte[ind] = 250
    IF (count_back GT 0) THEN nd_byte[ind_back] = 255
    IF (count_missing GT 0) THEN nd_byte[ind_missing] = 251
    nd_byte = BYTE(ROUND(nd_byte))
    flags_text = '{251=missing, 255=back}'
  ENDIF
  c = c + 1
  
  IF (p EQ 1) THEN tt = (m-1)*3 + 1 ELSE tt = (m-1)*3 + 3
  IF (p EQ 1) THEN per = (m-1)*2 + 1 ELSE per = (m-1)*2 + 2    
  IF (tt LT 10) THEN tt = '0' + STRTRIM(tt,2) ELSE tt = STRTRIM(tt,2)  
  IF (per LT 10) THEN per = '0' + STRTRIM(per,2) ELSE per = STRTRIM(per,2)
  yyyytt = STRTRIM(y,2) + tt
  yyyyper = STRTRIM(y,2) + per
  
  
  IF (p EQ 1) THEN d = 1 ELSE d = 16
;  IF (p EQ 1) THEN d = 8 ELSE BEGIN
;    ;see how many days has this month
;    CALDAT, TIMEGEN(1, DAYS=31, MONTHS = m, YEAR = y), mm, lastDayInMonth, yy
;    d = ROUND(MEDIAN(INDGEN(lastDayInMonth-16+1, START = 16)))
;  ENDELSE
  ;date = 20080101
  
  
  
  IF (m LT 10) THEN mdate = '0' + STRTRIM(m,2) ELSE mdate = STRTRIM(m,2)
  IF (d LT 10) THEN ddate = '0' + STRTRIM(d,2) ELSE ddate = STRTRIM(d,2)
  line_date = 'date = ' + STRTRIM(y,2) + mdate + ddate
  yyyymmdd = STRTRIM(y,2) + mdate + ddate
  fn = out_dir + '\' + pre + yyyymmdd + suf + '.img'
  OPENW, lun, fn, /GET_LUN
  WRITEU, lun, nd_byte
  FREE_LUN, lun
  res = write_envi_hdr(pre + yyyymmdd + suf + '.hdr', $
                       ns, nl, 1, DIR = out_dir, $
                       MAPINFO=map_info, FREE_TEXT = line_date, FLAGS=flags_text) 
  
  ;update time
  IF (p EQ 2) THEN BEGIN
    p = 1
    m = m + 1
    IF (m GT 12) THEN BEGIN
      m = 1
      y = y +1
    ENDIF
  ENDIF ELSE p = p +1
  ;check that it is not the last to be processed
  IF (y EQ lst_year) AND (m EQ lst_month) AND (p EQ lst_per) THEN pre_ex = 1
ENDWHILE

END