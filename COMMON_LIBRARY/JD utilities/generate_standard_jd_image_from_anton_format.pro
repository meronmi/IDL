PRO generate_standard_jd_image_from_anton_format
fn = 'D:\LTDR\LTDR10day_MVC_datestack_V5';
OPENR, R1, fn, /GET_LUN
fn_o = fn + '_stndrJD'
IF FILE_TEST(fn_o) eq 1 THEN FILE_DELETE, fn_o
OPENW, W1, fn_o, /GET_LUN, /APPEND
ns = read_info('samples', fn + '.hdr')
nl = read_info('lines', fn + '.hdr')
nb = read_info('bands', fn + '.hdr')
x = FINDGEN(nb)
const = JULDAY(12,31,1980)
line_ass_data = ASSOC(R1, INTARR(ns,nb))
FOR l=0, nl-1, 1L DO BEGIN
  data=line_ass_data[l]
  data_o = LONARR(ns,nb) + const
  ;here I have to fill the -9999
  FOR s=0, ns-1, 1L DO BEGIN
    ind=where((data[s,*] LE 0), count)
    IF (count NE nb) AND (count GT 0) AND (nb-count GT 100)THEN BEGIN
      ;if (s eq 251) and (l eq 24) then stop
      ;first fill
      ind_available = where((data[s,*] GT 0), count_av)
      data_o[s,ind_available] = data[s,ind_available]
      data_o[s,ind] = INTERPOL(data[s,ind_available], x[ind_available], x[ind])
      ;than add add JULDAY(12,31,1980)
    ENDIF
  ENDFOR
  data_o = data_o + const
  WRITEU, W1, data_o
ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fn_o+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {JD from Anton adjusted}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 3'
printf,3,'interleave = bil'
printf,3,'sensor type = ltdr'
printf,3,'byte order = 0'


CLOSE, /ALL
END