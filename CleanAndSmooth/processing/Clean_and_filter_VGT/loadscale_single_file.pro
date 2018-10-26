Pro loadscale_single_file
path='X:\somalia Felix'
image_file='vt11121.img'
ns=7841                   ; number of samples
nl=8289                   ; number of lines
nb=1                   ; number of bands
filein=path+'\'+image_file
fileout=filein+'_sc'
; FOR blu
gain=(0.25)/100.0 & offset=0.0 & maxval=250
; FOR red
;gain=(0.25)/100.0 & offset=0.0 & maxval=250
; FOR nir
;gain=(0.33)/100.0 & offset=0.0 & maxval=250

OPENR, R1, filein, /GET_LUN
data=bytarr(ns, nl)
READU, R1, data
data=float(data)
ind=where(data gt maxval, count)
if (count ne 0) then data[ind]=!VALUES.F_NAN
data1=FLOAT(data)*gain+offset
OPENW, W1, fileout, /GET_LUN
WRITEU, W1, data1

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = {band scaled}'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(1)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 4'
printf,3,'interleave = bsq'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'


CLOSE, /ALL


End