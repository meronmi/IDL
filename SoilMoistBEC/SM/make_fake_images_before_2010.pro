PRO make_fake_images_before_2010

; se copio dal 2011 ho le date giuste a meno dell'anno (nel hdr non e' riportato 
years = [2007, 2008, 2009, 2010]
dekstart = 1
dekstop = 18

fns2011 = FILE_SEARCH('\\ies\d5\asap\BEC_soil_moisture\dataENVIformat', '*_2011*grid*.img')
outdir = '\\ies\d5\asap\BEC_soil_moisture\dataENVIformat\fakes'
out = 0
y = years[0]
d = dekstart
WHILE (out EQ 0) DO BEGIN
  ;copy and rename files
  fnout = FILE_BASENAME(fns2011[d-1], '.img')
  tmp = STRSPLIT(fnout, '_', /EXTRACT)
  fnout2 = STRJOIN([tmp[0], '_', STRTRIM(y,2), STRMID(tmp[1],4)]) 
  FILE_COPY, fns2011[d-1], outdir + '\' + fnout2 + '.img', /OVERWRITE
  FILE_COPY, FILE_DIRNAME(fns2011[d-1]) + '\' + fnout + '.hdr', outdir + '\' + fnout2 + '.hdr', /OVERWRITE
  ;open the file and set nodata
  res = ReadEnviWithHdr(outdir + '\' + fnout2 + '.img')
  res[*,*] = 255
  res = write_envi_img(res, outdir + '\' + fnout2 + '.img')
  ;update dek and check
  d = d + 1
  IF (d GT 36) THEN BEGIN
    d = 1
    y = y + 1
  ENDIF
  IF ((d EQ dekstop+1) AND (y EQ years[-1])) THEN out = 1
ENDWHILE


PRINT, 'qui'
END