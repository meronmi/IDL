PRO convert_all_dmp_zip_in_a_di_recursive
unzip = 1  ;set to 0 if already unzip in dir called ENVI
; use print, H5_BROWSER() to inspect
;find the files in subdir
;dir = 'X:\Active Projects\MARSOP and transition\Copernicus Gloabl Land Experts Meeting\Nov 2014\C0010978'
dir = 'E:\VGT-PV-overlap\PV'
res = FILE_SEARCH(dir, '*AFRI*rc1.zip')
;dir = 'E:\VGT-PV-overlap\VGT'
;res = FILE_SEARCH(dir, '*AFRI*.zip')
;for debug
;res = res[0:2]
PRINT, res
outDir = dir + '\ENVI'
IF (unzip EQ 1) THEN BEGIN
  ;make the dir to store output
  FILE_MKDIR, outDir
  ;copy the fils
  FILE_COPY, res, outDir
  ;copy the zip and unzip
  FILE_COPY, 'd:\Users\meronmi\Documents\IDL\G2BP\G2BPproject\7za.exe', outDir, /OVERWRITE
  ;unzip it (use 7za.ex as the IDL unzip gives error)
  CD, outDir
  res = FILE_SEARCH( '*.zip')
  FOR i = 0, N_ELEMENTS(res)-1 DO SPAWN, '7za e ' + res[i] +' *.h5 -r', /LOG_OUTPUT
  ;delete the zip
  FILE_DELETE, outDir + '\7za.exe'
  FILE_DELETE, res
ENDIF ELSE BEGIN
  CD, outDir
ENDELSE
res = FILE_SEARCH( '*.h5', /FULLY_QUALIFY_PATH)
path =  FILE_DIRNAME(res)
fn = FILE_BASENAME(res)
PRINT, fn
PRINT, path
FOR i = 0, N_ELEMENTS(res)-1 DO BEGIN
  PRINT, 'Processing ' + fn[i]
  tmp = SINGLE_FILE_CONVERSION_G2_to_MP(path[i], fn[i], 'NDVI', 'pv')
  ;tmp = SINGLE_FILE_CONVERSION_G2_to_MP(path[i], fn[i], 'DMP')
ENDFOR
res = FILE_SEARCH(dir, '*.h5')
;FILE_DELETE, res

END