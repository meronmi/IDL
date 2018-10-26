PRO make_meta_and_bil, check, fname

; se non funziona fai un full!!
; 
;let you select a list of files and make an ENVI META and convert it to a bil
;check        if check is set to 1 it also checks that there are no missing file (implemented for MODIS TERRA 16 days)
;fname_meta   filename for the output meta file
fnamesavfile = 'C:\tmp_meta.sav'

envi, /restore_base_save_files
envi_batch_init, log_file='batch.txt' 

rerun = FILE_SEARCH(fnamesavfile) 
IF (rerun NE '') THEN RESTORE, 'C:\tmp.sav' ELSE fapth = ''

flist =  DIALOG_PICKFILE(PATH = fpath, GET_PATH= sfpath ,/MULTIPLE_FILES, TITLE = 'Select ENVI files')
fpath = sfpath
save, fpath, FILENAME = fnamesavfile
;print, flist

;check that there are no missing 
namelist = flist
datelist = flist
IF (check EQ 1) THEN BEGIN
  FOR i=0, N_ELEMENTS(flist)-1 DO BEGIN
    tmp = STRSPLIT(flist[i],'\',/EXTRACT)
    namelist[i] = tmp[N_ELEMENTS(tmp)-1]
    tmp = STRSPLIT(namelist[i],'_',/EXTRACT)
    datelist[i] = tmp[1]
  ENDFOR
  print, datelist
  FOR i=1, N_ELEMENTS(datelist)-1 DO BEGIN
    year = FIX(STRMID(datelist[i], 0, 4))
    yearprec = FIX(STRMID(datelist[i-1], 0, 4))
    doy = FIX(STRMID(datelist[i], 2, /REVERSE_OFFSET))
    doyprec = FIX(STRMID(datelist[i-1], 2, /REVERSE_OFFSET))
    IF (doy EQ 1) THEN BEGIN
      IF ((doyprec NE 353) OR (year-yearprec NE 1)) THEN STOP
    ENDIF ELSE BEGIN
      IF ((doy-doyprec NE 16) OR (year-yearprec NE 0)) THEN STOP
    ENDELSE   
  ENDFOR
ENDIF

;make the meta file
fullpath_fname = fpath + '\' + fname
OPENW, lun, fullpath_fname+'.mta', /GET_LUN
ns = read_info('samples', flist[0]+'.hdr')
nl = read_info('lines', flist[0]+'.hdr')
PRINTF, lun, 'ENVI META FILE'
FOR i=0, N_ELEMENTS(flist)-1 DO BEGIN
  PRINTF, lun, 'File : ' + flist[i]
  PRINTF, lun, 'Bands : 1'
  PRINTF, lun, 'Dims : 1-' + STRTRIM(ns,2) + ',1-' + STRTRIM(nl,2)
  PRINTF, lun, ''
ENDFOR 
FREE_LUN, lun

;;make the bil with envi
;envi_open_file, fullpath_fname +'.mta', r_fid=fid
;envi_file_query, fid, dims=dims, nb=nb
;pos  = lindgen(nb)
;out_name = out_name
;envi_doit, 'convert_doit', $
;    fid=fid, pos=pos, dims=dims, $
;    o_interleave=1, out_name=fullpath_fname + '_bil', $
;    r_fid=r_fid
;ENVI_FILE_MNG, ID=fid, /REMOVE


PRINT, 'Finished!'

END