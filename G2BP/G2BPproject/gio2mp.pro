FUNCTION logFileStructDef
st = CREATE_STRUCT( $
  'VERSION', 1.00000, $
  'DATASTART', 1L, $
  'DELIMITER', 32B, $
  'MISSINGVALUE', !VALUES.F_NAN, $
  'COMMENTSYMBOL', '', $
  'FIELDCOUNT', 1L, $
  'FIELDTYPES', 7L, $
  'FIELDNAMES', 'FIELD1', $
  'FIELDLOCATIONS',  0L, $
  'FIELDGROUPS',     [1L])

RETURN, st
END


PRO GIO2MP
  ;Michele, 25 Feb 2014
  ; 
  ;Function used to import GIO NDVI v2 data, this function, for selected continental tiles, :
  ;
  ; - check the existence of new data on the storage_machine_GIO_DIR
  ; - if new data are found, these are copied on client_machine_DIR
  ; - unzip them
  ; - read h5 format
  ; - cut the required boundary (Africa now)
  ; - save an envi file
  ; - copy on the storage_machine_MARSOP_DIR
  ;
  ;Note:
  ; - Directories are set at code level
  ; - The utility '7za.exe' must be copied in client_machine_DIR
  ; - The name of files already processed are stored in a log file 'log.txt' in the client_machine_DIR
  ; - to use it as batch copy the sav file in the client_machine_DIR (idl bin must be in path of the client machine)
  ;
  ; Use the following to inspect the h5 
  ;Result = H5_BROWSER(fname)
  
  
  ;*************************************************
  ;USER settings
  ;name of GIO windows to be processed
  tiles = ['AFRI','ASIA'];,'Afica' was a test image
  
  ;directory where GIO products are copied
  storage_machine_GIO_DIR = 'S:\Actions\FOODSEC\base_data\MONDE_GIO_PUSH\'
  ;directory where GIO has to be copied to be processed
  client_machine_DIR = 'T:\tasks\RS_gap_filler\' 
  ;directory where products, converted to MARSOP format, are copied
  storage_machine_MP_DIR = ['M:\data\cid-bulk15\TS\42002\AFR\ACT2\S10\','M:\data\cid-bulk15\TS\42002\DPRK\ACT\S10\']
  
;  ;Local directory where GIO products are copied, comment to run on server
;  storage_machine_GIO_DIR = 'F:\storage_machine_GIO_DIR\'
;  ;directory where GIO has to be copied to be processed
;  client_machine_DIR ='F:\client_machine_DIR\'
;   ;directory where products, converted to MARSOP format, are copied
;  storage_machine_MP_DIR = ['F:\storage_machine_MP_DIR_AFR\', 'F:\storage_machine_MP_DIR_DPRK\'];, 'F:\storage_machine_MP_DIR_AFRG2\']
  
  ;log file that will be initialized and store on the client_machine_DIR
  log_fname = 'succesful_log.txt'
  error_log_fname = 'trial_log.txt'
  IDL_error_log_fname = 'IDL_error_log_fname.txt'
  ;*************************************************
  ;General settings 
  tile_info = CREATE_STRUCT( $
    'name', STRARR(10), $
    'ns_in', INTARR(10), $
    'nl_in', INTARR(10), $
    'ns_out', INTARR(10), $
    'nl_out', INTARR(10), $
    'gio_Xoffset', INTARR(10), $       ;GIO offset for cutting the image onto FS ROI
    'gio_Yoffset', INTARR(10), $       ;GIO offset for cutting the image onto FS ROI
    'pixsize', 0.00892857143, $
    'h5Variable', 'NDVI')
  
  ; use 'd:\Users\meronmi\Documents\IDL\G2BP\dimensions MARSOP and GIO.xlsx' to compute offset
  ;Info for ROI AFRI (as downloaded from G2 website and as available from GIO throgh Marco Clerici), Smets: AFRI is a continental tile, hence it ends at boundaries of 10° => bounding box from 40N 20W -> 60E 40S
  tile_info.name[0]='AFRI'
  tile_info.ns_in[0]=8961
  tile_info.nl_in[0]=8961
  tile_info.ns_out[0]=7841
  tile_info.nl_out[0]=8289
  tile_info.gio_Xoffset[0]=224
  tile_info.gio_Yoffset[0]=224
  ;Info for GIO ROI ASIA
  tile_info.name[1]='ASIA'
  tile_info.ns_in[1]=13441
  tile_info.nl_in[1]=8961
  tile_info.ns_out[1]=785
  tile_info.nl_out[1]=785
  tile_info.gio_Xoffset[1]=7168
  tile_info.gio_Yoffset[1]=4032
  ;Info for GIO ROI Africa (as from Clerici), it was a test image, Smets:  Africa is conform the definition of VGT4Africa and does end on other boundaries => bounding box from 38N 20W -> 60E 35S
  tile_info.name[2]='Africa'
  tile_info.ns_in[2]=9633
  tile_info.nl_in[2]=8177
  tile_info.ns_out[2]=7841
  tile_info.nl_out[2]=8289
  tile_info.gio_Xoffset[2]=896
  tile_info.gio_Yoffset[2]=0

  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    res = FILE_TEST(client_machine_DIR+IDL_error_log_fname)
    IF (res NE 1) THEN BEGIN
      OPENW, lun, client_machine_DIR+IDL_error_log_fname, /GET_LUN
      PRINTF, lun, 'IDL_Error_log_file_init'
      FREE_LUN, lun
    ENDIF
    OPENU, IDL_error_log_fname, client_machine_DIR+IDL_error_log_fname, /GET_LUN, /APPEND
    PRINTF, IDL_error_log_fname, '###################################################################'
    PRINTF, IDL_error_log_fname, SYSTIME(), ' IDL error issued: '
    PRINTF, IDL_error_log_fname, SYSTIME(), ' Error index: ', STRTRIM(Error_status,2)
    PRINTF, IDL_error_log_fname, SYSTIME(), ' Error message: ', !ERROR_STATE.MSG
    FREE_LUN, IDL_error_log_fname
    CATCH, /CANCEL
    EXIT
  ENDIF
  

  
  ;Check that the log file exists, if not create an empty one, same for error
  res = FILE_TEST(client_machine_DIR+log_fname)
  IF (res NE 1) THEN BEGIN
    OPENW, lun, client_machine_DIR+log_fname, /GET_LUN
    PRINTF, lun, 'Log_file_init'
    FREE_LUN, lun
  ENDIF
  res = FILE_TEST(client_machine_DIR+error_log_fname)
  IF (res NE 1) THEN BEGIN
    OPENW, lun, client_machine_DIR+error_log_fname, /GET_LUN
    PRINTF, lun, 'Error_log_file_init'
    FREE_LUN, lun
  ENDIF
  ;Read the log file, may be empty (thus containing only the line 'Log_file_init'
  sTemplate = logFileStructDef()
  res = READ_ASCII(client_machine_DIR+log_fname, template = sTemplate)
  IF (N_TAGS(res) NE 0) THEN archive_fnames =  res.FIELD1 ELSE archive_fnames=''
  
  ;Check what's new on storage machine
  FOR t = 0, N_ELEMENTS(tiles)-1 DO BEGIN
    files_on_storage = FILE_BASENAME(FILE_SEARCH(storage_machine_GIO_DIR+'*'+STRTRIM(tiles[t],2)+'*.zip'))
    ;process only if there are new files
    IF (files_on_storage[0] NE '') THEN BEGIN 
      a_index = -1
      res=intersection(files_on_storage,archive_fnames,a_index = a_index, b_index=b_index, /INDEX_TAG)
      IF (a_index[0] EQ -1) THEN BEGIN
        ;all are new
        new_files_on_storage = files_on_storage
        disjoint=1
      ENDIF ELSE BEGIN
        ;retrieve only those that have not been processed so far
        ind = INDGEN(N_ELEMENTS(files_on_storage))
        res=intersection(a_index, ind, disjoint = disjoint)
        IF (FINITE(disjoint[0]) EQ 1) THEN new_files_on_storage = files_on_storage[disjoint]
      ENDELSE
      ;proceed if there are new files 
      IF (FINITE(disjoint[0]) EQ 1) THEN BEGIN
        FOR i=0, N_ELEMENTS(new_files_on_storage)-1 DO BEGIN
          ;extract the tile info of the selected tile
          tt = WHERE(tile_info.name EQ tiles[t])
          ret = convertGIO2MARSOP(new_files_on_storage[i], tile_info, tt, storage_machine_GIO_DIR, storage_machine_MP_DIR[t], client_machine_DIR)
          IF (ret NE 0) THEN BEGIN
            ;register the failure
            OPENU, lun_error, client_machine_DIR+error_log_fname, /GET_LUN, /APPEND
            PRINTF, lun_error, SYSTIME(), ' Error converting file ', new_files_on_storage[i], ' Error: ', ret
            FREE_LUN, lun_error 
          ENDIF ELSE BEGIN
            ;add filename to log file
            OPENU, lun_log, client_machine_DIR+log_fname, /GET_LUN, /APPEND
            PRINTF, lun_log, new_files_on_storage[i]
            FREE_LUN, lun_log
          ENDELSE
        ENDFOR
      ENDIF ELSE BEGIN
        ;files were present but they are not new, recorder it on the log error
        OPENU, lun_error, client_machine_DIR+error_log_fname, /GET_LUN, /APPEND
        PRINTF, lun_error, SYSTIME(), ' New files were not found for ROI: ' + STRTRIM(tiles[t],2)
        FREE_LUN, lun_error
      ENDELSE
    ENDIF ELSE BEGIN
      ;there is no new files at all, recorder it on the log error
      OPENU, lun_error, client_machine_DIR+error_log_fname, /GET_LUN, /APPEND
      PRINTF, lun_error, SYSTIME(), ' New files were not found for ROI: ' + STRTRIM(tiles[t],2)
      FREE_LUN, lun_error
    ENDELSE
  ENDFOR
END
    
