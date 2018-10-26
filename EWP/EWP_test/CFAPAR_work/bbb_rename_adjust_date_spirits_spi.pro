PRO bbb_rename_adjust_date_SPIRITS_SPI
;this procedure adjust the filename of spi files (the spi 1 month of dek 3 is saved by SPIRITS as tt = 1 to follow the usual convention)
;this pro expect the files to be prefixYYYYTTsuffix
;set accumulation period (3 for monthli SPI, 9 for 3 months SPI)
ap = 3
;months = STRTRIM(ap/3,2)
;set input dir
dir = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_ROI_Africa\CHIRPS_SPI1'
dir_out = '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\CHIRPS_SPI_ROI_Africa\CHIRPS_SPI1_correct_fn'
;set prefix 
prefix = 'chirpsSPI1'
;and suffix
suffix = 'spi'  
;get the list of files to be renamed (both file name and hdr)
fn_list = FILE_BASENAME(FILE_SEARCH(dir, prefix+'*'+suffix+'.img'), '.img')
;sort it descending as we have to move the date forward in time. In this way we don't overwrite files
fn_list = fn_list(REVERSE(SORT(fn_list)))
FOR i = 0, N_ELEMENTS(fn_list)-1 DO BEGIN
  ;here we are accessing file in descending order, from the most recent to the oldest
  ;get the YYYY and TT
  yyyy_in = STRMID(fn_list[i], STRLEN(prefix), 4)
  yyyy_out = yyyy_in  ;set it temporarely, after check if dek is passing the end of the year
  tt_in =  FIX(STRMID(fn_list[i], STRLEN(prefix)+4, 2))
  tt_out = tt_in + ap -1 
  IF (tt_out GT 36) THEN BEGIN
    tt_out = tt_out - 36 
    yyyy_out = yyyy_out + 1
  ENDIF
  ;now I have the new date for the file, rename both .img and .hdr
  FILE_COPY, dir + '\' + fn_list[i] + '.img', dir_out + '\' + prefix + '_' + STRING(yyyy_out,FORMAT = '(I04)') + STRING(tt_out,FORMAT = '(I02)') + '_ttok' + '.img'
  FILE_COPY, dir + '\' + fn_list[i] + '.hdr', dir_out + '\' + prefix + '_' + STRING(yyyy_out,FORMAT = '(I04)') + STRING(tt_out,FORMAT = '(I02)') + '_ttok' +  '.hdr' 
  ;the open the .hdr and fix the date as well
  ;not needed as further processing is with idl and I never look at this date
ENDFOR


END