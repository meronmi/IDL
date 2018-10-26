PRO runit
 firstTT = 1
 firstYYYY = 1999
 lastTT = 36
 lastYYYY = 2013
 cum_period = [3,6,9,12,15,18];18;11;[13,14,15];[3,4,5,6,7,8,9,10,12]
 ns = 1867
 nl = 348
 dir = 'E:\WA\EWP\SPIx from CHIRPS';'E:\WA\EWP\SPI1 from Original Tamsat'
 
 suffix = 'spi'
 FOR i = 0, N_ELEMENTS(cum_period) - 1 DO BEGIN
  ;IF cum_period[i] EQ 3 THEN tmp = STRTRIM(1,2) ELSE tmp = STRTRIM(cum_period[i],2)
  tmp = STRTRIM(cum_period[i],2)
  ;prefix = 'SPI'+ tmp + '_from_dek'
  prefix = 'chirpsSPI'+ tmp + '_from_dek'
  build_SPI_meta, firstTT, firstYYYY, lastTT, lastYYYY, cum_period[i], ns, nl, dir, prefix, suffix
 ENDFOR
END


PRO build_SPI_meta, firstTT, firstYYYY, lastTT, lastYYYY, cum_period, ns, nl, dir, prefix, suffix
fname = dir + '\' + 'BBB_meta_SPI' + STRTRIM(cum_period,2) + '_' + $
        STRTRIM(firstTT,2) + '-' + STRTRIM(firstYYYY,2) + '_' + $
        STRTRIM(lastTT,2) + '-' + STRTRIM(lastYYYY,2) + '.mta'
OPENW, lun, fname, /GET_LUN
PRINTF, lun, 'ENVI META FILE'
n_images = (36-firstTT+1) + (lastTT) + (lastYYYY-firstYYYY+1-2)*36
PRINT, 'N images: ' + STRTRIM(n_images,2)
;now change the dates to align SPI

TTo = firstTT
YYYY = firstYYYY
;adjust dekad and year on the basis of the cumulation period
TT  = TTo - cum_period + 1
IF (TT LE 0) THEN BEGIN ;I have to go to the previous year
  YYYY = YYYY - 1
  TT = 36 + TT
ENDIF

FOR i = 0, (n_images-1) DO BEGIN
  ;make the name
  IF (TT LT 10) THEN TTstr = '0' + STRTRIM(TT,2) ELSE TTstr = STRTRIM(TT,2)
  YYstr = STRMID(STRTRIM(YYYY,2),2,2)
  fn = dir + '\' + prefix + YYstr + TTstr + suffix + '.img'
  PRINTF, lun, 'File : ' + fn
  PRINTF, lun, 'Bands: 1'
  PRINTF, lun, 'Dims : 1-' + STRTRIM(ns,2) + ',1-' + STRTRIM(nl,2)
  TT = TT + 1
  IF (TT GT 36) THEN BEGIN
    TT = 1
    YYYY = YYYY +1
  ENDIF
  
ENDFOR
FREE_LUN, lun

END