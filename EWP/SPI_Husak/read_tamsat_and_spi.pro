Pro read_tamsat_and_spi
;test for january (too many zeros) try with May
;from 1984 to 2013.
; to be compared with spirits file: date = 20000501, SPI1_from_dek0013spi.img

;note: 1986 , 1989, 93 missing so year 200 is IDL sub 13
;file = 'E:\WA\EWP\TAMSAT_MONTHLY_ROI_WA\float_BSQ_May_all_years'
;85, 88 missing
file = 'E:\WA\EWP\TAMSAT_MONTHLY_ROI_WA\float_BSQ_December_all_years'
ns = 1867
nl = 348
nb = 28
data = FLTARR(ns,nl,nb)
OPENR, lun, file, /GET_LUN
READU, lun, data
FREE_LUN, lun


ps1 = 1488  
pl1 = 242
PRINT, 'SPIRITS SPI = ' + STRTRIM(-89*0.001, 2)
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts, MIN_POSOBS = 3)
PRINT, 'SPI Greg = ' + STRTRIM(res,2)

ps1 = 1381
pl1 = 110
PRINT, 'SPIRITS SPI = ' + STRTRIM(1242*0.001, 2)
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts, MIN_POSOBS = 3)
PRINT, 'SPI Greg = ' + STRTRIM(res,2)

ps1 = 331
pl1 = 315
PRINT, 'SPIRITS SPI = ' + STRTRIM(2382*0.001, 2)
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts)
PRINT, 'SPI Greg = ' + STRTRIM(res[13],2)

ps1 = 213
pl1 = 124
PRINT, 'SPIRITS SPI = -11003 missing'
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts)
PRINT, 'SPI Greg = ' + STRTRIM(res[13],2)

ps1 = 207
pl1 = 119
PRINT, 'SPIRITS SPI = -11004 error'
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts)
PRINT, 'SPI Greg = ' + STRTRIM(res[13],2)

ps1 = 221
pl1 = 25
PRINT, 'SPIRITS SPI = -11004 error'
ts = data[ps1,pl1, *]
res = precip_2_spi_gh(ts)
PRINT, 'SPI Greg = ' + STRTRIM(res[13],2)

END