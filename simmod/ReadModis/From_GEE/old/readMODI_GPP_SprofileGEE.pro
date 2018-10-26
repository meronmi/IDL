PRO readmo_GPP_GEE
path = 'E:\SimMod_data\MODIS_data\Majadas';'X:\Active Projects\GEE\pixel extraction'
fname = 'profileGPP.csv'
;path = 'E:\SimMod_data\MODIS_data\Sudan'
;fname = 'profileGPP_sudan.csv'
res = readMODI_GPP_SprofileGEE(path, fname)
END

FUNCTION readMODI_GPP_SprofileGEE, path, fname
;read and process a time series of product MOD17A2H.006
;extracted with GEE
;profile is gathered with the GEE function of Guido 2016_modis_GPPprofile
;*****************************
;PARAMETERS
outNoData = -99999
;bands to be saved in output
bnames2output = ['Gpp_daily', 'PsnNet_daily'] ;GPP MOD is kgC m-2 in 8 days, scale = 0.0001
scale = 0.0001
k = 1000  ;CONVERSSION FROM kG TO g
;*****************************
fullPathFname = path + '\' + fname
res = QUERY_CSV(fullPathFname, Info)
IF (res NE 1) THEN STOP
nlines = info.lines
nfields = info.nfields
data = READ_CSV(fullPathFname, COUNT = nobs, HEADER=hdrRow, MISSING_VALUE=outNoData)


tableHdr = ['Year', 'DOY_comp', 'Gpp_daily', 'PsnNet_daily']
table = FLTARR(4, nobs)*!VALUES.F_NAN

;fill the table
;Year, DOY_comp, Year.dayfract, Year_acq
ind = WHERE(hdrRow EQ 'id' OR hdrRow EQ 'system:index')
column = data.(ind)
tmp = STRSPLIT(data.(ind), '_', /EXTRACT)

FOR i = 0, N_ELEMENTS(tmp)-1 DO BEGIN
  tmp = column[i]
  tmp = STRSPLIT(tmp, '_', /EXTRACT)
  ;Year
  table[0,i] = tmp[0]
  ;DOY_comp
  table[1,i] = ddmmyyyy2doy(tmp[2], tmp[1], tmp[0]) 
ENDFOR

ind = WHERE(hdrRow EQ 'Gpp')
table[2,*] = data.(ind) * scale * k / 8.0
ind = WHERE(hdrRow EQ 'PsnNet')
table[3,*] = data.(ind) * scale * k / 8.0

fname_out = STRSPLIT(fname, '.', /EXTRACT)
;adjust for leap year the last 8-day composite
;as from MOD17 User's Guide
ind = WHERE(table[1,*] EQ 361)
leaps_years = isleap( table[0,ind])
FOR i = 0, N_ELEMENTS(ind)-1 DO BEGIN
  IF (leaps_years[i] EQ 1) THEN BEGIN
    table[2,ind[i]] = table[2,ind[i]] * 8.0 / 6.0 
    table[3,ind[i]] = table[3,ind[i]] * 8.0 / 6.0
  ENDIF ELSE BEGIN
    table[2,ind[i]] = table[2,ind[i]] * 8.0 / 5.0
    table[3,ind[i]] = table[3,ind[i]] * 8.0 / 5.0
  ENDELSE
ENDFOR

WRITE_CSV, path + '\' + fname_out[0] + '_cleaned.csv', table, HEADER = tableHdr
RETURN, 0
END