FUNCTION retrieve_pheno, info_fn
;just copied from calval
;
;example: PRINT, retrieve_pheno('E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\info_validation_accb.txt')
;
;########################
;Program settings
boxdim = 3        ;dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
valnan2 = -999   ;value for no data in the Rs file (other than NaN). Typically -9999 for pheno variables

;Retrieve info for the required job
ground_data_fn = read_info('ground_data_fn', info_fn)

;Read the table of ground data
tmp =  READ_CSV(ground_data_fn, $
  HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN)
PRINT, hdr
;load data into a strcture with the following fields: ;ID,Site,XUTM,YUTM,Year,HB,TB
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7'], $
  ['ID','Site','XUTM','YUTM','Year','HB','TB'])

;########################
;New:
avg_sos_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\averages\1gspy_1s_A1sos-1997_sos1_DOC_TZPavg' 
avg_eos_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\averages\1gspy_1s_A1sos-1997_eos1_DOC_TZPavg'
avg_tom_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\pheno sahel\averages\1gspy_1s_A1sos-1997_maxt1_DOC_TZPavg'
idList = uniqlist(data.ID)
avg_sos = FLTARR(N_ELEMENTS(idList))
avg_tom = avg_sos
avg_eos = avg_sos
FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
  ind = WHERE(data.ID EQ idList[i])
  LonLat = fromUTM2LonLat(data.XUTM[ind[0]],data.YUTM[ind[0]],28)
  z_val_sos = zprofileFromLonLat(LonLat[0], LonLat[1], avg_sos_fn, boxdim, valnan2)
  avg_sos[i] = z_val_sos[0]
  z_val_eos = zprofileFromLonLat(LonLat[0], LonLat[1], avg_eos_fn, boxdim, valnan2)
  avg_eos[i] = z_val_eos[0]
  z_val_tom = zprofileFromLonLat(LonLat[0], LonLat[1], avg_tom_fn, boxdim, valnan2)
  avg_tom[i] = z_val_tom[0]
  ;PRINT, STRTRIM(idList[i],2)+': '+STRTRIM(z_val_sos[0],2)+' '+STRTRIM(z_val_tom[0],2)+' '+STRTRIM(z_val_eos[0],2)
ENDFOR
gh = PLOT(idList,avg_sos, "g",  XRANGE=[0,MAX(idList)+1], XTITLE='Site ID', YTITLE='SOS/TOM/EOS')
gh = PLOT(idList,avg_tom, "b", OVERPLOT=1)
gh = PLOT(idList,avg_eos, "r", OVERPLOT=1)
PRINT, 'Average SOS DOY: ' + STRTRIM(MEAN(avg_sos),2)
PRINT, 'Date', STRTRIM(JD2DDMMYYYY(DOY_YEAR2JD(MEAN(avg_sos),2013)),2)
PRINT, 'Average TOM DOY: ' + STRTRIM(MEAN(avg_tom),2) 
PRINT, 'Date', STRTRIM(JD2DDMMYYYY(DOY_YEAR2JD(MEAN(avg_tom),2013)),2)
PRINT, 'Average EOS DOY: ' + STRTRIM(MEAN(avg_eos),2)
PRINT, 'Date', STRTRIM(JD2DDMMYYYY(DOY_YEAR2JD(MEAN(avg_eos),2013)),2)
RETURN,0
END