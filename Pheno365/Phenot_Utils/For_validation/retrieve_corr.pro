FUNCTION retrieve_corr, info_fn
;just copied from calval
;
;example: 
;PRINT, retrieve_corr('E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\info_validation_accb.txt')
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
variables=STRARR(2,3)
fnames = variables
variables[*,0] = ['GSL_corr','GSL_p']
variables[*,1] = ['dltSOS_corr','dltSOS_p']
variables[*,2] = ['maxV_corr','maxV_p']
fnames[*,0] = 'X:\WA corr analyis GIS\sahel resuts\len\' + ['len1VSacc1_corr.img',    'len1VSacc1_Pval_gain.img']
fnames[*,1] = 'X:\WA corr analyis GIS\sahel resuts\sos\' + ['sos1dltVSacc1_corr.img', 'sos1dltVSacc1_Pval_gain.img']
fnames[*,2] = 'X:\WA corr analyis GIS\sahel resuts\maxv\'+ ['maxv1VSacc1_corr.img',   'maxv1VSacc1_Pval_gain.img']

idList = uniqlist(data.ID)
avg_0 = FLTARR(N_ELEMENTS(idList))
avg_1 = avg_0
FOR f=0, N_ELEMENTS(fnames[0,*])-1 DO BEGIN  
  FOR i = 0, N_ELEMENTS(idList)-1 DO BEGIN
    ind = WHERE(data.ID EQ idList[i])
    LonLat = fromUTM2LonLat(data.XUTM[ind[0]],data.YUTM[ind[0]],28)
    z_val_0 = zprofileFromLonLat(LonLat[0], LonLat[1], fnames[0,f], boxdim, valnan2)
    avg_0[i] = z_val_0[0]
    z_val_1 = zprofileFromLonLat(LonLat[0], LonLat[1], fnames[1,f], boxdim, valnan2)
    avg_1[i] = z_val_1[0]
  ENDFOR
  sdR = STRING(FORMAT='(F5.2)',STDDEV(avg_0, /NAN, /DOUBLE))
  gh0 = PLOT(idList,avg_0, "g",  MARGIN = [0.15, 0.15, 0.20, 0.15], XRANGE=[0,MAX(idList)+1], $
             XTITLE='Site ID', YTITLE=variables[0,f], AXIS_STYLE = 1, NAME=variables[0,f], $
             TITLE='AVG(r) = '+STRING(FORMAT='(F5.2)',MEAN(avg_0))+ ' SD(r)=' + sdR, $
             SYMBOL="o", LINESTYLE="-")
  gh0.SYM_SIZE = 0.75
  gh1 = PLOT(idList,avg_1, "b",  MARGIN = [0.15, 0.15, 0.20, 0.15], XRANGE=[0,MAX(idList)+1], $
             XTITLE='Site ID', /CURRENT, AXIS_STYLE = 0, NAME=variables[1,f], $
             SYMBOL="o", LINESTYLE="-")
  gh1.SYM_SIZE = 0.75
  gh2 = PLOT(idList,idList*0+0.05, "b",  MARGIN = [0.15, 0.15, 0.20, 0.15], $
             /OVERPLOT, AXIS_STYLE = 0, NAME='P=0.05', LINESTYLE=2)
  ah1 =    AXIS('y',  TARGET = gh1, LOCATION = [max(gh1.xrange),0,0], TEXTPOS = 1, TITLE = variables[1,f])
  !null = LEGEND(target=[gh0, gh1], SHADOW=0, THICK=0)           

  PRINT, 'Average '+ variables[0,f] + ': ' + STRTRIM(MEAN(avg_0),2) + '  Median: ' + STRTRIM(MEDIAN(avg_0),2)
  PRINT, 'Average '+ variables[1,f] + ': ' + STRTRIM(MEAN(avg_1),2) + '  Median: ' + STRTRIM(MEDIAN(avg_1),2)
ENDFOR ;f
RETURN,0
END