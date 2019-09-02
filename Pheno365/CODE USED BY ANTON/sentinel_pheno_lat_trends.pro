FUNCTION Sentinel_pheno_lat_trends, dir

;dir =  'Z:\RAPHAEL_pheno_test\Results_2019May29';'D:\RAPHAEL_pheno_test\Results_2019May29'
dir_out = dir + '\Lat_corr'
FILE_MKDIR, dir_out
csv_file = dir + '\pheno_results.csv'
res = READ_CSV(csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN )
x = rename_tags(res, TAG_NAMES(res), [hdr[0:-2],'Empty'])
PRINT, hdr
cropTypes = x.crop_type[UNIQ(x.crop_type,SORT(x.crop_type))]
indices = x.index[UNIQ(x.index,SORT(x.index))]
h1_array = !NULL

;;make the plots for the big crops
;n_biggest_to_select = 8
;n_per_crop_type = INTARR(N_ELEMENTS(cropTypes))
;FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
;  res.indRetr = WHERE(x.crop_type EQ cropTypes[i], count)
;  n_per_crop_type[i] = count
;ENDFOR
;sub = REVERSE(SORT(n_per_crop_type))
;bigCropTypes = cropTypes[sub[0:n_biggest_to_select-1]]
bigCropTypes = raphael_crop_select()
FOR k = 0, N_ELEMENTS(indices)-1 DO BEGIN
  ;now regression with lat
  FOR u = 0, 2 DO BEGIN ;all data, only selected based on gaps and perror
    FOR i = 0, N_ELEMENTS(bigCropTypes)-1 DO BEGIN
      tmp = STRSPLIT(bigCropTypes[i],'-',/EXTRACT)
      tmp = STRMID(STRTRIM(tmp[0],2),0,18)
      tit = tmp + ', ' + indices[k] 
      prefix = indices[k]
      dat = raphael_data_select(x, u, indices[k], bigCropTypes[i])
      ;ret = CREATE_STRUCT('indRetr', res.indRetr, 'countRetr',count, 'countRetrFailure', countRetrFailure, 'subTitle', subTitle, 'res.res.suffix', res.res.suffix)
      dat.subTitle = dat.subTitle + ', n=' +STRTRIM(dat.countRetr,2) 
      
      
      symb = '.'
      ;left plot of 20% SOS, EOS, LGS     
      res = linregstat(x.Lat[dat.indRetr], x.SOS20[dat.indRetr])
      mrg = [0.2,0,0.1,0.25]
      h1 = PLOT(x.Lat[dat.indRetr], x.SOS20[dat.indRetr], YTITLE='SOS20', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,1], DIMENSIONS=[800,800])
      ;[offset, gain, corr, pgain]
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      res = linregstat(x.Lat[dat.indRetr], x.EOS20[dat.indRetr])
      mrg = [0.2,0.1,0.1,0.25]
      h1 = PLOT(x.Lat[dat.indRetr], x.EOS20[dat.indRetr], YTITLE='EOS20', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,3], /CURRENT)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      res = linregstat(x.Lat[dat.indRetr], x.LGS20[dat.indRetr])
      mrg = [0.2,0.2,0.1,0.15]
      h1 = PLOT(x.Lat[dat.indRetr], x.LGS20[dat.indRetr], YTITLE='LGS20', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,5], /CURRENT)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      ;right plot of 50% SOS, EOS, LGS      
      res = linregstat(x.Lat[dat.indRetr], x.SOS50[dat.indRetr])
      mrg = [0.2,0,0.1,0.25]
      h1 = PLOT(x.Lat[dat.indRetr], x.SOS50[dat.indRetr], YTITLE='SOS50', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,2], /CURRENT)
      ;[offset, gain, corr, pgain]
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      res = linregstat(x.Lat[dat.indRetr], x.EOS50[dat.indRetr])
      mrg = [0.2,0.1,0.1,0.25]
      h1 = PLOT(x.Lat[dat.indRetr], x.EOS50[dat.indRetr], YTITLE='EOS50', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,4], /CURRENT)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      res = linregstat(x.Lat[dat.indRetr], x.LGS50[dat.indRetr])
      mrg = [0.2,0.2,0.1,0.15]
      h1 = PLOT(x.Lat[dat.indRetr], x.LGS50[dat.indRetr], YTITLE='LGS50', XTITLE='Lat N', LINESTYLE='', SYMBOL=symb, MARGIN = mrg, LAYOUT=[2,3,6], /CURRENT)
      h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
      hll = PLOT([0,0],[0,0], TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)'), COLOR='b',/OVERPLOT,/NODATA)
      hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
      
      ht = TEXT(0.5,0.975,tit, ALIGNMENT=0.5,FONT_SIZE= 16)
      ht = TEXT(0.5,0.95,dat.subTitle, ALIGNMENT=0.5,FONT_SIZE= 14)
     
      h1.save, dir_out + '\' + tmp + '_'+ prefix +'_lat_corr_' + dat.suffix + '.png'
      h1.close
    ENDFOR ; i crop type
  ENDFOR  ;u
ENDFOR  ; index k


;make the table for all crops
OPENW, lun, dir_out + '\pheno_lat_corr.csv', /GET_LUN
dlmtr = ','
PRINTF, lun, STRJOIN(['Crop','Index','Sampling','n','SOS20_r','SOS50_r','EOS20_r','EOS50_r','LGS20_r','LGS50_r','r if P<0.05'], dlmtr)

FOR k = 0, N_ELEMENTS(indices)-1 DO BEGIN
  ;now regression with lat
  FOR u = 0, 2 DO BEGIN ;all data, only selected based on gaps and perror
    FOR i = 0, N_ELEMENTS(bigCropTypes)-1 DO BEGIN
      ;tmp = STRSPLIT(cropTypes[i],'-',/EXTRACT)
      dat = raphael_data_select(x, u, indices[k], bigCropTypes[i])
      ;n = STRTRIM(N_ELEMENTS(res.indRetr),2)

      res = linregstat(x.Lat[dat.indRetr], x.SOS20[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN sos20_r = STRTRIM(res[2],2) ELSE sos20_r = 'n.s.'
      ENDIF ELSE sos20_r = 'n.a.'
      res = linregstat(x.Lat[dat.indRetr], x.EOS20[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN eos20_r = STRTRIM(res[2],2) ELSE eos20_r = 'n.s.'
      ENDIF ELSE eos20_r = 'n.a.'
      res = linregstat(x.Lat[dat.indRetr], x.LGS20[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN LGS20_r = STRTRIM(res[2],2) ELSE LGS20_r = 'n.s.'
      ENDIF ELSE LGS20_r = 'n.a.'
      res = linregstat(x.Lat[dat.indRetr], x.SOS50[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN sos50_r = STRTRIM(res[2],2) ELSE sos50_r = 'n.s.'
      ENDIF ELSE sos50_r = 'n.a.'
      res = linregstat(x.Lat[dat.indRetr], x.EOS50[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN eos50_r = STRTRIM(res[2],2) ELSE eos50_r = 'n.s.'
      ENDIF ELSE eos50_r = 'n.a.'
      res = linregstat(x.Lat[dat.indRetr], x.LGS50[dat.indRetr])
      IF (FINITE(res[3]) EQ 1) THEN BEGIN
        IF (res[3] LT 0.05) THEN LGS50_r = STRTRIM(res[2],2) ELSE LGS50_r = 'n.s.'
      ENDIF ELSE LGS50_r = 'n.a.'
      PRINTF, lun, STRJOIN([cropTypes[i],indices[k],dat.subTitle, STRTRIM(dat.countRetr,2),SOS20_r,SOS50_r,EOS20_r,EOS50_r,LGS20_r,LGS50_r], dlmtr)

      IF (CHECK_MATH() NE 0) THEN STOP
    ENDFOR ; i crop type
  ENDFOR  ;u
ENDFOR  ; index k
FREE_LUN, lun
RETURN, 0
END