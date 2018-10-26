PRO extract_table_from_country_analysis
dir = 'X:\Active Projects\FAO SOFI\pheno_copy_of_z\COUNTRY ANALYS P10\FOR IDL'
fns = FILE_SEARCH(dir, '*.csv')
fn_out = dir + '\summaryIDL_more_countries.csW'
OPENW, lun, fn_out, /GET_LUN
FOR f = 0, N_ELEMENTS(fns)-1 DO BEGIN
  t = READ_CSV(fns[f], HEADER=hdr, COUNT=countRec)  
  IF (countRec GT 1) THEN BEGIN
    FOR i = 0, N_ELEMENTS(hdr)-1 DO BEGIN
      tmp = hdr[i]
      STRREPLACE, tmp, '%', 'prcnt'
      STRREPLACE, tmp, '(', ''
      STRREPLACE, tmp, ')', ''
      STRREPLACE, tmp, ' ', ''
      STRREPLACE, tmp, ' ', ''
  ;    PRINT, hdr[i],' -> ', tmp
      hdr[i]= tmp
    ENDFOR
  
    ;help, t
    tags = TAG_NAMES(t)
    t = rename_tags(t, tags, hdr)
    PRINT, t.GAUL[0]
    IF (t.GAUL[0] EQ 'Philippines') THEN BEGIN
      PRINT, 'debiggalo'
    ENDIF
    ;help, t
    out = !NULL
    targetToConsider = !NULL
    ;crop is always analysed (if present and with 200), range only if more than 10% crops
    target_tmp = ['cropland','rangeland']
    target = ['cropland AND MKp<0.100000','rangeland AND MKp<0.100000']
    strTargetOut = ['Croplands','Rangelands']
    ind = WHERE(t.Mask EQ target_tmp[0], count)
    IF (count GT 0) THEN BEGIN
      IF (t.n[ind[0]] GT 200) THEN targetToConsider = 0
    ENDIF
    indC = WHERE((t.TrendInidcator EQ 'none') AND (t.Variable EQ 's') AND (t.Season EQ 1) AND (t.Mask EQ target_tmp[0]), countC)
    indR = WHERE((t.TrendInidcator EQ 'none') AND (t.Variable EQ 's') AND (t.Season EQ 1) AND (t.Mask EQ target_tmp[1]), countR)
    IF (countR GT 0) THEN BEGIN
      IF (t.n[indR[0]] GT 200) THEN BEGIN
        IF (t.n[indR]/FLOAT(t.n[indC]) GT 0.1) THEN  targetToConsider= [targetToConsider, 1]
      ENDIF
    ENDIF
    
    ;see if second seeas has to be considered
    FOR tti = 0, N_ELEMENTS(targetToConsider)-1 DO BEGIN
      tt = targetToConsider[tti]
      ss = 1
      IF (tt EQ 0) THEN fracbi = t.FractionofBimodal[indC[0]] ELSE fracbi = t.FractionofBimodal[indR[0]] 
      IF (fracbi GT 10) THEN ss = 2
      FOR s = 1, ss DO BEGIN
        ;find target, gsl, % with trend, % pos, % neg, mean TS of pos an mean ts of neg
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 'l') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
                    (t.SignofMKtau EQ 'all'))
        out = [out, t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 'l') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'pos'))
        out = [out,t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 'l') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'neg'))
        out = [out,t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 'l') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'pos'))
        out = [out,t.avg[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 'l') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'neg'))
        out = [out,t.avg[ind]]
        ;PRINT, STRJOIN(STRING(out), ',')
        ;same for SOS
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 's') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'all'))
        out = [out, t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 's') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'pos'))
        out = [out,t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 's') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'neg'))
        out = [out,t.SigprcntOftarget[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 's') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'pos'))
        out = [out,t.avg[ind]]
        ind = WHERE((t.TrendInidcator EQ 'TS_slope') AND (t.Variable EQ 's') AND (t.Season EQ s) AND (t.Mask EQ target[tt]) AND $
          (t.SignofMKtau EQ 'neg'))
        out = [out,t.avg[ind]]
        country_name = t.GAUL[0]
        STRREPLACE, country_name, 'AFR_', ''
        PRINT, country_name+','+strTargetOut[tt]+','+','+STRTRIM(s)+','+STRJOIN(STRING(out), ',')
        PRINTF, lun, country_name+','+strTargetOut[tt]+','+','+STRTRIM(s)+','+STRJOIN(STRING(out), ',')
        out = !NULL
        bebug =1
      ENDFOR  ;seasons
    ENDFOR  ;targets
  ENDIF
  ;PRINT, 'debug'
ENDFOR ;file fns
FREE_LUN, lun

END