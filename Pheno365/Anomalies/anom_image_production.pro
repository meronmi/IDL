FUNCTION anom_image_production, boi
; Note that the file output is alway a bsq, not a bil (it was easier to code)
; if boi is set to 0 in the info file then do it for all bands
COMMON Share_image_production, path_out, fname, fNameX, aoper, ns, nl, nb, transformJD2DOC, s, ngspy_image, doavg
bois = boi
;if boi was set to 0 in the info file, compute the anomalies for all bands
IF (bois EQ -1) THEN bois = INDGEN(nb)
fNameX_Aoper=fNameX+'_' + aoper


FOR ngspy=s, 2 DO BEGIN 
  ;Prepare the environment for writing files. Relevant bands are written to files 
  ;because AVG at depdt level works with file
  path_out_2 = path_out + '\' + fname
  IF (N_ELEMENTS(bois) EQ 1) THEN boi_tag = STRTRIM(bois+1,2) + 'boi_' $
      ELSE boi_tag = 'all_bands_'
  FILE_MKDIR, path_out_2
  tmp_pheno_full_path_name = path_out_2+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+boi_tag+fNameX
  tmp_pheno_dev_full_path_name = path_out_2+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+boi_tag+fNameX_Aoper
  IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPavg') THEN $
    tmp_pheno_avg_full_path_name = path_out_2+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+fNameX+'_TZPavg'
  IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPsd') THEN $
    tmp_pheno_sd_full_path_name = path_out_2+'\'+strtrim(ngspy,2)+'gspy_'+STRTRIM(s,2)+'s_'+fNameX+'_TZPsd'
  OPENW, W1, tmp_pheno_full_path_name , /GET_LUN      
  OPENW, W2, tmp_pheno_dev_full_path_name , /GET_LUN
  FOR i = 0, N_ELEMENTS(bois)-1 DO BEGIN
    ;if the rquired operation is not min or max, also an avg file is saved.
    ;So, if it exists (it was not a min max), include it in the analysis
    pheno_avg=0
    IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPavg') EQ 1 THEN BEGIN
      ;check if it is one band (when devY is issued it is multi band)
      temp_nb=read_info('bands', path_out+'\'+fNameX_Aoper+'_TZPavg'+'.hdr')
      IF (temp_nb EQ 1) THEN bb = 0 ELSE bb=bois[i]
      pheno_avg = read_1_BIL_band(path_out+'\'+fNameX_Aoper+'_TZPavg', 4, ns, nl, temp_nb, bb) 
    ENDIF
    ;same for the SD (actually redundant, if AVG exists, also SD exists)
    pheno_sd=0
    IF FILE_TEST(path_out+'\'+fNameX_Aoper+'_TZPsd') EQ 1 THEN BEGIN
      ;check if it is one band (when dev is issued or multiband, when devY is issued)
      temp_nb=read_info('bands', path_out+'\'+fNameX_Aoper+'_TZPsd'+'.hdr')
      IF (temp_nb EQ 1) THEN bb = 0 ELSE bb=bois[i]
      pheno_sd = read_1_BIL_band(path_out+'\'+fNameX_Aoper+'_TZPsd', 4, ns, nl, temp_nb, bb) 
    ENDIF
    
    
    ;Start the analysis for a given bois and areas with 1 GS and those with 2 GS
    pheno_boi=    read_1_BIL_band(path_out+'\'+fNameX, 4, ns, nl, nb, bois[i])  ;pheno is used to retrieve failed seasons (whic in dev computation can be set to 0 or NaN)
    IF (N_ELEMENTS(pheno_boi) EQ 1) THEN STOP ;we have a problem..
    ;if dekoc operation was not required, fNameX is just a copy, delete it
    ;IF (transformJD2DOC EQ 0) THEN FILE_DELETE, path_out + '\' + fNameX  
    
    pheno_dev_boi=read_1_BIL_band(path_out+'\'+fNameX_Aoper, 4, ns, nl, nb, bois[i])
    IF (N_ELEMENTS(pheno_dev_boi) EQ 1) THEN STOP
  

    ;e.g. if SOS1, divide in ngspy=1 and 2, is SOS2 compute only
    ;ngpsy=2 (it has to have a second season)
    ;work only on pixels having ngspy GS per year
    ind=WHERE(ngspy_image EQ ngspy, count)
    IF (count EQ 0) THEN STOP
    tmp_pheno = pheno_boi * !VALUES.F_NAN               ;initialize to NaN
    tmp_pheno_dev = pheno_dev_boi * !VALUES.F_NAN
    IF (N_ELEMENTS(pheno_avg) NE 0) THEN BEGIN  ;if it exists, use it..
      tmp_pheno_avg = pheno_avg * !VALUES.F_NAN
      tmp_pheno_avg[ind] = pheno_avg[ind]
    END
    IF (N_ELEMENTS(pheno_sd) NE 0) THEN BEGIN  ;if it exists, use it..
      tmp_pheno_sd = pheno_sd * !VALUES.F_NAN
      tmp_pheno_sd[ind] = pheno_sd[ind]
    END
    tmp_pheno[ind] = pheno_boi[ind]                     ;ind on correct ngspy
    tmp_pheno_dev[ind] = pheno_dev_boi[ind]
    
    ;treat seson failures
    ind999=WHERE(tmp_pheno EQ -999, count999)           ;ind999 on failed seasons
    indnf= WHERE((tmp_pheno NE -999) AND (ngspy_image EQ ngspy), countnf) ;ind on not failed and correct ngspy
    
    IF (N_ELEMENTS(bois) EQ 1) THEN BEGIN
      ;Compute and print some basic stat
      PRINT, '**********************************************************'
      PRINT, 'Basic stat of band of interest number = '+STRTRIM(bois+1,2)+' (bois = '+STRTRIM(bois,2)+')'
      PRINT, 'Analysing the area with ngspy = ' + STRTRIM(ngspy,2)
      PRINT, 'Number of pixels =              ' + STRTRIM(count,2)
      PRINT, 'Number of seson failures =      ' + STRTRIM(count999,2)
      PRINT, '% of season failures =          ' + STRTRIM(FIX(count999/double(count)*100.0d),2)
      PRINT, ''
      PRINT, 'Statistics derived on required bois (excluding season failures):'
      PRINT, fNameX + ' MIN-MAX =    ' + STRTRIM(MIN(tmp_pheno[indnf], /NAN))+'-'+STRTRIM(MAX(tmp_pheno[indnf], /NAN))
      PRINT, fNameX + ' AVG =    ' + STRTRIM(MEAN(tmp_pheno[indnf], /NAN, /DOUBLE))
      PRINT, fNameX + ' STDDEV = ' + STRTRIM(STDDEV(tmp_pheno[indnf], /NAN, /DOUBLE))
      
      PRINT, fNameX_Aoper + ' MIN-MAX =    ' + STRTRIM(MIN(tmp_pheno_dev[indnf], /NAN))+'-'+STRTRIM(MAX(tmp_pheno_dev[indnf], /NAN))
      PRINT, fNameX_Aoper + ' AVG =    ' + STRTRIM(MEAN(tmp_pheno_dev[indnf], /NAN, /DOUBLE))
      PRINT, fNameX_Aoper + ' STDDEV = ' + strtrim(STDDEV(tmp_pheno_dev[indnf], /NAN, /DOUBLE))
    ENDIF
    
    ;write the band
    WRITEU, W1, tmp_pheno
    WRITEU, W2, tmp_pheno_dev   
  ENDFOR  ;FOR i = 0, N_ELEMENTS(bois)-1
  FREE_LUN, W1
  FREE_LUN, W2
  
  ; WRITE HEADER OF THE OUTPUT
  HEADER_OUT=tmp_pheno_full_path_name+'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = pix based'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(nl)
  printf,3,'bands   ='+STRCOMPRESS(N_ELEMENTS(bois))
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 4'
  printf,3,'interleave = bsq'
  printf,3,'byte order = 0'
  CLOSE, 3
  

  HEADER_OUT=tmp_pheno_dev_full_path_name+'.hdr'
  OPENW, 3, HEADER_OUT
  printf,3,'ENVI'
  printf,3,'description = pix based'
  printf,3,'samples ='+STRCOMPRESS(ns)
  printf,3,'lines   ='+STRCOMPRESS(nl)
  printf,3,'bands   ='+STRCOMPRESS(N_ELEMENTS(bois))
  printf,3,'header offset = 0'
  printf,3,'file type = ENVI Standard'
  printf,3,'data type = 4'
  printf,3,'interleave = bsq'
  printf,3,'byte order = 0'
  CLOSE, 3
  IF (N_ELEMENTS(pheno_avg) NE 0) THEN BEGIN
    OPENW, W1, tmp_pheno_avg_full_path_name , /GET_LUN
    WRITEU, W1, tmp_pheno_avg
    FREE_LUN, W1
    HEADER_OUT=tmp_pheno_avg_full_path_name+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = pix based'
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(nl)
    printf,3,'bands   ='+STRCOMPRESS(1)
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 4'
    printf,3,'interleave = bsq'
    printf,3,'byte order = 0'
    CLOSE, 3
  ENDIF
  IF (N_ELEMENTS(pheno_sd) NE 0) THEN BEGIN
    OPENW, W1, tmp_pheno_sd_full_path_name , /GET_LUN
    WRITEU, W1, tmp_pheno_sd
    FREE_LUN, W1
    HEADER_OUT=tmp_pheno_sd_full_path_name+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = pix based'
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(nl)
    printf,3,'bands   ='+STRCOMPRESS(1)
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 4'
    printf,3,'interleave = bsq'
    printf,3,'byte order = 0'
    CLOSE, 3
  ENDIF

  IF (doavg EQ 1) THEN BEGIN
    dt=read_info('data type', dep_fname +'.hdr')
    res=dep_AVG_of_pheno(tmp_pheno_full_path_name, 4, dep_fname, dt, ns, nl, 1)
    IF res NE 0 THEN RETURN, 20 
    res=dep_AVG_of_pheno(tmp_pheno_dev_full_path_name, 4, dep_fname, dt, ns, nl, 1)
    IF res NE 0 THEN RETURN, 20 
    ;FILE_DELETE, tmp_pheno_full_path_name, tmp_pheno_dev_full_path_name
  ENDIF ELSE BEGIN
    PRINT, 'Files were not averaged at dep level'
  ENDELSE
ENDFOR  ;ngspy


CLOSE, /ALL
RETURN, 0
END