FUNCTION failure_analysis
;  Purpose:
;     Use aligned sos (or sos dekoc, it's the same) to determine, for each 
;     department, the number of pixels showing season failure is computed for 
;     each the available year(excluding incomplete years, see 
;     variable ‘bex’), and then anomaly % variation is computed, 
;     always at the department level, for that particular year (failure_analysis),
;     that is for a given band of pheno (boi, band of interest in the phenol 
;     product), which is not included in the average. It is thus a devY.
;     Also compute simply the fraction of pixel failed for the dep, that boi
;  Outcome:
;     - 1ngspy_1s_?boi_failure_devY_depAVG  dep avg anomaly files for areas with 1 gs per year
;     - 2ngspy_1s_?boi_failure_devY_depAVG  dep avg anomaly files for areas with 2 gs per year
;     - 2ngspy_2s_?boi_failure_devY_depAVG  dep avg anomaly files for areas with 2 gs per year
;     - 1ngspy_1s_?boi_failure_%_depAVG  dep avg anomaly files for areas with 1 gs per year
;     - 2ngspy_1s_?boi_failure_%_depAVG  dep avg anomaly files for areas with 2 gs per year
;     - 2ngspy_2s_?boi_failure_%_depAVG  dep avg anomaly files for areas with 2 gs per year


;  Usage:
;     adjust parameter setting in the code for the moment!!

;  Input parameters: 
;     - fnames:     sos file base name
;                   *only base name without 1 or 2 (e.g. 'sos') 
;     - path:       working path (e.g. 'X:\IGAD\VGT data\pheno_products\somalia v2')
;     - fname_rel:  file name of the reliability map, only rel<1000 is considered
;                   if fname_rel='' this check is not performed
;                   *only base name without 1 or 2 (e.g. 'rel') 
;  Return value: 
;     0:  normal completion 
;    10:  error has occurred
;    
;###################################################################################
;########### USER SETTINGS #########################################################

;**Define here the band to be analyzed in detail
boi=13    ;Band Of Interest (starts from 0)
;**Define the bands to be exclude from the analysis because they store incomplete years
;e.g. 19/1997-18/1998 and 19/2011-18/2012 (so first and lst
bex=[0,14]  ;Bands to EXclude (starts from 0)

;**Pheno products to be analyzed, analyze separately pdekad (progressive dekad as sos and eos)
;that must be dekoc, and TZP data (len, acc) that don't have to be dekoc
fnames=['A31-1997_sos'];['A19-1997_sos']         ;fnames=['sos1','len1','acc1', 'sos2', 'len2', 'acc2']
dtype=4
;**work path 
path='K:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv\REALIGN_eos_31';'X:\HoA\VGT data\pheno_products';'K:\HoA\Pheno_products'
;**reliability map (provide '' if no relaibility check has to be performed'
;if a correct file name is provided only thos season having reliability LT 1000 are used
fname_rel=''; 'rel'     
;**globstat map
fname_globstat='globstat'
;**absolute path of the raster department map
dep_fname='D:\Users\meronmi\Documents\JRC\JRC Documents\HoA 2011\GIS\HoA_Gaul2_raster'
dep_dtype=12  ;data type of department map
;**file specitification
ns=3586
nl=3810
nb=15
data_type=4
;###################################################################################





;Open globstat to retrieve the areas with 1/2 GS
ngspy_image=read_1_BIL_band(path+'\'+fname_globstat, 2, ns, nl, 2, 1)

 ;open dep and retrieve ids
IF FILE_TEST(dep_fname) EQ 0 THEN RETURN, 10
OPENR, R2, dep_fname, /GET_LUN
; Load dep
CASE dep_dtype OF
  2:  dept=INTARR(ns, nl)
  12: dept=UINTARR(ns, nl)
  ELSE: return, 20 
ENDCASE 
READU, R2, dept
dept=FLOAT(dept)
;set 0 (background value) to NaN
ind=WHERE(dept EQ 0, count)
IF count NE 0 THEN dept[ind]=!VALUES.F_NAN
;Find all unique codes
arr2=dept
ind=WHERE(FINITE(arr2) EQ 1)
arr=arr2[ind]
sortarr2=arr[SORT(arr)]
ids=sortarr2[UNIQ(sortarr2)]  ;this is the list of available departments


;bands to be analysed
bex=[bex,boi]
bex=bex[SORT(bex)]
b_inds = SetDifference(INDGEN(nb), bex)

fname=fnames[0]  
;work first on SOS1 and then SOS2
FOR s=1, 2 DO BEGIN  
  fname0=fname+strtrim(s,2)
  IF (fname_rel NE '') THEN fname0_rel=fname_rel+strtrim(s,2)
  ;OPEN FILES and load
  ;sos and rel
  IF FILE_TEST(path+'\'+fname0) EQ 0 THEN return, 10
  OPENR, R1, path+'\'+fname0, /GET_LUN
  IF (fname_rel NE '') THEN BEGIN
    IF FILE_TEST(path+'\'+fname0_rel) EQ 0 THEN PRINT, 'Relalibility map not used!'
    OPENR, R2, path+'\'+fname0_rel, /GET_LUN
    asslinerel = ASSOC(R2, FLTARR(ns,nb))
  ENDIF
  CASE dtype OF
    2: assline = ASSOC(R1, INTARR(ns,nb))
    4: assline = ASSOC(R1, FLTARR(ns,nb))
    ELSE: return, 20 
  ENDCASE
  data=FLTARR(ns, nl, nb)
  ;load data excluding reliability GT 1000 if required
  FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
      tmp=float(assline[line])
      ;Exclude reliability over 1000 if required
      IF (fname_rel NE '') THEN BEGIN ;set to NaN those data having reliability greater than 1000
        rel=float(asslinerel[line])
        indf=WHERE(FINITE(rel), countf) ;check finitness to avoid NaN
        IF countf NE 0 THEN BEGIN 
           ind=WHERE(rel[indf] GE 1000, count)
           ind=indf[ind]
           IF (count NE 0) THEN tmp[ind]=!VALUES.F_NAN
        ENDIF
      ENDIF
      data[*,line,*]=tmp
  ENDFOR
 
  ;We are now working with sos1 or sos2
  ;in the case of sos1 analyse two season
  ;Start the analysis for areas with 1 GS and those with 2 GS
  
  FOR ss=s, 2 DO BEGIN 
  ;we are working with the SOS number 's'
  ;and focusing on those pixels having 'ss' number of season per year 
  ;e.g. if SOS1, divide in ss=1 and 2, is SOS2 compute only
  ;ss=2 (it has to have a second season)
          
    ;select pixels having ss GS per year 
    ind=WHERE(ngspy_image EQ ss, count)
    IF (count EQ 0) THEN STOP
    ;buld a binary image (1 when the pixel has ss GS)
    ss_binary_image=ngspy_image*0
    ss_binary_image[ind]=1
    ;remove non ss data from data    
    ss_data=data*!VALUES.F_NAN
    FOR b=0, nb-1 DO BEGIN
      tmp=REFORM(data[*,*,b])
      tmp2=tmp*!VALUES.F_NAN
      tmp2[ind]=tmp[ind]
      ;ss_data is the working variable for pixels 
      ;having 's' ngspy and anylizing the season 'ss'
      ss_data[*,*,b]=tmp2
    ENDFOR
    PRINT, 'Analysing NGSPY = '+STRTRIM(ss,2)+' season = '+STRTRIM(s)
    failures = FLTARR(N_ELEMENTS(b_inds))
    FOR k = 0, N_ELEMENTS(b_inds)-1 DO BEGIN
      ind = WHERE(ss_data[*,*,b_inds[k]] EQ -999, count)
      PRINT, b_inds[k], count
      failures[k] = count
    ENDFOR 
    PRINT, 'mean failures =', MEAN(failures, /DOUBLE)
    PRINT, 'sd failures =', STDDEV(failures, /DOUBLE)
    ind = WHERE(ss_data[*,*,boi] EQ -999, count)
    PRINT, 'failures in boi = ', count
    ;create the table_fail of number of season failures 
    ;b0_n_DEP1 b0_n_DEP2 .. b0_n_DEPn
    ;b1_n_DEP1 b1_n_DEP2 .. b1_n_DEPn
    ;...
    table_fail=FLTARR(N_ELEMENTS(ids), nb)*!VALUES.F_NAN
    ;and the table with total number of pixels detected 
    ;as showing seasonality (total number of pixels expected to have a season )
    table_veg=FLTARR(N_ELEMENTS(ids))
        
    ;Compute the dep level number of failures
    FOR d=0, N_ELEMENTS(ids)-1 DO BEGIN
      indd=WHERE(dept EQ ids[d], countd)
      ;compute the number of pixel showing seasonality in that dep (based on ngspy)
      veg_dep=ss_binary_image[indd]
      table_veg[d]=TOTAL(veg_dep)  
      ;for every year compute the number of failure in the department
      FOR b=0, nb-1 DO BEGIN
        tmp=REFORM(ss_data[*,*,b])
        count999=0
        ;check if they are all NaN (department may be out of the mask used to compute pheno) 
        indNaN=WHERE(FINITE(tmp[indd]) NE 1, countNaN)
        IF (countNaN EQ countd) THEN BEGIN
          table_fail[d,b]=!VALUES.F_NAN
        ENDIF ELSE BEGIN
          ind999=WHERE(tmp[indd] EQ -999, count999)  
          table_fail[d,b]=count999
        ENDELSE
      ENDFOR  ;b
    ENDFOR  ;d
    
    ;var for storing the anomalies for boi exclunding bex
    dev_table=FLTARR(N_ELEMENTS(ids))
    ;var for storing the fraction of failures over total number of pix showing seasonality
    frct_table=FLTARR(N_ELEMENTS(ids))
    ;var for storing the anomaly in % failure
    var_table=FLTARR(N_ELEMENTS(ids))
    
    FOR d=0, N_ELEMENTS(ids)-1 DO BEGIN
 ;     if ids[d] eq 1208 then stop
      
      ;Deviation as 
      ;(nfailures_boi-avg_nfailures_exluding_bex)/avg_nfailures_exluding_bex
      avg=MEAN(table_fail[d,b_inds], /NaN, /DOUBLE)
      IF (avg EQ 0) THEN BEGIN
        IF (table_fail[d,boi] EQ 0) THEN dev_table[d]=0.0 ELSE dev_table[d]=!VALUES.F_INFINITY 
      ENDIF ELSE BEGIN
        dev_table[d]=(table_fail[d,boi]-avg)/avg*100.0
      ENDELSE
     
      
      ;Fraction of pixels for the boi as:
      ;nfailures_boi/npixels_potentially_vegetated
      IF (table_veg[d] EQ 0) THEN $
        frct_table[d]=!VALUES.F_NAN $ 
      ELSE $
        frct_table[d]=table_fail[d,boi]/DOUBLE(table_veg[d])*100.0
      
      ;Anomaly in fraction as
      ;(nfailures_boi-avg_nfailures_exluding_bex)/npixels_potentially_vegetated
      IF (table_veg[d] EQ 0) THEN $
        var_table[d]=!VALUES.F_NAN $
      ELSE $
        var_table[d]=(table_fail[d,boi]-MEAN(table_fail[d,b_inds], /NaN, /DOUBLE))$
                      /DOUBLE(table_veg[d])*100.0
      
 ;     a=0
    ENDFOR ;d
    
    ;output map
    dev_map=dept*!VALUES.F_NAN
    frct_map=dev_map
    var_map=dev_map
    FOR d=0, N_ELEMENTS(ids)-1 DO BEGIN
      indd=WHERE(dept EQ ids[d], countd)
      dev_map[indd]=dev_table[d]
      frct_map[indd]=frct_table[d]
      var_map[indd]=var_table[d]
    ENDFOR
    
    
    ;Relevant bands are written to files because AVG at depdt level works with files
    dev_map_full_path_name=path+'\'+strtrim(ss,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+'boi_'+'dev_failures_dep'
    OPENW, W1, dev_map_full_path_name , /GET_LUN
    WRITEU, W1, dev_map
    CLOSE, W1
    
    HEADER_OUT=dev_map_full_path_name+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = anomalies of failures for boi =' + STRTRIM(boi,2)
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(nl)
    printf,3,'bands   = 1
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 4'
    printf,3,'interleave = bil'
    printf,3,'byte order = 0'
    printf,3,'band names = {fail_dev boi '+STRTRIM(boi,2)+'}'
    CLOSE, 3  
    
    ;Relevant bands are written to files because AVG at depdt level works with files
    frct_map_full_path_name=path+'\'+strtrim(ss,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+'boi_'+'%_failures_dep'
    OPENW, W1, frct_map_full_path_name , /GET_LUN
    WRITEU, W1, frct_map
    CLOSE, W1
    
    HEADER_OUT=frct_map_full_path_name+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = % failures (n_fails/n_vegetated) for boi =' + STRTRIM(boi,2)
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(nl)
    printf,3,'bands   = 1
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 4'
    printf,3,'interleave = bil'
    printf,3,'byte order = 0'
    printf,3,'band names = {%fail boi '+STRTRIM(boi,2)+'}'
    close, 3
    
    var_map_full_path_name=path+'\'+strtrim(ss,2)+'gspy_'+STRTRIM(s,2)+'s_'+STRTRIM(boi+1,2)+'boi_'+'%fail_var_dep'
    OPENW, W1, var_map_full_path_name , /GET_LUN
    WRITEU, W1, var_map
    CLOSE, W1
    
    HEADER_OUT=var_map_full_path_name+'.hdr'
    OPENW, 3, HEADER_OUT
    printf,3,'ENVI'
    printf,3,'description = variation of % failures for boi =' + STRTRIM(boi,2)
    printf,3,'samples ='+STRCOMPRESS(ns)
    printf,3,'lines   ='+STRCOMPRESS(nl)
    printf,3,'bands   = 1
    printf,3,'header offset = 0'
    printf,3,'file type = ENVI Standard'
    printf,3,'data type = 4'
    printf,3,'interleave = bil'
    printf,3,'byte order = 0'
    printf,3,'band names = {delta %fail boi '+STRTRIM(boi,2)+'}'
    close, 3
    
    
    CLOSE, /ALL
    
  ENDFOR  ;ss
ENDFOR  ;s
;ENDFOR  ;i

                       
PRINT, 'Task failure_analysis completed'
RETURN, 0
END