FUNCTION Sentinel_pheno_compare_S2andS1_GS12_matching_v2, dir
; for spring and summer crops I can only have one season retrieved 
; - > compare timings of THAT season as derived fron S2 and S1
; for winter crop I can have one of 2 season depending if 
;   - there is the winter dormancy
;   - AND it is detected (either by S1 and S2)
; So, a priori I do not know if a a double season is peresent and detected by which sensor. 
; 1. take al S2 bimodal and find in the correspondi S1 (mono or bi) the closest timings (can be one or two). Make: 
;     -scatter plot
;     -matrix
;           S2G1  S2G2
;     S1G1  100 0
;     S1G2  0 100
;     - number of S2 bimodal that could not be matched by S1 that was mono, mean time and SD
;     - number of S1 bimodal that could not be matched by S1 that was mono, mean time and SD
; 2. same for S2 monomodal


;dir =  'D:\RAPHAEL_pheno_test\Results_2019_6_17';'D:\RAPHAEL_pheno_test\Results_2019May29';'Z:\RAPHAEL_pheno_test\Results_2019May27'
dir_out = dir + '\Pheno_from_index_corr'
FILE_MKDIR, dir_out
csv_file = dir + '\pheno_results.csv'
res = READ_CSV(csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN )
x0 = rename_tags(res, TAG_NAMES(res), [hdr[0:-2],'Empty']) ;this final empty is because I have a comma at the end of each record
PRINT, hdr

;cropTypes = x.crop_type[UNIQ(x.crop_type,SORT(x.crop_type))]
;indices = x.index[UNIQ(x.index,SORT(x.index))]
h1_array = !NULL
cropTypes = raphael_crop_select(/SUMMER)

;NDVI index
S2index = 'NDVI'
;s1 indices
S1index = ['RVI_ad'];,'CR_ad'];,'CR_a','CR_d','RVI_a','RVI_d']
vars = ['SOS20','SOS50','EOS20','EOS50','LGS20','LGS50']
;selection based on quality of fitting
VarThresholds = [10000];, 0.05, 0.1, 0.2, 0.25]   ;leave only 10000 to avoid any selection 
thresholdVar = 'FractUnrel';possible values: 'P','r','unrel', 'FractUnrel'
;P and r refer to the DHT fit, P is a test proposed by Dominique. unrel and FractUnrel is an unreliability measure of the pheno event
;P and r were found to have little if not detrimental effect

;make the table for all crops
OPENW, lun, dir_out + '\Pheno_from_index_corr.csv', /GET_LUN
OPENW, lunRetr, dir_out + '\Pheno_retrieval_success_and_failure.csv', /GET_LUN
OPENW, lunPolyErrors, dir_out + '\Pheno_poly_erros_RVI.csv', /GET_LUN
OPENW, lunWinterCrops, dir_out + '\Pheno_winter_crop_matches.csv', /GET_LUN
dlmtr = ','
PRINTF, lun, STRJOIN(['Crop','Index','Sampling','Modality','n_poly', $
                      vars[0] + ['_n','_r','_P','_RMSD','_MAE','_ME'], $;'SOS20_r','SOS20_P','SOS20_RMSD','SOS20_MAE','SOS20_ME',$
                      vars[1] + ['_n','_r','_P','_RMSD','_MAE','_ME'], $;vars[0] + '_r','_P','_RMSD','_MAE','_ME', $;'EOS20_r','EOS20_P','EOS20_RMSD','EOS20_MAE','EOS20_ME',$
                      vars[2] + ['_n','_r','_P','_RMSD','_MAE','_ME'], $;'GSL20_r','GSL20_P','GSL20_RMSD','GSL20_MAE','GSL20_ME',$
                      vars[3] + ['_n','_r','_P','_RMSD','_MAE','_ME'], $;'SOS50_r','SOS50_P','SOS50_RMSD','SOS50_MAE','SOS50_ME',$
                      vars[4] + ['_n','_r','_P','_RMSD','_MAE','_ME'], $;'EOS50_r','EOS50_P','EOS50_RMSD','EOS50_MAE','EOS50_ME',$
                      vars[5] + ['_n','_r','_P','_RMSD','_MAE','_ME']], dlmtr);'GSL50_r','GSL50_P','GSL50_RMSD','GSL50_MAE','GSL50_ME'], dlmtr)
PRINTF, lunRetr, STRJOIN(['Crop','Sampling', 'nPolygons', 'nS2ok_S1ok', 'nS2failS1fail','nS2successS1fail', 'nS2failS1success'], dlmtr)            
PRINTF, lunPolyErrors, STRJOIN(['Poly_ID','Crop','Lat','Lon','MEAN_CV_VARRAT_INSEASON','prctHeterogObs_inSeason','Sampling', $
                                'G1dSOS20', 'G1dEOS20', 'G1mean_abs_dev','G2dSOS20', 'G2dEOS20', 'G2mean_abs_dev'], dlmtr)
PRINTF, lunWinterCrops, STRJOIN(['Crop','Index','Pheno_Var','Modality','S2G1mS1G1','S2G2mS1G2','S2G1mS1G2','S2G2mS1G1',$
                                 'n_S2notMatched','avg_S2notMatched','SD_S2notMatched','n_S1notMatched','avg_S1notMatched','SD_S1notMatched'], dlmtr)
          
FOR u = 0, 0 DO BEGIN ;all data, only selected based on gaps and perror
  FOR t = 0, N_ELEMENTS(VarThresholds)-1 DO BEGIN ;trhresold on pheno reliability
    x = x0  ;retake the orginal
    fitStatStr = ''
    IF (VarThresholds[t] NE 10000) THEN BEGIN ;for 10000 do nothing 
      PRINT, '*********************************************************************************'
      CASE thresholdVar OF
        'r': BEGIN
          PRINT, 'Retrieval with ' + VarThresholds[t] +' lt ' + STRTRIM(VarThreshold,2) + ' are removed'
          fitStatStr = 'rGT' + STRTRIM(VarThreshold,2) + '_'
          ;trick to quicly remove fittings with R2<0.5. Find them and set SOS and EOS to -9999
          indG1BadFit = WHERE((x.G1_R_FIT LT VarThreshold), countG1BadFit, /NULL)
          IF (countG1BadFit GT 0) THEN BEGIN
            FOR g = 0, vars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G1_'+vars[g]))[indG1BadFit] = -9999
          ENDIF
          indG2BadFit = WHERE((x.G2_R_FIT LT VarThreshold), countG2BadFit, /NULL)
          IF (countG2BadFit GT 0) THEN BEGIN
            FOR g = 0, vars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G2_'+vars[g]))[indG2BadFit] = -9999
          ENDIF
        END
        'P': BEGIN
          PRINT, 'Retrieval with ' + VarThresholds[t] +' gt ' + STRTRIM(VarThreshold,2) + ' are removed'
          fitStatStr = '_pLT' + STRTRIM(VarThreshold,2) + '_'
          ;trick to quicly remove fittings with R2<0.5. Find them and set SOS and EOS to -9999
          indG1BadFit = WHERE((x.G1_PVAL_FIT GT VarThreshold), countG1BadFit, /NULL)
          IF (countG1BadFit GT 0) THEN BEGIN
            FOR g = 0, vars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G1_'+vars[g]))[indG1BadFit] = -9999
          ENDIF
          indG2BadFit = WHERE((x.G2_PVAL_FIT GT VarThreshold), countG2BadFit, /NULL)
          IF (countG2BadFit GT 0) THEN BEGIN
            FOR g = 0, vars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G2_'+vars[g]))[indG2BadFit] = -9999
          ENDIF
        END
        'FractUnrel': BEGIN
          PRINT, 'Retrieval with ' + STRTRIM(VarThresholds[t],2) +' gt ' + STRTRIM(VarThresholds[t],2) + ' are removed'
          fitStatStr = '_FractUnrellLT' + STRTRIM(VarThresholds[t],2) + '_'
          ;Find them and set SOS and EOS to -9999
          ;first season
          indG1SOSFRACTUNREL = WHERE((x.G1_SOSFRACTUNREL GT VarThresholds[t]), countG1SOSFRACTUNREL, /NULL)
          IF (countG1SOSFRACTUNREL GT 0) THEN BEGIN
            ;vars to remove
            indVars = WHERE((vars.Contains('SOS') OR vars.Contains('LGS')) EQ 1)
            FOR g = 0, indVars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G1_'+vars[indVars[g]]))[indG1SOSFRACTUNREL] = -9999
          ENDIF
          indG1EOSFRACTUNREL = WHERE((x.G1_EOSFRACTUNREL GT VarThresholds[t]), countG1EOSFRACTUNREL, /NULL)
          IF (countG1EOSFRACTUNREL GT 0) THEN BEGIN
            ;vars to remove
            indVars = WHERE(vars.Contains('EOS') OR vars.Contains('LGS') EQ 1)
            FOR g = 0, indVars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G1_'+vars[indVars[g]]))[indG1EOSFRACTUNREL] = -9999
          ENDIF
          ;second season
          indG2SOSFRACTUNREL = WHERE((x.G2_SOSFRACTUNREL GT VarThresholds[t]), countG2SOSFRACTUNREL, /NULL)
          IF (countG2SOSFRACTUNREL GT 0) THEN BEGIN
            ;vars to remove
            indVars = WHERE(vars.Contains('SOS') OR vars.Contains('LGS') EQ 1)
            FOR g = 0, indVars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G2_'+vars[indVars[g]]))[indG2SOSFRACTUNREL] = -9999
          ENDIF
          indG2EOSFRACTUNREL = WHERE((x.G2_EOSFRACTUNREL GT VarThresholds[t]), countG2EOSFRACTUNREL, /NULL)
          IF (countG2EOSFRACTUNREL GT 0) THEN BEGIN
            ;vars to remove
            indVars = WHERE(vars.Contains('EOS') OR vars.Contains('LGS') EQ 1)
            FOR g = 0, indVars.LENGTH-1 DO x.(WHERE(TAG_NAMES(x) EQ 'G2_'+vars[indVars[g]]))[indG2EOSFRACTUNREL] = -9999
          ENDIF
        END
      ENDCASE
      PRINT, '*********************************************************************************'
    ENDIF ;VarThresholds[t] NE 10000
    ;PRINT, 'u ', u
    FOR m = 0, 1 DO BEGIN ;winter / non winter
      ;PRINT, 'u ', u, 'm ', m
      IF (m EQ 0) THEN cropTypes = raphael_crop_select(/WINTER) ELSE cropTypes = raphael_crop_select(/SUMMER)
      FOR i = 0, N_ELEMENTS(cropTypes)-1 DO BEGIN
        ;PRINT, 'u ', u, 'm ', m, 'i', i
        FOR k = 0, N_ELEMENTS(S1index)-1 DO BEGIN
          PRINT, 'u ', u.ToString(), ', m ', m.ToString(), ', i ', i.ToString()
          tit0 = STRSPLIT(cropTypes[i],'-',/EXTRACT)
          tit0 = STRMID(STRTRIM(tit0[0],2),0,18)
          tit = tit0 + ', Pheno NDVI vs. Pheno ' + S1index[k]
          PRINT, cropTypes[i], ' ', S1index[k]
          CASE u OF
            0: BEGIN
              indS2 = WHERE((x.index EQ S2index) AND (x.crop_type EQ cropTypes[i]), n_sample_S2, /NULL) 
              indS1 = WHERE((x.index EQ S1index[k]) AND (x.crop_type EQ cropTypes[i]), n_sample_S1, /NULL)
              IF ((n_sample_S2 NE n_sample_S1) AND (n_sample_S2 GT 0)) THEN BEGIN
                ;find out what happens
                 idS2 = x.ID[indS2]
                 idS1 = x.ID[indS1]
                 FOR m = 0, N_ELEMENTS(idS2) - 1 DO BEGIN
                  indTmp = WHERE(idS1 EQ idS2[m], countTmp) 
                  IF (countTmp EQ 0) THEN BEGIN
                    PRINT,  idS2[m]
                    STOP ;this should not happen (i save also fit failures)
                  ENDIF
                 ENDFOR
                STOP ;this should not happen (i save also fit failures)
              ENDIF
              samp = 'all_samples' + fitStatStr
            END
            1: BEGIN    
              ;remove thos ids where at least one of the two seasons is heterogenous
              indS2 = WHERE((x.index EQ S2index) AND (x.crop_type EQ cropTypes[i]) AND $
                            (x.G1_PRCTHETEROGOBS_INSEASON LE 20) AND (x.G2_PRCTHETEROGOBS_INSEASON LE 20), n_sample_S2, /NULL)
              ;now, for S1 keep only the ids selected above
              indS1 = WHERE((x.index EQ S1index[k]) AND (x.crop_type EQ cropTypes[i]), n_sample_S1)
              match, x.ID[indS2], x.ID[indS1], subS2, subS1
              indS1 = indS1[subS1]
              n_sample_S1 = indS1.LENGTH
              IF ((n_sample_S2 NE n_sample_S1) AND (n_sample_S2 GT 0)) THEN STOP
              samp =  'InSeasNdviHetLT20' + fitStatStr
            END
            2: BEGIN
              ;no more used, if necessary takes and adapt from Sentinel_pheno_compare_S2andS1.pro
            END
          ENDCASE
  
          
          ;Modif 2/7/2018, success if it has SO OR EOS
          ;Now get those retrievals that were successful (at least one full season present) and (the rest) failed. To be seuccessful they must have sos OR eos
          indS2s = indS2[WHERE((x.G1_SOS20[indS2] NE -9999 OR x.G1_EOS20[indS2] NE -9999) OR $
            (x.G2_SOS20[indS2] NE -9999 OR x.G2_EOS20[indS2] NE -9999), COMPLEMENT = indS2f, /NULL)]
          indS2f = indS2[indS2f]
          indS1s = indS1[WHERE((x.G1_SOS20[indS1] NE -9999 OR x.G1_EOS20[indS1] NE -9999) OR $
            (x.G2_SOS20[indS1] NE -9999 OR x.G2_EOS20[indS1] NE -9999), COMPLEMENT = indS1f, /NULL)]
          indS1f = indS1[indS1f]
          IF ((indS2s[0] EQ !NULL) OR (indS1s[0] EQ !NULL)) THEN STOP
          
          ;now I have to match them so that they point to the same samples
          idS2s = x.ID[indS2s]
          idS1s = x.ID[indS1s]
          MATCH, idS2s, idS1s, subS2s, subS1s, COUNT = count
          IF (count GT 0) THEN BEGIN ;it may happen that they are different items
            ;index of samples where both were successful
            indS2ss = indS2s[subS2s]
            indS1ss = indS1s[subS1s]
            IF (N_ELEMENTS(indS2ss) NE N_ELEMENTS(indS1ss)) THEN STOP
            n_ss = STRTRIM(N_ELEMENTS(indS2ss),2) ;both successful
          ENDIF ELSE BEGIN
            n_ss = 0
          ENDELSE
          
          ;here make a structure to store the difference in pheno timing for each polygon, ONLY FOR RVI_ad
          IF (S1index[k] EQ 'RVI_ad') THEN BEGIN
            poly_errors = CREATE_STRUCT('PolyID', LONARR(n_ss), 'CROP', STRARR(n_ss) + cropTypes[i], 'Lat', FLTARR(n_ss), 'Lon', FLTARR(n_ss), $
                                        'mean_cv',FLTARR(n_ss), 'prctHeterog', FLTARR(n_ss), 'Retrival_selection', STRARR(n_ss) + samp, $
                                        'dSOS20', FLTARR(n_ss,2)-9999, 'dEOS20', FLTARR(n_ss,2)-9999, 'mean_abs_dev', FLTARR(n_ss,2)-9999)
          ENDIF
          ;find those that both failed
          ;check first that some failed
          IF ((indS2f NE !NULL) AND (indS1f NE !NULL)) THEN BEGIN
            idS2f = x.ID[indS2f]
            idS1f = x.ID[indS1f]
            MATCH, idS2f, idS1f, subS2f, subS1f, COUNT = count
            IF (count GT 0) THEN BEGIN
              ;index of samples where both failed
              indS2ff = indS2f[subS2f]
              indS1ff = indS1f[subS1f]
              IF (N_ELEMENTS(indS2ff) NE N_ELEMENTS(indS1ff)) THEN STOP
              n_ff = STRTRIM(N_ELEMENTS(indS2ff),2) ;both failed
            ENDIF ELSE BEGIN
              n_ff = 0
            ENDELSE
          ENDIF ELSE BEGIN
            n_ff = 0
          ENDELSE
          
          ;find those where S2 success and S1 fail
          IF ((indS2s NE  !NULL) AND (indS1f NE  !NULL)) THEN BEGIN
            idS2s = x.ID[indS2s]
            idS1f = x.ID[indS1f]
            MATCH, idS2s, idS1f, subS2s, subS1f, COUNT = count
            IF (count GT 0) THEN BEGIN 
              ;index of samples where s2 success and s1 fail
              indS2sf = indS2s[subS2s]
              indS1sf = indS1f[subS1f]
              IF (N_ELEMENTS(indS2sf) NE N_ELEMENTS(indS1sf)) THEN STOP
              n_sf = STRTRIM(N_ELEMENTS(indS2sf),2) 
            ENDIF ELSE BEGIN
              n_sf = 0
            ENDELSE
          ENDIF ELSE BEGIN
            n_sf = 0
          ENDELSE
          
          ;find those where S2 fail and S1 success
          IF ((indS2f NE  !NULL) AND (indS1s NE  !NULL)) THEN BEGIN
            idS2f = x.ID[indS2f]
            idS1s = x.ID[indS1s]
            MATCH, idS2f, idS1s, subS2f, subS1s, COUNT = count
            IF (count GT 0) THEN BEGIN
              ;index of samples where s2 success and s1 fail
              indS2fs = indS2f[subS2f]
              indS1fs = indS1s[subS1s]
              IF (N_ELEMENTS(indS2fs) NE N_ELEMENTS(indS1fs)) THEN STOP
              n_fs = STRTRIM(N_ELEMENTS(indS2fs),2) ;both failed
            ENDIF ELSE BEGIN
              n_fs = 0
            ENDELSE
          ENDIF ELSE BEGIN
            n_fs = 0
          ENDELSE
          
          str = STRTRIM(n_sample_S2,2) + ' polygons, retr: ' +  STRTRIM(n_ss,2) + ' both, ' + $
                STRTRIM(n_ff,2) + ' none, ' + $
                STRTRIM(n_sf,2) + ' S2sS1f, ' + $
                STRTRIM(n_fs,2) + ' S2fS1s' 
          PRINTF, lunRetr, STRJOIN([cropTypes[i],samp,STRTRIM(n_sample_S2,2),STRTRIM(n_ss,2),STRTRIM(n_ff,2),STRTRIM(n_sf,2),STRTRIM(n_fs,2)], dlmtr)     
          tit2 = samp + ', ' + str
         
          ;now plot them
          IF (m EQ 0) THEN BEGIN
            ;prepare for analysing winter crops with different S2 modalities
            modalities = ['Monomodal','Bimodal','All-modalities']
          ENDIF ELSE BEGIN
            modalities = ['No-modality']
          ENDELSE       
          symb = '.'     
          FOR md = 0, N_ELEMENTS(modalities)-1 DO BEGIN
            IF (modalities[md] NE 'No-modality') THEN BEGIN
              CASE modalities[md] OF
                'Monomodal': BEGIN
                  ;find where S2 is mono and find the closest elements in S1
                  ;Note that indS2ss referes to seasons that have at least one partial season (sos OR eos) but may have two (in both S1 and S2)
                  ;here I want to find out those that have one seaon only (exclde those that have one (full or partial) and oanother one full or partial
                  tmp = WHERE((x.G1_SOS20[indS2ss] NE -9999 OR x.G1_EOS20[indS2ss] NE -9999) XOR $
                    (x.G2_SOS20[indS2ss] NE -9999 OR x.G2_EOS20[indS2ss] NE -9999), /NULL)
                  indS2Mod = indS2ss[tmp]
                  indS1Mod = indS1ss[tmp]
                END
                'Bimodal': BEGIN
                  ;find where S2 is bi and find the closest elements in S1
                  ;Note that indS2ss referes to seasons that have at least one partial season (sos OR eos) but may have two (in both S1 and S2)
                  tmp = WHERE((x.G1_SOS20[indS2ss] NE -9999 OR x.G1_EOS20[indS2ss] NE -9999) AND $
                    (x.G2_SOS20[indS2ss] NE -9999 OR x.G2_EOS20[indS2ss] NE -9999), /NULL)
                  indS2Mod = indS2ss[tmp]
                  indS1Mod = indS1ss[tmp]
                END
                'All-modalities': BEGIN
                  indS2Mod = indS2ss
                  indS1Mod = indS1ss
                END
              ENDCASE
              str_n_poly_by_mod = STRTRIM(N_ELEMENTS(indS2Mod),2)
              ;PRINT, modalities[md] + ' n = ' + str_n_poly_by_mod
            ENDIF
            strArrOut = [cropTypes[i],S1index[k],samp,modalities[md],n_ss]
            FOR g = 0, 5 DO BEGIN
              IF (modalities[md] EQ 'No-modality') THEN BEGIN
                indVar = WHERE(TAG_NAMES(x) EQ 'G1_'+vars[g])
                IF (indVar EQ -1) THEN STOP
                xx = x.(indVar)[indS2ss]
                yy = x.(indVar)[indS1ss]
                indBothValid = WHERE((xx NE -9999) AND (yy NE -9999), countBothValid)
                IF (countBothValid GT 0) THEN BEGIN
                  xx = xx[indBothValid]
                  yy = yy[indBothValid]
                  idsxx = x.ID[indS2ss[indBothValid]]
                ENDIF ELSE STOP  
                str_n_poly_by_mod = STRTRIM(N_ELEMENTS(xx),2)
                ;PRINT, modalities[md] + ' n = ' + str_n_poly_by_mod        
              ENDIF 
              IF (modalities[md] NE 'No-modality') THEN BEGIN 
                ;-matrix
                ;           S2G1  S2G1
                ;     S1G1  100 0
                ;     S1G2  0 100
                mat = LONARR(2,2)
                ; - number of S2 bimodal that could not be matched by S1 that was mono, mean time and SD
                s2notMatched = [!NULL]
                ;- number of S1 bimodal that could not be matched by S1 that was mono, mean time and SD
                s1notMatched = [!NULL]
                ;for debug
                idMatchHit = !NULL ;reoport the ID whenever a match is found
                ;now I have to associate to each of this S2 timings the closesst S1 timing
                indVar1 = WHERE(TAG_NAMES(x) EQ 'G1_'+vars[g])
                indVar2 = WHERE(TAG_NAMES(x) EQ 'G2_'+vars[g])
                xx = [!NULL] & yy = [!NULL] & idsxx = [!NULL]
                FOR z = 0, N_ELEMENTS(indS2Mod) -1 DO BEGIN
                  ;be care, not all timing are now finite (because a season may have sos but not eos and vice versa)
                  s2timings = [x.(indVar1)[indS2Mod[z]], x.(indVar2)[indS2Mod[z]]]
                  indFail = WHERE(s2timings EQ -9999, countFail)
                  IF (countFail GT 0) THEN BEGIN
                    s2timings[indFail] = !VALUES.F_NAN
                    ;it may happen that the absent one is the G1 (because located but wth small bump), put the aviable as the first
                    IF (countFail EQ 1) THEN IF (indFail EQ 0) THEN s2timings = REVERSE(s2timings)
                  ENDIF
                  s1timings = [x.(indVar1)[indS1Mod[z]], x.(indVar2)[indS1Mod[z]]]
                  indFail = WHERE(s1timings EQ -9999, countFail)
                  IF (countFail GT 0) THEN BEGIN
                    s1timings[indFail] = !VALUES.F_NAN
                    ;it may happen that the absent one is the G1 (because located bu wth small bump), put the aviable as the first
                    IF (countFail EQ 1) THEN IF (indFail EQ 0) THEN s1timings = REVERSE(s1timings)
                  ENDIF
                  ;again be care NAN values may be in both s1 and s2 
                  ;find best match. sXtimings are filled from the left
                  ;debug
                  s2timings0 = s2timings
                  s1timings0 = s1timings
                  ;end debug
                  s2Deltas = [ABS(s2timings[0]-s1timings), ABS(s2timings[1]-s1timings)]
                  res = MIN(s2Deltas, subBestMatch, /NAN)
                  WHILE (FINITE(res) EQ 1) DO BEGIN
                    CASE subBestMatch OF
                      0: BEGIN;S2First with S1First
                        xx = [xx, s2timings[0]]
                        idsxx = [idsxx, x.ID[indS2Mod[z]]]
                        yy = [yy, s1timings[0]]
                        mat[0,0] = mat[0,0]+1  
                        s2timings[0] = !VALUES.F_NAN & s1timings[0] = !VALUES.F_NAN
                        idMatchHit = [idMatchHit, x.id[indS2Mod[z]]]
                      END 
                      1: BEGIN;S2First with S1Second
                        xx = [xx, s2timings[0]]
                        idsxx = [idsxx, x.ID[indS2Mod[z]]]
                        yy = [yy, s1timings[1]]
                        mat[0,1] = mat[0,1]+1
                        s2timings[0] = !VALUES.F_NAN & s1timings[1] = !VALUES.F_NAN
                        idMatchHit = [idMatchHit, x.id[indS2Mod[z]]]
                      END
                      2: BEGIN ;S2Second with S1First
                        xx = [xx, s2timings[1]]
                        idsxx = [idsxx, x.ID[indS2Mod[z]]]
                        yy = [yy, s1timings[0]]
                        mat[1,0] = mat[1,0]+1
                        s2timings[1] = !VALUES.F_NAN & s1timings[0] = !VALUES.F_NAN
                        idMatchHit = [idMatchHit, x.id[indS2Mod[z]]]
                      END
                      3: BEGIN  ;S2Second with S1Second
                        xx = [xx, s2timings[1]]
                        idsxx = [idsxx, x.ID[indS2Mod[z]]]
                        yy = [yy, s1timings[1]]
                        mat[1,1] = mat[1,1]+1
                        s2timings[1] = !VALUES.F_NAN & s1timings[1] = !VALUES.F_NAN
                        idMatchHit = [idMatchHit, x.id[indS2Mod[z]]]
                      END
                    ENDCASE
                    ; see what is left 
                    s2Deltas = [ABS(s2timings[0]-s1timings), ABS(s2timings[1]-s1timings)]
                    res = MIN(s2Deltas, subBestMatch, /NAN)
                    ;for debug
                    ;IF (FINITE(res) EQ 1) THEN STOP
                  ENDWHILE
                  ;everything that could be matched was matched, see if there something that could not find a match
                  indNoMatch = WHERE(FINITE(s2timings) EQ 1, countNoMatch)
                  IF (countNoMatch GT 0) THEN  s2notMatched = [s2notMatched, s2timings[indNoMatch]] 
                  indNoMatch = WHERE(FINITE(s1timings) EQ 1, countNoMatch)
                  IF (countNoMatch GT 0) THEN  s1notMatched = [s1notMatched, s1timings[indNoMatch]]
                ENDFOR
                ;now compute the number, the mean, the sd of those that could not be matched, save it, and save the matrix
                IF N_ELEMENTS(s2notMatched) GT 0 THEN $
                  s2notMatchedStats = [N_ELEMENTS(s2notMatched), MEAN(s2notMatched), STDDEV(s2notMatched)] $
                ELSE  s2notMatchedStats = [0, -9999, -9999]
                IF N_ELEMENTS(s1notMatched) GT 0 THEN $
                  s1notMatchedStats = [N_ELEMENTS(s1notMatched), MEAN(s1notMatched), STDDEV(s1notMatched)] $
                ELSE s1notMatchedStats = [0, -9999, -9999]
                IF N_ELEMENTS(s2notMatched) GT 0 THEN BEGIN
                  s2Mean = MEAN(S2notMatched) & s2SD = STDDEV(S2notMatched)
                ENDIF ELSE BEGIN
                  s2Mean = '' & s2SD = '' 
                ENDELSE
                IF N_ELEMENTS(s1notMatched) GT 0 THEN BEGIN
                  s1Mean = MEAN(S1notMatched) & s1SD = STDDEV(S1notMatched)
                ENDIF ELSE BEGIN
                  s1Mean = '' & s1SD = ''
                ENDELSE
                PRINTF, lunWinterCrops, STRJOIN([cropTypes[i],S1index[k],vars[g],modalities[md],STRTRIM([mat[0,0],mat[1,1],mat[0,1],mat[1,0],$
                  N_ELEMENTS(S2notMatched)],2),STRTRIM(s2Mean,2),STRTRIM(s2SD,2), $
                  STRTRIM(N_ELEMENTS(S1notMatched),2), STRTRIM(s1Mean,2),STRTRIM(s1SD,2)], dlmtr)
              ENDIF ;end of winter crop
              
              res = linregstat(xx, yy)
              r = 'n.a' & p = 'n.a'
              IF (FINITE(res[3]) EQ 1) THEN BEGIN
                r = STRTRIM(res[2],2)
                p = STRTRIM(res[3],2)
              ENDIF
              es = error_stats(xx, yy)
              minMax = [MIN([xx,yy]), MAX([xx,yy])]
              IF (g EQ 0 ) THEN BEGIN
                graphSpace = [0,0,1,0.95] ;[X1, Y1, X2, Y2] leave some space up for the text
                nrows = 3.0
                ncolumns = 2.0
                xmrg = 0.1
                ymrg = 0.05
                pos = [xmrg+0,ymrg+graphSpace[3]-graphSpace[3]/nrows, -xmrg+graphSpace[2]-graphSpace[2]/ncolumns, -ymrg+graphSpace[3]];[X1, Y1, X2, Y2]  
                h1 = PLOT(xx, yy, XTITLE='ND ' + vars[g], YTITLE=S1index[k] + ' ' + vars[g], LINESTYLE='', SYMBOL=symb, POSITION=pos, DIMENSIONS=[800,800], XRANGE=minMax, YRANGE=minmax)
              ENDIF ELSE BEGIN
                IF ((g MOD 2) EQ 0) THEN $ ;new line
                  pos = pos + [-graphSpace[2]/ncolumns,-graphSpace[3]/nrows,-graphSpace[2]/ncolumns,-graphSpace[3]/nrows] $ ;go left and move down
                ELSE $
                  pos = pos + [graphSpace[2]/ncolumns,0,graphSpace[2]/ncolumns,0]  ;go right
                h1 = PLOT(xx, yy, XTITLE='ND ' + vars[g], YTITLE=S1index[k] + ' ' + vars[g], LINESTYLE='', SYMBOL=symb, POSITION=pos, /CURRENT, XRANGE=minMax, YRANGE=minmax)
              ENDELSE
              h1b = PLOT(h1.XRANGE, res[0]+res[1]*h1.XRANGE, COLOR='b', /OVERPLOT)
              hll = PLOT(minMax,minMax, TITLE ='r='+STRING(res[2],FORMAT='(F6.3)')+', P='+STRING(res[3],FORMAT='(F6.3)')+ $
                    ', npoly='+STRTRIM(str_n_poly_by_mod,2)+', nsamp='+STRTRIM(N_ELEMENTS(xx),2), COLOR='black', LINESTYLE='--',/OVERPLOT)
              hll.TITLE.FONT_COLOR='b' & hll.TITLE.FONT_SIZE=10
              ;compute stats
              strArrOut = [strArrOut, STRTRIM(xx.LENGTH,2), r, p, STRTRIM(es.rmsd,2), STRTRIM(es.mae,2), STRTRIM(es.me,2)]
              IF ((modalities[md] EQ 'No-modality') OR (modalities[md] EQ 'All-modalities')) THEN BEGIN 
                ;compute poly errors
                IF (S1index[k] EQ 'RVI_ad') AND (vars[g] EQ 'SOS20') THEN BEGIN
                  poly_errors.PolyID = x.ID[indS2ss]
                  tmp = [[x.g1_MEAN_CV_VARRAT_INSEASON[indS2ss]],[x.g2_MEAN_CV_VARRAT_INSEASON[indS2ss]]] ;(n,2), may contain -9999
                  indNaN = WHERE(tmp EQ -9999, countNan)
                  IF (countNan GT 0) THEN tmp[indNaN] = !VALUES.F_NAN
                  poly_errors.mean_cv = MEAN(tmp, DIMENSION = 2, /NAN)
                  tmp = [[x.g1_prctHeterogObs_inSeason[indS2ss]],[x.g2_prctHeterogObs_inSeason[indS2ss]]] ;(n,2), may contain -9999
                  indNaN = WHERE(tmp EQ -9999, countNan)
                  IF (countNan GT 0) THEN tmp[indNaN] = !VALUES.F_NAN
                  poly_errors.prctHeterog = MEAN(tmp, DIMENSION = 2, /NAN)
                  ;and id mya have multiple seasons, for each ID compute one or two errors 
                  FOR p = 0, N_ELEMENTS(poly_errors.PolyID)-1 DO BEGIN
                    ;see how many sample I have in xx and yy
                    indp = WHERE(idsxx  EQ poly_errors.PolyID[p], countp)
                    poly_errors.dSOS20[p,0] = xx[indp[0]]-yy[indp[0]]
                    IF countp EQ 2 THEN poly_errors.dSOS20[p,1] = xx[indp[1]]-yy[indp[1]]                 
                  ENDFOR
  ;                poly_errors.dSOS20 = xx-yy
                  poly_errors.Lat = x.lat[indS2ss]
                  poly_errors.Lon = x.lon[indS2ss]
                ENDIF
                IF (S1index[k] EQ 'RVI_ad') AND (vars[g] EQ 'EOS20') THEN BEGIN
                  FOR p = 0, N_ELEMENTS(poly_errors.PolyID)-1 DO BEGIN
                    ;see how many sample I have in xx and yy
                    indp = WHERE(idsxx  EQ poly_errors.PolyID[p], countp)
                    poly_errors.dEOS20[p,0] = xx[indp[0]]-yy[indp[0]]
                    IF countp EQ 2 THEN poly_errors.dEOS20[p,1] = xx[indp[1]]-yy[indp[1]]
                  ENDFOR
                  ;poly_errors.dEOS20 = xx-yy
                  FOR p = 0, N_ELEMENTS(poly_errors.PolyID) - 1 DO BEGIN
                    poly_errors.mean_abs_dev[p,0] = MEAN([ABS(poly_errors.dSOS20[p,0]),ABS(poly_errors.dEOS20[p,0])])
                    strTmp = STRJOIN([STRING(poly_errors.PolyID[p]),poly_errors.CROP[p], STRING(poly_errors.Lat[p]),STRING(poly_errors.Lon[p]), $
                      STRING(poly_errors.mean_cv[p]),STRING(poly_errors.prctHeterog[p]),poly_errors.Retrival_selection[p], $
                      STRING( poly_errors.dSOS20[p,0]), STRING(poly_errors.dEOS20[p,0]), STRING(poly_errors.mean_abs_dev[p,0]), $
                      STRING( poly_errors.dSOS20[p,1]), STRING(poly_errors.dEOS20[p,1]), STRING(poly_errors.mean_abs_dev[p,1])], dlmtr)
                    strTmp = strTmp.Replace('-9999.00','')
                    PRINTF, lunPolyErrors, strTmp
                  ENDFOR
                ENDIF
              ENDIF            
            ENDFOR
            
         
                  
            PRINTF, lun, STRJOIN(strArrOut, dlmtr)
            ht = TEXT(0.5,0.975, tit + ', ' + modalities[md], ALIGNMENT=0.5,FONT_SIZE= 16)
            ht = TEXT(0.5,0.95, tit2, ALIGNMENT=0.5,FONT_SIZE= 12)
      
            h1.save, dir_out + '\' + tit0 + '_ND_'+S1index[k]+'_pheno_corr_' + samp + '_' + modalities[md] +'.png'
            h1.close
          ENDFOR  ;md
        ENDFOR ; k
      ENDFOR  ;i
    ENDFOR  ;m
  ENDFOR  ;t
ENDFOR  ;u
FREE_LUN, lun
FREE_LUN, lunRetr
FREE_LUN, lunPolyErrors
FREE_LUN, lunWinterCrops  
RETURN, 0 
END