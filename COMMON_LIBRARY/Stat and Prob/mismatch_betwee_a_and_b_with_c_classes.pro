FUNCTION Mismatch_betwee_A_and_B_with_C_classes, A, B, C
;compute the mismatch between two images of classes
;A: image
;B: image
;C: array of possible classes,[0,1,2,3,4,5,6]; C must be 7 elemnts, order and 0,1,2,3,etc  (whatever start, order and alwys Natural numbers WHITOUT GAPS) and must be odd (symmetric aroud a central value representing NORMAL conditions)

;Example
;A = FLTARR(7,7)
;for i = 0, 6 do a[i,*]=i
;B = A
;B[0:6,0] = !VALUES.F_NAN
;B[0,*] = 6
;Mismatch_betwee_A_and_B_with_C_classes(A, B, [0,1,2,3,4,5,6])


IF (N_ELEMENTS(B) NE N_ELEMENTS(A)) THEN STOP
IF (TOTAL(INDGEN(N_ELEMENTS(C))+C[0]-C) NE 0) THEN STOP ; C is not natural withouth gaps
IF ((N_ELEMENTS(C) MOD 2) NE 1) THEN STOP
IF (N_ELEMENTS(C) NE 7) THEN STOP
cm = FLTARR(N_ELEMENTS(C)+1, N_ELEMENTS(C)+1) ;on columns I have the ref dataset (A) and on rows the other (B), all classes plus unclassified,
                                              ;to which I assing the class "label" C[-1]+1
CC = [C, C[-1]+1]                                              
AA = A
BB = B
indNaN = WHERE(~FINITE(AA), countNaN)
IF (countNaN GT 0) THEN AA[indNaN] = C[-1]+1
indNaN = WHERE(~FINITE(BB), countNaN)
IF (countNaN GT 0) THEN BB[indNaN] = C[-1]+1
                                            
FOR k = 0, N_ELEMENTS(CC)-1 DO BEGIN
  ;located indexes of category k on reference
  indCatI = WHERE(AA EQ CC[k], countCatI)
  ;now see how were these AA pixels are classified in B
  ;compute the confusion matrix cm 
  IF (countCatI GT 0) THEN cm[k,*] = HISTOGRAM(REFORM(BB[indCatI]), MIN=CC[0], MAX=CC[-1], BINSIZE = 1, LOCATIONS = loc) ELSE  cm[k,*] = 0
  ;PRINT, HISTOGRAM(REFORM(BB[indCatI]), MIN=CC[0], MAX=CC[-1], BINSIZE = 1, LOCATIONS = loc)
  ;PRINT, ''
ENDFOR

;res = confusion_mat_stats(cm, variable, variable + '_' + NL_names[iNRT,jLTA], anom_cat_str, anom_cat_num_str, dir_results + dirsep + variable + '_' + NL_names[iNRT,jLTA] + '_veg_active_confusion_matrix_stat.csv')

;Treat class C[-1]+1 separately
;total numer of pixels
n_pix = TOTAL(cm)
IF (n_pix NE N_ELEMENTS(A)) THEN STOP
;Total NaN number that is in B but not in A, last row without last element
percent_B_NaN_not_in_A = TOTAL(cm[0 : N_ELEMENTS(CC)-2, N_ELEMENTS(CC)-1])/FLOAT(n_pix)*100
;Total NaN number that is in A but not in B, last column without last element
percent_A_NaN_not_in_B = TOTAL(cm[N_ELEMENTS(CC)-1, 0 : N_ELEMENTS(CC)-2])/FLOAT(n_pix)*100
;Total numebr that is NaN in both, right bottom corner elemnt
percent_A_and_B_NaN = cm[N_ELEMENTS(CC)-1, N_ELEMENTS(CC)-1]/FLOAT(n_pix)*100
PRINT, '############# Info from Mismatch_betwee_A_and_B_with_C_classes.pro' 
PRINT, 'percent_B_NaN_not_in_A', 'percent_A_NaN_not_in_B', 'percent_A_and_B_NaN'
PRINT, percent_B_NaN_not_in_A, percent_A_NaN_not_in_B, percent_A_and_B_NaN

;Now forget about NaN and look at other classes
cm = cm[0:N_ELEMENTS(CC)-2, 0:N_ELEMENTS(CC)-2]
sz = SIZE(cm)
;here make agreemnt considering that classes are symmetric around the central one
;['Ext. bad','Very bad','Mod. bad','Normal','Mod. good','Very good','Ext. good']
PRINT, 'OVERALL STATISTICS'
n = FLOAT(TOTAL(cm))
oa = TRACE(cm)/n*100.0
PRINT, 'Overall accuracy = ' + STRING(oa)
PRINT, 'What was the accuracy of B in predicting the correct category of A relative to that of random chance?'
rowtot = FLOAT(TOTAL(cm, 1))
coltot = FLOAT(TOTAL(cm, 2))
num = TRACE(cm)/n - (1.0/n^2) * TOTAL(rowtot*coltot)
den = 1.0 -  (1.0/n^2) * TOTAL(rowtot*coltot)
hss = num/den
PRINT, 'Heidke skill score (-1:1; 0 no skill; 1 perfect) = ' + STRING(hss) + '  (accuracy of B in predicting the correct A category relative to that of random chance?)'
PRINT, '% of:'
PRINT, 'Agreement = ' + STRTRIM(oa,2)
subOfNorm = MEDIAN(INDGEN(sz[1])) ;subscript of the contingency table row (and column) for the normal conditions
RCnormSum = TOTAL(cm[subOfNorm,*]) + TOTAL(cm[*, subOfNorm]) - cm[subOfNorm,subOfNorm]
upRightBloc = TOTAL(cm[subOfNorm+1:-1,0:subOfNorm-1])
lowLeftBlock = TOTAL(cm[0:subOfNorm-1,subOfNorm+1:-1])
;Agreement
rank_a = oa
;Minor mismatch
rank_mm = (n - TRACE(cm) - RCnormSum + cm[subOfNorm,subOfNorm] -upRightBloc - lowLeftBlock)/FLOAT(n)*100
PRINT, 'Minor mismatch = ' + STRTRIM(rank_mm,2)
;Mismatch
rank_m = TOTAL([cm[subOfNorm-1, subOfNorm], cm[subOfNorm, subOfNorm-1], cm[subOfNorm+1, subOfNorm], cm[subOfNorm, subOfNorm+1]]/FLOAT(n)*100)
PRINT, 'Mismatch = ' + STRTRIM(rank_m,2)
;Serious Mismatch
rank_sm = TOTAL([REFORM(cm[subOfNorm-3:subOfNorm-2, subOfNorm]), REFORM(cm[subOfNorm, subOfNorm-3:subOfNorm-2]), REFORM(cm[subOfNorm+2:subOfNorm+3, subOfNorm]), REFORM(cm[subOfNorm, subOfNorm+2:subOfNorm+3])])/FLOAT(n)*100
PRINT, 'Serious mismatch = ' + STRTRIM(rank_sm,2)
;Unacceptable Mismatch
rank_um = (upRightBloc+lowLeftBlock)/FLOAT(n)*100
PRINT, 'Unacceptable mismatch =' + STRTRIM(rank_um,2)
PRINT, '############# END OF Info from Mismatch_betwee_A_and_B_with_C_classes.pro'
statsOut = CREATE_STRUCT('oa',oa,'hss',hss,'rank_a',rank_a,'rank_mm',rank_mm,'rank_m',rank_m,'rank_sm',rank_sm,'rank_um',rank_um)
RETURN, statsOut
END 