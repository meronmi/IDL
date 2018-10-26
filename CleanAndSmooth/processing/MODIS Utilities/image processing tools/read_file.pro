PRO read_file, fn, mat_Long, mat_Short, years_mat
;utility to read csv file in the specific format

;open and read gimms
OPENR, R1, fn, /GET_LUN
tmp = ''
READF, R1, tmp ;read the hdr line
tmp = '' & READF, R1, tmp
tmp = STRSPLIT(tmp, ',', /EXTRACT) & tmp = tmp[2 : N_ELEMENTS(tmp)-1]
year = FIX(STRMID(tmp,0,4))
season = STRMID(tmp,4,1)
indL = WHERE(season EQ 'L')
indS = WHERE(season EQ 'S')
yearL = year[indL]
yearS = year[indS]
 
WHILE (NOT(EOF(R1))) DO BEGIN
 tmp = '' & READF, R1, tmp
 tmp = STRSPLIT(tmp, ',', /EXTRACT)
 IF (FIX(tmp[0]) GE 1) THEN BEGIN  ;this is because with michele's file there is division 0 as well
   adminId = FIX(tmp[0]) 
   tmp = tmp[2 : N_ELEMENTS(tmp)-1]
   ind_mat = WHERE(years_mat EQ yearL[0])
   mat_Long[ind_mat:ind_mat+N_ELEMENTS(yearL)-1, adminId-1] = tmp[indL]
   ind_mat = WHERE(years_mat EQ yearS[0])
   mat_Short[ind_mat:ind_mat+N_ELEMENTS(yearS)-1, adminId-1] = tmp[indS]
 ENDIF
ENDWHILE
 
FREE_LUN, R1
END