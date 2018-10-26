PRO christelle_NIGER
;only for pixels with 1 GS
;save files with 0/1 (1 for start and afterwards)
;from dekad 10 (first of april) to dek 22 (2nd of August)
path='K:\MODIS_Niger\Pheno_ProductsXOFFconstant'
sos_fname='sos'
globstat_fname='globstat'
ns=7730
nl=3122
nb=2

ds = 10 ;dekad out start
de = 24 ;dekad out end

;OPEN SOS FILES FOR INPUT
;-SOS1
fname=path+'\'+sos_fname+'1'
IF FILE_TEST(fname) EQ 0 THEN STOP
OPENR, RSOS1, fname, /GET_LUN
assoSOS1 = ASSOC(RSOS1, FLTARR(ns,nb))
  
;globstat
fname=path+'\'+globstat_fname
IF FILE_TEST(fname) EQ 0 THEN STOP
OPENR, RGlobStat, fname, /GET_LUN
assoGlobStat = ASSOC(RGlobStat, INTARR(ns,2))

;build the matrixis
sosmat=FLTARR(ns,nl,nb)
ngspymat=FLTARR(ns,nl)

;read the matrixes
FOR line=0, nl-1, 1L DO BEGIN
 globstat=assoGlobStat[line]
 ngspymat[*, line]=REFORM(globstat[*,1])
 sosmat[*, line,*]=assoSOS1[line]
ENDFOR
CLOSE, /ALL

;make sos (staring from 0) starting from 1
sosmat=sosmat+1
;identify where there are 1 gspy
;ind1gs=WHERE(ngspymat EQ 1, count1gs)
mat=BYTARR(ns,nl)
;dekads to be saved 10-22 +36, 10:22 +36
FOR y=0,1 DO BEGIN
  FOR d=ds,de DO BEGIN
    mat[*,*]=0
    ;open file
    fname=path+'\'+'mm'+STRTRIM(y+2008,2)+'_last_start_cumul_dek'+STRTRIM(d-ds+1,2)
    OPENW, lun, fname, /GET_LUN
    FOR i=0, ns-1 DO BEGIN
      FOR j=0, nl-1 DO BEGIN
        ;IF (i EQ 6537) AND (j EQ 2056) THEN STOP
        IF (ngspymat[i, j] EQ 1) THEN BEGIN ;work only ig ngspy eq 1 
          soss=REFORM(sosmat[i, j,*])
          sossc=INTARR(2)+1000
          
          FOR u=0,1 DO BEGIN
            IF (FINITE(soss[u]) EQ 1) THEN BEGIN
              IF soss[u] GT 36 THEN sossc[1]=soss[u]
              IF (soss[u] LE 36) AND (soss[u] GT 0) THEN sossc[0]=soss[u]
            ENDIF 
          ENDFOR
          current_pdek=d+(y)*36
          IF current_pdek LT sossc[y] THEN mat[i,j]=0 ELSE mat[i,j]=1          
        ENDIF 
      ENDFOR
    ENDFOR
    WRITEU, lun, mat    
    FREE_LUN, lun
    ;write hdr
    fname=fname+'.hdr'
    OPENW, lun, fname, /GET_LUN
    PRINTF,lun,'ENVI'
    PRINTF,lun,'description = {boolean SOS files}'
    PRINTF,lun,'samples ='+STRCOMPRESS(ns)
    PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
    PRINTF,lun,'bands   = 1'
    PRINTF,lun,'header offset = 0'
    PRINTF,lun,'file type = ENVI Standard'
    PRINTF,lun,'data type = 1'
    PRINTF,lun,'interleave = bsq'
    PRINTF,lun,'byte order = 0'
    FREE_LUN, lun
  ENDFOR
ENDFOR

END