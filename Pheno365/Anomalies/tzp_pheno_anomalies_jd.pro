Function TZP_pheno_anomalies_JD, operation, fname_in, fname_out, dtype, ns, nl, nb, rel_fname, rel_threshold, val999, save_abs_DOC
;Compute the anomalies/extreme events for a given file representing a pheno metric M that
;can be a PE (Pheno Event such as SOS, EOS) or an associate variable GSL (Growing Season Length), acc (ACCumulated fAPAR), etc.



;11 sep 2014
;optionally compute the max

;RESTRICTIONS
;- input file must be bil
;- for a correct interpretation, input must be realigned.
;- input must be TZP (True Zero Point) data referring to a given year. 
;SINCE THEY ARE TZP data the normal mean is used insteda of vector mean
;  So acc, len are ok while sos and eos must be DOC (relative Day Of Cycle) 

;INPUT
;operation:
;   'dlt': The delta from avg. For pixel located at i,j and time t, dM(i,j,t), with t=(1, .., T)=the number of 
;          bands of the file (that may contain NaN) is:
;          dM(i,j,t)=M(i,j,t) - Mean(M(i,j,*))
;          NOTE that 'dlt' also saves the average value as as standalone file '_avg'
;                    the Mean is used because it is easier to compute the mean of circular data
;                    rather than the median 
;   'dltY': As 'dlt' but the avg is computed from all years excluding the year under analyis
;   'min' ['max']:   The band of min [max] for pixel located at i,j with t=(1, .., T)=the number of 
;          bands of the file (that may contain NaN) is:
;          year(i,j)= where(M(i,j,t) eq min[max] of M(i,j,*))
;   'maxval': extract the maximum value
;   'dev': The deviation from avg. For pixel located at i,j and time t, dM(i,j,t), with t=(1, .., T)=the number of 
;          bands of the file (that may contain NaN) is:
;          dM(i,j,t)=[M(i,j,t) - Mean(M(i,j,*))]/Mean(M(i,j,*)) *100
;   'devY': As 'dev' but the avg is computed from all years excluding the year under analyis
;   'Zsc' or 'ZscY': anomalies in term of Zscores  
;     
;fname_in:    file name with full path
;dtype:       datatype (2 integer, 4 float)
;ns, nl, nb:  number of samples, lines, bands
;rel_fname:   if not null (rel_fname='') it is the reliability file, only obs with rel <1000
;             are considered (if rel > 1000 the algorithm was trying to extend the search period beyond the limits of fapar array)
;val999:      how to consider -999, can be 0 or !VALUES.FNAN
;             if is 0, it means that negative values are not valid, set them to NaN
;save_abs_DOC: set it 1 for PEs expressed in DOC (SOS, EOS etc..). In this case the average variable may <1 or > 365
;              so it saves, besides a TZPavg (which could be > 365 or less than 1) a ABSavg in which the value are 1-365 (so 366 becomes 1, and -1 becomes 365) 
;OUTPUT
;return code:
;0:     normal completion    
;10:    input file does not exists
;20:    invalid datatype
;30:    invalid operation

;EXAMPLE
;print, TZPpheno_anomalies('min','X:\HoA\Pheno_products\Somalia_v3\A19-1997_sos1',4,3586,3810,15,'') 
;print, TZPpheno_anomalies('min','K:\HoA\Pheno_products\A19-1997_sos1_dekoc', 4,3586,3810,15,'')

;HISTORY
;2 feb 2011 add Z scores
;23 August 2012, move everything from dekad to days


;Implemeted operations
ops=['min','max','dlt','dltY','dev','devY','Zsc','ZscY','maxval']

IF FILE_TEST(fname_in) EQ 0 THEN return, 10
OPENR, R1, fname_in, /GET_LUN
IF (rel_fname NE '') THEN BEGIN
  OPENR, R2, rel_fname, /GET_LUN
  asslinerel = ASSOC(R2, FLTARR(ns,nb))
ENDIF ELSE PRINT, 'Relalibility map not used!'

;FILE NAMING and work variables
;Check the operation requested is implemented
ind = WHERE(operation EQ ops, count)
IF (count EQ 0) THEN RETURN, 30
 
IF (operation EQ 'min') OR (operation EQ 'max') OR ((operation EQ 'maxval')) THEN BEGIN
  IF (operation EQ 'maxval') THEN fileout = fname_out+'_'+STRTRIM(operation,2)+'_of_all' ELSE $
      fileout = fname_out+'_b'+STRTRIM(operation,2) 
  out=FLTARR(ns)  ;float because I want to store NAN
  nbout=1
ENDIF ELSE BEGIN
   fileout = fname_out+'_'+STRTRIM(operation,2)
   fileout2 = fname_out+'_'+STRTRIM(operation,2)+'_TZPavg'
   IF (save_abs_DOC EQ 1) THEN fileout3 = fname_out+'_'+STRTRIM(operation,2)+'_ABSavg'
   
   fileout4 = fname_out+'_'+STRTRIM(operation,2)+'_TZPsd'
   IF (save_abs_DOC EQ 1) THEN fileout5 = fname_out+'_'+STRTRIM(operation,2)+'_ABSsd'
   ;check if band names are reported
   bnames=read_band_names(fname_in)
ENDELSE 

IF FILE_TEST(fileout) EQ 1 THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND


CASE dtype OF
  2: assline = ASSOC(R1, INTARR(ns,nb))
  4: assline = ASSOC(R1, FLTARR(ns,nb))
  ELSE: return, 20 
ENDCASE

data=FLTARR(ns, nb)

PRINT, 'Warning (tzp_pheno_anomalies_JD.pro): use only REALIGNED, True Zero Point (acc, len) or DOC Pheno Events (sos, eos) data as input!' 


IF (operation EQ 'dlt') OR (operation EQ 'dev') OR (operation EQ 'Zsc') THEN BEGIN 
  out = FLTARR(ns, nb)
  out2 = FLTARR(ns, 1)
  out4 = FLTARR(ns, 1)
  nbout=nb
  nbout2=1  ;only one grand avg
  IF FILE_TEST(fileout2) EQ 1 THEN FILE_DELETE, fileout2
  OPENW, W2, fileout2, /GET_LUN, /APPEND
  IF FILE_TEST(fileout4) EQ 1 THEN FILE_DELETE, fileout4
  OPENW, W4, fileout4, /GET_LUN, /APPEND
END
IF (operation EQ 'dltY') OR (operation EQ 'devY') OR (operation EQ 'ZscY') THEN BEGIN 
  out = FLTARR(ns, nb)
  out2 = FLTARR(ns, nb) ;one avg per band
  out4 = FLTARR(ns, nb) ;one avg per band
  nbout=nb
  nbout2=nb
  IF FILE_TEST(fileout2) EQ 1 THEN FILE_DELETE, fileout2
  OPENW, W2, fileout2, /GET_LUN, /APPEND
  IF FILE_TEST(fileout4) EQ 1 THEN FILE_DELETE, fileout4
  OPENW, W4, fileout4, /GET_LUN, /APPEND
END
IF (operation EQ 'maxval') THEN op='maxval' 
;consider min max together
IF (operation EQ 'min') OR (operation EQ 'max') THEN op='minmax' 
;consider dlt and dev and Zsc toghether
IF (operation EQ 'dlt') OR (operation EQ 'dev') OR (operation EQ 'Zsc') THEN op='dltdev' 
;consider dltY and devY and Zsc toghether
IF (operation EQ 'dltY') OR (operation EQ 'devY') OR  (operation EQ 'ZscY') THEN op='dltdevY' 


FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  data=float(assline[line])
  ;Exclude reliability over 1000 if required
  IF (rel_fname NE '') THEN BEGIN ;set to NaN those data having reliability greater than 1000
    rel=float(asslinerel[line])
    indf=WHERE(FINITE(rel), countf) ;check finitness to avoid NaN
    IF countf NE 0 THEN BEGIN 
       ind=WHERE(rel[indf] LE rel_threshold, count) ;find those to be rejected
       IF (count NE 0) THEN BEGIN 
        ind=indf[ind]
        data[ind]=!VALUES.F_NAN
       ENDIF
    ENDIF
  ENDIF
  
  FOR column=0, ns-1, 1L DO BEGIN      ; loop over all lines
    ;IF (line EQ 982)  AND (column EQ 3263) THEN STOP ;PRINT, column 
;    !EXCEPT = 2
;    JUNK = CHECK_MATH(/PRINT)
    ;IF (line eq 1617) and (column eq 1309) THEN stop
    indf=WHERE(FINITE(data[column,*]),countf)
    IF countf NE 0 THEN BEGIN ;if they are all NaN do  nothing..
      ;treat -999, Treat season failures (-999) excluding them
      z = data[column,*]     
      ind999 = WHERE(z EQ -999, count999)
      ;replace -999 with NaN so thay are not used in any computatio.
      IF (count999 NE 0) THEN BEGIN
        z[ind999]=val999
      ENDIF
      ;to be uncommented only to compute avg of acc withou considereing the zeros
      ;**************
;      indZeros = WHERE(z EQ 0.0, countZeros)
;      IF (countZeros GT 0) THEN z[indZeros]=!VALUES.F_NAN
      ;**************
      IF FINITE(val999) EQ 1 THEN BEGIN
        IF (val999 EQ 0) THEN BEGIN
         ;qui arrivano dei NaN
          indNeg = WHERE(z[indf] LT 0.0, countNeg)
          IF (countNeg NE 0) THEN BEGIN
            z[indf[indNeg]] = !VALUES.F_NAN
          END
        ENDIF
      ENDIF 
      CASE op OF
        'maxval': BEGIN
          val=MAX(z, ind, /NAN)
          IF (FINITE(val) EQ 1) THEN out[column]=val ELSE out[column]=!VALUES.F_NAN
         END
        'minmax': BEGIN
          IF (operation EQ 'min') THEN val=MIN(z, ind, /NAN) ELSE val=MAX(z, ind, /NAN)
          ;check there is at least one extreme
          IF (FINITE(val) EQ 1) THEN BEGIN
            ;check if there is only one min/max
            inds=WHERE(z EQ val, counts)
            ;check if there is more than one
            IF (counts GT 1) THEN BEGIN
              out[column]=inds[counts-1]+1  ;take the last (the most recent)
            ENDIF ELSE BEGIN
              out[column]=ind+1 ;only one, take it (it is expressed as band nummber (counting from 1)
            ENDELSE
          ENDIF ELSE BEGIN
            ;data are all NaN
             out[column]=!VALUES.F_NAN
          ENDELSE
        END 

        'dltdev': BEGIN   
            avgZ = MEAN(z, /DOUBLE, /NAN)
            ;there may be a single element 
            indZfin = WHERE(FINITE(z), countZfin)
            IF (countZfin GT 1) THEN sdZ = STDDEV(z, /DOUBLE, /NAN) ELSE sdZ = !VALUES.F_NAN
            ;For the anomaly other than dlt consider the case where the mean is 0 (dev is +inf)
            CASE operation OF
              'dlt': out[column,*] = z - avgZ
              'dev': $                 ;if the mean is 0, set the anomaly to NaN
                  IF (avgZ EQ 0.0) THEN $
                    out[column,*] = !VALUES.F_NAN $
                  ELSE $
                    out[column,*] = (z - avgZ) / avgZ * 100.0
              'Zsc': $                 ;if the sd is 0, set the anomaly to NaN
                  IF (sdZ EQ 0.0) THEN $
                    out[column,*] = !VALUES.F_NAN $
                  ELSE $
                    out[column,*] = (z - avgZ) / sdZ  
            ENDCASE
            
            ;Consider the case where the operation is performed on the circular variable expressed in DOC (which is not circular anymore, is tzp)
            IF (save_abs_DOC EQ 1) THEN BEGIN
              f = WHERE(FINITE(z) EQ 1)
              IF f[0] NE -1 THEN BEGIN    ;value can be between 1.0 (first day of cycle) and 365.999 (last day) 
                IF (avgZ GE 366.0) THEN avgZ = avgZ - 365.0  
                IF (avgZ LT 1.0)  THEN avgZ = avgZ + 365.0
                ;if they are still out of range we have a problem.. set it to NaN
                IF ((avgZ GE 366.0) OR (avgZ LT 1.0)) THEN avgZ = !VALUES.F_NAN
              ENDIF
            ENDIF
            ;store mean and SD
            out2[column,*] = avgZ                       ;the mean
            out4[column,*] = sdZ                        ;the SD
            ;report season failures (-999) if season failure are ignored
            IF (count999 NE 0) THEN out[column,ind999]=!VALUES.F_NAN
        END
        
        'dltdevY': BEGIN
            FOR b = 0, nb-1 DO BEGIN
              ;remove the year for which the anomaly is computed
              z1=REFORM(z)
              z1[b]=!VALUES.F_NAN
              avgZ1 = MEAN(z1, /DOUBLE, /NAN)
              ;there may be a single element 
              indZ1fin = WHERE(FINITE(z1), countZ1fin)
              IF (countZ1fin GT 1) THEN sdZ1 = STDDEV(z1, /DOUBLE, /NAN) ELSE sdZ1 = !VALUES.F_NAN
              ;For the anomaly other than dltY consider the case where the mean is 0 (dev is +inf)
              CASE operation OF
                'dltY': out[column,b] = z[b] - avgZ1
                'devY': $                 ;if the mean is 0, set the anomaly to NaN
                    IF (avgZ1 EQ 0.0) THEN $
                      out[column,b] = !VALUES.F_NAN $
                    ELSE $
                      out[column,b] = (z[b] - avgZ1) / avgZ1 * 100.0  
                'ZscY': $                 ;if the sd is 0, set the anomaly to NaN
                    IF (sdZ1 EQ 0.0) THEN $
                      out[column,b] = !VALUES.F_NAN $
                    ELSE $
                      out[column,b] = (z[b] - avgZ1) / sdZ1  
              ENDCASE
              ;Consider the case where the operation is performed on a DOC variable
              IF (save_abs_DOC EQ 1) THEN BEGIN
                f = WHERE(FINITE(z) EQ 1)
                IF f[0] NE -1 THEN BEGIN
                IF (avgZ GE 366.0) THEN avgZ = avgZ - 365.0  
                IF (avgZ LT 1.0)  THEN avgZ = avgZ + 365.0
                ;if they are still out of range we have a problem.. set it to NaN
                IF ((avgZ GE 366.0) OR (avgZ LT 1.0)) THEN avgZ = !VALUES.F_NAN
                ENDIF
              ENDIF
              
              ;store mean and SD
              out2[column,b] = avgZ1                       ;the mean
              out4[column,b] = sdZ1                        ;the SD
              
              ;report season failures (-999) if season failure are ignored
              IF (count999 NE 0) THEN out[column,ind999]=!VALUES.F_NAN                
            ENDFOR
        END
      ENDCASE
    ENDIF ELSE BEGIN  ;they are all NaN, output NaN
      CASE op OF
        'maxval': out[column]=!VALUES.F_NAN
        'minmax': out[column]=!VALUES.F_NAN                
        'dltdev': BEGIN
            out[column,*]=!VALUES.F_NAN
            out2[column,*]=!VALUES.F_NAN 
            out4[column,*]=!VALUES.F_NAN
        END
        'dltdevY': BEGIN
            out[column,*]=!VALUES.F_NAN
            out2[column,*]=!VALUES.F_NAN
            out4[column,*]=!VALUES.F_NAN
        END
      ENDCASE
    ENDELSE
;  IF CHECK_MATH() NE 0 THEN STOP
  ENDFOR
  WRITEU, W1, out
  IF (op EQ 'dltdev') OR (op EQ 'dltdevY') THEN BEGIN
    WRITEU, W2, out2
    WRITEU, W4, out4
  ENDIF
ENDFOR


; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = anomalies of ' + string(fileout)
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nbout)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 4'
printf,3,'interleave = bil'
printf,3,'byte order = 0'

IF (op EQ 'dltdev') OR (op EQ 'dltdevY')THEN BEGIN
  ;ADD band names info to HDR
  IF (bnames[0] NE '') THEN BEGIN
    FOR i = 0, N_ELEMENTS(bnames)-1 DO PRINTF, 3, bnames[i]
  ENDIF
  ; WRITE HEADER OF THE OUTPUT2
  HEADER_OUT=fileout2+'.hdr'
  OPENW, 4, HEADER_OUT
  printf,4,'ENVI'
  printf,4,'description = avg of ' + string(fileout)
  printf,4,'samples ='+STRCOMPRESS(ns)
  printf,4,'lines   ='+STRCOMPRESS(nl)
  printf,4,'bands   ='+STRCOMPRESS(nbout2)
  printf,4,'header offset = 0'
  printf,4,'file type = ENVI Standard'
  printf,4,'data type = 4'
  printf,4,'interleave = bil'
  printf,4,'byte order = 0'
   ;ADD band names info to HDR
  PRINTF, 4, 'relative AVG (may be GE 37.0)'
  
  ; WRITE HEADER OF THE OUTPUT4
  HEADER_OUT=fileout4+'.hdr'
  OPENW, 6, HEADER_OUT
  printf,6,'ENVI'
  printf,6,'description = SD of ' + string(fileout)
  printf,6,'samples ='+STRCOMPRESS(ns)
  printf,6,'lines   ='+STRCOMPRESS(nl)
  printf,6,'bands   ='+STRCOMPRESS(nbout2)
  printf,6,'header offset = 0'
  printf,6,'file type = ENVI Standard'
  printf,6,'data type = 4'
  printf,6,'interleave = bil'
  printf,6,'byte order = 0'
  
END

CLOSE, /ALL

RETURN, 0
End
