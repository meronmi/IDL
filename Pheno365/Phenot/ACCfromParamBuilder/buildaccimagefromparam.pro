PRO buildAccImageFromParam
;this function read the DHTM parameter from files and compute the desired acc

;User settings
threshSOStype = '%'
threshSOS = 20 
threshEOStype = 'DOY' 
threshEOS = 283

pheno_path = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos'
base_par_fn = ['p0','p1','p2','p3','p4','p5','p6','t0','t1']
base_par_fn = 'A1sos-1997_'+base_par_fn
gs=1
base_par_fn = base_par_fn + STRTRIM(gs,2)

ns = 7841                      ; number of samples
nl = 1458                      ; number of lines
nb = 16                        ; number of years  from 1997-1998
frstyear = 1997

fname_acc = pheno_path + '\' + 'A1sos-1997_' + 'RecomputedAcc' + STRTRIM(gs,2) + 'EOS' + STRTRIM(threshEOS,2)
fname_accb = pheno_path + '\' + 'A1sos-1997_' + 'RecomputedAccb' + STRTRIM(gs,2) + 'EOS' + STRTRIM(threshEOS,2)

;part to adjust for the time in dekads instead of JD (the new version of pheno correctly store JD so 
;this part should be removed)
JDacq_fn='E:\WA\all sahel\data\UpTo1301g_bil'
nbAcq = 532
OPENR, lunAcq, JDacq_fn, /GET_LUN
JDvar = ASSOC(lunAcq, LONARR(ns,nbAcq))
;see also in code for removing, seaarch for ACQ_CORRECTION

;Computation
;array used to store the luns
luna= LONARR(N_ELEMENTS(base_par_fn))
FOR i=0, N_ELEMENTS(luna)-1 DO BEGIN
  OPENR, lun, pheno_path+'\'+base_par_fn[i], /GET_LUN & luna[i]=lun
ENDFOR
p0assvar = ASSOC(luna[0], FLTARR(ns,nb)) & p1assvar = ASSOC(luna[1], FLTARR(ns,nb))
p2assvar = ASSOC(luna[2], FLTARR(ns,nb)) & p3assvar = ASSOC(luna[3], FLTARR(ns,nb))
p4assvar = ASSOC(luna[4], FLTARR(ns,nb)) & p5assvar = ASSOC(luna[5], FLTARR(ns,nb))
p6assvar = ASSOC(luna[6], FLTARR(ns,nb)) & t0assvar = ASSOC(luna[7], FLTARR(ns,nb))
t1assvar = ASSOC(luna[8], FLTARR(ns,nb))
OPENW, W1, fname_acc, /GET_LUN
OPENW, W2, fname_accb, /GET_LUN
;variable to store the line output
pheno_line_acc = FLTARR(ns,nb)
pheno_line_accb = FLTARR(ns,nb)
;D) LOOP ON (i,j) pixels
FOR line=0, nl-1, 1L DO BEGIN   ;loop on lines
  ;debug
  ;line=801
  p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
  p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
  p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
  p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
  t1line=float(t1assvar[line])
  ;ACQ_CORRECTION
  JDline=FLOAT(JDvar[line])
  FOR sample=0, ns-1 DO BEGIN ;loop on samples
    ;debug
    ;sample = 2038
    ;E) LOOP ON years and fill the output
    FOR y=0, nb-1 DO BEGIN
      ;process the year if not NAN and season failure (put 0 in this case)
      IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN
        IF (p0line[sample,y] NE -999) THEN BEGIN
          ;compute the fitted curve
          pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
                      p3line[sample,y], p4line[sample,y], p5line[sample,y], $
                      p6line[sample,y]]
          ;ACQ_CORRECTION
          t0line[sample,y] = JDline[sample,t0line[sample,y]]
          ;ACQ_CORRECTION
          t1line[sample,y] = JDline[sample,t1line[sample,y]]  
          tmp = accFromParam(pdhtf_pars, [t0line[sample,y], t1line[sample,y]], threshSOStype, threshSOS, threshEOStype, threshEOS, frstyear+y)
          pheno_line_acc[sample,y] = tmp[0]
          pheno_line_accb[sample,y] = tmp[1]
        ENDIF ELSE BEGIN ;is -999
          pheno_line_acc[sample,y]=0.0
          pheno_line_accb[sample,y]=0.0
        ENDELSE
      ENDIF ELSE BEGIN
        ;fill output with NaN
        pheno_line_acc[sample,y]=!VALUES.F_NAN
        pheno_line_accb[sample,y]=!VALUES.F_NAN
      ENDELSE
    ENDFOR  ;years
  ENDFOR  ;sample
  ;F) write the output
  WRITEU, W1, pheno_line_acc
  WRITEU, W2, pheno_line_accb
ENDFOR  ;line
FREE_LUN, W1
FREE_LUN, W2
FREE_LUN, lunAcq
CLOSE, /ALL
END