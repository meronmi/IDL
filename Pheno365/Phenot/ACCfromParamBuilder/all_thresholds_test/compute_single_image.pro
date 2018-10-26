FUNCTION compute_single_image, pheno_path, output_path, base_par_fn, ns, nl, nb, frstyear, JDacq_fn, nbAcq, $
         globrad_fname, nbGlobrad, data, s_option, e_option

;compose output filename
fname_out = output_path + '\' + 'Ba0'+ 'Gr0' + $
            'S' + STRTRIM(s_option,2) + 'E' + STRTRIM(e_option,2)
fname_outb = output_path + '\' + 'Ba1'+ 'Gr0'  + $
            'S' + STRTRIM(s_option,2) + 'E' + STRTRIM(e_option,2)
fname_outgr = output_path + '\' + 'Ba0'+ 'Gr1' + $
            'S' + STRTRIM(s_option,2) + 'E' + STRTRIM(e_option,2)
fname_outbgr = output_path + '\' + 'Ba1'+ 'Gr1'  + $
            'S' + STRTRIM(s_option,2) + 'E' + STRTRIM(e_option,2)            

;part to adjust for the time in dekads instead of JD (the new version of pheno correctly store JD so
;this part should be removed)
;JDacq_fn='E:\WA\all sahel\data\UpTo1301g_bil'
;nbAcq = 532
OPENR, lunAcq, JDacq_fn, /GET_LUN
JDvar = ASSOC(lunAcq, LONARR(ns,nbAcq))
;see also in code for removing, seaarch for ACQ_CORRECTION

;open globrad file
OPENR, lungr, globrad_fname, /GET_LUN
grAssVar = ASSOC(lungr, LONARR(ns,nbGlobrad))

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
 
;variable to store the line output
pheno_acc = FLTARR(ns,nl,nb) * !VALUES.F_NAN
pheno_accb = FLTARR(ns,nl,nb) * !VALUES.F_NAN  
pheno_accgr = FLTARR(ns,nl,nb) * !VALUES.F_NAN
pheno_accbgr = FLTARR(ns,nl,nb) * !VALUES.F_NAN
;loop on sites
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  FOR ll=0,2 DO BEGIN
    line = data.coordSL[i,0,ll,1] ;position to the UL corner at the first loop, then move to the next line
    p0line=float(p0assvar[line]) & p1line=float(p1assvar[line])
    p2line=float(p2assvar[line]) & p3line=float(p3assvar[line])
    p4line=float(p4assvar[line]) & p5line=float(p5assvar[line])
    p6line=float(p6assvar[line]) & t0line=float(t0assvar[line])
    t1line=float(t1assvar[line])
    ;ACQ_CORRECTION
    JDline= FLOAT(JDvar[line])
    GRline = FLOAT(grAssVar[line])
    FOR ss=0,2 DO BEGIN
      sample = data.coordSL[i,ss,ll,0]
      year = data.year[i]
      y = year - frstYear
      grpix = REFORM(grline[sample,*])  ;globrad of the pixel
      IF ((y GT 0) AND (y LT 16))THEN BEGIN    ;the current year is GE 1998
        IF (FINITE(p0line[sample,y]) EQ 1) THEN BEGIN
          IF (p0line[sample,y] NE -999) THEN BEGIN
            ;globrad of that year, from t0 to t1 (in deks from 10 1998), to be corrected ACQ_CORRECTION
            gry=grpix[t0line[sample,y]:t1line[sample,y]] 
            ;x axis of global rad in JD
            grx = REFORM(JDline[sample,t0line[sample,y]:t1line[sample,y]])
            ;compute the fitted curve
            pdhtf_pars=[p0line[sample,y], p1line[sample,y], p2line[sample,y], $
              p3line[sample,y], p4line[sample,y], p5line[sample,y], $
              p6line[sample,y]]
            ;ACQ_CORRECTION
            t0 = JDline[sample,t0line[sample,y]]
            ;ACQ_CORRECTION
            t1 = JDline[sample,t1line[sample,y]]
            threshSOStype = '%'
            IF e_option NE 200 THEN BEGIN
              threshEOStype = '%'
              tmp = accWithGlobradFromParam(pdhtf_pars, [t0, t1], threshSOStype, s_option, threshEOStype, e_option, frstyear+y, gry, grx)  
            ENDIF ELSE BEGIN
              threshEOStype = 'DOY'
              IF (FINITE(data.biomass[i])) AND  (FINITE(data.Month[i]) AND FINITE(data.Day[i])) THEN BEGIN
                doy = DDMMYYYY2DOY(data.Day[i], data.Month[i], year)
                threshEOS = doy
                tmp = accWithGlobradFromParam(pdhtf_pars, [t0, t1], threshSOStype, s_option, threshEOStype, threshEOS, frstyear+y, gry, grx)
              ENDIF ELSE BEGIN
                tmp=[!VALUES.F_NAN,!VALUES.F_NAN,!VALUES.F_NAN,!VALUES.F_NAN]
              ENDELSE
            ENDELSE
            pheno_acc[sample,line,y] = tmp[0]
            pheno_accb[sample,line,y] = tmp[1]
            pheno_accgr[sample,line,y] = tmp[2]
            pheno_accbgr[sample,line,y] = tmp[3]
          ENDIF ELSE BEGIN ;is -999
            pheno_acc[sample,line,y] = 0.0
            pheno_accb[sample,line,y] = 0.0
            pheno_accgr[sample,line,y] = 0.0
            pheno_accbgr[sample,line,y] = 0.0
          ENDELSE
        ENDIF
      ENDIF
    ENDFOR ; ss
  ENDFOR ;ll
ENDFOR ;i
OPENW, W1, fname_out, /GET_LUN
;F) write the output (ATTENZIONE now bsq)
WRITEU, W1, pheno_acc
FREE_LUN, W1

OPENW, W1, fname_outb, /GET_LUN
;F) write the output (ATTENZIONE now bsq)
WRITEU, W1, pheno_accb
FREE_LUN, W1

;F) write the output (ATTENZIONE now bsq)
OPENW, W1, fname_outgr, /GET_LUN
WRITEU, W1, pheno_accgr
FREE_LUN, W1

OPENW, W1, fname_outbgr, /GET_LUN
;F) write the output (ATTENZIONE now bsq)
WRITEU, W1, pheno_accbgr
FREE_LUN, W1

;hdr
OPENW, W1, fname_out+'.hdr', /GET_LUN
PRINTF, W1, 'ENVI'
PRINTF, W1, 'description = {realigned pheno results}'
PRINTF, W1, 'samples = 7841'
PRINTF, W1, 'lines   = 1458'
PRINTF, W1, 'bands   = 16'
PRINTF, W1, 'header offset = 0'
PRINTF, W1, 'file type = ENVI Standard'
PRINTF, W1, 'data type = 4'
PRINTF, W1, 'interleave = bsq'
PRINTF, W1, 'byte order = 0'
PRINTF, W1, 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
PRINTF, W1, 'values = {If time is Julian Day) of 1997}'
PRINTF, W1, 'band names = {'
PRINTF, W1, 'sos:1/1997 - 365/1997,'
PRINTF, W1, 'sos:1/1998 - 365/1998,'
PRINTF, W1, 'sos:1/1999 - 365/1999,'
PRINTF, W1, 'sos:1/2000 - 365/2000,'
PRINTF, W1, 'sos:1/2001 - 365/2001,'
PRINTF, W1, 'sos:1/2002 - 365/2002,'
PRINTF, W1, 'sos:1/2003 - 365/2003,'
PRINTF, W1, 'sos:1/2004 - 365/2004,'
PRINTF, W1, 'sos:1/2005 - 365/2005,'
PRINTF, W1, 'sos:1/2006 - 365/2006,'
PRINTF, W1, 'sos:1/2007 - 365/2007,'
PRINTF, W1, 'sos:1/2008 - 365/2008,'
PRINTF, W1, 'sos:1/2009 - 365/2009,'
PRINTF, W1, 'sos:1/2010 - 365/2010,'
PRINTF, W1, 'sos:1/2011 - 365/2011,'
PRINTF, W1, 'sos:1/2012 - 365/2012}'
FREE_LUN, W1

OPENW, W1, fname_outb+'.hdr', /GET_LUN
PRINTF, W1, 'ENVI'
PRINTF, W1, 'description = {realigned pheno results}'
PRINTF, W1, 'samples = 7841'
PRINTF, W1, 'lines   = 1458'
PRINTF, W1, 'bands   = 16'
PRINTF, W1, 'header offset = 0'
PRINTF, W1, 'file type = ENVI Standard'
PRINTF, W1, 'data type = 4'
PRINTF, W1, 'interleave = bsq'
PRINTF, W1, 'byte order = 0'
PRINTF, W1, 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
PRINTF, W1, 'values = {If time is Julian Day) of 1997}'
PRINTF, W1, 'band names = {'
PRINTF, W1, 'sos:1/1997 - 365/1997,'
PRINTF, W1, 'sos:1/1998 - 365/1998,'
PRINTF, W1, 'sos:1/1999 - 365/1999,'
PRINTF, W1, 'sos:1/2000 - 365/2000,'
PRINTF, W1, 'sos:1/2001 - 365/2001,'
PRINTF, W1, 'sos:1/2002 - 365/2002,'
PRINTF, W1, 'sos:1/2003 - 365/2003,'
PRINTF, W1, 'sos:1/2004 - 365/2004,'
PRINTF, W1, 'sos:1/2005 - 365/2005,'
PRINTF, W1, 'sos:1/2006 - 365/2006,'
PRINTF, W1, 'sos:1/2007 - 365/2007,'
PRINTF, W1, 'sos:1/2008 - 365/2008,'
PRINTF, W1, 'sos:1/2009 - 365/2009,'
PRINTF, W1, 'sos:1/2010 - 365/2010,'
PRINTF, W1, 'sos:1/2011 - 365/2011,'
PRINTF, W1, 'sos:1/2012 - 365/2012}'
FREE_LUN, W1

;hdr
OPENW, W1, fname_outgr+'.hdr', /GET_LUN
PRINTF, W1, 'ENVI'
PRINTF, W1, 'description = {realigned pheno results}'
PRINTF, W1, 'samples = 7841'
PRINTF, W1, 'lines   = 1458'
PRINTF, W1, 'bands   = 16'
PRINTF, W1, 'header offset = 0'
PRINTF, W1, 'file type = ENVI Standard'
PRINTF, W1, 'data type = 4'
PRINTF, W1, 'interleave = bsq'
PRINTF, W1, 'byte order = 0'
PRINTF, W1, 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
PRINTF, W1, 'values = {If time is Julian Day) of 1997}'
PRINTF, W1, 'band names = {'
PRINTF, W1, 'sos:1/1997 - 365/1997,'
PRINTF, W1, 'sos:1/1998 - 365/1998,'
PRINTF, W1, 'sos:1/1999 - 365/1999,'
PRINTF, W1, 'sos:1/2000 - 365/2000,'
PRINTF, W1, 'sos:1/2001 - 365/2001,'
PRINTF, W1, 'sos:1/2002 - 365/2002,'
PRINTF, W1, 'sos:1/2003 - 365/2003,'
PRINTF, W1, 'sos:1/2004 - 365/2004,'
PRINTF, W1, 'sos:1/2005 - 365/2005,'
PRINTF, W1, 'sos:1/2006 - 365/2006,'
PRINTF, W1, 'sos:1/2007 - 365/2007,'
PRINTF, W1, 'sos:1/2008 - 365/2008,'
PRINTF, W1, 'sos:1/2009 - 365/2009,'
PRINTF, W1, 'sos:1/2010 - 365/2010,'
PRINTF, W1, 'sos:1/2011 - 365/2011,'
PRINTF, W1, 'sos:1/2012 - 365/2012}'
FREE_LUN, W1

OPENW, W1, fname_outbgr+'.hdr', /GET_LUN
PRINTF, W1, 'ENVI'
PRINTF, W1, 'description = {realigned pheno results}'
PRINTF, W1, 'samples = 7841'
PRINTF, W1, 'lines   = 1458'
PRINTF, W1, 'bands   = 16'
PRINTF, W1, 'header offset = 0'
PRINTF, W1, 'file type = ENVI Standard'
PRINTF, W1, 'data type = 4'
PRINTF, W1, 'interleave = bsq'
PRINTF, W1, 'byte order = 0'
PRINTF, W1, 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
PRINTF, W1, 'values = {If time is Julian Day) of 1997}'
PRINTF, W1, 'band names = {'
PRINTF, W1, 'sos:1/1997 - 365/1997,'
PRINTF, W1, 'sos:1/1998 - 365/1998,'
PRINTF, W1, 'sos:1/1999 - 365/1999,'
PRINTF, W1, 'sos:1/2000 - 365/2000,'
PRINTF, W1, 'sos:1/2001 - 365/2001,'
PRINTF, W1, 'sos:1/2002 - 365/2002,'
PRINTF, W1, 'sos:1/2003 - 365/2003,'
PRINTF, W1, 'sos:1/2004 - 365/2004,'
PRINTF, W1, 'sos:1/2005 - 365/2005,'
PRINTF, W1, 'sos:1/2006 - 365/2006,'
PRINTF, W1, 'sos:1/2007 - 365/2007,'
PRINTF, W1, 'sos:1/2008 - 365/2008,'
PRINTF, W1, 'sos:1/2009 - 365/2009,'
PRINTF, W1, 'sos:1/2010 - 365/2010,'
PRINTF, W1, 'sos:1/2011 - 365/2011,'
PRINTF, W1, 'sos:1/2012 - 365/2012}'
FREE_LUN, W1

FREE_LUN, lunAcq
FREE_LUN, lungr
CLOSE, /ALL 
RETURN, 0
END