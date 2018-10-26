FUNCTION dep_AVG_of_pheno, pheno_fname, pheno_dtype, dep_fname, dep_dtype, ns, nl, nb
;  Purpose:
;     To make an average at department level of a given pheno product (can be 1 or multi band)
;     if intersect=1 (in the code below) it assign the avg value to those
;     pixesl having a finite and non failure (-999) pheno product
;     if set to 0, all pixel belonging to the dept are mapped with the avg value

;  Restriction:
;     To be used with true zero point data, NO CIRCULAR data as SOS for example
;  Outcome:
;     the department level avg of pheno is written to a file

;  Usage:
;     res = dep_AVG_of_pheno(pheno_fname, pheno_dtype, dep_fname, dep_dtype, ns, nl, nb)

;  Example:
;     print, dep_AVG_of_pheno('D:\Users\meronmi\Documents\JRC\JRC Documents\HoA 2011\GIS\HoA_Gaul2_raster_integer', 2, 'D:\Users\meronmi\Documents\JRC\JRC Documents\HoA 2011\GIS\HoA_Gaul2_raster', 12, 3586, 3810, 1)

;  Input parameters: 
;    pheno_fname:       file name with full path (IT HAS TO BE bil)
;    pheno_dtype:       datatype (2 integer, 4 float)
;    dep_fname:         file name with full path
;    dep_dtype:         datatype (2 integer, 12 unsigned integer), = is no data
;    ns, nl, nb:        number of samples, lines, bands of pheno (ns, nl has to be valid for dep as well)
;
;  Return value: 
;     0:    normal completion
;     10:   input file does not exists
;     20:   invalid datatype

intersect = 1

; Open files
IF FILE_TEST(pheno_fname) EQ 0 THEN RETURN, 10
OPENR, R1, pheno_fname, /GET_LUN
IF FILE_TEST(dep_fname) EQ 0 THEN RETURN, 10
OPENR, R2, dep_fname, /GET_LUN
fileout = pheno_fname+'_depAVG'

OPENW, W1, fileout, /GET_LUN ; a BSQ will be written

; Load pheno
CASE pheno_dtype OF
  2: assline = ASSOC(R1, INTARR(ns,nb))
  4: assline = ASSOC(R1, FLTARR(ns,nb))
  ELSE: return, 20 
ENDCASE 
data=FLTARR(ns, nl, nb)
out=data*!VALUES.F_NAN
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
    data[*,line,*]=float(assline[line])
ENDFOR

; Load dep
CASE FIX(dep_dtype) OF
  1:  dept=BYTARR(ns, nl)
  2:  dept=INTARR(ns, nl)
  12: dept=UINTARR(ns, nl)
  ELSE: return, 20 
ENDCASE 
READU, R2, dept
dept=FLOAT(dept)
;set 0 (background value) to NaN
ind=WHERE(dept EQ 0, count)
IF count NE 0 THEN dept[ind]=!VALUES.F_NAN

FOR b=0, nb-1 DO BEGIN    ;at the moment it is just one band
  tmp=AVG_arr1_with_arr2_spatial_support(REFORM(data[*,*,b]), dept)
  IF N_ELEMENTS(tmp) EQ 1 THEN STOP
  ;if required, keep only those pixels having a finite value in the input pheno
  ;product
  IF intersect EQ 1 THEN BEGIN
    ind = WHERE((FINITE(REFORM(data[*,*,b])) NE 1) OR (REFORM(data[*,*,b]) EQ -999), count)
    IF count NE 0 THEN tmp[ind]=!VALUES.F_NAN
  ENDIF
  out[*,*,b]=tmp
ENDFOR
WRITEU, W1, out

; WRITE HEADER OF THE OUTPUT
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = dept avg'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nb)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 4'
printf,3,'interleave = bsq'
printf,3,'byte order = 0'

CLOSE, /ALL
RETURN, 0
END 