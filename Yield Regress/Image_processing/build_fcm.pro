FUNCTION build_dept, ns, nl, X10_fname, X1_fname
;Purpose
; prepare the raster department map at X1 (VGT) resolution
; from a dep map at X10 spatial resolution

;Input
; ns: n sample of the X1 map
; nl: n lines of the X1 map
; X10_fname: full path and file name of the map at 10x resolution
; X1_fname: X1 map desired output filename

;Restrictions
; X10_fname has to be a byte file with ns'=10*ns and nl'=10*nl

;Outcome 
; X1_fname file

;Return value
; the X1 map

dept=BYTARR(ns,nl)         ;varaible for output
;open the X10 map
OPENR, R1, X10_fname, /GET_LUN
dp100=BYTARR(ns*10L,nl*10L)
READU, R1, dp100
CLOSE, R1
;check if the X1 pixel is completely filled by a unique department value 
FOR s=0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    ;CHECK THAT CONTAINS ONLY ONE VALUE, AND THIS VALUE IS NOT 0
    sub = dp100[s*10:(((s+1)*10)-1),l*10:(((l+1)*10)-1)]
    ind = UNIQ(sub[SORT(sub)])
    IF ((N_ELEMENTS(IND) eq 1) AND (SUB[0,0] ne 0)) THEN $
      dept[s,l]= sub[0,0] ELSE dept[s,l]= 0 
  ENDFOR  ;l
ENDFOR  ;s
;WRITE FILE
OPENW, W1, X1_fname, /GET_LUN
WRITEU, W1, dept
CLOSE, W1
;WRITE HDR
res=write_hdr(X1_fname, 1, ns, nl, 1, 'bsq')
RETURN, dept
END



FUNCTION build_fcm, ns, nl, X10_fname, fcm_fp_fname
;prepare the agri weighted map at VGT resolution from a crop mask raster at X10 spatial resolution
 
fcm=FLTARR(ns,nl)         ;fractional crop mask (0-1)
;open the 100 m crop mask
OPENR, R1, X10_fname, /GET_LUN
cm100=BYTARR(ns*10L,nl*10L)
READU, R1, cm100
CLOSE, R1
;compute the fractional cover of agri
FOR s=0, ns-1 DO BEGIN
  FOR l=0, nl-1 DO BEGIN
    fcm[s,l]=TOTAL(cm100[s*10:(((s+1)*10)-1),l*10:(((l+1)*10)-1)])/100.0
    ;modification to consider only those pixels having fcm > 50%
    IF (fcm[s,l] lt 0.5) THEN fcm[s,l]=0.0
  ENDFOR  ;l
ENDFOR  ;s
;WRITE FILE
OPENW, W1, fcm_fp_fname, /GET_LUN
WRITEU, W1, fcm
CLOSE, W1
;WRITE HDR
res=write_hdr(fcm_fp_fname, 4, ns, nl, 1, 'bsq')
RETURN, fcm
END