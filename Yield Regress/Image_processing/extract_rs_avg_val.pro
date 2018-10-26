FUNCTION extract_RS_avg_val, RsLayer, DepLayer, IdDep, $
                             UseMaxFaparThreshold, AvgMaxFaparLayer, MaxFaparThreshold, $
                             UseCropMask, AFILayer, MinCropFract, $
                             MinPixPerDept
                             
;Compute the weighted average of the input RS layer for a given department.
;The wighted average id based on the Area Fraction Image.
;
;
;INPUT:
; - RsLayer: 2D raster of RS indicator 
; - DepLayer: 2D raster map of Departments
; - IdDep: id of the department to be extracted
; - UseMaxFaparThreshold: 0/1, use a threshold on a third variable (AvgMaxFaparLayer) to select the pixels
; - AvgMaxFaparLayer: 2D raster of the third variable layer (set to 0 is the above is 0) 
; - MaxFaparThreshold: threshold of the operation (set to 0 is the above is 0)
; - UseCropMask: 0/1, 1: use Area Fraction Image and compute a weighted average at department level
; - AFI_layer: Area Fraction image, scaled 0-1
; - MinCropFract: Minum Area Fraction for including the pixel in th average
; - MinPixPerDept: required minimum number of pixel per dept 
;OUTPUT
; Structure out containing:
; - n:        n pix in dept
; - nCm:      as above plus AFI GT 0
; - nAmt:     as above plus GT MaxFaparThreshold
; - nAat:     as above plus GT AFI treshold
; - nFrs:     as above plus finite RS value 
; - totalw:   sum of weights used in the avarege
; - wRSindex: weighted rs index

out = {$
  n    : 0.0, $
  nCm    : 0.0, $
  nAmt: 0.0, $
  nAat: 0.0, $
  nFrs: 0.0, $
  totalw: 0.0, $
  wRSindex: 0.0}

;A) compute the wighted average for RsLayer

;A1) mask for that dept, and store the number of pixel of that dept
ind_dep = WHERE(DepLayer EQ IdDep, tmp) & out.n = tmp
  
;A2) refine mask for those pixels with a Fractional Cover of Crop GT 0
ind_cm = WHERE(AFILayer[ind_dep] GT 0, tmp) & out.ncm = tmp
ind_cm = ind_dep[ind_cm] 

;A3) refine mask for those pixels GT MaxFaparThreshold (Above Fapar Threshold), if required
IF (UseMaxFaparThreshold EQ 1) THEN  BEGIN
  ind_cm_AFT = WHERE(avg_max_fapar[ind_cm] GT MaxFaparThreshold, tmp) & out.nAmt = tmp
  ind_cm_AFT = ind_cm[ind_cm_AFT]
ENDIF ELSE BEGIN
  ind_cm_AFT = ind_cm
  out.nAmt = out.ncm
ENDELSE

;A4) refine mask for those pixels having at least MinCropFract (Above Crop Mask Threshold), if required
IF (UseCropMask EQ 1) THEN BEGIN
  ind_cm_AFT_ACMT = WHERE(AFILayer[ind_cm_AFT] GT MinCropFract, tmp) & out.nAat = tmp
  ind_cm_AFT_ACMT = ind_cm_AFT[ind_cm_AFT_ACMT]
ENDIF ELSE BEGIN
  ind_cm_AFT_ACMT = ind_cm_AFT
  out.nAat = out.ncmaft
ENDELSE

;A5) Year dependent: refine mask for those pixels having a finite pheno product this year
ind_ncm_AFT_ACMT_FCY = WHERE(FINITE(RsLayer[ind_cm_AFT_ACMT]) EQ 1, tmp) & out.nFrs = tmp
IF (tmp EQ 0) THEN RETURN, 10 
ind_ncm_AFT_ACMT_FCY = ind_cm_AFT_ACMT[ind_ncm_AFT_ACMT_FCY]

;give a warning if such pixels are less the minimum 
IF out.nFrs LT MinPixPerDept THEN $
  PRINT,'Warning: '+strtrim(out.nFrs,2)+' pixels in dept n. '+strtrim(IdDep,2)

;A6) Year dependent: compute total weight
out.totalw=TOTAL(AFILayer[ind_ncm_AFT_ACMT_FCY])
;A7) Year dependent: compute the weighted pheno indicator
out.wRSindex=TOTAL((RsLayer[ind_ncm_AFT_ACMT_FCY] * AFILayer[ind_ncm_AFT_ACMT_FCY]))/$
                         out.totalw
                         
                         
RETURN, out
END