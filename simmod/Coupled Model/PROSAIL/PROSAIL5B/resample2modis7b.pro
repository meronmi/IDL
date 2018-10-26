; PROCEDURE COMMENTS +
; NAME: resample2MODIS7b
; AUTHOR: Michele Meroni
;
; CONTACT INFO: michele.meroni@gmail.com
; DESCRIPTION:
; Resample 1 nm prosail output to MODIS terra 7 bands


; CALLING SEQUENCE:
;       output = resample2MODIS7b(x, y, rsrArray)
; INPUTS: 
; x and y: wavelength and reflectance to be resampled
; rsrArray: 8 colunms array with 
; wl  Relative_SpecRes_B1 Relative_SpecRes_B2 Relative_SpecRes_B3 Relative_SpecRes_B4 Relative_SpecRes_B5 Relative_SpecRes_B6 Relative_SpecRes_B7
; as derived from function dataRSR_MODIST_7b
; OUTPUTS: 7 reflectance value (reflectances in the MODIS bands)
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY: written on the 14 July 2014
; CODING NOTES:
; 
FUNCTION resample2MODIS7b, x, y, rsrArray
res = FLTARR(7)
res[0] = TOTAL(y[rsrArray.b1_inds[0]:rsrArray.b1_inds[1]] * rsrArray.rsr_norm_b1)
res[1] = TOTAL(y[rsrArray.b2_inds[0]:rsrArray.b2_inds[1]] * rsrArray.rsr_norm_b2)
res[2] = TOTAL(y[rsrArray.b3_inds[0]:rsrArray.b3_inds[1]] * rsrArray.rsr_norm_b3)
res[3] = TOTAL(y[rsrArray.b4_inds[0]:rsrArray.b4_inds[1]] * rsrArray.rsr_norm_b4)
res[4] = TOTAL(y[rsrArray.b5_inds[0]:rsrArray.b5_inds[1]] * rsrArray.rsr_norm_b5)
res[5] = TOTAL(y[rsrArray.b6_inds[0]:rsrArray.b6_inds[1]] * rsrArray.rsr_norm_b6)
res[6] = TOTAL(y[rsrArray.b7_inds[0]:rsrArray.b7_inds[1]] * rsrArray.rsr_norm_b7)

;FOR i = 0, 6 DO BEGIN
;  indRSR = WHERE(rsrArray[i+1, *] GT 0.0)
;  indY = WHERE((x GE rsrArray[0, indRSR[0]]) AND (x LE rsrArray[0, indRSR[N_ELEMENTS(indRSR1)-1]]))
;  ;debug +
;;  h = plot (indRSR)
;;  h2 = plot (indY)
;  ;debug -
;  ;check that wl corresponds
;  ;IF (TOTAL(x[indY] - rsrArray[0, indRSR]) NE 0) THEN STOP
;  res[i] = TOTAL(y[indY] * rsrArray[i+1, indRSR])/TOTAL(rsrArray[i+1, indRSR])
;ENDFOR
RETURN, res

;RETURN, rsrArray ## y
END
