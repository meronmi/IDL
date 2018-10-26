FUNCTION  B_matchQA, MODA, MODQ

;output compatible with previous files
;Code  qc  Year  DOY_comp  Year.dayfract Year_acq  DOY_acq RAA SZA VZA R1_645  R2_858.5  R3_469  R4_555  R5_1240 R6_1640 R7_2130
n = N_ELEMENTS(MODQ.GeeSysInd)
res = CREATE_STRUCT('Code',MODQ.code,'qc',MODQ.qc,'Year',MODQ.Year_comp,'DOY_comp',MODQ.Doy_comp,$
      'YearFract', FLTARR(n),'Year_acq',LONARR(n),'DOY_acq',LONARR(n),'RA',FLTARR(n),'SZA',FLTARR(n),'VZA',FLTARR(n),$
      'R1',FLTARR(n),'R2',FLTARR(n),'R3',FLTARR(n),'R4',FLTARR(n),'R5',FLTARR(n),'R6',FLTARR(n),'R7',FLTARR(n)) 


;extract date string from sysInd
stringDateA = MODA.GeeSysInd
tmp = STRSPLIT(MODA.GeeSysInd, '_', /EXTRACT)
tmp = tmp.ToArray()
dateCompA = tmp[*,2]+'_'+tmp[*,3]+'_'+tmp[*,4]
tmp = STRSPLIT(MODQ.GeeSysInd, '_', /EXTRACT)
tmp = tmp.ToArray()
dateCompQ = tmp[*,2]+'_'+tmp[*,3]+'_'+tmp[*,4]

;loop: match A on Q (if no Q is available there is no need to have A), 
;by date and code
;check that computed DOY of comp is equal and store in a new structure
FOR i = 0, n-1 DO BEGIN
  ind = WHERE((dateCompA EQ dateCompQ[i]) AND (MODA.code EQ MODQ.code[i]), count)
  ;some checks
  IF (count NE 1) THEN STOP
  IF (MODA.Doy_comp[ind] NE MODA.Doy_comp[i]) THEN STOP
  ;fill the structure
  res.YearFract[i] = MODA.YearFract[ind]
  res.Year_acq[i] = MODA.Year_acq[ind]
  res.DOY_acq[i] = MODA.DOY_acq[ind]
  res.RA[i] = MODA.RA[ind]
  res.SZA[i] = MODA.SZA[ind]
  res.VZA[i] = MODA.VZA[ind]
  res.R1[i] = MODA.R1[ind]
  res.R2[i] = MODA.R2[ind]
  res.R3[i] = MODA.R3[ind]
  res.R4[i] = MODA.R4[ind]
  res.R5[i] = MODA.R5[ind]
  res.R6[i] = MODA.R6[ind]
  res.R7[i] = MODA.R7[ind]
ENDFOR
PRINT, 'FINISHED'
RETURN, 0
END