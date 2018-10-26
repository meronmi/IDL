PRO create_tom 
ns =  4538
nl =  4124
nb = 1
dir = 'E:\Extended_HoA\NDVI4prob\pheno'
sos_fn = dir + '\' + $
  ['1gspy_1s_15A1sos-1997_sos1_DOC_TZPavg','2gspy_1s_15A1sos-1997_sos1_DOC_TZPavg','2gspy_2s_15A1sos-1997_sos2_DOC_TZPavg']
eos_fn = dir + '\' + $
  ['1gspy_1s_15A1sos-1997_eos1_DOC_TZPavg','2gspy_1s_15A1sos-1997_eos1_DOC_TZPavg','2gspy_2s_15A1sos-1997_eos2_DOC_TZPavg']
tom_fn = dir + '\' + $
  ['TOM_Not_Used_Now_monomodal1','TOM_Not_Used_Now1','TOM_Not_Used_Now2']
FOR i=0,2 DO BEGIN
  sos = FLTARR(ns, nl)
  eos = sos
  tom = sos
  
  OPENR, lun,  sos_fn[i], /GET_LUN
  READU, lun, sos
  FREE_LUN, lun
  
  OPENR, lun,  eos_fn[i], /GET_LUN
  READU, lun, eos
  FREE_LUN, lun
  tom = (sos+eos)/2.0
  ind = WHERE(sos gt eos, count)
  IF (count GT 0) THEN BEGIN
    tom[ind] = (sos[ind]+365.0+eos[ind])/2.0
    ind = WHERE(tom GE 366, count)
    IF (count GT 0) THEN tom[ind] = tom[ind] -365.0 
  ENDIF
  OPENW, lun,  tom_fn[i], /GET_LUN
  WRITEU, lun, tom
  FREE_LUN, lun
  FILE_COPY, sos_fn[i] + '.hdr',  tom_fn[i] + '.hdr', /OVERWRITE
ENDFOR

END