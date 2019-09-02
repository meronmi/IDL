; NAME:
;   SAGO_INTERPOL
;
; PURPOSE:
;   Savitsky Golay filtering of S1 time series
;

;

PRO sago_interpol3_for_s1, data, width, smNDVI=smNDVI, iNDVI=iNDVI, maxNDVI=maxNDVI, minNDVI=minNDVI
  ; added line to avoid values going over maxNDVI or under minNDVI
;  maxNDVI = 1.0
;  minNDVI = 0


  ;find size of arrays and reform array variables
  data  = REFORM(data)
  s = N_ELEMENTS(data)
  i_data = FLOAT(data)

  ; The first Savitzky-Golay filtering for long term change trend fitting
  savgolFilter = SAVGOL(width,width ,0,3)
  sgfit = CONVOL(i_data,savgolFilter,/EDGE_TRUNCATE)
  ;Assign return values
  index1 = WHERE(sgfit gt maxNDVI)
  index2 = WHERE(sgfit lt minNDVI)
  smNDVI=sgfit
  IF index1[0] NE -1 THEN smNDVI[index1]=maxNDVI
  IF index2[0] NE -1 THEN smNDVI[index2]=minNDVI
END
