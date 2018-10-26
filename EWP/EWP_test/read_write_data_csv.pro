PRO read_write_data_csv
res = READ_CSV('E:\WA\EWP\CFAPAR_work\TAMSAT resolution\correlation\FOR TUKEY test\data.csv', HEADER=hdr)
PRINT, 'ok'
mat = FLTARR(N_ELEMENTS(res.field2)+1,(MAX(res.field1)-res.field1)+1)
FOR i = MIN(res.field1), MAX(res.field1) DO BEGIN
  ind = WHERE(res.field1 EQ i)
  mat[0,i] = i
  mat[1:*,i] = res.field2[ind]
ENDFOR
WRITE_CSV, 'E:\WA\EWP\CFAPAR_work\TAMSAT resolution\correlation\FOR TUKEY test\data_dendro_format.csv',mat 
END