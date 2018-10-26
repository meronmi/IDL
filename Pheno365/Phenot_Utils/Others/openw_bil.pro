Function openw_bil, lstLU, resume, path, fname
;open a file using as logical unit lstLU+1
;return the opend LU
lstLU=lstLU+1 & LU=lstLU
fileout=path+'\'+fname
IF (FILE_TEST(fileout) EQ 1) AND (resume EQ 0) THEN FILE_DELETE, fileout
OPENW, LU, fileout,  /APPEND ;, /GET_LUN
RETURN, LU
END