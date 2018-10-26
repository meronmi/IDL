FUNCTION queryDb, objDB, query
RSObj = OBJ_NEW('IDLdbRecordset', objDB, SQL = query)
;determine if the result of the queery is empty
res = RSObj->MoveCursor(/FIRST)
IF (res EQ 0) THEN RETURN, 'empty'
;detrmine the total number of records
res = RSObj->MoveCursor(/LAST)
n_records = RSObj->CurrentRecord()+1
;determine the numer of fields
RSobj.GetProperty, FIELD_INFO = field_info
dim = SIZE(field_info)
data = FLTARR(dim[1],n_records)
res = RSObj->MoveCursor(/FIRST)
FOR i = 0, n_records-1 DO BEGIN
  IF (i EQ 0) THEN res = RSObj->MoveCursor(/FIRST) ELSE res = RSObj->MoveCursor(/NEXT)
  FOR f = 0, dim[1]-1 DO data[f,i] =  RSObj->GetField(f)
ENDFOR
RETURN, data
END