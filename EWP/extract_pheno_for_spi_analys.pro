PRO EXTRACT_PHENO_FOR_SPI_ANALYS

data_path='X:\SPI'
adminFILE  = data_path+'\'+'GAUL1_Africa.img'
phenoFile =  data_path+'\'+'averagesPhenoWeeks_Oscar.csv'   
sos_file= data_path+'\'+'sos'
eos_file= data_path+'\'+'eos'
nl        = 540                        ; number of lines
ns        = 510                        ; number of samples

; Open the Admin1-file
OPENR, lun, adminFILE, /GET_LUN
adminref = ASSOC(lun, LONARR(ns,nl))
adminraster = adminref[0]                                 ; the raster file holding the admin1 codes
free_lun, lun

; Open the pheno-file & get arrays to make link of admin1 code with start week and end week
read = READ_ASCII(phenoFile, data_start=1, DELIMITER=',')
phenoArray = LONG(read.(0))
adminArray = phenoArray[0,*]

cycle=1
IF cycle eq 1 THEN BEGIN
  SOSArray = phenoArray[1,*]
  EOSArray = phenoArray[2,*]
ENDIF ELSE BEGIN
  SOSArray = phenoArray[3,*]
  EOSArray = phenoArray[4,*]
ENDELSE

IF FILE_TEST(sos_file) eq 1 THEN FILE_DELETE, sos_file
OPENW, Wsos, sos_file, /GET_LUN, /APPEND
IF FILE_TEST(eos_file) eq 1 THEN FILE_DELETE, eos_file
OPENW, Weos, eos_file, /GET_LUN, /APPEND

sos=BYTARR(ns,nl)
eos=sos
;create sos and eos
FOR line=0,nl-1,1L DO BEGIN  
  FOR sample=0,ns-1,1L DO BEGIN
    adminPoint = adminraster[sample,line]
    index = WHERE(adminArray eq adminPoint,CNT)
    IF CNT EQ 1 THEN BEGIN
      sos[sample,line]=SOSArray[index]
      eos[sample,line]=EOSArray[index]
    ENDIF 
  ENDFOR
ENDFOR
WRITEU, Wsos, sos
WRITEU, Weos, eos

CLOSE, /ALL

END