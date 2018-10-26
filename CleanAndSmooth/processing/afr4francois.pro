Pro afr4francois
path='K:\AFR06-11'
image_file='bil_VGTa_sc4';'Tunisia_VGTas'
fileout1 = path+'\'+'07-11_AvgMaxGT0-15fAPAR'
fileout2 = path+'\'+'07-11_AvgMaxGT0-5fAPAR_and_max-minLT0-3'
thresh1 = 0.15
thresh2 = 0.5
thresh3 = 0.3
ns=7841 & nl=8289 & nb=60        ; number of samples, lines, bands
;ns=400 & nl=400 & nb=60        ; number of samples, lines, bands

OPENR, R1, path+'\'+image_file, /GET_LUN

line_ass_data = ASSOC(R1, FLTARR(ns,nb))
; Create files for output

IF FILE_TEST(fileout1) eq 1 THEN FILE_DELETE, fileout1
OPENW, W1, fileout1, /GET_LUN, /APPEND
IF FILE_TEST(fileout2) eq 1 THEN FILE_DELETE, fileout2
OPENW, W2, fileout2, /GET_LUN, /APPEND

data=FLTARR(ns, nb)
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  ;if line eq 843 then stop
  data=float(line_ass_data[line])
  lineGTt1 = bytarr(ns)
  lineGTt2andVarLTt3 = bytarr(ns)
  FOR column=0, ns-1, 1L DO BEGIN
    
    maxy=FLTARR(5)
    miny=maxy
    
    maxy[0]=MAX(data[column,0:11], /NAN)
    miny[0]=MIN(data[column,0:11], /NAN)
    
    maxy[1]=MAX(data[column,12:23], /NAN)
    miny[1]=MIN(data[column,12:23], /NAN)
    
    maxy[2]=MAX(data[column,24:35], /NAN)
    miny[2]=MIN(data[column,24:35], /NAN)
    
    maxy[3]=MAX(data[column,36:47], /NAN)
    miny[3]=MIN(data[column,36:47], /NAN)
    
    maxy[4]=MAX(data[column,48:59], /NAN)
    miny[4]=MIN(data[column,48:59], /NAN)
    
    maxo=MEAN(maxy, /NAN)
    mino=MEAN(miny, /NAN)
    IF ((FINITE(maxo) EQ 0) OR (FINITE(mino) EQ 0)) THEN BEGIN
      lineGTt1[column]=0 
      lineGTt2andVarLTt3[column]=0
    ENDIF ELSE BEGIN
     IF (maxo GT thresh1) THEN lineGTt1[column]=1 ELSE lineGTt1[column]=0 
     IF ((maxo GT thresh2) AND ((maxo-mino) LT thresh3)) THEN  $
        lineGTt2andVarLTt3[column]=1 ELSE  lineGTt2andVarLTt3[column]=0
    ENDELSE
  ENDFOR
  WRITEU, W1, lineGTt1
  WRITEU, W2, lineGTt2andVarLTt3
ENDFOR
CLOSE, /ALL
; WRITE HEADER OF THE OUTPUTs
HEADER_OUT=fileout1+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(1)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 1'
printf,3,'interleave = bil'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'
CLOSE, 3

HEADER_OUT=fileout2+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(1)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 1'
printf,3,'interleave = bil'
printf,3,'sensor type = VGT'
printf,3,'byte order = 0'
CLOSE, 3

CLOSE, /ALL
End