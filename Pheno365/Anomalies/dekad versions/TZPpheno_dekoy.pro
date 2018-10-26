Function TZPpheno_dekoy, circular, ord_offset_dek, fname, dtype, ns, nl, nb
;Transform pheno indicator from absolute decade (0.5 is the central day of the first dekad of the fAPAR
;time series) to relative dekad (dekad of the calendar year) 
;Write a file with the pheno indicator as decak of the year

;Results are stored in a file named fname+'_dekoy'

;RESTRICTIONS
;- input file must be bil
;-999 in pheno product (expected but failed season) is kept

;INPUT
;circular: 0/1 if data are not/ are to be treated as circular data (e.g. Length Of the SeaSon is not)
;ord_offset_dek:      
;          ordinal dekad offsetting the data first, second, ..        
;          X (1,..,36) if data are to be treated as ciruclar data (as decedes form a starting date, SOS for example).
;          X is the ordinal dekad offsetting the data 
;          
;          pheno results are in number of dek from the first dek in the time series
;          (so '0' means the first dek). You get the data in number of dekad since the first year by
;          adding the actual number of the firs dek (e.g. 10 for VGT).
;          
;          In the case of realigned data they already start at the first dekad of
;          given year, so the offset is 0. 
;          Be care: realigned data comply with the standard
;          [0.1-1.0, 1.1-2.0, ..], so that 0.5 means the central day of the first dekad
;          In pheno results 0.0 is to be considered the central day of the first dekad of the time series 
;    
;fname:       file name with full path
;dtype:       datatype (2 integer, 4 float)
;ns, nl, nb:  number of samples, lines, bands

;OUTPUT
; dekoyed bands. Results are stored in a file named fname+'_dekoy'
;
;return code:
;0:     normal completion    
;10:    input file does not exists
;20:    invalid datatype


;EXAMPLE
;res=pheno_dekoy(1, 0, 'K:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv\REALIGN\A19-1997_sos1',4,3586,3810,15)
;res=pheno_dekoy(10,'X:\IGAD\VGT data\pheno_products\somalia v2\sos1',4,3586,3810,14)
;print, pheno_dekoy(10,'X:\HoA\Pheno_products\Somalia_v3\A19_sos1',4,3586,3810,15)
;print, pheno_dekoy(10,'K:\Tunisia\VGT_data\Pheno_products 09 2011\realigned\A27-1997_sos1', 4, 494, 842, 15)
 
IF FILE_TEST(fname) EQ 0 THEN return, 10
OPENR, R1, fname, /GET_LUN

fileout = fname+'_dekoy'

IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

CASE dtype OF
  2: assline = ASSOC(R1, INTARR(ns,nb))
  4: assline = ASSOC(R1, FLTARR(ns,nb))
  ELSE: return, 20 
ENDCASE

data=FLTARR(ns, nb)

out=FLTARR(ns, nb)
nbout=nb



FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  data=float(assline[line])
  ;treat circulare data
  IF (circular NE 0) THEN datac=pdekad2dekoy(data, ord_offset_dek, 0)   
  FOR column=0, ns-1, 1L DO BEGIN      ; loop over all lines
    ;IF (line eq 2592) and (column eq 3017) then stop 
    IF (circular NE 0) THEN BEGIN
      out[column,*]=datac[column,*]
    ENDIF ELSE BEGIN
      PRINT, 'decoy operation asked with no circular data request, the program will stop'
      STOP
    ENDELSE    
  ENDFOR
  WRITEU, W1, out
ENDFOR


; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, 3, HEADER_OUT
printf,3,'ENVI'
printf,3,'description = dekoy of ' + string(fileout)
printf,3,'samples ='+STRCOMPRESS(ns)
printf,3,'lines   ='+STRCOMPRESS(nl)
printf,3,'bands   ='+STRCOMPRESS(nbout)
printf,3,'header offset = 0'
printf,3,'file type = ENVI Standard'
printf,3,'data type = 4'
printf,3,'interleave = bil'
printf,3,'byte order = 0'

CLOSE, /ALL

return, 0
End
