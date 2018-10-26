Function TZPpheno_dekoc, ord_offset_dek_in, ord_offset_dek_out, fname_in, fname_out, dtype, ns, nl, nb

;INPUT dekad expressed as ordinal dekads (ord_offset_dek_in is first)
;
;Transform pheno indicator from absolute decade 
;(REALIGNED FILES ALWAYS START FROM the first , meaning that pdekad=[0.1-1.0] is the first dekad, dekad 1)
;to relative dekad with respect to the cycle of interest

;            Dekoc in output can be interpreted as completion of dekad according to that cycle.
;            The first dekad goes from GT 0.0 to LE 1.0 and so on.
;            [0.0001-1.0, 1.0001-2.0, ..] 
;            first dekad = "dekad 1"   = cover the dekoy range 0.0001- 1
;            xth dekad = "dekad x"     = cover the dekoy range dekoy x-0.9999 - x
;            10th dekad (start of VGT) = cover the dekoy range dekoy 9.0001-10 and its
;            central point is 9.5


;if the cycle of interest (36 dekds) is the calendar year, so starts at the first dekad (0.1-1.0)
;(offset_dek_out=0), the output will be 0.1-36.0 if the pdekad falls in the year, 36.x, 37, 38, .. if
;it falls in the next, -0.x, -1, -2 if in the previous..

;If it it's not the calendar but starts at offset_dek_out (e.g. offset_dek_out = 18 for the 19th dekad)
;the output 0.1-36.0 is expressed in this cycle (so starting from offset_dek_out = 19, so
;0.1-1 is calendar dekad 18.1-19. Remeber, from 18.1 I am in dek 19!
;(E' come dire che dalle ore 3.1 pm sono nella quarta ora).
;Again the output will  36.x, 37, 38, .. if it falls in the next cycle,
;-0.x, -1, -2 if in the previous..


;Write a file with the pheno indicator as dekad of the cycle

;Results are stored in a file named fname+'_dekoc'

;RESTRICTIONS
;- input file must be bil
;-999 in pheno product (expected but failed season) is kept

;INPUT

;offset_dek_in:   calendar dekad offsetting the data (0 for realigned)
;offset_dek_out:    calendar dekad of the first dekad of the cycle
;    
;fname_in:       file name with full path in
;fname_out:      base file name with full path out
;dtype:       datatype (2 integer, 4 float)
;ns, nl, nb:  number of samples, lines, bands

;OUTPUT
; dekoyed bands
;
;return code:
;0:     normal completion    
;10:    input file does not exists
;20:    invalid datatype


;EXAMPLE
;print, TZPpheno_dekoc(1, 1, 'Q:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv_15_Mar\REALIGN_ON_eos\A31eos-1997_eos1',  'Q:\HoA\VGT data\raw\bil\DIR_RECOMPOSED_UppEnv_15_Mar\REALIGN_ON_eos\A31eos-1997_eos1', 4, 3586, 3810,15)
;print, TZPpheno_dekoc(1, 20, 'K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv\REALIGN\A20-1997_sos1',  'K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv\REALIGN\A20-1997_sos1', 4, 494, 842,15)
 
abs_offset_dek_in = ord_offset_dek_in - 1
abs_offset_dek_out = ord_offset_dek_out - 1 

IF (abs_offset_dek_in NE 0) THEN PRINT, 'Warning: abs_offset_dek_in is set to '+strtrim(abs_offset_dek_in, 2)+ $
                                 'Realingned products normally start at 1!!!! (abs_offset_dek_in = 0)'
 
 
IF FILE_TEST(fname_in) EQ 0 THEN return, 10
OPENR, R1, fname_in, /GET_LUN

fileout = fname_out+'_dekoc'

IF (FILE_TEST(fileout) EQ 1) THEN FILE_DELETE, fileout
OPENW, W1, fileout, /GET_LUN, /APPEND

CASE dtype OF
  2: assline = ASSOC(R1, INTARR(ns,nb))
  4: assline = ASSOC(R1, FLTARR(ns,nb))
  ELSE: return, 20 
ENDCASE

data=FLTARR(ns, nb)
out=FLTARR(ns, nb)

;Load band names from HDR

IF (FILE_TEST(fname_in+'.hdr') NE 1) THEN BEGIN
  PRINT, 'Pheno_dekoc Warning: hdr of input file was not found'
ENDIF ELSE BEGIN
  strarray=read_band_names(fname_in)
ENDELSE

;New method not assuming that the first band refer to the first year
FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines  
  dataLine=float(assline[line]) ;+ abs_offset_dek_in
  dataLineOut = dataLine *!VALUES.F_NAN
  FOR column = 0, ns-1, 1L DO BEGIN
    ;if column eq 110 and line eq 179 then stop
    
    dataLineOut[column, *] = pdekad2dekoc(REFORM(dataLine[column, *]), ord_offset_dek_in, ord_offset_dek_out)
  
  ENDFOR
  WRITEU, W1, dataLineOut
ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=fileout+'.hdr'
OPENW, lun, HEADER_OUT, /GET_LUN
PRINTF,lun,'ENVI'
PRINTF,lun,'description = dekoc of ' + string(fileout)
PRINTF,lun,'samples ='+STRCOMPRESS(ns)
PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
PRINTF,lun,'bands   ='+STRCOMPRESS(nb)
PRINTF,lun,'header offset = 0'
PRINTF,lun,'file type = ENVI Standard'
PRINTF,lun,'data type = 4'
PRINTF,lun,'interleave = bil'
PRINTF,lun,'byte order = 0'
PRINTF,lun,'values = {Number of dekad since caldendar ordinal dekad' + STRTRIM(abs_offset_dek_out,2)+'}'
FOR i = 0, N_ELEMENTS(strarray)-1 DO PRINTF, lun, strarray[i]
CLOSE, /ALL

return, 0
End