PRO dec_files_job_handlerVGT4AFR
;  Purpose:
;     Job handler for prepare_dec_files2

;####################################################################################
;USER DEFINED SETTINGS
;Full path file name of the time series bil stack file (it must be floating type)
;fname='K:\Tunisia\BP_data\BIL\TUN_BP_FAPAR_9836_1128_sc4'
;fname='K:\Tunisia\BP_data\BIL\TUN_BP_NDVI_9836_1128_sc'
fname='K:\Tunisia\VGT_data\bil\VGT4AFR\bil_redim_NDVI_stacking_sc4'
;Input file characteristics
ns=494    ;number of samples (columns)
nl=841;842    ;number of line (rows)
nb=498    ;number of bands
;Full path of the output directory (it must already exists) 
;out_path='K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv\REALIGN'
out_path='K:\Tunisia\TEST NDVI VGT4AFR\dekadal_files';'K:\Tunisia\BP_data\BIL\test3';'K:\Tunisia\BP_data\BIL\dek_files'
;PREFIX for output files
;prefix = 'BPdFAPAR'
prefix = 'VGT4AFRdNDVI'
;Dekad of the first band of the input stack file
fst_calendar_dek_in=10;36              ;for VGT time series
;Year of the first band of the input stack file
fst_year_in=1998                    ;for VGT time series
;First dekad to be extracted from the profile
fst_calendar_dek_out=1;              ;1st of gen 1st of September (for Tunisia)
;Number of dekads to be extracted starting from fst_calendar_dek_out (to extract all dekads set it to 36)
n_consequent_dekads_to_extract=36   ;until mid may of the following year
;Year of the first band in output (the main use it to be consisten with realigned products)
fst_year_out=1997                   ;to be consisten with realigned products
;Year of the last band in output  (the main use it to be consisten with realigned products)
lst_year_out=2011                   
;####################################################################################

prepare_dec_files2, fname, out_path, prefix, ns, nl, nb, $
      fst_calendar_dek_in, fst_year_in, fst_calendar_dek_out, n_consequent_dekads_to_extract, $
      fst_year_out, lst_year_out
END

PRO dec_files_job_handler2
;  Purpose:
;     Job handler for prepare_dec_files2

;####################################################################################
;USER DEFINED SETTINGS
;Full path file name of the time series bil stack file (it must be floating type)
;FAPAR
;fname='K:\Tunisia\VGT_data\bil\Tunisia_VGTa_sc'
;NDVI
fname='E:\Tunisia\VGT_data\bil\Tunisia_VGTi_up1336_sc'
;Input file characteristics
ns=494    ;number of samples (columns)
nl=842    ;number of line (rows)
nb=567    ;number of bands
;Full path of the output directory (it must already exists) 
;out_path='K:\Tunisia\VGT_data\bil\DIR_RECOMPOSED_UppEnv\REALIGN'
out_path='E:\Tunisia\VGT_data\bil\dek_files'
;
;PREFIX for output files
prefix = 'MarsNDVI';'dFAPAR'
;Dekad of the first band of the input stack file
fst_calendar_dek_in=10              ;for VGT time series
;Year of the first band of the input stack file
fst_year_in=1998                    ;for VGT time series
;First dekad to be extracted from the profile
fst_calendar_dek_out=1; 25             ;1st of gen 1st of September (for Tunisia)
;Number of dekads to be extracted starting from fst_calendar_dek_out (to extract all dekads set it to 36)
n_consequent_dekads_to_extract=24   ;until mid may of the following year
;Year of the first band in output (the main use it to be consisten with realigned products)
fst_year_out=1997                   ;to be consisten with realigned products
;Year of the last band in output  (the main use it to be consisten with realigned products)
lst_year_out=2013                   
;####################################################################################

prepare_dec_files2, fname, out_path, prefix, ns, nl, nb, $
      fst_calendar_dek_in, fst_year_in, fst_calendar_dek_out, n_consequent_dekads_to_extract, $
      fst_year_out, lst_year_out
END



PRO prepare_dec_files2, fname, out_path, prefix,ns, nl, nb, $
      fst_calendar_dek_in, fst_year_in, fst_calendar_dek_out, n_consequent_dekads_to_extract, $
      fst_year_out, lst_year_out
                   
;  Purpose:
;     To extract a given dekad from all year of a time series of dekadal RS products
;     The time series must be a bil stack file of consequent dekads starting from
;     the dekad fst_calendar_dek_in.
;     The code can extract the yearly profile of several dekads 
;     (n_consequent_dekads_to_extract) starting from 'fst_calendar_dek_out' 
;     Such files can be used in regress as if they were pheno products
;
;  Outcome:
;     files of yearly profiles of dekad X (dX, when X > 36, the calendar dekad is X-36)
;
;  Restriction:
;     The input file must be a floating bil envi file
;
;  Usage:
;     prepare_dec_files2, fname, out_path, ns, nl, nb, $
;     fst_calendar_dek_in, fst_year_in, fst_calendar_dek_out, n_consequent_dekads_to_extract, $
;     fst_year_out, lst_year_out

;  Input parameters: 
;     fname
;     out_path
;     ns, nl, nb
;     fst_calendar_dek_in
;     fst_year_in
;     fst_calendar_dek_out
;     n_consequent_dekads_to_extract
;     fst_year_out
;     lst_year_out
;
;  Output parameters: None.
;     
;  Return values: None.
;
;  Author:
;     Michele Meroni, michele.meroni@jrc.ec.europa.eu
;     
;  History:
;     Version 01:  Beta version, May 2011
;     Version 0.1: Beta version with comments added for CNCT, November 2011

;  References:
;     none
;





;READ the stack
in=FLTARR(ns,nl,nb)
OPENR, R1, fname, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
FOR line=0, nl-1, 1L DO in[*, line, *]=float(line_ass_data[line])
CLOSE, R1

;deifine the correspondance bewteen bands and progressive dekads from dek 0 fst_year_out
bands=INDGEN(nb)    ;0,1,..,N
pdekads=bands+fst_calendar_dek_in+(fst_year_in-fst_year_out)*36     ;progressive dekads from 0 of first year out (so the first is 1 and so on)
;define the file out
nbout=(lst_year_out-fst_year_out)+1
out=FLTARR(ns,nl,nbout)
;build the file out
FOR i = 0,  n_consequent_dekads_to_extract-1 DO BEGIN
  ;defines the dekads to be stored in this files as pdekads
  target_dek = fst_calendar_dek_out + i
  ;if target_dek EQ 39 then stop
  target_dek_progressive = target_dek ;for output name
  IF (target_dek GT 36) THEN BEGIN
    target_dek = target_dek - 36
    target_pdekads=INDGEN(nbout)*36 + target_dek + 36
  ENDIF ELSE BEGIN
    target_pdekads=INDGEN(nbout)*36 + target_dek
  ENDELSE
  FOR bout = 0, nbout-1 DO BEGIN
    ;fill the output file
    ind = WHERE(pdekads EQ target_pdekads[bout], count)
    IF (count EQ 0) THEN out[*,*,bout] = !VALUES.F_NAN $
    ELSE  out[*,*,bout] = in [*,*,ind]
  ENDFOR   
  ;save the file
  ;with all indentified dekads, save a bil file
  fileout=out_path+'\'+STRTRIM(prefix,2)+strtrim(target_dek_progressive,2)
  IF FILE_TEST(fileout) eq 1 THEN FILE_DELETE, fileout
  OPENW, W1, fileout, /GET_LUN, /APPEND
  FOR line=0, nl-1, 1L DO WRITEU, W1, out[*,line,*]
  FREE_LUN, W1
  ; WRITE HEADER OF THE OUTPUT
  HEADER_OUT=fileout+'.hdr'
  OPENW, lun, HEADER_OUT, /GET_LUN
  printf,lun,'ENVI'
  printf,lun,'description = dek product'
  printf,lun,'samples ='+STRCOMPRESS(ns)
  printf,lun,'lines   ='+STRCOMPRESS(nl)
  printf,lun,'bands   ='+STRCOMPRESS(nbout)
  printf,lun,'header offset = 0'
  printf,lun,'file type = ENVI Standard'
  printf,lun,'data type = 4'
  printf,lun,'interleave = bil'
  printf,lun,'byte order = 0'
  printf,lun,'band names = {'
  FOR j = 0, nbout-2 DO PRINTF, lun, STRTRIM(fst_year_out + j, 2) + ','
  PRINTF, lun, STRTRIM(fst_year_out + j, 2)+'}'
  
  FREE_LUN, lun
ENDFOR


 

END