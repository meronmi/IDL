FUNCTION generate_acqJULDAY_image_memory, mvc, path, ns, nl, year_start, dek_start, $
                                year_stop, dek_stop, fn_out
;  Purpose:
;  
;     Memory saving version of generate_acqJULDAY_image (save one file per one band and create a mta file)
;  
;     To build a bil image containing the acquisition date referring
;     to the RS indicator (e.g., FAPAR) made with a MVC rule (mvc set to 1),
;     or mean. The acquisition day in the month is reported in the *g files
;     The output is the Julian day. Each year is considered made up of 
;     365 or 366 days (so considering leap years)
;     The output is  thus JULDAY(month,day,year).
;     To get the DOY from this number x:
;     x = JULDAY(12,31,1999)
;     CALDAT, x, Monthx, Dayx, Yearx
;     doy = x - JULDAY(12, 31, Yearx-1)

;  Outcome:
;     Bil file

;  Usage:
;     rc = generate_acqJULDAY_image_memory(mvc, path, ns, nl, year_start, dek_start, year_stop, dek_stop, fn_out)
;     boku: rc = generate_acqJULDAY_image_memory(2, 'X:\FSNAU_pheno', 1344, 1624, 2002, 9, 2016, 18, 'boku_som_jd')
;     boku2: rc = generate_acqJULDAY_image_memory(2, 'E:\WA MODIS\WA ROI\bil files', 7842, 1459, 2002, 9, 2016, 35, 'boku_of_wa_jd')

;     eMODIS: rc = generate_acqJULDAY_image_memory(0, 'E:\ILRI\pheno', ??, ??, 2001, 1, 2014, 36, 'eMODIS_wa_ndvi_36_2014_JD')
;     eM Niger: rc = generate_acqJULDAY_image_memory(0, 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\ACT_S10\JD_NE_south_eM_010101-151231', 6673, 3896, 2001, 1, 2015, 36, 'JD_160301')
                                                                                                                                  
;     eM Niger: rc = generate_acqJULDAY_image_memory(0, 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\JD_160301', 6673, 3896, 2001, 1, 2015, 36, 'JD_160301')
;     eMODIS IGAD: rc = generate_acqJULDAY_image_memory(0, 'E:\GRACE\emodis\JD_1km', 3473, 3977, 2001, 1, 2015, 1, 'eMODIS_ndvi_01_15_JD')
;     rc = generate_acqJULDAY_image_memory(1,'Q:\WA\raw\g files\update', 4716, 1233, 2012, 28, 2012, 32, 'update_meta')

;  Input parameters: None.
;      mvc: 0 is Mean/Median composite, 1 if Maximum Value Composite, 2 if boku data (last day of the of dek)
;           if the mean/median comp is used, the dek is assigned to the 5th day of the dekad
;           in this case the g files are not needed
;      path: full path where vtTTYYg (TT is dekad, YY is year) files are stored,
;            output bil file will be placed here as well
;      ns: number of samples
;      nl: number of lines
;      year_start: YYYY of first image (e.g., 1998 for VGT, maybe different if updating..)
;      dek_start: TT of first image (e.g., 10 for VGT, may be different if updating)
;      year_stop: YYYY of last image
;      dek_stop: TT of last image
;      fn_out: (not used!) output bil file name without path, it will be stored in path
;  Output parameters: None.

;  Return values:
;     0: Normal completion.
;    10: Error in finding/reading the files

;  Examples:
;      print, generate_acqJULDAY_image_memory(1,'E:\WA\all sahel\data\g images', 7841, 1458, 2013, 02, 2014, 07, 'aaa_mta_udate1407')
;      print, generate_acqJULDAY_image_memory(1,'Q:\Extended_HoA\raw G', 4538, 4124, 1998, 10, 2012, 24, 'up1224_bil_ExtHoA_g')
;      print, generate_acqJULDAY_image_memory(1,'Q:\WA\raw\g files', 4716, 1233, 1998, 10, 2012, 27, 'WA_VGT_up2012_27_g')
;      print, generate_acqJULDAY_image_memory(0,'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS\ACT_S10\JD_14_12_2015', 6155, 1721, 2001, 1, 2015, 33, 'jdMODIS_niger_010101_151121')

;  History:
;     Version 1.0: Michele Meroni, 07/08/2012

; compute the number of band of the bil file
nb = (year_stop - year_start - 1) * 36 + (36 - dek_start + 1) + dek_stop
;check that there are enough file in path (if files ahs to be found)
IF (mvc EQ 1) THEN BEGIN
  res = FILE_SEARCH(path, '*g.hdr',   COUNT = nfiles)
  IF (nfiles LT nb) THEN RETURN, 10
ENDIF

;Open the mta
OPENW, lunMta, path+'\metafile_gJD.mta', /GET_LUN
PRINTF, lunMta, 'ENVI META FILE'
mat_in = LONARR(ns, nl)
tmp = BYTARR(ns, nl)
dek = dek_start
year = year_start

IF (mvc EQ 1) THEN BEGIN
  ;read the g files (all in a huge matrix for the time being)
  FOR i = 0, nb - 1 DO BEGIN
    TT = dek
    IF (dek LT 10) THEN TT = '0' + STRTRIM(dek,2) ELSE TT = STRTRIM(dek,2)
    YY = STRMID(STRTRIM(year,2), 2, 2) 
    fn = path + '\' + 'vt' + YY + TT + 'g.img' 
    IF (FILE_TEST(fn) NE 1) THEN STOP 
    OPENR, lun, fn, /GET_LUN
    READU, lun, tmp
    FREE_LUN, lun
    indNaN = WHERE(tmp GT 250, countNaN)
    ;mat_in[*,*,i] = tmp
    month = CEIL(dek / 3.0)
    tmpL = JULDAY(month, tmp, year)
    tmpL[indNaN] = -999 
    mat_in[*,*] = tmpL
    ;save the file
    fn = path + '\' + 'vt' + YY + TT + 'gJD.img'
    ;save its hdr
    OPENW, lun, fn + '.hdr', /GET_LUN
    PRINTF, lun,'ENVI'
    PRINTF, lun,'description = JULIAN DAY OF ACQUISITION'
    PRINTF, lun,'samples ='+STRCOMPRESS(ns)
    PRINTF, lun,'lines   ='+STRCOMPRESS(nl)
    PRINTF, lun,'bands   ='+STRCOMPRESS(1)
    PRINTF, lun,'header offset = 0'
    PRINTF, lun,'file type = ENVI Standard'
    PRINTF, lun,'data type = 3'
    PRINTF, lun,'interleave = bsq'
    PRINTF, lun,'sensor type = VGT'
    PRINTF, lun,'byte order = 0'
    FREE_LUN, lun 
    OPENW, lun, fn, /GET_LUN
    WRITEU, lun, mat_in
    FREE_LUN, lun
    ;upadte the mta
    PRINTF, lunMta, 'File : ' + STRTRIM(fn, 2)
    PRINTF, lunMta, 'Bands : 1'
    PRINTF, lunMta, 'Dims : 1-' + STRTRIM(ns, 2) + ',1-' + STRTRIM(nl, 2)
    PRINTF, lunMta, '' 
    dek0 = dek
    year0 = year
    dek = dek + 1
    IF (dek GT 36) THEN BEGIN
      dek = 1
      year = year + 1
    ENDIF
  ENDFOR
  ;check that all file were considered
  IF (dek0 NE dek_stop) OR (year0 NE year_stop) THEN STOP
ENDIF ELSE BEGIN
  ;here mvc is 0 or 2
   FOR i = 0, nb - 1 DO BEGIN
    IF (dek LT 10) THEN TT = '0' + STRTRIM(dek,2) ELSE TT = STRTRIM(dek,2)
    YY = STRMID(STRTRIM(year,2), 2, 2) 
    ;retrive the month
    month = CEIL(dek / 3.0)
    IF (mvc EQ 0) THEN BEGIN
      IF ((dek MOD 3) EQ 1) THEN day = 5
      IF ((dek MOD 3) EQ 2) THEN day = 15
      IF ((dek MOD 3) EQ 0) THEN BEGIN
       ;retrieve the first JUL day of the dekad
       frst_jul = JULDAY(month, 21, year)
       ;retrieve the last JUL day of the dekad (last of the month)
       lst_jul = JULDAY(month+1, 1, year)-1
       CALDAT, frst_jul, tmpm, day, tmpy
       day = day + FLOOR((lst_jul - frst_jul)/2)
      ENDIF
    ENDIF
    IF (mvc EQ 2) THEN BEGIN
      IF ((dek MOD 3) EQ 1) THEN day = 10;1
      IF ((dek MOD 3) EQ 2) THEN day = 20;11
      IF ((dek MOD 3) EQ 0) THEN BEGIN ;day = 21
        ;retrieve the last JUL day of the dekad (last of the month)
        lst_jul = JULDAY(month+1, 1, year)-1
        CALDAT, lst_jul, tmpm, day, tmpy
      ENDIF
    ENDIF
    mat_in[*,*] = JULDAY(month, day, year)
    ;save the file
    fn = path + '\' + 'vt' + STRTRIM(YY,2) + STRTRIM(TT,2) + 'gJD' 
;    OPENW, lun, fn + '.img', /GET_LUN
;    WRITEU, lun, mat_in
;    FREE_LUN, lun
    ;save its hdr
    OPENW, lun, fn + '.hdr', /GET_LUN
    PRINTF, lun,'ENVI'
    PRINTF, lun,'description = JULIAN DAY OF ACQUISITION'
    PRINTF, lun,'samples ='+STRCOMPRESS(ns)
    PRINTF, lun,'lines   ='+STRCOMPRESS(nl)
    PRINTF, lun,'bands   ='+STRCOMPRESS(1)
    PRINTF, lun,'header offset = 0'
    PRINTF, lun,'file type = ENVI Standard'
    PRINTF, lun,'data type = 3'
    PRINTF, lun,'interleave = bsq'
    PRINTF, lun,'sensor type = unknown'
    PRINTF, lun,'byte order = 0'
    FREE_LUN, lun
    OPENW, lun, fn, /GET_LUN
    WRITEU, lun, mat_in
    FREE_LUN, lun
    ;upadte the mta
    PRINTF, lunMta, 'File : ' + STRTRIM(fn, 2)
    PRINTF, lunMta, 'Bands : 1'
    PRINTF, lunMta, 'Dims : 1-' + STRTRIM(ns, 2) + ',1-' + STRTRIM(nl, 2)
    PRINTF, lunMta, '' 
    dek0 = dek
    year0 = year
    dek = dek + 1
    IF (dek GT 36) THEN BEGIN
      dek = 1
      year = year + 1
    ENDIF
  ENDFOR 
  ;check that all file were considered
  IF (dek0 NE dek_stop) OR (year0 NE year_stop) THEN STOP
ENDELSE 
;Free the mta

FREE_LUN, lunMta
RETURN, 0                          
END        