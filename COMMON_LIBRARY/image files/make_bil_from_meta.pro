PRO handler
  ;PRINT, make_bil_from_meta('X:\tmp\STACKS\WSI_a1991-2016C02.mta', 'X:\tmp\STACKS\WSI_a1991-2016C02_bil')
  ;  PRINT, make_bil_from_meta('E:\WA MODIS\WA ERA INT\ECMWF_rad.mta', 'E:\WA MODIS\WA ERA INT\ECMWF_rad_bil')
  ;  PRINT, make_bil_from_meta('E:\WA MODIS\WA ERA INT\original data\ECMWF_tav.mta', 'E:\WA MODIS\WA ERA INT\ECMWF_tav_bil')
  ;  PRINT, make_bil_from_meta('E:\WA MODIS\WA ROI CHIRPS\chirps.mta', 'E:\WA MODIS\WA ROI CHIRPS\chirps_bil')
  ;  PRINT, make_bil_from_meta('Y:\meteo\GWSI\v4_2017_05_04\Africa_ROI\AAA_C1_91-16.mta', 'X:\WSI_v4_2017_05_04\STACKS\WSI_v4_1991-2016C01_bil')
  ;PRINT, make_bil_from_meta('Y:\meteo\GWSI\v4_2017_05_04\Africa_ROI\AAA_C2_91-16.mta', 'X:\WSI_v4_2017_05_04\STACKS\WSI_v4_1991-2016C02_bil')
  ;PRINT, make_bil_from_meta('\\ies\d5\asap\TEST_PREDICTORS\DATA_X\SPI_1km_ROI_Africa\metaSPI1_200701-2017-14.mta', '\\ies\d5\asap\TEST_PREDICTORS\DATA_X\SPI_1km_ROI_Africa\metaSPI1_200701-2017-14_bil')
  ;PRINT, make_bil_from_meta('\\ies\h04\Foodsec\meteo\GWSI\v6_noRoot_kcTimingUpdated\Africa_ROI\AAA_C1_91-16_v6.mta', 'X:\WSI_v6\WSI_v6_1991-2016C01_bil')
  PRINT, make_bil_from_meta('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\NEP of SPIRITS\AAA_nepNfLf_of_spirits.mta', '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\NEP of SPIRITS\AAA_nepNfLf_of_spirits_bil')
END

PRO make_all_in_a_dir
  dir_in = 'D:\HoA';'W:\TEST_BOKU_CONSOLIDATION_STAGE\3tiles\H22V08ENVI
  files_in = FILE_SEARCH(dir_in,'*.mta')
  files_out = FILE_BASENAME(files_in, '.mta')
  FOR i = 0, N_ELEMENTS(files_in)-1 DO  BEGIN
    fo = files_out[i]
    fo = STRJOIN(STRSPLIT(fo,'AAA_meta_',/EXTRACT,/REGEX)) ;specific for file containing this string
    ;PRINT, make_bil_from_meta(files_in[i], dir_in + '\' + files_out[i] + '_bil')
    PRINT, make_bil_from_meta(files_in[i], dir_in + '\' + fo + '_bil')
  ENDFOR
END

FUNCTION make_bil_from_meta, meta_file, out_bil_name
  ;convert a mta pointing to a list of bsq envi file (as generated from SPIRITS or ENVI) to bil
  ;stack file named out_bil_name

  ;Read the file
  OPENR, lun, meta_file, /GET_LUN
  file_list =!NULL
  ns = 0L
  nl = 0L
  c = 1
  WHILE ~ EOF(lun) DO BEGIN
    str = ''
    READF, lun, str
    str = STRTRIM(STRSPLIT(str, /EXTRACT, ": ", /REGEX),2)
    CASE str[0] OF
      'File': file_list = [file_list,str[1]]
      'Dims': BEGIN
        tmp = LONG(STRSPLIT(str[1], /EXTRACT, "-,"))
        IF (ns NE 0) THEN BEGIN
          IF (ns NE tmp[1]) AND (nl NE tmp[3]) THEN STOP
        ENDIF ELSE BEGIN
          ns = tmp[1]
          nl = tmp[3]
        ENDELSE
      END
      'Bands': IF (LONG(str[1]) NE 1) THEN STOP
      'File': BEGIN
        PRINT, 'File: ' + STRTRIM(c)
        c = c + 1
      END
      ELSE: BEGIN
        IF (str[0] NE '' AND str[0] NE 'ENVI META FILE') THEN BEGIN
          PRINT, 'Anomalous String in MTA: ' + str[0]
        ENDIF
      END
    ENDCASE
  ENDWHILE
  FREE_LUN, lun
  nb = N_ELEMENTS(file_list)
  ;read the datatype of one and consider equal for all
  fn_without_ext = STRSPLIT(file_list[0],'.', /EXTRACT)
  fn_without_ext = STRJOIN(fn_without_ext[0:-2],'.')
  dt = FIX(read_info('data type',fn_without_ext+'.hdr'))
  mapinfo = read_info('map info',fn_without_ext+'.hdr')
  flags = read_info('flags',fn_without_ext+'.hdr')
  ;make the bil, line by line
  overallclock = TIC()
  OPENW, lunout, out_bil_name, /GET_LUN
  chunck10perct = ROUND(nl/10.0)
  FOR l = 0, nl -1 DO BEGIN
    IF (l MOD chunck10perct EQ 0) THEN BEGIN
      PRINT, SYSTIME()
      PRINT, 'Done: ' + STRTRIM(l/chunck10perct*10,2) + '%'
    ENDIF
    IF (l EQ 0) THEN clock = TIC()
    FOR b = 0, nb -1 DO BEGIN
      ;with assoc
      var = MAKE_ARRAY(ns, TYPE=dt)
      OPENR, lun, file_list[b], /GET_LUN
      line = ASSOC(lun,var)
      WRITEU, lunout, line[l]
      FREE_LUN, lun
      ;with read direct (it is slower)
      ;    WRITEU, lunout, READ_BINARY(file_list[b], DATASTART = b*ns, DATA_TYPE = dt, DATA_DIMS = ns)
    ENDFOR
    IF (l EQ 0) THEN BEGIN
      time = TOC(clock)
      PRINT, 'Seconds for a line = ' + STRTRIM(time,2)
      PRINT, 'Minutes for the job = ' + STRTRIM(time*nl/60.0,2)
      PRINT, 'Hours for the job = ' + STRTRIM(time*nl/(60.0*60.0),2)
    ENDIF
  ENDFOR
  FREE_LUN, lunout
  fnout_without_ext = STRSPLIT(out_bil_name,'.',/EXTRACT)
  band_names = STRARR(nb)
  FOR b = 0, nb - 1 DO BEGIN
    str = STRSPLIT(file_list[b], ":\", /EXTRACT)
    band_names[b] = str[-1]
  ENDFOR
  IF (STRING(flags) EQ '-1') THEN ret = write_envi_hdr(fnout_without_ext[0]+'.hdr', ns, nl, dt, NBANDS=nb, $
    INTERLEAVE='bil', MAPINFO=mapinfo, BAND_NAMES=band_names) $
  ELSE ret = write_envi_hdr(fnout_without_ext[0]+'.hdr', ns, nl, dt, NBANDS=nb, $
    INTERLEAVE='bil', MAPINFO=mapinfo, BAND_NAMES=band_names, FLAGS=flags)
  time = TOC(overallclock)
  PRINT, 'Job completed'
  PRINT, 'Minutes for the job = ' + STRTRIM(time/60.0,2)
  PRINT, 'Hours for the job = ' + STRTRIM(time/(60.0*60.0),2)
  RETURN, out_bil_name
END