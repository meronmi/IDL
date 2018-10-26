Function zprofileFromLonLat, xDeg, yDeg, imagefilename, boxdim, valNaN2
  ; Purpose:
  ;     To retrieve a z-profile of a site (lat lon coord) from a given bil multiband image (in example from phenological procedure result file)
  ;     either as profile of 1-3-5-7-any odd number pixel
  ;   ; CALLING SEQUENCE:
  ;     result = zprofileFromLonLat()
  ;
  ; INPUT:
  ;     xDeg:       x coordinate in degree
  ;     yDeg:       y coordinate in degree
  ;     image_file: name of image file from which z-profile should be extracted
  ;     boxDIM:     dimension of the box around the site (1: 1x1 Pixel, 3: 3x3 Pixel)
  ; OUTPUT:
  ;     zprof:      array of mean z-profile plus standard deviations plus amount of valid pixel used for mean calculation
  ;     -1:         an error was encountered
  ; Example:
  ;     
  ; History:
  ;     Version 1.0: created by AS
  
  LonLat = [xDeg, yDeg]
    
  ;Read header file of image  and store header info
  ;if the image file comes with extention '.img' take into account to form the hdr fname
  basename = STRSPLIT(imagefilename, '.', /EXTRACT)
  tmp = STRSPLIT(read_info('map info', basename[0] + '.hdr') , ',', /EXTRACT)
  ref_x_CL = DOUBLE(tmp[1])                                   ; reference pixel x location in column/line (CL) unit           
  ref_y_CL = DOUBLE(tmp[2])                                   ; reference pixel y location in column/line (CL) unit
  ref_x_deg = DOUBLE(tmp[3])                                  ; reference pixel x location in degrees (deg)
  ref_y_deg = DOUBLE(tmp[4])                                  ; reference pixel y location in degrees (deg)
  size_x_deg = DOUBLE(tmp[5])                                 ; pixel size in x directtion in deg
  size_y_deg = DOUBLE(tmp[6])                                 ; pixel size in y directtion in deg
  nc = FIX(read_info('samples', basename[0] + '.hdr'))      ; number of columns
  nl = FIX(read_info('lines', basename[0] + '.hdr'))        ; number of lines
  nb = FIX(read_info('bands', basename[0] + '.hdr'))        ; number of bands
  dt = FIX(read_info('data type', basename[0] + '.hdr'))    ; data type
  interleave = STRTRIM(read_info('interleave', basename[0] + '.hdr'),2)
  
  IF (boxdim MOD 2 EQ 0) THEN BEGIN
    PRINT, 'Boxdim must be an odd number'
    STOP
  ENDIF
  
  IF (dt NE 4) THEN BEGIN
    PRINT, 'Data type ' + STRTRIM(dt,2) + ' not admitted, only floating'
    STOP  ;only floating images are treated now
  ENDIF
  
  ; check if reference pixel x location (in CL) has correct value (1.0 or 1.5)
  IF (ref_x_CL NE 1.5) AND (ref_x_CL NE 1.0) THEN BEGIN
    print, '[ERROR]: value for x location of reference pixel is not 1 (referring to upper left corner) or 1.5 (referring to centre of upper left pixel)'
    return, -1
  ENDIF
  
  ; check if reference pixel y location (in CL) has correct value (1.0 or 1.5)
  IF (ref_y_CL NE 1.5) AND (ref_x_CL NE 1.0) THEN BEGIN
    print, '[ERROR]: value for y location of reference pixel is not 1 (referring to upper left corner) or 1.5 (referring to centre of upper left pixel)'
    return, -1
  ENDIF
    
  ; check if value for x and y location (in CL) of reference pixel is 1.5 and if yes transform it to 1.0 (and also shift transform values in deg accordingly)
  IF (ref_x_CL EQ 1.5) THEN BEGIN
    ref_x_CL = 1.0
    ref_y_CL = 1.0
    ref_x_deg = ref_x_deg - size_x_deg/2.0
    ref_y_deg = ref_y_deg - size_y_deg/2.0
  ENDIF
  
  ; Check if site is outside the image
  IF (LonLat[0] LT ref_x_deg) OR (LonLat[0] GT ref_x_deg + nc * size_x_deg) OR (LonLat[1] GT ref_y_deg) OR (LonLat[1] LT ref_y_deg - nc * size_y_deg) THEN BEGIN
    PRINT, '[ERROR]: Pixel is outside the image'
    return, -1
  ENDIF
  
  ; Convert LonLat in a fraction of columns/lines
  c_fraction = (LonLat[0] - ref_x_deg)/ size_x_deg
  l_fraction = (ref_y_deg - LonLat[1])/ size_y_deg
  
  ; Assign column and line number to site
  c = FLOOR(c_fraction)   
  l = FLOOR(l_fraction)   
  ;DEBUG
;  IF (c EQ 530) AND (l EQ 631) THEN BEGIN
;    PRINT, 'check'
;  ENDIF
  
;  ; check if box dimension is 1 (single pixel) or 3 (3 pixel x 3 pixel)
;  
;  IF boxdim NE 1 AND boxdim NE 3 THEN BEGIN
;    print, '[ERROR]: no valid box dimension (should be 1 or 3)'
;    return, -1
;  ENDIF
  
  ; open image file and extract z-profile for single pixel, making distinction between bil and bsq
  boxSemiDim = FLOOR(boxdim / 2.0) 
  OPENR, R1, imagefilename, /GET_LUN
  ; create empty array to store z-profiles of boxdim x boxdim pixels
  z = DBLARR(boxdim, boxdim, nb) * !VALUES.F_NAN
  CASE interleave OF
    'bil':BEGIN
      ; access data line wise
      line_ass_data = ASSOC(R1, FLTARR(nc,nb))
      ; get z-profiles of boxdim^2 pixel
      FOR i = l-boxSemiDim,l+boxSemiDim,1 DO BEGIN
        data=FLOAT(line_ass_data[i])
        z[*,i-(l-boxSemiDim),*] = data[c-boxSemiDim:c+boxSemiDim,*]
      ENDFOR
    END
    'bsq':BEGIN
      data = FLTARR(nc,nl,nb)
      READU, R1, data
      z = data[c-boxSemiDim : c+boxSemiDim, l-boxSemiDim : l+boxSemiDim, *]
    END
    ELSE: BEGIN
      PRINT, 'Interleave ' + interleave + ' is not admitted'
      STOP
    END
  ENDCASE
  FREE_LUN, R1
  
  ;replace flagged values with NaN
  ind = WHERE(z EQ valNaN2, count)
  IF (count GT 0) THEN z[ind] = !VALUES.F_NAN
  
  ; create array for mean z-profile, std, number of finite values used for mean calculation
  zprof = DBLARR(3,nb)
  
  FOR i = 0,nb-1,1 DO BEGIN
    slice =  z[*,*,i]
    ; we can have from 1 to n pixels
    ; if all pixels are NaN, just assign NaN and do not calculate mean, std
    IF (TOTAL(FINITE(slice)) EQ 0) THEN BEGIN
      zprof[*,i] = !VALUES.F_NAN
    ENDIF ELSE BEGIN
      ; calculate mean of all pixel values of a slice
      zprof[0,i] = mean(slice,/DOUBLE, /NAN)
      ; calculate standard deviation of 9 pixel values of a slice
      ;stdev issue a "% Program caused arithmetic error: Floating illegal operand" when only one obs is
      ;present and the /NAN option is used. Small trick to avoid the problem
      IF (TOTAL(FINITE(slice)) EQ 1) THEN zprof[1,i] = !VALUES.F_NAN ELSE zprof[1,i] = stddev(slice,/DOUBLE, /NAN)
      ; count pixels with finite values (that are not NaN)
      ind = WHERE(FINITE(slice), countFin)
      zprof[2,i] = countFin
    ENDELSE
  ENDFOR
  
  RETURN, zprof
END

