Function SampleLineFromLonLat, xDeg, yDeg, imagefilename, boxdim
  ; Purpose:
  ;     To retrieve sample and lines from a given bil multiband image 
  ;     result = SampleLineFromLonLat()
  ;
  ; INPUT:
  ;     xDeg:       x coordinate in degree
  ;     yDeg:       y coordinate in degree
  ;     image_file: name of image file from which samaple and line shoud be computed
  ;     boxDIM:     dimension of the box around the central coord (1: 1x1 Pixel, 3: 3x3 Pixel)
  ; OUTPUT:
  ;     coord:      array (boxDIM, boxDIM, 2) of coordinates. [boxDIM, boxDIM, 0] is sample, [boxDIM, boxDIM, 1] is line
  ;     -1:         an error was encountered
  ; Example:
  ;     
  ; History:
  ;     Version 1.0: zprofileFromLonLat created by AS
  ;     Vesrion 1.o: change of purpose and modifications by MM 
  
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
  coord = LONARR(boxdim, boxdim, 2)
  boxSemiDim = FLOOR(boxdim / 2.0)
  ;lines coordinates, lop on columns and inster lines
  FOR cc=0,boxdim-1 DO coord[cc,*,1] = [l-boxSemiDim+LINDGEN(boxdim)]
  ;column coordinates
  FOR ll=0,boxdim-1 DO coord[*,ll,0] = [c-boxSemiDim+LINDGEN(boxdim)]
  
  
  RETURN, coord
END

