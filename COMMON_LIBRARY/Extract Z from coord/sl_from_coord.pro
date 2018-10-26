FUNCTION sl_from_coord, fid, east, north
  n = N_ELEMENTS(north)
  map_coord = {$
    north   : FLTARR(n), $
    east    : FLTARR(n)}
  FOR i = 0, n-1 DO BEGIN
    ENVI_CONVERT_FILE_COORDINATES, FID, xx,yy, east, north
     map_coord.east[i] = xx
     map_coord.north[i] = yy
  ENDFOR
  RETURN, map_coord
END