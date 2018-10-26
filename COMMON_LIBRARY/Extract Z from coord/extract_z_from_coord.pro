PRO extract_z_from_coord
  envi, /restore  ;test
  fname = 'Q:\WA\new_window\bilWA_FAPAR_sc'
  y = [15.84722222]
  x = [6.813069013]
  ENVI_OPEN_FILE, fname, /INVISIBLE, /NO_REALIZE, R_FID = fid 
  map_coord = sl_from_coord(fid, x, y)
  PRINT, 'EAST:  '+ STRTRIM(map_coord.east,2)
  PRINT, 'NORTH: '+ STRTRIM(map_coord.north,2)
END