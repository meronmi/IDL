function MapGrid_Labels, orientation, location, fractional, defaultlabel



  if (location eq 0) then $

    return, orientation ? 'Equator' : 'Prime Meridian'



  degree = '!M' + STRING(176b) ; Use the Math symbol

  label = STRTRIM(ROUND(ABS(location)),2) + degree

  suffix = orientation ? ((location lt 0) ? 'S' : 'N') : $

    ((location lt 0) ? 'W' : 'E')



  return, label + suffix

end