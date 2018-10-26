Function zprofile, bil_file, ns, nl, nb, c, r
;  Purpose:
;     To retrieve the a Z profile from a given bil multiband image, datatype has to be float
;  Usage:
;     z-profile= zprofile()

;  Input parameters: None.
;     bil_file: input bil file 
;     ns
;     nl
;     nb
;     c: 0-based colum coordinate 
;     r: 0-based raw coordinate

;  Return values:
;     profile: the z-profile of nb elements
;     -1: an error was encountered
;  Example:
;     z=zprofile('X:\Niger\input\bil_niger_a_fapar_396d_sc', 2018, 1038, 396, 1092, 838)
;  History:
;     Version 1.0: created by MM
  if (r gt nl) or (c gt ns) then begin
    print, '[ERROR]: coordinates are outside image range'
    return, -1
  endif
  
  OPENR, R1, bil_file, /GET_LUN
  line_ass_data = ASSOC(R1, FLTARR(ns,nb))
  line=FLTARR(ns, nb)
  
  line=line_ass_data[r]
  profile=reform(line[c,*])
  
  CLOSE, /ALL ;test
  return, profile
End
