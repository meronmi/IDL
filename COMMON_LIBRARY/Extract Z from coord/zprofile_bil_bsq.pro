Function zprofile_bil_bsq, fn, c, r
;  Purpose:
;     To retrieve the a Z profile from a given bil multiband image, datatype has to be float
;  Usage:
;     z-profile= zprofile()

;  Input parameters: None.
;     fn: input bil or bsq file 
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
;     Version 2.0: work for a genric file, read dimensions from file
  fn_dir = FILE_DIRNAME(fn, /MARK_DIRECTORY)
  fn_base = FILE_BASENAME(fn, '.img')
  hdr_fn = fn_base +'.hdr'
  ns = LONG(read_info('samples', fn_dir + hdr_fn))
  nl = LONG(read_info('lines', fn_dir + hdr_fn))
  nb = read_info('bands', fn_dir + hdr_fn)
  intrlv = read_info('interleave', fn_dir + hdr_fn)
  dt = FIX(read_info('data type', fn_dir + hdr_fn))
  IF (dt NE 4) THEN BEGIN
    PRINT, 'Only floating for now'
    STOP
  ENDIF
  if (r gt nl) or (c gt ns) then begin
    print, '[ERROR]: coordinates are outside image range'
    return, -1
  endif
  CASE intrlv OF
    'bil': BEGIN
      OPENR, R1, fn, /GET_LUN
      line_ass_data = ASSOC(R1, FLTARR(ns,nb))
      line=FLTARR(ns, nb)
      line=line_ass_data[r]
      z_profile=reform(line[c,*])
      FREE_LUN, R1
    END
    'bsq': BEGIN
      OPENR, R1, fn, /GET_LUN
      data = FLTARR(ns,nl,nb)
      READU, R1, data
      z_profile=reform(data[c,r,*])
      FREE_LUN, R1
    END
  ENDCASE
  
  return, z_profile
End