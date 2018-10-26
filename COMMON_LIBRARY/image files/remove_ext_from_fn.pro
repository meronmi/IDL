FUNCTION remove_ext_from_fn, fn
;this function return the filename withou extension
;example:
; res = remove_ext_from_fn('image.img')
; PRINT, res
; > image
pos = STRPOS( fn, '.', /REVERSE_SEARCH )
IF (pos NE -1) THEN RETURN, STRMID(fn, 0, pos) ELSE RETURN, fn
END