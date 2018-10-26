Function compute_j1_j2, first_julian_day, n_dekads
;returns the arrays of first and last julian days for the n_dekads
;structure for containg returned values
ret = { julian1: fltarr(n_dekads), julian2: fltarr(n_dekads)} 


julian_day=first_julian_day
for d=0, n_dekads-1 do begin
   CALDAT, julian_day, mm, dd, yy
   ret.julian1[d]=julian_day
   case dd of
    1:  ret.julian2[d]=ret.julian1[d]+9
    11: ret.julian2[d]=ret.julian1[d]+9 
    21: begin
      ;compute julian day for the first day of next month and subtract 1
      if mm eq 12 then begin
        tmp_mm=1
        tmp_yy=yy+1
      endif else begin
        tmp_mm=mm+1
        tmp_yy=yy
      endelse
      ret.julian2[d]=float(floor(JULDAY(tmp_mm, 01, tmp_yy, 13, 0, 0)))-1.0
    end
    else: stop
  endcase
  julian_day=ret.julian2[d]+1.0
  ;debug
;  CALDAT, ret.julian1[d], mm1, dd1, yy1
;  CALDAT, ret.julian2[d], mm2, dd2, yy2
;  print, ret.julian1[d], ret.julian2[d], mm1, dd1, yy1, mm2, dd2, yy2, $
;  FORMAT='(F14.4, F14.4, I4, X, I4, X, I4, X, I4, X, I4, X, I4)'
endfor


return, ret
End