
function lst2ct, lst,lng,tz,day,month,year

  ;AUTHOR: S. Leach
  ;PURPOSE: Returns the local time for a give local sidereal time,
  ;         based on a 'reverse search' using ct2lst.pro. Intended
  ;         to be accurate to the nearest second.

  ntest       = 175001L
  time_test   = dindgen(ntest)/(double(ntest)-1d0)*24d0
  ct2lst,lst_test,lng,tz,time_test,day,month,year
  index       = where( abs(lst-lst_test) eq min(abs(lst-lst_test)))

  time = time_test[index]

  return, time

end