FUNCTION DekYear2centralDOY, t, year
;return the central DOY of the dekad
doy = [ddmmyyyy2doy(5,1,year), ddmmyyyy2doy(15,1,year), ddmmyyyy2doy(25,1,year), $
       ddmmyyyy2doy(5,2,year), ddmmyyyy2doy(15,2,year), ddmmyyyy2doy(25,2,year), $
       ddmmyyyy2doy(5,3,year), ddmmyyyy2doy(15,3,year), ddmmyyyy2doy(25,3,year), $
       ddmmyyyy2doy(5,4,year), ddmmyyyy2doy(15,4,year), ddmmyyyy2doy(25,4,year), $
       ddmmyyyy2doy(5,5,year), ddmmyyyy2doy(15,5,year), ddmmyyyy2doy(25,5,year), $
       ddmmyyyy2doy(5,6,year), ddmmyyyy2doy(15,6,year), ddmmyyyy2doy(25,6,year), $
       ddmmyyyy2doy(5,7,year), ddmmyyyy2doy(15,7,year), ddmmyyyy2doy(25,7,year), $
       ddmmyyyy2doy(5,8,year), ddmmyyyy2doy(15,8,year), ddmmyyyy2doy(25,8,year), $
       ddmmyyyy2doy(5,9,year), ddmmyyyy2doy(15,9,year), ddmmyyyy2doy(25,9,year), $
       ddmmyyyy2doy(5,10,year), ddmmyyyy2doy(15,10,year), ddmmyyyy2doy(25,10,year), $
       ddmmyyyy2doy(5,11,year), ddmmyyyy2doy(15,11,year), ddmmyyyy2doy(25,11,year), $
       ddmmyyyy2doy(5,12,year), ddmmyyyy2doy(15,12,year), ddmmyyyy2doy(25,12,year)]
RETURN, doy[t-1]
END