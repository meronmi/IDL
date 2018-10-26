FUNCTION computeLocalSolarTime, longitude, YYYY, MM, DD, civilHours, civilMinutes, TimeZoneOffset, DaylightSaveOn
; example for Ispra
;PRINT, computeLocalSolarTime(8.610, 2017, 05, 19, 12, 00, 1, 1)

;  https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/bXOISA93XA0
;  The local hour angle of
;  the sun is effectively the local solar time, plus a constant:
;
;  > > local hour angle of the sun = local sidereal time[1] - RA of sun[2]
;
;  and local apparent solar time = local hour angle + 12 hours
;
;  [1] is calculated with CT2LST
;  [2] is calculated with SUNPOS

;compute JD
;add day light saving to time zone
IF (TimeZoneOffset GE 0) THEN Tz = TimeZoneOffset + DaylightSaveOn ELSE Tz = TimeZoneOffset - DaylightSaveOn

CT2LST, lst, longitude, Tz, ten(civilHours,civilMinutes), DD, MM, YYYY

PRINT, 'Local Mean Sidereal Time: ', lst
JDCNV, YYYY, MM, DD, 0, JD
SUNPOS,  JD, ra, dec
PRINT, 'Ra: ', ra
;RA is "Right Ascension," a common astronomical term.  RA
;measures the longitude of a point on the sky in the celestial
;(equatorial) system.  It is commonly measured in units of hours by
;convention, and since it is useful to astronomical observatories (360
;degrees = 24 h).
PRINT, 'Solar local (fraction): ', lst - ra/360.0*24 + 12
PRINT, 'Solar local (time): ', SIXTY(lst - ra/360.0*24 + 12)
RETURN, 0

END