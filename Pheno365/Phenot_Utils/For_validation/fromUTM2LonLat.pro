FUNCTION fromUTM2LonLat, XUTM, YUTM, ZoneUTM
;transform UTM to LonLat (Zone is 28 for Senegal) using WGS84 as ellipsoid
;
;INPUT:
;XUTM: longitude in UTM, can be a n-element vector
;YUTM: latitude in UTM, can be a n-element vector
;ZoneUTM: UTM Zone, postive value above equator
;OUTPUT
;(2, n) array containing the longitude/latitude coordinates
;
;Example:
;fromUTM2LatLon(656694, 1707656, 28)

;Result = MAP_PROJ_INIT( Projection [, ELLIPSOID=value] [, /GCTP] [, LIMIT=vector] [, /RADIANS] [, /RELAXED] )
mapStruct = MAP_PROJ_INIT(101, ZONE = ZoneUTM, ELLIPSOID = 24)
;Result = MAP_PROJ_INVERSE (X [, Y] [, MAP_STRUCTURE=value] [, /RADIANS] )
LonLatcoordinates = MAP_PROJ_INVERSE (XUTM, YUTM, MAP_STRUCTURE=mapStruct)
RETURN, LonLatcoordinates
END