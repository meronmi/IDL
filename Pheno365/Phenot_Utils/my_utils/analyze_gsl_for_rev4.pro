PRO analyze_GSL_for_rev4
ns = 7841
nl = 1458
; m = mean pheno
filein= 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\len\1gspy_1s_A1sos-1997_len1_TZPavg.img'
OPENR, lun, filein, /GET_LUN
m=FLTARR(ns, nl)
READU, lun, m
FREE_LUN, lun
; s = sd pheno
filein= 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\len\1gspy_1s_A1sos-1997_len1_TZPsd.img'
OPENR, lun, filein, /GET_LUN
s=FLTARR(ns, nl)
READU, lun, s
FREE_LUN, lun

; glc = reclass glc (1 grass, 2 crop, NaN are present)
filein= 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\glc_reclass.img'
OPENR, lun, filein, /GET_LUN
glc=BYTARR(ns, nl)
READU, lun, glc
FREE_LUN, lun
; eco = ecoregions (all ok, expect 255)

filein= 'X:\WA corr analyis GIS\masks\New Masks\aaa LAST correct clip\5ecoregions.img'
OPENR, lun, filein, /GET_LUN
eco=BYTARR(ns, nl)
READU, lun, eco
FREE_LUN, lun

;make CV
ind = WHERE(FINITE(m) AND FINITE(s))
cv = m * !VALUES.F_NAN
cv[ind] = s[ind]/m[ind]


indeco = WHERE(eco NE 255)
eco01 = eco * 0
eco01[indeco] = 1
eco01 = BYTE(eco01)

indAll = WHERE(((glc EQ 1) OR (glc EQ 2)) AND (eco01 EQ 1))
indGlcGRASS =WHERE((glc EQ 1) AND (eco01 EQ 1))
indGlcCROP = WHERE((glc EQ 2) AND (eco01 EQ 1))

PRINT, 'mean CV ', MEAN(cv[indAll], /NAN)
PRINT, 'mean CV crop', MEAN(cv[indGlcGRASS], /NAN)
PRINT, 'mean CV grass', MEAN(cv[indGlcCROP], /NAN)

fileout = 'X:\WA corr analyis GIS\sahel resuts\AVG pheno 4 ARCGIS\len\cv'
OPENW, lun, fileout+'.img', /GET_LUN
WRITEU, lun, cv
FREE_LUN, lun
OPENW, lun, fileout+'.hdr', /GET_LUN
PRINTF,lun,'ENVI'
PRINTF,lun,'samples ='+STRCOMPRESS(ns)
PRINTF,lun,'lines   ='+STRCOMPRESS(nl)
PRINTF,lun,'bands   ='+STRCOMPRESS(1)
PRINTF,lun,'data type = 4'
PRINTF,lun,'interleave = bsq'
PRINTF, lun , 'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 21.00446430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees}'
PRINTF, lun , 'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
FREE_LUN, lun



END