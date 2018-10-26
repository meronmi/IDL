PRO fromBilToSingleBsq
e= ENVI()
fn = 'E:\WA\EWP\EWP_images\standardize with spirits\cwp_hl2000_bil'
fnbsq = 'E:\WA\EWP\EWP_images\standardize with spirits\cwp_hl2000_bsq'
raster1 = e.OpenRaster(fn)
; Create a temporary output file


raster1.Export, fnbsq, 'envi', INTERLEAVE='bsq'
samples = 1867
lines = 348
bands = 900
dt = 4
;just take the last 540

OPENR,lun, fnbsq, /GET_LUN
ass_image = ASSOC(lun, FLTARR(samples, lines))
FOR i = bands-540, bands-1 DO BEGIN
  out_image = FIX(ass_image[i])
  
ENDFOR
FREE_LUN, lun 

END