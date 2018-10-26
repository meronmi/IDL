FUNCTION mask_image_by_afi_gt_x, fn_image_2B_masked, fn_afi, x, value_masked
; x is the DN below which, image is set to value value_masked
; 
; Example
; PRINT, mask_image_by_afi_gt_x('\\ies\h04\Foodsec\users\trainee\Pheno_asap2_0\pheno1-108\phenos1.img', 'Y:\asap.2.0\data\ref\land_cover\mask_crop_afi_masked.img', 50, 255)
; 
;open AFI
maskafi = ReadEnviWithHdr(fn_afi)
;select those with afi greater than 
ind = WHERE(maskafi LE x)
sz_mask = SIZE(maskafi)

;open AFI
data = ReadEnviWithHdr(fn_image_2B_masked)
sz_data = SIZE(data)
;now only for one band image
IF sz_data[0] NE 2 THEN STOP

IF (TOTAL([sz_data[1]-sz_mask[1], sz_data[2]-sz_mask[2]]) NE 0) THEN BEGIN
  PRINT, 'mask_image_by_afi_gt_x.pro: mask and file have different x and y dimensions'
  STOP
ENDIF


data[ind] = value_masked
fn_base =  FILE_DIRNAME(fn_image_2B_masked) + '\' + FILE_BASENAME(fn_image_2B_masked, '.img')
fn_out =  fn_base + '_AfiGT' + STRTRIM(x,2) 
res = write_envi_img(data, fn_out + '.img')
FILE_COPY, fn_base + '.hdr', fn_out + '.hdr'




RETURN, 0
END