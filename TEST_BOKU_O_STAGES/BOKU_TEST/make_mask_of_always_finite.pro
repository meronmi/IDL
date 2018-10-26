PRO make_mask_OF_always_finite
fn_of = '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\OF2003-2016_bil'
x =  ReadBilWithHdr(fn_of)
sz = SIZE(x)
nbands = sz[3]
mask = BYTARR(sz[1],sz[2])+1
miss = TOTAL(x GT 250, 3)
ind = WHERE(miss EQ 0)
mask[ind] = 2
ind = WHERE(miss EQ nbands)
mask[ind] = 0 
mask = BYTE(mask)
;2 all present
;1 some present
;0 never present
res = write_envi_img(mask, '\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\masks\OF_finiteness')
res = write_envi_hdr('\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\masks\OF_finiteness.hdr', sz[1], sz[2], 1)

END