Function write_hdr, fname, datatype, ns, nl, nb, intrlv
; writes the ENVI hdr file
; return 0 if succesful
; use res=write_hdr(fname, datatype, ns, nl, nb, intrlv)

OPENW, W1, fname + '.hdr', /GET_LUN
PRINTF, W1, 'ENVI'
PRINTF, W1,'samples = '+STRCOMPRESS(ns)
PRINTF, W1,'lines   = '+STRCOMPRESS(nl)
PRINTF, W1,'bands   = '+STRCOMPRESS(nb)
PRINTF, W1,'header offset = 0'
PRINTF, W1,'file type = ENVI Standard'
PRINTF, W1,'data type = ' + STRCOMPRESS(datatype)
PRINTF, W1,'interleave = ' + STRCOMPRESS(intrlv)
PRINTF, W1,'byte order = 0'
CLOSE, W1
RETURN, 0

End