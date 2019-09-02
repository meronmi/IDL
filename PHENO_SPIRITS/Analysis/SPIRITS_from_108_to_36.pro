PRO SPIRITS_from_108_to_36
  valNaN = 251
  res = DIALOG_PICKFILE(/MULTIPLE_FILES, FILTER='*.img', GET_PATH = dir)
  n = N_ELEMENTS(res)
  
  FOR i=0, n-1 DO BEGIN
    hdr_fn = STRSPLIT(res[i], '\', /EXTRACT)
    hdr_fn = STRSPLIT(hdr_fn[-1], '.', /EXTRACT)
    hdr_fn = dir + '\' + hdr_fn[0] + '.hdr'
    ns = LONG(read_info('samples', hdr_fn))
    nl = LONG(read_info('lines', hdr_fn))
    dt = FIX(read_info('data type', hdr_fn))
    mapinfo = read_info('map info',hdr_fn)
    ;projectioninfo = read_info('projection info', hdr_fn)
    CASE dt OF
      1: data = BYTARR(ns,nl)
      2: data = INTARR(ns,nl)
      4: data = FLTARR(ns,nl)
      5: data = DBLARR(ns,nl)
      ELSE: STOP
    ENDCASE
    OPENR, lun, res[i], /GET_LUN
    READU, lun, data
    FREE_LUN, lun
    IF (dt EQ 1) THEN BEGIN
      matOut = BYTE(data) * 0B ;+ 254B
    ENDIF ELSE BEGIN
      matOut = FLTARR(ns,nl) -9999
      indNaN = WHERE(~FINITE(data), countNaN)
      IF (countNaN GT 0) THEN data[indNaN] = -9999 
    ENDELSE
    ;change data and put -9999 where gt 250
    
    ind1 = WHERE((data GE 1.0) AND (data LE 36.0), count1)
    IF (count1 GT 0) THEN matOut[ind1] = data[ind1]
    ind2 = WHERE((data GT 36.0) AND (data LE 72.0), count2)
    IF (count2 GT 0) THEN matOut[ind2] = data[ind2] - 36.0
    ind3 = WHERE((data GT 72.0) AND (data LE 108.0), count3)
    IF (count3 GT 0) THEN matOut[ind3] = data[ind3] - 72.0 
    OPENW, lun, remove_ext_from_fn(res[i])+'_1_36.img', /GET_LUN
    WRITEU, lun, matOut
    FREE_LUN, lun
    IF (dt EQ 1) THEN BEGIN
      res2 = write_envi_hdr(remove_ext_from_fn(res[i])+'_1_36.hdr', ns, nl, 1, $
      MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
    ENDIF ELSE BEGIN
      res2 = write_envi_hdr(remove_ext_from_fn(res[i])+'_1_36.hdr', ns, nl, 4, $
      MAPINFO=mapinfo, PROJECTIONINFO=projectioninfo)
    ENDELSE
    
    CLOSE, /ALL
  ENDFOR

END