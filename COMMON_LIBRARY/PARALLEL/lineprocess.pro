FUNCTION lineProcess, line, ns, nb, linenumber
  ;test process, loop on sample and regress
  nbOut = 4
  lineOut = FLTARR(ns, nbOut)
  FOR i = 0, ns-1 DO BEGIN
    lineOut[i,*] = TOTAL(REFORM(line[i,*])^2, /NAN) ;spend some cpu time computing something
    lineOut[i,*] = linenumber
  ENDFOR
  WAIT, 0.01; lineOut[i,*] = FLOAT(linregstat(REFORM(FINDGEN(nb)),REFORM(line[i,*])))
  
  RETURN, lineOut
END