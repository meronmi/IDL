FUNCTION NDIST_Zscore, ts, $
  MIN_POSOBS = MIN_POSOBS     ; set number of positive observations required to calculate SPI

if ~KEYWORD_SET(MIN_POSOBS) then $
   min_posobs    = 12       ; unless we get this many non-zero rain events, return all 0 spi vals
  
zdim = n_elements(ts)
ts   = double(reform(ts,zdim))
zs   = double(reform(ts,zdim)) * !VALUES.D_NAN 
indFin = WHERE(FINITE(ts), countFin)
IF (countFin EQ 0) THEN return,replicate(!VALUES.F_NAN,zdim)
pos_ids = where(ts[indFin] gt 0.00)
pvals   = float(n_elements(pos_ids))
if pvals lt min_posobs then return,replicate(!VALUES.F_NAN,zdim) ; not enough non-zeros
pos_ids = indFin[pos_ids]
zs[pos_ids] = (ts[pos_ids]-mean(ts[pos_ids]))/stddev(ts[pos_ids])
 
return,zs
END
