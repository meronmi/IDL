Pro plotpartitioning
  ;just to check the partitioning functions
  alpha_l=fltarr(101)
  alpha_r=alpha_l
  alpha_g=alpha_l
  for i=0,100 do begin
    alpha_l[i]=partitioning_on_fract_cycle(i/100.0,'a')
    alpha_g[i]=partitioning_on_fract_cycle(i/100.0,'g')
    ;if i eq 80 then stop
  endfor
  alpha_r[*]=1.0
  alpha_r=alpha_r-alpha_l-alpha_g
  x=indgen(101)/100.0
  !P.MULTI = 0
  plot, x, alpha_l, yrange=[0,1.2],BACKGROUND = 16777215, COLOR = 0, XTITLE='Fract development', YTITLE='Partitioning based on fract cycle'
  xyouts, 0.5, 0.9, 'Leaves', /NORMAL, COLOR = 0
  oplot, x, alpha_g, color=3000
  xyouts, 0.5, 0.85, 'Grains', /NORMAL, COLOR = 3000
  oplot, x, alpha_r, color=30000
  xyouts, 0.5, 0.8, 'Roots', /NORMAL, COLOR = 30000
  print, 'finished'
END
