function theil_sen, x, y
  num = n_elements(x)
  n=float(num)*(num-1)/2
  theil=fltarr(n)
  t=0
  for i=0,num-2 do begin
    for j=i+1,num-1 do begin
      theil[t]=float(y[j]-y[i])/(x[j]-x[i])
      t=t+1
    endfor
  endfor
  slope=median(theil ,/even)
  intercept=median(y-slope*x, /even)
  results=[intercept, slope]
  return, results
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;taken from: https://ftian2016.wordpress.com/2016/11/18/theil-sen-slope-and-mann-kendall-significance-test-in-idl/
;pro cal_sen_trend
;  compile_opt idl2
;  envi, /restore_base_save_files
;  envi_batch_init
;
;  file=envi_pickfile(TITLE=’Input stacked time series image’)
;  outfile=file+’_trend_Sen’
;  outfile1=file+’_trend_MK_p’
;
;  envi_open_file, file, r_fid=fid, /NO_REALIZE
;  envi_file_query, fid, dims=dims, nb=nb, ns=ns, nl=nl
;
;  data=fltarr(ns,nl,nb)
;  for p=0,nb-1 do begin
;    data[*,*,p]=envi_get_data(fid=fid,dims=dims,pos=p)
;  endfor
;
;  time_series = findgen(nb)
;
;  sen=fltarr(ns,nl)/0
;  MK_p=fltarr(ns,nl)/0
;
;  for i=0,nl-1 do begin
;    for j=0, ns-1 do begin
;      ts=fltarr(nb)
;      for k=0,nb-1 do ts[k]=data[j,i,k]
;      if finite(data[j,i,0]) then begin
;        sen[j,i]= theil_sen(time_series,ts)
;        res=r_correlate(time_series,ts, /KENDALL)
;        MK_p[j,i]=res[1]
;      endif
;    endfor
;  endfor
;
;  openw, unit, outfile, /Get_LUN
;  writeu,unit,sen
;  free_lun,unit
;
;  openw, unit1, outfile1, /Get_LUN
;  writeu,unit,MK_p
;  free_lun,unit
;
;  inherit = envi_set_inheritance(fid, dims, /SPATIAL)
;  envi_setup_head, fname=outfile, $
;    ns=ns, nl=nl, nb=1, $
;    data_type=4,INTERLEAVE =0, $
;    offset=0,inherit=inherit, /write
;
;  envi_setup_head, fname=outfile1, $
;    ns=ns, nl=nl, nb=1, $
;    data_type=4,INTERLEAVE =0, $
;    offset=0,inherit=inherit, /write
;
;end