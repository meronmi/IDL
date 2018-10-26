FUNCTION pheno_struct_define, ns,n_calendar_years
;Defines the structure pheno


;gscomp Growing Season Completeness
;This is an indicator of the actual presence of data for a given season. It is computed for the all 
;the season for concistency with other outputs and it is usefull for the first one and the last (one or two).
;After full processing of the time series, for all season where rel<1000 (all needed data present)
;the avg dekad of SOS and EOS (5 % criterion) is computed and transformed into progressive dekad form 0.
;Such periods will contain avg_n observations. Each season will actually have current_n.
;The % = current_n/avg_n * 100 is then an indicator of how much the season is covered. A percent > 100
;indicates that also the tails are presents 

;phenological parameters
pheno  = {$
  sos1    : {data:fltarr(ns,n_calendar_years), label: 'sos1', lun:0 } , $  ;GS1
  eos1    : {data:fltarr(ns,n_calendar_years), label: 'eos1' , lun:0 } , $    ;eos at 10%
  eos1e   : {data:fltarr(ns,n_calendar_years), label: 'eose1' , lun:0 } , $   ;early eos at 80%
  len1    : {data:fltarr(ns,n_calendar_years), label: 'len1' , lun:0 } , $
  len1e   : {data:fltarr(ns,n_calendar_years), label: 'lene1' , lun:0 } , $   ;early eos at 80%
  acc1    : {data:fltarr(ns,n_calendar_years), label: 'acc1' , lun:0 } , $    ;eos at 10%
  acc1e   : {data:fltarr(ns,n_calendar_years), label: 'acce1' , lun:0 } , $   ;eos at 80%
  acc1b   : {data:fltarr(ns,n_calendar_years), label: 'accb1' , lun:0 } , $   ;eos at 10% using baseline (total - baseline)
  acc1eb  : {data:fltarr(ns,n_calendar_years), label: 'acceb1' , lun:0 } , $  ;eos at 80% using baseline (total - baseline)
  maxv1   : {data:fltarr(ns,n_calendar_years), label: 'maxv1' , lun:0 } , $  ;max val
  maxt1   : {data:fltarr(ns,n_calendar_years), label: 'maxt1' , lun:0 } , $  ;time of max val
  flag1   : {data:fltarr(ns,n_calendar_years), label: 'flag1' , lun:0 } , $
  stat1   : {data:fltarr(ns,n_calendar_years), label: 'stat1' , lun:0 } , $
  rel1    : {data:fltarr(ns,n_calendar_years), label: 'rel1' , lun:0 } , $
  comp1   : {data:fltarr(ns,n_calendar_years), label: 'comp1' , lun:0 } , $
  p01     : {data:fltarr(ns,n_calendar_years), label: 'p01' , lun:0 } , $ ;NEW!! dht model parameters
  p11     : {data:fltarr(ns,n_calendar_years), label: 'p11' , lun:0 } , $ ;NEW!! dht model parameters
  p21     : {data:fltarr(ns,n_calendar_years), label: 'p21' , lun:0 } , $ ;NEW!! dht model parameters
  p31     : {data:fltarr(ns,n_calendar_years), label: 'p31' , lun:0 } , $ ;NEW!! dht model parameters
  p41     : {data:fltarr(ns,n_calendar_years), label: 'p41' , lun:0 } , $ ;NEW!! dht model parameters
  p51     : {data:fltarr(ns,n_calendar_years), label: 'p51' , lun:0 } , $ ;NEW!! dht model parameters
  p61     : {data:fltarr(ns,n_calendar_years), label: 'p61' , lun:0 } , $ ;NEW!! dht model parameters
  t01     : {data:fltarr(ns,n_calendar_years), label: 't01' , lun:0 } , $ ;NEW!! gsinistartDek
  t11     : {data:fltarr(ns,n_calendar_years), label: 't11' , lun:0 } , $ ;NEW!! gsinistop
  sos2    : {data:fltarr(ns,n_calendar_years), label: 'sos2' , lun:0 } , $  ;GS2************************************
  eos2    : {data:fltarr(ns,n_calendar_years), label: 'eos2' , lun:0 } , $
  eos2e   : {data:fltarr(ns,n_calendar_years), label: 'eose2' , lun:0 } , $  ;early eos at 80%
  len2    : {data:fltarr(ns,n_calendar_years), label: 'len2' , lun:0 } , $
  len2e   : {data:fltarr(ns,n_calendar_years), label: 'lene2' , lun:0 } , $  ;early eos at 80%
  acc2    : {data:fltarr(ns,n_calendar_years), label: 'acc2' , lun:0 } , $    ;eos at 20%
  acc2e   : {data:fltarr(ns,n_calendar_years), label: 'acce2' , lun:0 } , $   ;eos at 80%
  acc2b   : {data:fltarr(ns,n_calendar_years), label: 'accb2' , lun:0 } , $   ;eos at 20% using baseline (total - baseline)
  acc2eb  : {data:fltarr(ns,n_calendar_years), label: 'acceb2' , lun:0 } , $  ;eos at 80% using baseline (total - baseline)
  maxv2   : {data:fltarr(ns,n_calendar_years), label: 'maxv2' , lun:0 } , $  ;max val
  maxt2   : {data:fltarr(ns,n_calendar_years), label: 'maxt2' , lun:0 } , $  ;time of max val
  flag2   : {data:fltarr(ns,n_calendar_years), label: 'flag2' , lun:0 } , $
  stat2   : {data:fltarr(ns,n_calendar_years), label: 'stat2' , lun:0 } , $
  rel2    : {data:fltarr(ns,n_calendar_years), label: 'rel2' , lun:0 }  , $
  comp2   : {data:fltarr(ns,n_calendar_years), label: 'comp2' , lun:0 } , $
  p02     : {data:fltarr(ns,n_calendar_years), label: 'p02' , lun:0 } , $ ;NEW!! dht model parameters
  p12     : {data:fltarr(ns,n_calendar_years), label: 'p12' , lun:0 } , $ ;NEW!! dht model parameters
  p22     : {data:fltarr(ns,n_calendar_years), label: 'p22' , lun:0 } , $ ;NEW!! dht model parameters
  p32     : {data:fltarr(ns,n_calendar_years), label: 'p32' , lun:0 } , $ ;NEW!! dht model parameters
  p42     : {data:fltarr(ns,n_calendar_years), label: 'p42' , lun:0 } , $ ;NEW!! dht model parameters
  p52     : {data:fltarr(ns,n_calendar_years), label: 'p52' , lun:0 } , $ ;NEW!! dht model parameters
  p62     : {data:fltarr(ns,n_calendar_years), label: 'p62' , lun:0 } , $ ;NEW!! dht model parameters
  t02     : {data:fltarr(ns,n_calendar_years), label: 't02' , lun:0 } , $ ;NEW!! gsinistartDek
  t12     : {data:fltarr(ns,n_calendar_years), label: 't12' , lun:0 }} ;NEW!! gsinistop




RETURN, pheno
END