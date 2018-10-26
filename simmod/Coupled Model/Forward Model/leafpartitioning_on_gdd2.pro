FUNCTION LeafPartitioning_on_GDD2, GDD, a, b
  ; New function for partitioning that explicitly use two relevant parameters:
  ; - a: (0, +inf) the length of the period (expressed in GDD) for which assimilates are partioned into leaves
  ; - b: (-inf, +inf) a parameter controlling the curvature of the partitioning, special cases:
  ;      b<0: convex function(U shape)
  ;      b=0: linear decreasong function
  ;      b>0: concave function (standard behaviour, in this case it resample the exponential of GRAMI) 
  
  
  IF (GDD LE a) THEN P1 = MAX([1.0 - (GDD/(GDD+EXP(b)*(a-GDD))),0.0]) ELSE P1 = 0 
  
  RETURN, P1
END