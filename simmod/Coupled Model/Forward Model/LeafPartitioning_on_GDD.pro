FUNCTION LeafPartitioning_on_GDD, GDD, a, b
; computes the partitioning into Above Ground Organs using the equation of 
; Maas, S.J., 1993a. Parameterized model of gramineous crop growth. I. Leaf area and dry mass simulation. Agron. J. 85, 348–353

RETURN, MAX([1.0-a*EXP(b*GDD),0.0])
END