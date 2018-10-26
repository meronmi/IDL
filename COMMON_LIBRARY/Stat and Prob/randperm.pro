FUNCTION RandPerm, numberOfElements, SEED=seed
  x = Lindgen(numberOfElements)
  RETURN, x[Sort(Randomu(seed, numberOfElements))]
END