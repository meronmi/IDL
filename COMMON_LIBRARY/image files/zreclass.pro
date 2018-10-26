FUNCTION zreclass, data
  dataout = data*!VALUES.F_NAN
  indFin = WHERE(FINITE(data))

  ind = WHERE((data[indFin] LE -2), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 0

  ind = WHERE((data[indFin] LE -1.5) AND (data[indFin] GT -2), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 1

  ind = WHERE((data[indFin] LE -1) AND (data[indFin] GT -1.5), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 2

  ind = WHERE((data[indFin] LT 1) AND (data[indFin] GT -1), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 3

  ind = WHERE((data[indFin] LT 1.5) AND (data[indFin] GE 1), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 4

  ind = WHERE((data[indFin] LT 2) AND (data[indFin] GE 1.5), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 5

  ind = WHERE((data[indFin] GE 2), count)
  IF (count GT 0) THEN dataout[indFin[ind]] = 6

  return, dataout
END