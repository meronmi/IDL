FUNCTION JD2ComIndex, JD, t
; JD is an array of Julian days
; t is length of the mpositing period, in days 

;detremine the number of compositing periods
n = CEIL(365.0 / FLOAT(t))
tDOY = JD2DOY(JD)

RETURN, CEIL(tDOY / FLOAT(t))
END