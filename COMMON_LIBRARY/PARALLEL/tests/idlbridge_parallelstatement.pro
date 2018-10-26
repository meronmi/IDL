PRO test1, x
x=x*2
END

FUNCTION idlbridge_parallelStatement, x
;this function MUST be positioned in the IDL path (Window-Preferences IDL-Paths)

;sample statement making something in 10 seconds
y = x^2
;test1, x
;gh = PLOT(x,y)
;y = y[-1]
RETURN, y
END