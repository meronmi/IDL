FUNCTION paired_test, xy, xz, yz, n1, n2
;IDL IMPLEMENTATION OF http://personality-project.org/r/html/paired.r.html
;two-tailed version implemented, errors corrected
; Citation Steiger, J.H. (1980), Tests for Comparing Elements of a Correlation Matrix. Psychological Bulletin, 87(2), 245-251

n = n1

diff = xy - xz
determin = ABS(1.0 - xy * xy - xz * xz - yz * yz + 2.0d * xy * xz * yz)
av = (xy + xz)/2.0d
cube = (1.0d - yz) * (1.0d - yz) * (1.0d - yz)
t2 = diff * sqrt((n - 1.0d) * (1.0d + yz)/(((2.0d * (n - 1.0d)/(n - 3.0d)) * determin + av * av * cube)))
;p = 1.0-T_PDF(ABS(t2), n-3)
p = 1.0-T_PDF(t2, n-3)
P = 2.0*p

;p <- pt(abs(t2), n - 2, lower.tail = FALSE)
;if (twotailed) 
;    p <- 2 * p
;value <- list(test = "test of difference between two correlated  correlations", 
;    t = t2, p = p, Call = cl)

RETURN, p
END 