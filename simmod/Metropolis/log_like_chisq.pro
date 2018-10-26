function log_like_chisq, residuals, err, params
  ;Returns -0.5*chi^2
  return, (-total(residuals^2./err^2., /double))
end;function