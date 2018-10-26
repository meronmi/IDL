;+
; NAME:
;   test_mh_gibbs
;     
; PURPOSE:
;   Provides an example showing how to use find_betas and mh_gibbs
;
; CALLING SEQUENCE:
;   test_mh_gibbs
;
; INPUTS:
;
; REQUIRES:
;   mh_gibbs, find_betas, log_like_chisq -- 
;     available at http://www.lpl.arizona.edu/~bjackson/idl_code/index.html
;
; SIDE EFFECTS:
;   Plots the Markov Chain to the screen and shows the correct value
;
; EXAMPLE:
;   test_mh_gibbs ;That's it.
;
; MODIFICATION HISTORY:
;   2013 Jun 10 -- Written by Brian Jackson (decaelus@gmail.com)
;   2014 Jan 12 -- Added complete comments
;
;-

pro test_mh_gibbs
  ;2013 Jun 10 -- An example MCMC analysis

  ;generate model data
  x = dindgen(1001)/1000. 
  ;model is 0th order polynomial
  c = randomn(seed, 1)
  y = poly(x, c)
  ;add noise
  SNR = 3.
  noise = replicate(max([mean(y)/SNR, 0.1]), n_elements(x))
  noisy_y = y + randomn(seed, n_elements(x))*noise

  ;initial guess
  p0 = c

  ;determine beta parameters
  ;find_betas, <model_func>, <likelihood_func>, <independent var.>, 
  ;  <data>, <uncertainties>, <initial guesses>, <beta params>
  find_betas, "poly", "log_like_chisq", x, noisy_y, noise, p0, betamu

  ;run mcmc
  num_links = 5000
  num_chains = 5
  ;mh_gibbs, <num links>, <num chains>, <model func> , <likelihood_func>, 
  ;  <independent var.>, <data>, <uncertainties>, <initial guesses>, 
  ;  <beta params>, <MCMC chain>
  mh_gibbs, num_links, num_chains, "poly", "log_like_chisq", x, noisy_y, noise, p0, betamu, chain

  ;Usually some initial part of the chain depends on the initial guess 
  ;  so you drop that part, called the "burn-in phase".
  ind = lindgen(0.8*n_elements(chain[0,0,*])) + 0.2*n_elements(chain[0,0,*])

  ;plot trace and right answer for parameter 0
  plot, ind, chain[0,0,ind], yr=[min(chain[0,*,*]), max(chain[0,*,*])]
  for i = 1, num_chains-1 do $
    oplot, ind, chain[0,i,ind], linestyle=i

  loadct, 13, ncolors=16
  device,decomposed=0
  ;And here's the right answer
  oplot, [0, num_links], [1., 1.]*mean(noisy_y), color=15, thick=5

end;pro
