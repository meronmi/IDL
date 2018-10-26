;+
; NAME:
;   find_betas
;     
; PURPOSE:
;   Estimates beta_mu parameters for MCMC analysis by determining acceptance
;     fraction for trial betas spanning many orders of magnitude; 
;     See Ford (2005) ApJ 129, 1706 for more
;
; CALLING SEQUENCE:
;   find_betas, model_func, like_func, x, y, err, init_p, betamu[, $
;     beta_pool=beta_pool, parinfo=parinfo, /verbose]
;
; INPUTS:
;   model_func -- name of model function
;   like_func -- function that returns log(likelihood) for jump
;   x/y/err -- in/dependent variables and uncertainties
;   init_p -- initial guesses for model parameters
;
; OUTPUTS:
;   betamu -- the sought-for beta_mu's
;
; KEYWORD PARAMETERS:
;   beta_pool -- beta's to try
;   verbose -- prints out acceptance fraction for different trial betas; 
;     If it's your first time running this routine for a problem, it's a good
;     idea to activate this flag.
;   num_tries -- number of links in chain to sample beta_mu (defaults to 100)
;
; RESTRICTIONS:
;   This routine tries beta_mu's scaled by the initial guesses 
;     from 1e-6 to 1.7 times the initial guesses. If the right betamu isn't in 
;     that range, this routine won't find it.
;
; EXAMPLE:
;   test_mh_gibbs provides an example
;
; MODIFICATION HISTORY:
;   2011 Nov 27 -- Written by Brian Jackson (decaelus@gmail.com)
;   2013 Apr 17 -- Modified by BJ to allow for like_func
;   2013 Jun 4 -- Added print time
;
;-
pro find_betas, model_func, like_func, x, y, err, init_p, betamu, $
                parinfo=parinfo, $
                beta_pool=beta_pool, num_tries=num_tries, verbose=verbose

  if(~keyword_set(num_tries)) then num_tries = 100

  ;If parinfo NOT set, vary everything
  ind = indgen(n_elements(init_p))
  ;Otherwise, only vary the values you're supposed to
  if(keyword_set(parinfo)) then begin
    ind = where(parinfo[*].fixed eq 0)
  end

  if(n_elements(ind) eq 1) then begin
    desired_frac = 0.44
  end else begin
    desired_frac = 0.25
  end;else

  init_xmu = init_p
  if(keyword_set(parinfo)) then begin
    oind = where((parinfo[*].fixed eq 0) and (init_xmu eq 0.))
  end else begin
    oind = where(init_xmu eq 0.)
  end;else
  if(oind[0] ne -1) then init_xmu[oind] = 1d-6

; if(~keyword_set(beta_pool)) then beta_pool = 10.^(dindgen(2500)/40.-6.)
  if(~keyword_set(beta_pool)) then beta_pool = 10.^(dindgen(250)/4.-6.)

  ;I will try betas that are some fraction of the given initial parameter values

  betamu = replicate(0.d0, n_elements(init_p))
  for i = 0, n_elements(ind)-1 do begin
    print, i
    xmu_old = init_xmu[ind[i]]

    ;Start out with a very large value
    mn = !values.d_infinity
    k = 0
    actual_frac = 1.0
;   actual_frac = 0.0

    while((k lt n_elements(beta_pool)) and $
          (actual_frac ge desired_frac)) do begin
;         (actual_frac le desired_frac)) do begin
      trial_beta = abs(beta_pool[k])
      if(abs(init_xmu[ind[i]]) gt min(beta_pool)) then $
        trial_beta = abs(beta_pool[k]*init_xmu[ind[i]])

      num_jumps = 0
      xmu_old = init_p[ind[i]]

      sys = systime(1)
      for j = 1, num_tries do begin
        ;print, i, j
        p = init_p
        p[ind[i]] = xmu_old
  
        y_old = call_function(model_func, x, p)
        old_resid = (y_old - y)
        old_like = call_function(like_func, old_resid, err, p) 
  
        rand = randomu(seed, /normal)
        trial_xmu = (rand*trial_beta+xmu_old)
  
        p[ind[i]] = trial_xmu
        ;2015 Michele, set this contro before model evaluation to avoid it gives error
        ;2013 Jun 4 -- This line keeps parameters in limits.
        outParinfoLimits = 0
        if(keyword_set(parinfo)) then begin
          ;Check lower limits
          oind = where(parinfo[*].limited[0] eq 1)
          if(oind[0] ne -1) then begin
            pind = where(p[oind] lt parinfo[oind].limits[0])
            if(pind[0] ne -1) then begin
              outParinfoLimits = 1
              trial_like = -!values.d_infinity
            end;if
          end;if
          ;Check upper limits
          oind = where(parinfo[*].limited[1] eq 1)
          if(oind[0] ne -1) then begin
            pind = where(p[oind] gt parinfo[oind].limits[1])
            if(pind[0] ne -1) then begin
              outParinfoLimits = 1
              trial_like = -!values.d_infinity
            end;if
          end;if
        end;if
        
        IF (outParinfoLimits EQ 0) THEN BEGIN
          trial_y = call_function(model_func, x, p)
          trial_resid = (trial_y - y)
          trial_like = call_function(like_func, trial_resid, err, p)
        ENDIF
;        ;2013 Jun 4 -- This line keeps parameters in limits.
;        if(keyword_set(parinfo)) then begin
;          ;Check lower limits
;          oind = where(parinfo[*].limited[0] eq 1)
;          if(oind[0] ne -1) then begin
;            pind = where(p[oind] lt parinfo[oind].limits[0])
;            if(pind[0] ne -1) then begin 
;              trial_like = -!values.d_infinity
;            end;if
;          end;if
;          ;Check upper limits
;          oind = where(parinfo[*].limited[1] eq 1)
;          if(oind[0] ne -1) then begin
;            pind = where(p[oind] gt parinfo[oind].limits[1])
;            if(pind[0] ne -1) then begin 
;              trial_like = -!values.d_infinity
;            end;if
;          end;if
;        end;if
  
        rat = exp(trial_like - old_like)
        u = randomu(seed)
        alpha = min([rat, 1.d0])
  
        if(u le alpha) then begin
          num_jumps++
          xmu_old = trial_xmu
        end;if
      end;for j
      actual_frac = double(num_jumps)/double(num_tries)
      if(keyword_set(verbose)) then $
        print, systime(1) - sys, init_p[ind[i]], trial_beta, actual_frac, $
          num_jumps
        sys = systime(1)
  
      if(abs(actual_frac-desired_frac) lt mn) then begin 
        betamu[ind[i]] = trial_beta
        mn = abs(actual_frac-desired_frac)
      end;if
    k++
    end;while
    if(keyword_set(verbose)) then begin 
      print, betamu[ind[i]] & print, "---"
    end;if

  end;for i

end;pro 
