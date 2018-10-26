;+
; NAME:
;   mh_gibbs
;     
; PURPOSE:
;   Markov-chain Monte-Carlo routine using the Metropolis-Hastings algorithm
;   and Gibbs sampler
;
; CALLING SEQUENCE:
;   mh_gibbs, num_links, num_chains, model_func, like_func, x, y, err, init_p, $
;     betamu, chain[, save_file=save_file, parinfo=parinfo, 
;     restore_file=restore_file, /verbose]
;
; INPUTS:
;   num_links -- number of links in each Markov chain; typically 1000s
;   num_chains -- number of simultaneous Markov chains; typically ~5
;   model_func -- the name of the model function to evaluate
;   like_func -- the name of the function to evaluate log(likelihood) for jumping
;   x/y/err -- in/dependent variables and associated uncertainties
;   init_p -- initial guesses for model parameters
;   betamu -- scaling params -- Ford [2005] ApJ 129, 1706 and find_betas.pro
;
; OUTPUTS:
;   chain -- MCMC chain, shaped as [# fit params, num_chains, num_links], so,
;     for example, chain[0,0,*] represents all values in the first chain for
;     the first fit parameter.
;
; KEYWORD PARAMETERS:
;   save_file -- name of save file to which to write chain
;   verbose -- If set, the acceptance fraction for each chain will be printed
;     every 100 links. If only one parameter is varied, acceptance fraction 
;     should be ~0.44. If more than one, it should be ~0.25.
;   parinfo -- structure containing information about parameters, exactly
;     like parinfo for mpfitfun 
;     (http://www.physics.wisc.edu/~craigm/idl/down/mpfitfun.pro), 
;     except that only the .limited and .limits fields are used
;   restore_file -- name of IDL save file that contains a previously-run chain
;
; RESTRICTIONS:
;   This routine isn't too clever, so be sure you know what you're doing.   
;
; EXAMPLE:
;   test_mh_gibb.pro provides a test case.
;
; MODIFICATION HISTORY:
;   2011 Nov 27 -- Written by Brian Jackson (decaelus@gmail.com).
;   2013 Apr 17 -- Modifed by BJ to allow for likelihood function
;   2013 Jun 3 -- Added parinfo keyword
;   2013 Jun 11 -- BJ changed the way in which initial values are chosen and
;     in which variables are saved
;
;-
pro mh_gibbs, num_links, num_chains, model_func, like_func, x, y, err, init_p, betamu, chain, parinfo=parinfo, save_file=save_file, restore_file=restore_file, verbose=verbose
    
  ;If parinfo NOT set, vary everything
  ind = indgen(n_elements(init_p))
  ;Otherwise, only vary the values you're supposed to
  if(keyword_set(parinfo)) then begin
    ind = where(parinfo[*].fixed eq 0)
  end;if

  ;2012 Dec 5 -- Bounds on frac -- Directly from Ford (2005)
  min_frac = 0.25
  max_frac = 0.55

  ;the pre-jump parameter values
  old_xmu = dblarr(num_chains, n_elements(init_p))

  ;An array that stores the parameter values
  ;keepers = [# fit params, num_chains, num_links]
  if(keyword_set(restore_file)) then begin 
    restore, restore_file
    num_last_chain = n_elements(keepers[0,0,*])
    for i = 0, num_chains-1 do begin 
      old_xmu[i,*] = init_p
      old_xmu[i,ind] = keepers[*,i,num_last_chain-1]
    end;for i
    keepers = [[[chain]], $
      [[dblarr(n_elements(ind), num_chains, num_links)]]]

  end else begin
    num_last_chain = 0
    keepers = dblarr(n_elements(ind), num_chains, num_links)

    for i = 0, num_chains-1 do begin 
      old_xmu[i,*] = init_p
      ;randomize the starting values
      mn = init_p - 10.*betamu
      mx = init_p + 10.*betamu

      ;If limits are given
      if(keyword_set(parinfo)) then begin
        for j = 0, n_elements(ind)-1 do begin
          if((parinfo[ind[j]].fixed ne 1) and $ 
             (parinfo[ind[j]].limited[0] eq 1)) then $
            mn[ind[j]] = max([mn[ind[j]], parinfo[ind[j]].limits[0]])

          if((parinfo[ind[j]].fixed ne 1) and $ 
             (parinfo[ind[j]].limited[1] eq 1)) then $
            mx[ind[j]] = min([mx[ind[j]], parinfo[ind[j]].limits[1]])
        end;for j
      end;if

      if(ind[0] ne -1) then $
        old_xmu[i,ind] = (mx[ind] - mn[ind])*randomu(seed,n_elements(ind)) $
                        + mn[ind]


    end;for i

  end;else

  ;number of successful jumps in each chain
  num_jumps = lonarr(num_chains)

  ;time variables
  a = systime(1)
  b = systime(1)

  j = long(num_last_chain + 1)
  j_mod_100 = 1
  while(j le num_last_chain + num_links) do begin

    if((j mod 100) eq 0) then begin 
      if(keyword_set(verbose)) then begin
        print, "---"
        print, "j, time, old_like, frac: ", j, systime(1)-b, old_like, frac
        print, "old_xmu: ", transpose(old_xmu[0,*])
      end;if
      b = systime(1)

      ;2012 Dec 5 -- If frac is too small/larger, decrease/increase beta
      if((min(frac) lt min_frac) or (max(frac) gt max_frac)) then begin
        if(keyword_set(verbose)) then begin 
          print, "frac out of bounds"
          print, frac
          print
          print, old_xmu
        end;if

        if((min(frac, mnd) lt min_frac) and ~(max(frac) gt max_frac)) then begin
          betamu /= 2.
          if(keyword_set(verbose)) then print, "rescaled betamu: ", betamu[ind]
        end;if
        if((max(frac,mnd) gt max_frac) and ~(min(frac) lt min_frac)) then begin 
          betamu *= 2.
          if(keyword_set(verbose)) then print, "rescaled betamu: ", betamu[ind]
        end;if

;       keepers[*,*,j:j+99L] = 0.
        j -= 99L
        frac = replicate(0., n_elements(num_chains))
        num_jumps = lonarr(num_chains)
        j_mod_100 = 1

      end;if
    end;fi

    ;Choose one or two parameters to update
    ;This ASSUMES at least one of the parameters can be varied!
    one_or_two = min([fix(randomu(seed)*2)+1, n_elements(ind)])
    temp_which_p = replicate(0, n_elements(init_p))
    which_ones = fix(randomu(seed, one_or_two)*n_elements(ind))

    for i = 0, num_chains-1 do begin
      c=systime(1)

      ;Evaluate the model for the pre-jump parameters
      y_old = call_function(model_func, x, old_xmu[i,*])

      ;Calculate residuals
      old_resid = (y_old - y)
      ;Evaluate log(likelihood)
      old_like = call_function(like_func, old_resid, err, old_xmu[i,*])

      ;And now select some new parameter values
      trial_xmu = old_xmu[i,*]
      rand = randomn(seed, /double)
      ;If the parameters are only allowed to be positive
      temp = (rand*betamu + old_xmu[i,*])
      ;Assign the new parameters
      trial_xmu[ind[which_ones]] = temp[ind[which_ones]]

      ;Calculate the model for the trial parameters
      trial_y = call_function(model_func, x, trial_xmu)

      ;Residuals for trial params
      trial_resid = (trial_y - y)
      ;And log of the trial likelihood
      trial_like = call_function(like_func, trial_resid, err, trial_xmu)

      if(keyword_set(parinfo)) then begin

        ;Check lower limits
        oind = where(parinfo[*].limited[0] eq 1)
        if(oind[0] ne -1) then begin
          pind = where(trial_xmu[oind] lt parinfo[oind].limits[0])
          if(pind[0] ne -1) then begin
            trial_like = -!values.d_infinity
          end;if
 
        end;if

        ;Check upper limits
        oind = where(parinfo[*].limited[1] eq 1)
        if(oind[0] ne -1) then begin
          pind = where(trial_xmu[oind] gt parinfo[oind].limits[1])
          if(pind[0] ne -1) then begin
            trial_like = -!values.d_infinity
          end;if
        end;if

      end;if

      ;This is the likelihood that the new parameters are a better fit to the
      ;  likelihood that the pre-jump parameters are -- Eqn (9) from Ford (2005)
      rat = exp(0.5*(trial_like - old_like))

      u = randomu(seed)
      ;Eqn (13) from Ford (2005)
      alpha = min([rat, 1.d0])

      ;Conditionally assign the pre-jump parameters to the chain
      keepers[*,i,j-1] = old_xmu[i,ind]

      ;But if u <= alpha,then assign the trial parameters instead
      if(u le alpha) then begin 
        keepers[*,i,j-1] = trial_xmu[ind]
        old_xmu[i,*] = trial_xmu
        num_jumps[i]++
      end;if
    end;for i

    ;calculate the new acceptance fraction for all the chains
    frac = double(num_jumps)/double(j_mod_100)
    j++
    j_mod_100++

  end;while(j le num_links)
  if(keyword_set(verbose)) then $
    print, 'time, frac: ', systime(1)-a, num_jumps/double(num_links)

  chain = keepers
  if(keyword_set(save_file)) then save, /variables, filename=save_file
end;
