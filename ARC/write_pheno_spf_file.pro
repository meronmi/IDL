FUNCTION write_pheno_spf_file, out_dir, sos_thresh, eos_thresh, sen_thresh
;this writes a 4-touple of SPF files, give the passed parameters
;sos_thresh, eos_thresh, sen_thresh are given as fraction (e.g. 0.25)
;max_thresh, rng_thresh are NDVI values

fn_base_name = 'S_' + STRTRIM(ROUND(sos_thresh * 100),2) + 'E_' + STRTRIM(ROUND(eos_thresh * 100),2)
fns = [fn_base_name + '_for_SOS_EOS', fn_base_name + '_for_SEN', fn_base_name + '_for_MAX', fn_base_name + '_for_RNG'] + '.SPF'

;set parameters by file
FEN0Max = STRING([0.120, 0.120, 0.180,  0.120])
FEN0Min = STRING(0.80)
FEN0Rng = STRING([0.02,0.02,0.02,0.048])
FENsos = STRING([sos_thresh, sos_thresh, sos_thresh, sos_thresh], FORMAT='(F6.2)')   
FENeos =  STRING([eos_thresh, sen_thresh, eos_thresh, eos_thresh], FORMAT='(F6.2)')
FOR i = 0, 3 DO BEGIN
  OPENW, lun, out_dir + '\' + fns[i], /GET_LUN
  PRINTF, lun, '==============================================================================='
  PRINTF, lun, 'SPF file generated for ARC sensitivity analysis'
  PRINTF, lun, '==============================================================================='
  PRINTF, lun, '                   Preliminary elimination of pixels without seasonality (Ymax and Ymin are computed over the 36 dekads [37-72] of the central year)'
  PRINTF, lun, 'FEN0Max  = ' +STRING(FEN0Max[i]) + '   ' + 'If Ymax        < FEN0Max => No seasonality (deserts)'
  PRINTF, lun, 'FEN0Min  = ' + STRING(FEN0Min) + '   ' + '***************[0.75]If Ymin        > FEN0Min => No seasonality (evergreen)'
  PRINTF, lun, 'FEN0Rng  =' + STRING(FEN0Rng[i]) + '   ' + '***************[0.075] If (Ymax-Ymin) < FEN0Rng => No seasonality (variability too low)'
  PRINTF, lun, '-------------------------------------------------------------------------------'
  PRINTF, lun, '                   The Yi-profiles of the remaining pixels are smoothed with a weighted, running mean filter (RMF) => Smoothed curve Ys.'
  PRINTF, lun, 'FENrmf   = 2       Half length of the RMF (2=best, 0=skip filtering)'
  PRINTF, lun, 'FENw     = 4       All dekadal values get default weight W=1, but the extreme values (MIN/MAX) get weight FENw (best FENw=4)'
  PRINTF, lun, 'NB: Tsos/Tmos and Teos (for both seasons) are derived from this smoothed Ys-curve. But the corresponding Y-values are extracted from the original Yi-values'
  PRINTF, lun, '-------------------------------------------------------------------------------'
  PRINTF, lun, '                   Each smoothed Ys-profile is further decomposed in "segments", i.e. pairs of MIN-MAX or MAX-MIN.'
  PRINTF, lun, '                   Each cycle or season comprises two subsequent segments: the rising MIN1-MAX plus the descending MAX-MIN2'
  PRINTF, lun, '                   But even after the RMF-smoothing, pixel profiles can still show many irrelevant cycles.'
  PRINTF, lun, '                   Via five subsequent tests (all executed iteratively), the irrelevant cycles are searched and eliminated (always in subsequent pairs MIN-MAX or MAX-MIN),'
  PRINTF, lun, 'FENdY    = 0.015   ***************[0.025] Test1: Eliminate segment if the absolute value of its Y-difference             < FENdY    '
  PRINTF, lun, 'FENdT    = 10             AND TOGETHER      if the interval between both extremes (Tmin <-> Tmax) < FENdT (in dekads)'
  PRINTF, lun, 'FENmax   = 0.000   Test2: Eliminate all maxima with Ymax < FENmax. Also eliminate the highest neighbouring minimum. NB: Best skip this test via'
  PRINTF, lun, '                   Test3: The overall Ymin and Ymax are searched (over 3 years) and a threshold is defined: Yt = Ymin + FENratio * (Ymax-Ymin)'
  PRINTF, lun, 'FENratio = 0.250          Eliminate all maxima with Y < Yt. Also eliminate the highest neighbouring minimum. Set FENratio=0 to skip this test.'
  PRINTF, lun, 'FENmaxDt = 6       Test4: If interval between 2 maxima   < FENmaxDt (in dekads), remove intermediate minimum and lowest maximum. '
  PRINTF, lun, 'FENextDt = 3       Test5: If interval between 2 extremes < FENextDt (in dekads), remove entire segment.'
  PRINTF, lun, '-------------------------------------------------------------------------------' 
  PRINTF, lun, '                   At this stage and for most pixels, only one or two maxima (thus seasons) remain within the central year.'
  PRINTF, lun, '                   But exceptionally there can be more: three or even four.'
  PRINTF, lun, '                   In that case, the program searches and removes the least relevant maxima (+ highest neighbouring minimum) until at the end only 2 seasons remain.'
  PRINTF, lun, '                   For each season, the Area is computed below the profile between MIN1-MAX-MIN2. At each iteration, the season with smallest area is eliminated. '
  PRINTF, lun, '                   NB: No keywords are needed for this step.'
  PRINTF, lun, '-------------------------------------------------------------------------------'
  PRINTF, lun, '                    Two ratios with values between 0.0 and 1.0 (Ymin1/2 are the minima before/after the concerned maximum Ymax)'
  PRINTF, lun, 'FENsos   =' + STRING(FENsos[i]) + '   ' + 'Tsos is defined as the dekad between Tmin1 and Tmax  where the profile cuts the value Y = Ymin1 + FENsos * (Ymax - Ymin1). Tsos seached LEFTward  from Tmax to Tmin1.'
  PRINTF, lun, 'FENeos   =' + STRING(FENeos[i]) + '   ' + 'Teos is defined as the dekad between Tmax  and Tmin2 where the profile cuts the value Y = Ymin2 + FENeos * (Ymax - Ymin2). Teos seached RIGHTward from Tmax to Tmin2.'
  PRINTF, lun, '-------------------------------------------------------------------------------'
  PRINTF, lun, 'FENlDEK  = 1       Express season Lengths (in OUT-IMGs L1, L2, LT0): 0=as % of the year, 1=in dekads.'
  PRINTF, lun, '-------------------------------------------------------------------------------'
  PRINTF, lun, 'Classes k in the optional OUT-classification (OUT-IMG KK0) are defined as: k = (100 * Ns) + (10 * Kmu) + Krg:'
  PRINTF, lun, '- Ns  [0-2] = Seasons in year.'
  PRINTF, lun, '- Kmu [0-4] = INTEGER[(Ya0 - MUlo)/ MUdelt], with Ya0 = Annual mean Y-value       . If Kmu<0 or Kmu>4, it is reset to range [0-4].'
  PRINTF, lun, '- Krg [0-4] = INTEGER[(Yr0 - RGlo)/ RGdelt], with Yr0 = Annual Y-range (Ymax-Ymin). If Krg<0 or Krg>4, it is reset to range [0-4].'
  PRINTF, lun, 'Hence, the range of k-classes is limited to 0-244 (byte). The maximum of 244 occurs if Ns=2, Kmu=4 and Krg=4.'
  PRINTF, lun, ''
  PRINTF, lun, 'KeyWord FENkMU contains 2 comma-separated values: MUlo, MUdelt'
  PRINTF, lun, '        FENkRG contains 2 comma-separated values: RGlo, RGdelt'
  PRINTF, lun, ''
  PRINTF, lun, 'FENkMU   = 0.0, 0.20'
  PRINTF, lun, 'FENkRG   = 0.0, 0.15'
  FREE_LUN, lun
ENDFOR
RETURN, 0
END 