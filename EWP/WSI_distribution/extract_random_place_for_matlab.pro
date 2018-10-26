FUNCTION extract_random_place_for_matlab, ns, kernel_size, suffix
  ;extract ns samples making sure that there is a kernel size around
  ;suffix can be 'C01' or 'C02'
 
  ;random extract (1)or use last (0)
  do_rnd_extract = 1
  no_data_out = 252
  
  dir = 'X:\tmp\STACKS\WSI_by_prog'
  dir_out = dir + '\images20samples' 
  FILE_MKDIR, dir_out
  
  prog = [0, 25, 50, 75, 100]
  seasNames = ['mono','bi1','bi2']
  
  fn = dir + '\' + seasNames[0] + '_wsi_at_prog_' + STRTRIM(prog[0],2)+ '_' + suffix+'.hdr'
  samples = FIX(read_info('samples', fn))
  lines = FIX(read_info('lines', fn))
  bands = FIX(read_info('bands', fn))
   
  FOR seas = 0, 2 DO BEGIN
    fn = dir + '\' + seasNames[seas] + '_wsi_at_prog_' + STRTRIM(prog,2) + '_' + suffix
    ks = kernel_size
    IF (ks MOD 2) EQ 0 THEN STOP ;the kernel must be odd
    hks = (ks-1)/2  ;this is half a kernel
 
    mmap = BYTARR(samples, lines)*0+200
    ;get ind of valid pixels
    tmp = BYTARR(samples, lines, bands)
    OPENR, lun, fn[0], /GET_LUN
    READU, lun, tmp
    FREE_LUN, lun
    tmp = tmp[*,*,0]
    ;ind = WHERE(FINITE(tmp), count)
    ind = WHERE(tmp NE no_data_out, count)
    ;ind = WHERE(tmp NE 252, count)
    
    IF (do_rnd_extract) THEN BEGIN
      ;extract n random
      randomNumbers = RANDOMU(seed, count)
      sortedRandomNumbers = SORT(randomNumbers)
      rndind = sortedRandomNumbers[0:ns-1] 
      rndind = ind[rndind]
      ;make sure that all of them have valid kernel pixels, if one has no, make a new random slection
      selected = 0
      i = 0
      WHILE (selected NE ns) DO BEGIN 
        res = ARRAY_INDICES(mmap,rndind[i])
        ;if there is a least one missing, redraw 
        ;check also that adding the kernel does not go outside the image
        out = 0
        IF (res[0]-20 LT 0) THEN out = 1
        IF (res[0]+20 GT samples-1) THEN out = 1
        IF (res[1]-20 LT 0) THEN out = 1
        IF (res[1]+20 GT lines-1) THEN out = 1
        IF (TOTAL(~FINITE(tmp[res[0]-hks:res[0]+hks,res[1]-hks:res[1]+hks])) OR (out EQ 1)) THEN BEGIN
          ; rndind[i] has to be changed
          randomNumbers = RANDOMU(seed, count)
          sortedRandomNumbers = SORT(randomNumbers)
          sorted = sortedRandomNumbers[0]
          rndind[i] = ind[sorted]
        ENDIF ELSE BEGIN
          selected = selected + 1
          i = i + 1
        ENDELSE
      ENDWHILE
      ;save the indexes for re-use
      SAVE, rndind, FILENAME = dir_out + '\'+seasNames[seas]+'_'+suffix+'_ind_of_last_run.sav'
    ENDIF ELSE BEGIN
      ;load the last one
      rndind = 0
      RESTORE, dir_out + '\'+seasNames[seas]+'_'+suffix+'_ind_of_last_run.sav'
    ENDELSE
    ;save a map
    mmap[ind]=130
    res_all = LONARR(2,N_ELEMENTS(rndind))
    FOR i = 0, N_ELEMENTS(rndind)-1 DO BEGIN
      res = ARRAY_INDICES(mmap,rndind[i])
      res_all[*,i] = res     
      mmap[res[0]-20:res[0]+20,res[1]-20:res[1]+20]=0
    ENDFOR
    ;here print (or better save a file) for matlab processing
    PRINT, res_all
    WRITE_CSV, dir_out +  '\'+seasNames[seas]+'_'+suffix+'_rnd_positions.csv', res_all
;    OPENW, luntxt, dir_out +  '\'+seasNames[seas]+'_rnd_positions.txt', /GET_LUN
;    PRINTF, luntxt, 'c l'
;    PRINTF, luntxt, res_all
;    FREE_LUN, luntxt
    fn_map = dir_out +  '\'+seasNames[seas]+'_'+suffix+'_random_selection_position'
    im = IMAGE(mmap, /ORDER, MIN_VALUE=0, MAX_VALUE=255)
    ;add number
    FOR i = 0, N_ELEMENTS(rndind)-1 DO BEGIN
      res = ARRAY_INDICES(mmap,rndind[i])
      ;landingsite = ARROW([100, 100],[res[0],res[1]], TARGET=im, /DATA, ARROW_STYLE=1, COLOR='blue', THICK=2)
      text = TEXT(res[0],lines-res[1], STRTRIM(i+1,2), 'red', TARGET=im, /DATA, FONT_SIZE=12)
    ENDFOR
    im.save, fn_map + '.png'
  ENDFOR
  RETURN, 0
END