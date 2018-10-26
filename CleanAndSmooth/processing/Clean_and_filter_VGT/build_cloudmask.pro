Function flag_clouds2, data, blu, pc_blu, search_lim, fract_threshold
;  Purpose:
;     To remove invalid cloudy data from a profile exlpoiting the data profile and the VGT blu band.
;     A window of serach_lim elements to the left and right of the central elements is analysed.  
;     Spikes are first detected as elements having, within search_lim elements on the left and right,
;     a value bigger than value_of_central_elements+variation_threshold.
;     Spikes are then flagged if one or both the following conditions are met
;     1. the blu[i] > blu_threshold (it is suspected to be a cloud)
;     2. the threshold difference is exceed with both closest elements (previous and successive elements, having
;     removed the NaN
;     The process is iterative, removes and serach again until no more data are to be removed or
;     max_iter is reached (in that case the function abrubtly stops.
;     blu_treshold and data_trashold are computed as follows:
;     data_trashold=abs(fract_threshold)*(pc95-pc5), where pcX is the X percentile of data distributation
;     blu_threshold is the value of specified by pc_blu
;  Usage:
;    res=flag_clouds2(data, blu, pc_blu, search_lim, fract_threshold)

;  Input parameters: 
;     data: the array to cloud screen (time series of fapar for example)
;     blu: the array of temporal profile of the blu band
;     pc_blu: the percentile of blu over which spikes are allowed to be flagged (e.g. 90 percentile)
;     search_lim: length of the half wing window to be serached for spikes (e.g. 5 -> window of 5+1+5)
;     fract_threshold: the max fract decrease with respect to pc95-pc5 in array (e.g. -0.1=-0.1*(pc95-pc5)),
;                     greater values are checked for cloud. Pc5 and 95 are 5% and 95% percentiles 

;  Remarks:
;     the function does not operate on the first and last serach_lim elements of data array
;  Return values:
;     structure ret - > ret.cloud is the array of cloud mask (1 if cloudy)
;                       ret.data is the cleaned data (NaN where clody)
;  Example:
;     res=flag_clouds2(data, blu, 90, 5, 0.2)
;  History:
;     Version 2.0: created by MM after some trials 23/02/2011




doplot=0  ;0 does not plot, 1 makes the plots (break at the for loop to see various steps of the algoritm)
max_iter=100  ;maximum number of iteration on the same array

;structure for containg returned values, cloud, cleaned fapar
ret = { cloud: indgen(N_ELEMENTS(data))*0, data:fltarr(N_ELEMENTS(data))} 
ret.data=data
;make a back up copy of data for plotting
if doplot eq 1 then begin
  budata=data
  x=indgen(N_ELEMENTS(data))
  avg_blu=mean(blu, /NAN)
  sd_blu=stddev(blu, /NAN)
  window, /free, xsize=1200, ysize=250
end
;mask as cloud those already NaN
;ind_finite_orig = where(finite(ret.data) eq 1, count_finite_orig)
ind_nan_orig    = where(finite(ret.data) ne 1, count_nan_orig)
if count_nan_orig ne 0 then begin
  ret.cloud[ind_nan_orig]=1
  ret.data[ind_nan_orig]=!VALUES.F_NAN
endif
;if they are all NaN, exit the function with cloud all 1
if count_nan_orig eq N_ELEMENTS(ret.data) then return, ret

;COMPUTATION OF THRESHOLDS 
;compute blu treshold basd on the blu percentile (pc_blu=
blu_threshold=blu[ (SORT(blu))[pc_blu * N_ELEMENTS(blu) / 100] ]
;compute actual trashold on fapar variation using percentiles and as a function
;of fract_threshold
pc5=data[ (SORT(data))[5 * N_ELEMENTS(data) / 100] ] 
pc95=data[ (SORT(data))[95 * N_ELEMENTS(data) / 100] ]
;print, pc5, pc95
data_threshold=abs(fract_threshold)*(pc95-pc5)

;START THE ITERATIVE ANALYSIS
removing = 1 
c=0
while removing eq 1 do begin
  c=c+1
  if c gt max_iter then begin
    ret.data[0]=-999
    return, ret
  end
  removing=0
  ;move the window centerd in [i] along the array
  ;cycle on elements ignoring first and last search_lim
  for i = search_lim+1, N_ELEMENTS(ret.data)-1-search_lim-1 do begin
    ;extract an array subset for the left and right parts of the window
    sub_left=ret.data[i-1-search_lim:i-1]
    sub_right=ret.data[i+1:i+1+search_lim]
    ;compute the difference with elements [i] for le left and right part of the window
    delta=sub_left-ret.data[i]  ;-> pos when decreasing
    ind_pos=where(finite(delta) eq 1, count_pos)
    ;keep only finite values, if there are none set the diff to 0
    if count_pos ne 0 then delta_left=delta[ind_pos] else delta_left=0.0
    delta=sub_right-ret.data[i] ; -> pos when increasing
    ind_pos=where(finite(delta) eq 1, count_pos)
    if count_pos ne 0 then delta_right=delta[ind_pos] else delta_right=0.0
    ;check if the variation at both sides exceed the threshold
    ;if it is in one side only it may be just an increase/decrease, do not remove
    if (max(delta_left) gt data_threshold) AND (max(delta_right) gt data_threshold) then begin 
      ;it's a local minima (in the sense that at least one element bigger that element[i]+threshold
      ;was found both at the left and the right of element[i]
      if doplot eq 1 then begin
        plot, x, ret.data, title='Actual data_threshold='+strtrim(data_threshold,2) ;color = red + 256*green + (256^2)*blue
        oplot, x, ret.data, psym=1
        oplot, x, blu, color=100000
        oplot, x, blu*0.0+blu_threshold, color=0L + 256L*0 + (256L^2)*255
        oplot, [x[i]], [ret.data[i]], psym=6, color=3000
      endif
            
      ;compute the delat with the closest finite elements
      delta_wp=delta_left[N_ELEMENTS(delta_left)-1]
      delta_ws=delta_right[0]
      ;now remove it if one (or both) of the following two conditions are met
      ;1. the blu[i] > blu_threshold (it is suspected to be a cloud)
      ;2. the threshold difference is exceed with both closest elements
      if (blu[i] gt blu_threshold) OR ((delta_wp gt data_threshold) AND (delta_ws gt data_threshold)) then begin
        if doplot eq 1 then oplot, [x[i]], [ret.data[i]], psym=1, color=3000
        ret.cloud[i]=1
        ret.data[i]=!VALUES.F_NAN
        removing=1
      endif 
    endif
  endfor
endwhile

;final plots if required 
if doplot eq 1 then begin
  plot, x, budata, title='Actual data_threshold='+strtrim(data_threshold,2)
  oplot, x, budata, psym=1
  oplot, x, ret.data, color=3000
  oplot, x, ret.data, psym=1, color=3000
  oplot, x, blu, color=100000
  oplot, x, blu*0.0+blu_threshold, color=0L + 256L*0 + (256L^2)*255
endif
return, ret
End

;
;
;debug pixels
;restore, 'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c920r860_very_bad'
;restore,'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c655r745_bad'
;restore,'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c584r527'
;restore,'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c770r370'
;restore,'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c337r393_low_fapar'
;restore,'D:\Users\meronmi\Documents\IDL\WS G2\Niger\data\debug\c586r589_high_fapar'


Function build_cloudmask, data_file_in, blu_file_in, file_data_out, file_mask_out, ns, nl, nb, blu_pc, search_lim, fract_threshold
; Input files are opened by performing ASSOC to one line
; Input is scaled fapar 
; when a decrase of fAPAR gt data_threshold the observation is masked as cloudy
; the decrease is compute with the first "backward" obs available
; that is if the previous obs is NaN, we ogo to the previos previos, an so on

OPENR, R1, data_file_in, /GET_LUN
line_ass_data = ASSOC(R1, FLTARR(ns,nb))
OPENR, R2, blu_file_in, /GET_LUN
line_ass_blu = ASSOC(R2, FLTARR(ns,nb))
; Create files for output
IF FILE_TEST(file_mask_out) eq 1 THEN FILE_DELETE, file_mask_out
OPENW, W1, file_mask_out, /GET_LUN, /APPEND
IF FILE_TEST(file_data_out) eq 1 THEN FILE_DELETE, file_data_out
OPENW, W2, file_data_out, /GET_LUN, /APPEND

data=FLTARR(ns, nb)
blu=FLTARR(ns, nb)
cmask=BYTARR(ns, nb)
dataout=FLTARR(ns, nb)

FOR line=0, nl-1, 1L DO BEGIN      ; loop over all lines
  ; Now create per line the stack of all dates
  data=float(line_ass_data[line])
  blu=float(line_ass_blu[line])
  ;print, line
  FOR sample=0, ns-1, 1L DO BEGIN      ; loop over samples
    res=flag_clouds2(reform(data[sample,*]), reform(blu[sample,*]), blu_pc, search_lim, fract_threshold)
    if res.data[0] eq -999 then print, 'Max iter exceeded for line sample', line, sample
    cmask[sample,*]=res.cloud
    dataout[sample,*]=res.data
  ENDFOR 
  WRITEU, W1, cmask
  WRITEU, W2, dataout

ENDFOR

; WRITE HEADER OF THE OUTPUT1
HEADER_OUT=file_mask_out+'.hdr'
OPENW, W3, HEADER_OUT, /GET_LUN
printf,W3,'ENVI'
printf,W3,'description = {Cloud mask}'
printf,W3,'samples ='+STRCOMPRESS(ns)
printf,W3,'lines   ='+STRCOMPRESS(nl)
printf,W3,'bands   ='+STRCOMPRESS(nb)
printf,W3,'header offset = 0'
printf,W3,'file type = ENVI Standard'
printf,W3,'data type = 4'
printf,W3,'interleave = bil'
printf,W3,'sensor type = VGT'
printf,W3,'byte order = 0'

HEADER_OUT2=file_data_out+'.hdr'
OPENW, W4, HEADER_OUT2, /GET_LUN
printf,W4,'ENVI'
printf,W4,'description = {Cloud maskE Fapar}'
printf,W4,'samples ='+STRCOMPRESS(ns)
printf,W4,'lines   ='+STRCOMPRESS(nl)
printf,W4,'bands   ='+STRCOMPRESS(nb)
printf,W4,'header offset = 0'
printf,W4,'file type = ENVI Standard'
printf,W4,'data type = 4'
printf,W4,'interleave = bil'
printf,W4,'sensor type = VGT'
printf,W4,'byte order = 0'

CLOSE, /ALL
return, 1
End




;old version until the 23 of feb 2011
;Function flag_clouds, data, blu, pc_blu, search_lim, fract_threshold
;doplot=1  ;0 does not plot, 1 makes the plots (break at the for loop to see various steps of the algoritm)
;;this function try to mask undetected clouds exploiting the temporal profile of
;;fapar (or NDVI) and the blu band
;
;;data is the array to cloud screen (fapar for example)
;;pc_blu is the percentile of blu over which spikes are flagged (e.g. 85 percentile)
;;search_lim is back position maximum for searching a finite number
;;blu is the temporal profile of the blu band
;;fract_tershold is the max fract decrease with respect to max-min in array (e.g. -0.1=-0.1*(max-min)),
;;greater values are checked for cloud. Min and max are 5% and 95% percentiles 
;;never work on first, delta[0] set to 0.0!
;
;;blu band statistics, compute blu percentile
;blu_threshold=blu[ (SORT(blu))[pc_blu * N_ELEMENTS(blu) / 100] ]
;
;;make a back up copy of data
;if doplot eq 1 then begin
;  budata=data
;  x=indgen(N_ELEMENTS(data))
;  avg_blu=mean(blu, /NAN)
;  sd_blu=stddev(blu, /NAN)
;  window, /free, xsize=1200, ysize=250
;end
;
;;debug +
;;range=[0,150]
;;data=data[range[0]:range[1]];[180:230]
;;budata=data
;;blu=blu[range[0]:range[1]];[220:300]
;;x=indgen(N_ELEMENTS(data))
;;x=x+range[0]
;;debug +
;
;;compute actual trashold using percentiles
;pc5=data[ (SORT(data))[5 * N_ELEMENTS(data) / 100] ] 
;pc95=data[ (SORT(data))[95 * N_ELEMENTS(data) / 100] ]
;;print, pc5, pc95
;data_threshold=fract_threshold*(pc95-pc5)
;
;;structure for containg returned values, cloud, cleaned fapar
;ret = { cloud: indgen(N_ELEMENTS(data))*0, data:fltarr(N_ELEMENTS(data))} 
;ret.data=data
;
;;mask as cloud those already NaN
;ind_finite_orig = where(finite(ret.data) eq 1, count_finite_orig)
;ind_nan_orig    = where(finite(ret.data) ne 1, count_nan_orig)
;if count_nan_orig ne 0 then begin
;  ret.cloud[ind_nan_orig]=1
;  ret.data[ind_nan_orig]=!VALUES.F_NAN
;endif
;;if they are all NaN, exit the function with cloud all 1
;if count_finite_orig eq 0 then return, ret
;
;;make the array of data without NaN
;data_wo_nan=ret.data[ind_finite_orig]
;
;;compute the delta_wp between two consecutive data (note that they may not be two consecutive dekads
;;since NaN values may be in between and removed
;;delta with previous element (wp)
;delta_wp=[0.0,data_wo_nan[1:*]-data_wo_nan[0:N_ELEMENTS(data_wo_nan)-2]]
;;delta with successive element (ws)
;delta_ws=[data_wo_nan[0:N_ELEMENTS(data_wo_nan)-2]-data_wo_nan[1:N_ELEMENTS(data_wo_nan)-1],0.0]
;
;;check where the delta_wp is greater than the data_threshold
;ind_delta_wp_to_flag=where(delta_wp lt data_threshold, count_delta_wp_to_flag)
;if count_delta_wp_to_flag ne 0 then ind_data_to_flag=ind_finite_orig[ind_delta_wp_to_flag]
;
;;now check if there is any delta_wp lt tershold, and in this case flag it if
;;1 if it was computed from elements closer than serach_lim
;;2 if it the blu band is greater than data_threshold
;
;while count_delta_wp_to_flag ne 0 do begin
;  ;check one by one
;  if doplot eq 1 then begin
;    ;color = red + 256*green + (256^2)*blue
;    plot, x, ret.data, title='Actual data_threshold='+strtrim(data_threshold,2)
;    oplot, x, ret.data, psym=1
;    oplot, x, blu, color=100000
;    oplot, x, blu*0.0+avg_blu, color=3000
;    oplot, x, blu*0.0+avg_blu+sd_blu, color=30000
;    oplot, x, blu*0.0+blu_threshold, color=0L + 256L*0 + (256L^2)*255
;    oplot, x[ind_data_to_flag], ret.data[ind_data_to_flag], psym=6, color=3000
;  endif
;  for i=0, count_delta_wp_to_flag-1 do begin
;    ;delta_wp_pos=ind_finite_orig[ind_delta_wp_to_flag[i]]-(ind_finite_orig[ind_delta_wp_to_flag[i]]-1)
;    delta_wp_pos=ind_data_to_flag[i]-(ind_finite_orig[ind_delta_wp_to_flag[i]]-1)
;    remove = 0
;    ;if it is not too far (within search_lim) it has to be flagged
;    if (delta_wp_pos le search_lim) then begin
;      ;it is within search limit, check if it increase again
;      if delta_ws[ind_delta_wp_to_flag[i]] lt data_threshold then begin
;        ;check that the successive is is not too far
;        ;if ind_finite_orig[ind_data_to_flag[i]]+1 - ind_finite_orig[ind_data_to_flag[i]] $
;        if ind_finite_orig[ind_delta_wp_to_flag[i]]+1 - ind_data_to_flag[i] $
;        le search_lim then remove = 1  ;it is increasing, remove it    
;      endif else begin    ;it is not increasing again, check the blu band
;        if (blu[ind_data_to_flag[i]] gt blu_threshold) $
;        then remove = 1 else remove = 0      ;it is within search limit but not increasing again, check if the blu is over avg+sd
;      endelse    
;    endif    
;    if remove eq 1 then begin
;      if doplot eq 1 then oplot, [x[ind_data_to_flag[i]]], [ret.data[ind_data_to_flag[i]]], psym=1, color=3000
;      ;it is within search limit, check if the blu is over avg+sd
;      ret.cloud[ind_data_to_flag[i]]=1
;      ret.data[ind_data_to_flag[i]]=!VALUES.F_NAN
;    endif else begin
;      if doplot eq 1 then oplot, [x[ind_data_to_flag[i]]], [ret.data[ind_data_to_flag[i]]], psym=6;, color=97000
;    endelse
;  endfor
;  ind_finite_orig=where(finite(ret.data) eq 1, count_finite_orig)
;  data_wo_nan=ret.data[ind_finite_orig]
;  delta_wp=[0.0,data_wo_nan[1:*]-data_wo_nan[0:N_ELEMENTS(data_wo_nan)-2]]
;  delta_ws=[data_wo_nan[0:N_ELEMENTS(data_wo_nan)-2]-data_wo_nan[1:N_ELEMENTS(data_wo_nan)-1],0.0]
;  
;  ind_delta_wp_to_flag2=where(delta_wp lt data_threshold, count_delta_wp_to_flag2)
;  ;exit the loop if the data to be flagged are finished or are not being flagged because of the blu band test 
;  ;is not passed 
;  if (count_delta_wp_to_flag2 eq 0) then begin
;    ;exit the loop
;    count_delta_wp_to_flag=0 
;  endif else begin
;    if ((count_delta_wp_to_flag2 eq count_delta_wp_to_flag) $
;    AND (total(ind_data_to_flag-ind_finite_orig[ind_delta_wp_to_flag2]) eq 0)) then begin
;      ;exit the loop
;      count_delta_wp_to_flag=0 
;    endif else begin
;      ind_delta_wp_to_flag=ind_delta_wp_to_flag2
;      count_delta_wp_to_flag=count_delta_wp_to_flag2
;      ind_data_to_flag=ind_finite_orig[ind_delta_wp_to_flag2]
;    endelse
; endelse
;  
;endwhile
;if doplot eq 1 then begin
;  plot, x, budata, title='Actual data_threshold='+strtrim(data_threshold,2)
;  oplot, x, budata, psym=1
;  oplot, x, ret.data, color=3000
;  oplot, x, ret.data, psym=1, color=3000
;endif
;return, ret
;End
