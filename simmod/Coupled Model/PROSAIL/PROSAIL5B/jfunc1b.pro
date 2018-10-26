; PROCEDURE COMMENTS +
; NAME: Jfunc1b
; AUTHOR: Translated from Matlab to IDL by Eben N. Broadbent 
;  Dept. of Biology, Stanford University.
;  
; Original Matlab code written by: Jean-Baptiste Féret (feret@ipgp.fr)
; Institut de Physique du Globe de Paris, Space and Planetary Geophysics
; during October 2009 based on a version of PROSAIL provided by
; Wout Verhoef, NLR on April/May 2003.
;  
; CONTACT INFO: ebennb@gmail.com
; DESCRIPTION:
; CALLING SEQUENCE:
;       output = Jfunc1(k,l,t)
; INPUTS:
; OUTPUTS:
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
;       Downloaded from "http://teledetection.ipgp.jussieu.fr/prosail/" on 09/10/2010
;       Translated from matlab to IDL by: Eben N. Broadbent on '09/27/2010'
; CODING NOTES:
; Same as Matlab (09/24/2010)
;-

function Jfunc1b,k,l,t
 ; J1 function with avoidance of singularity problem
 k=double(k);;
 l=double(l);;
 t=double(t);;
 
 del=(k-l)*t;;
 Jout = del
 
; gt_index = where(abs(del) gt 1e-3,gt_count)
 gt_index = WHERE(ABS(del) GT 1e-3, gt_count, COMPLEMENT = le_index, NCOMPLEMENT=le_count)
 ;M+ incersed speed
 if (gt_count gt 0.0) then $
  Jout(gt_index) = (exp(-l[gt_index]*t)-exp(-k*t))/(k-l[gt_index])
; if (gt_count gt 0.0) then begin
;  for i=0,gt_count-1 do begin
;   Jout(gt_index(i)) = (exp(-l(gt_index(i))*t)-exp(-k*t))/(k-l(gt_index(i)));
;  endfor
; endif
 
 ;le_index = where(abs(del) le 1e-3,le_count)
 if (le_count gt 0.0) then $
  Jout[le_index] = 0.5*t*(exp(-k*t)+exp(-l[le_index]*t))*(1.0-del[le_index]*del[le_index]/12.0)
; if (le_count gt 0.0) then begin
;  for i=0,le_count-1 do begin
;   Jout(le_index(i)) = 0.5*t*(exp(-k*t)+exp(-l(le_index(i))*t))*(1.0-del(le_index(i))*del(le_index(i))/12.0);
;  endfor
; endif
 
 return, Jout
end