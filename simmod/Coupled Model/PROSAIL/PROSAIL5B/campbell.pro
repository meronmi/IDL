; PROCEDURE COMMENTS +
; NAME: campbell
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
;       output = campbell(ala)
; INPUTS: ala
; OUTPUTS: output
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
;       Downloaded from "http://teledetection.ipgp.jussieu.fr/prosail/" on 09/10/2010
;       Translated from matlab to IDL by: Eben N. Broadbent on '09/27/2010'
; CODING NOTES:
; Same as Matlab (09/27/2010)
;
;********************************************************************************
;*                          Campbell.f                            
;*     
;*    Computation of the leaf angle distribution function value (freq) 
;*    Ellipsoidal distribution function caracterised by the average leaf 
;*    inclination angle in degree (ala)                                     
;*    Campbell 1986                                                      
;*                                                                              
;********************************************************************************
;
;-

function campbell, ala
 tx2=[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85];
 tx1=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90];

 n=18.0;
 x=(tx2+tx1)/2.0;
 tl1=tx1*(!pi/180.0);
 tl2=tx2*(!pi/180.0);
 excent=exp(-1.6184e-5*ala^3.0+2.1145e-3*ala^2.0-1.2390e-1*ala+3.2491); value currently wrong, MIC:apparently not, check with Verhoef ellipse e ladgen
 sum0 = 0;

 freq=fltarr(18);
 for i=0, n-1 do begin
  x1  = excent/(sqrt(1.0+excent^2.0*tan(tl1(i))^2.0));
  x2  = excent/(sqrt(1.0+excent^2.0*tan(tl2(i))^2.0));
  if (excent eq 1) then begin
   freq(i) = abs(cos(tl1(i))-cos(tl2(i)));
  endif else begin
   alpha  = excent/sqrt(abs(1.0-excent^2.0));
   alpha2 = alpha^2.0;
   x12 = x1^2.0;
   x22 = x2^2.0;
   if excent gt 1 then begin
    alpx1 = sqrt(alpha2+x12);
    alpx2 = sqrt(alpha2+x22);
    dum   = x1*alpx1+alpha2*alog(x1+alpx1);
    freq(i) = abs(dum-(x2*alpx2+alpha2*alog(x2+alpx2)));
   endif else begin
    almx1 = sqrt(alpha2-x12);
    almx2 = sqrt(alpha2-x22);
    dum   = x1*almx1+alpha2*asin(x1/alpha);
    freq(i) = abs(dum-(x2*almx2+alpha2*asin(x2/alpha)));
   endelse
  endelse
 endfor
 
 sum0 = total(freq);
 freq0=freq/sum0;
 return, freq0
end