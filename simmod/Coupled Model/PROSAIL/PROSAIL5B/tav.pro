; PROCEDURE COMMENTS +
; NAME: tav
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
;       output = tav(teta,ref)
; INPUTS: teta, ref
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
; ***********************************************************************
; Stern F. (1964), Transmission of isotropic radiation across an
; interface between two dielectrics, Appl. Opt., 3(1):111-113.
; Allen W.A. (1973), Transmission of isotropic light across a
; dielectric surface in two and three dimensions, J. Opt. Soc. Am.,
; 63(6):664-666.
; ***********************************************************************
; Féret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical
; Properties Model Separating Photosynthetic Pigments, Remote Sensing of
; Environment
; ***********************************************************************
;-

function tav,teta,ref

 teta = double(teta);;
 ref = double(ref);;

 s=size(ref,/DIM); reverse of matlab idl=(col,row), matlab=(row,col)
 teta=teta*!pi/180.0;;
 r2=ref^2.0;;
 rp=r2+1.0;;
 rm=r2-1.0;;
 a=(ref+1.0)^2.0/2.0;;
 k=-(r2-1.0)^2.0/4.0;;
 ds=sin(teta);;

 k2=k^(2.0);;
 rm2=rm^2.0;;

 if (teta eq 0) then begin
  f=4.0*ref/(ref+1.0)^2.0;;
 endif else begin
  if (teta eq !pi/2.0) then begin;; when teta input equals 90
    b1=fltarr(1,s(1));;
  endif else begin ; seems to return same results
    b1=sqrt((ds^2.0-rp/2.0)^2.0+k);;
  endelse
  b2=ds^2.0-rp/2.0;;
  b=b1-b2;;
  ts=(k2/(6.0*b^3.0)+k/b-b/2.0)-(k2/(6.0*a^3.0)+k/a-a/2.0);;
  tp1=-2.0*r2*(b-a)/(rp^2.0);;
  tp2=-2.0*r2*rp*alog(b/a)/rm2;;
  tp3=r2*(b^(-1.0)-a^(-1.0))/2.0;;
  tp4=16.0*r2^(2.0)*(r2^2.0+1.0)*alog((2*rp*b-rm2)/(2.0*rp*a-rm2))/(rp^(3.0)*rm2);;
  tp5=16.0*r2^(3.0)*((2.0*rp*b-rm2)^(-1.0)-(2.0*rp*a-rm2)^(-1.0))/rp^3.0;;
  tp=tp1+tp2+tp3+tp4+tp5;;
  f=(ts+tp)/(2.0*ds^2.0);;
 end
 return, f
end