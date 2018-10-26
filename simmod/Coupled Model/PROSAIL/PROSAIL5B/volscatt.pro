; PROCEDURE COMMENTS +
; NAME: volscatt
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
;       volscatt,tts,tto,psi,ttl,chi_s,chi_o,frho,ftau
; INPUTS: tts,tto,psi,ttl
; OUTPUTS: chi_s,chi_o,frho,ftau
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
;-

pro volscatt,tts,tto,psi,ttl,chi_s,chi_o,frho,ftau

;********************************************************************************
;*  tts   = solar zenith
;*  tto   = viewing zenith
;*  psi   = azimuth
;*  ttl   = leaf inclination angle
;*  chi_s = interception functions
;*  chi_o = interception functions
;*  frho  = function to be multiplied by leaf reflectance rho
;*  ftau  = functions to be multiplied by leaf transmittance tau
;********************************************************************************

; Compute volume scattering functions and interception coefficients
; for given solar zenith, viewing zenith, azimuth and leaf inclination angle.

; chi_s and chi_o are the interception functions.
; frho and ftau are the functions to be multiplied by leaf reflectance rho and
; leaf transmittance tau, respectively, in order to obtain the volume scattering
; function.

; Wout Verhoef, april 2001, for CROMA

; REAL(KIND=8),INTENT(in) :: tts
; REAL(KIND=8),INTENT(in) :: tto
; REAL(KIND=8),INTENT(in) :: psi
; REAL(KIND=8),INTENT(in) :: ttl
; REAL(KIND=8),INTENT(inout) :: chi_s
; REAL(KIND=8),INTENT(inout) :: chi_o
; REAL(KIND=8),INTENT(inout) :: frho
; REAL(KIND=8),INTENT(inout) :: ftau
; 
; REAL(KIND=8) costs,costo,sints,sinto,cospsi
; REAL(KIND=8) psir
; REAL(KIND=8) costl,sintl,cs,co,ss,so,ds
; REAL(KIND=8) cosbts,cosbto,bts,bto
; REAL(KIND=8) btran1,btran2,bt1,bt2,bt3,t1,t2
; REAL(KIND=8) doo
; REAL(KIND=8) denom

 rd=!pi/180.0;
 costs=cos(rd*tts);
 costo=cos(rd*tto);
 sints=sin(rd*tts);
 sinto=sin(rd*tto);
 cospsi=cos(rd*psi);
 psir=rd*psi;
 costl=cos(rd*ttl);
 sintl=sin(rd*ttl);
 cs=costl*costs;
 co=costl*costo;
 ss=sintl*sints;
 so=sintl*sinto; 

;c ..............................................................................
;c     betas -bts- and betao -bto- computation
;c     Transition angles (beta) for solar (betas) and view (betao) directions
;c     if thetav+thetal>pi/2, bottom side of the leaves is observed for leaf azimut 
;c     interval betao+phi<leaf azimut<2pi-betao+phi.
;c     if thetav+thetal<pi/2, top side of the leaves is always observed, betao=pi
;c     same consideration for solar direction to compute betas
;c ..............................................................................


 cosbts=5.0;;
 cosbto=5.0;;

 if (abs(ss) gt 1e-6) then cosbts = -cs/ss;;
 if (abs(so) gt 1e-6) then cosbto = -co/so;;
 if (abs(cosbts) lt 1.0) then begin;;
  bts=acos(cosbts);;
  ds=ss;;
 endif else begin;;
  bts=!pi;;
  ds=cs;;
 endelse;;

 chi_s=2.0/!pi*((bts-!pi*0.5)*cs+sin(bts)*ss);;

 if (abs(cosbto) lt 1.0) then begin
  bto=acos(cosbto);
  doo=so;
 endif else begin
  if(tto lt 90.0) then begin
   bto=!pi;
   doo=co;
  endif else begin
   bto=0.0;
   doo=-co;
  endelse
 endelse

 chi_o=2.0/!pi*((bto-!pi*0.5)*co+sin(bto)*so);

;c ..............................................................................
;c   Computation of auxiliary azimut angles bt1, bt2, bt3 used          
;c   for the computation of the bidirectional scattering coefficient w              
;c .............................................................................

 btran1=abs(bts-bto);
 btran2=!pi-abs(bts+bto-!pi);

 if (psir le btran1) then begin
  bt1=psir;
  bt2=btran1;
  bt3=btran2;
 endif else begin
  bt1=btran1;
  if (psir le btran2) then begin
   bt2=psir;
   bt3=btran2;
  endif else begin
   bt2=btran2;
   bt3=psir;
  endelse
 endelse

 t1=2.0*cs*co+ss*so*cospsi;
 t2=0.0;
 if (bt2 gt 0.0) then t2=sin(bt2)*(2.0*ds*doo+ss*so*cos(bt1)*cos(bt3));

 denom=2.0*!pi*!pi;
 frho=((!pi-bt2)*t1+t2)/denom;
 ftau=(-bt2*t1+t2)/denom;

 if (frho lt 0.0) then frho=0.0;
 if (ftau lt 0.0) then ftau=0.0;

end