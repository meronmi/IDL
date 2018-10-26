; PROCEDURE COMMENTS +
; NAME: volscatt_array
; AUTHOR: Translated from Matlab to IDL by Eben N. Broadbent 
;  Dept. of Biology, Stanford University.
;  Michele Meroni made a vector version on 08/07/2016
; Original Matlab code written by: Jean-Baptiste Féret (feret@ipgp.fr)
; Institut de Physique du Globe de Paris, Space and Planetary Geophysics
; during October 2009 based on a version of PROSAIL provided by
; Wout Verhoef, NLR on April/May 2003.
;  
; CONTACT INFO: ebennb@gmail.com
; DESCRIPTION:
; CALLING SEQUENCE:
;       volscatt,tts,tto,psi,ttl,chi_s_vec,chi_o_vec,frho_vec,ftau_vec
; INPUTS: tts,tto,psi,ttl
; OUTPUTS: chi_s_vec,chi_o_vec,frho_vec,ftau_vec
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

pro volscatt_array,tts,tto,psi,ttl_vec,chi_s_vec,chi_o_vec,frho_vec,ftau_vec
;same as previous volscatt bu here ttl_vec is an array
;********************************************************************************
;*  tts   = solar zenith
;*  tto   = viewing zenith
;*  psi   = azimuth
;*  ttl_vec   = leaf inclination angle
;*  chi_s_vec = interception functions
;*  chi_o_vec = interception functions
;*  frho_vec  = function to be multiplied by leaf reflectance rho
;*  ftau_vec  = functions to be multiplied by leaf transmittance tau
;********************************************************************************

; Compute volume scattering functions and interception coefficients
; for given solar zenith, viewing zenith, azimuth and leaf inclination angle.

; chi_s_vec and chi_o_vec are the interception functions.
; frho_vec and ftau_vec are the functions to be multiplied by leaf reflectance rho and
; leaf transmittance tau, respectively, in order to obtain the volume scattering
; function.

; Wout Verhoef, april 2001, for CROMA

; REAL(KIND=8),INTENT(in) :: tts
; REAL(KIND=8),INTENT(in) :: tto
; REAL(KIND=8),INTENT(in) :: psi
; REAL(KIND=8),INTENT(in) :: ttl_vec
; REAL(KIND=8),INTENT(inout) :: chi_s_vec
; REAL(KIND=8),INTENT(inout) :: chi_o_vec
; REAL(KIND=8),INTENT(inout) :: frho_vec
; REAL(KIND=8),INTENT(inout) :: ftau_vec
; 
; REAL(KIND=8) costs,costo,sints,sinto,cospsi
; REAL(KIND=8) psir
; REAL(KIND=8) costl,sintl,cs_vec,co_vec,ss_vec,so_vec,ds_vec
; REAL(KIND=8) cosbts_vec,cosbto_vec,bts_vec,bto_vec
; REAL(KIND=8) btran1_vec,btran2_vec,bt1_vec,bt2_vec,bt3_vec,t1_vec,t2_vec
; REAL(KIND=8) doo_vec
; REAL(KIND=8) denom
 
 n = N_ELEMENTS(ttl_vec)
 rd=!pi/180.0;
 costs=cos(rd*tts);
 costo=cos(rd*tto);
 sints=sin(rd*tts);
 sinto=sin(rd*tto);
 cospsi=cos(rd*psi);
 psir=rd*psi;
 costl_vec=cos(rd*ttl_vec);
 sintl_vec=sin(rd*ttl_vec);
 cs_vec=costl_vec*costs;
 co_vec=costl_vec*costo;
 ss_vec=sintl_vec*sints;
 so_vec=sintl_vec*sinto; 

;c ..............................................................................
;c     betas -bts_vec- and betao -bto_vec- computation
;c     Transition angles (beta) for solar (betas) and view (betao) directions
;c     if thetav+thetal>pi/2, bottom side of the leaves is observed for leaf azimut 
;c     interval betao+phi<leaf azimut<2pi-betao+phi.
;c     if thetav+thetal<pi/2, top side of the leaves is always observed, betao=pi
;c     same consideration for solar direction to compute betas
;c ..............................................................................


; cosbts_vec=5.0;;
; cosbto_vec=5.0;;
 cosbts_vec=FLTARR(n) + 5.0;;
 cosbto_vec=FLTARR(n) + 5.0;;

 ;if (abs(ss_vec) gt 1e-6) then cosbts_vec = -cs_vec/ss_vec;;
 ind = WHERE(abs(ss_vec) gt 1e-6, count)
 IF (count GT 0) THEN cosbts_vec[ind] =  -cs_vec[ind]/ss_vec[ind]
 ;if (abs(so_vec) gt 1e-6) then cosbto_vec = -co_vec/so_vec;;
 ind = WHERE(abs(so_vec) gt 1e-6, count)
 IF (count GT 0) THEN cosbto_vec[ind] =  -co_vec[ind]/so_vec[ind]
 
; if (abs(cosbts_vec) lt 1.0) then begin;;
;  bts_vec=acos(cosbts_vec);;
;  ds_vec=ss_vec;;
; endif else begin;;
;  bts_vec=!pi;;
;  ds_vec=cs_vec;;
; endelse;;
 bts_vec=FLTARR(n)+!pi;;
 ds_vec=cs_vec
 ind = WHERE(abs(cosbts_vec) lt 1.0, count)
 IF (count GT 0) THEN BEGIN
   bts_vec[ind]=acos(cosbts_vec[ind]);;
   ds_vec[ind]=ss_vec[ind];;
 ENDIF
 
 chi_s_vec=2.0/!pi*((bts_vec-!pi*0.5)*cs_vec+sin(bts_vec)*ss_vec);;

; if (abs(cosbto_vec) lt 1.0) then begin
;  bto_vec=acos(cosbto_vec);
;  doo_vec=so_vec;
; endif else begin
;  if(tto lt 90.0) then begin
;   bto_vec=!pi;
;   doo_vec=co_vec;
;  endif else begin
;   bto_vec=0.0;
;   doo_vec=-co_vec;
;  endelse
; endelse
 
 IF(tto LT 90.0) THEN BEGIN
   bto_vec=FLTARR(n)+!pi;
   doo_vec=co_vec;
 endif else begin
   bto_vec=FLTARR(n);
   doo_vec=-co_vec;
 endelse
 ind = WHERE(abs(cosbto_vec) lt 1.0, count)
 IF (count GT 0) THEN BEGIN
   bto_vec[ind]=acos(cosbto_vec[ind]);
   doo_vec[ind]=so_vec[ind]
 ENDIF
 

 chi_o_vec=2.0/!pi*((bto_vec-!pi*0.5)*co_vec+sin(bto_vec)*so_vec);

;c ..............................................................................
;c   Computation of auxiliary azimut angles bt1_vec, bt2_vec, bt3_vec used          
;c   for the computation of the bidirectional scattering coefficient w              
;c .............................................................................

 btran1_vec=abs(bts_vec-bto_vec);
 btran2_vec=!pi-abs(bts_vec+bto_vec-!pi);

; if (psir le btran1_vec) then begin
;  bt1_vec=psir;
;  bt2_vec=btran1_vec;
;  bt3_vec=btran2_vec;
; endif else begin
;  bt1_vec=btran1_vec;
;  if (psir le btran2_vec) then begin
;   bt2_vec=psir;
;   bt3_vec=btran2_vec;
;  endif else begin
;   bt2_vec=btran2_vec;
;   bt3_vec=psir;
;  endelse
; endelse
 
 bt1_vec = btran1_vec;
 bt2_vec = btran2_vec
 bt3_vec = FLTARR(n) + psir
 ind = WHERE(psir le btran2_vec, count)
 IF (count GT 0) THEN BEGIN
   bt2_vec[ind]=psir;
   bt3_vec[ind]=btran2_vec[ind];
 ENDIF
 ind = WHERE(psir le btran1_vec, count)
 IF (count GT 0) THEN BEGIN
   bt1_vec[ind]=psir;
   bt2_vec[ind]=btran1_vec[ind];
   bt3_vec[ind]=btran2_vec[ind];
 ENDIF
 
 
 t1_vec=2.0*cs_vec*co_vec+ss_vec*so_vec*cospsi;
 ;t2_vec=0.0;
 t2_vec = FLTARR(n)
 ind = WHERE(bt2_vec gt 0.0, count)
 IF (count GT 0) THEN t2_vec[ind]=sin(bt2_vec[ind])*(2.0*ds_vec[ind]*doo_vec[ind]+ss_vec[ind]*so_vec[ind]*cos(bt1_vec[ind])*cos(bt3_vec[ind]));
 

 denom=2.0*!pi*!pi;
 frho_vec=((!pi-bt2_vec)*t1_vec+t2_vec)/denom;
 ftau_vec=(-bt2_vec*t1_vec+t2_vec)/denom;

 ;if (frho_vec lt 0.0) then frho_vec=0.0;
 ind = WHERE(frho_vec lt 0.0, count)
 IF (count GT 0) THEN frho_vec[ind] = 0.0
 ;if (ftau_vec lt 0.0) then ftau_vec=0.0;
 ind = WHERE(ftau_vec lt 0.0, count)
 IF (count GT 0) THEN ftau_vec[ind] = 0.0
end