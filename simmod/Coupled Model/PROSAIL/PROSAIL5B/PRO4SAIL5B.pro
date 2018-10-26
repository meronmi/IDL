; PROCEDURE COMMENTS +
; NAME: PRO4SAIL5B
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
;       PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,angl,lai,hspot,tts,tto,psi,psoil,skyl,resh,resv,absh
; INPUTS: Cab,Car,Cbrown,Cw,Cm,Ns,angl,lai,hspot,tts,tto,psi,psoil,skyl
; OUTPUTS: resh,resv,absh
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
;       Downloaded from "http://teledetection.ipgp.jussieu.fr/prosail/" on 09/10/2010
;       Translated from matlab to IDL by: Eben N. Broadbent on '09/27/2010'
;       Modified by Michele Meroni (michele.meroni@gmail.com) as follows:
;       1) absh, canopy absorption for diffuse radiation computed as
;       absh = 1.0 - rddt - tdd/dn + tdd/dn * rsoil0
;       2)Leaf Angle distribution
;       Function Ladgen (a and b parameters) implented according to W. Verhoef
;       3)Diffuse/Total irradiance to weght the two r (do, so) 
;       A fixed spectral diffuse/total radiation is used as simulated by MODTRAN (see dataSkyle.pro 
;       for the detail of the simulation)
;       3) Add common block to avoid calling PROSPECT if parameters are not changed
; CODING NOTES:
; Same as Matlab (09/24/2010)
;
;-

PRO PRO4SAIL5B,Cab,Car,Cbrown,Cw,Cm,Ns,lidf_a,lidf_b,lai,hspot,tts,tto,psi,rsoil0,resh,resv,absh,absa

;common blocks
@cb_prospect_last_parameters.comm      ;last parameters must be initialized
@cb_sail_last_parameters.comm       ;last run parameters
@cb_prosail_data.comm                  ;skyl and others
@cb_ladgen.comm                        ;ladgen input/output
 ;data=dataSpec_P5B() ;;   read it in common block
 ;Es=data(7,*);;
 ;Ed=data(8,*);;
 ;skyl = dataSkyle()
 ;Rsoil1=data(9,*);;
 ;Rsoil2=data(10,*);;
 ;Soil Reflectance Properties
 ;rsoil1 = dry soil
 ;rsoil2 = wet soil
 ;rsoil0=psoil*Rsoil1+(1.0-psoil)*Rsoil2;;


 ; Outputs for the case LAI = 0
 if (lai le 0) then begin;    ;M+ corrected LE instead of LT
   ;M+ added
   resh  = (1.0 - skyl) * rsoil0 + skyl * rsoil0
   resv  = (1.0 - skyl) * rsoil0 + skyl * rsoil0    ;this is an HCRF
   absh = resv * 0.0
   absa = resv * 0.0
   ;lresh = resh & lresv = resv & labsh = absh
   ;   llidf_a = lidf_a
   ;   llidf_b = lidf_b
   ;WRITE_CSV, 'd:\Users\meronmi\Documents\IDL\PROSAIL_5B_IDL\r_lai0.csv', [rsdt,rddt], HEADER=['rsdt','rddt']
   return
 endif;
 ;see if prosail was called exactly with the same var, if this is the case just
 ;copy the last results 
 IF ((TOTAL([Cab,Car,Cbrown,Cw,Cm,Ns] EQ [lCab,lCar,lCbrown,lCw,lCm,lNs]) EQ 6) AND $
     (TOTAL([lidf_a, lidf_b, lai, hspot, tts, tto, psi] EQ $
     [llidf_a, llidf_b, llai, lhspot, ltts, ltto, lpsi]) EQ 7)) THEN BEGIN
   resh = lresh & resv = lresv & absh = labsh & absa = labsa
   ;Print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
 ENDIF ELSE BEGIN     
   ;update last parameters
   llai = lai & lhspot = hspot & ltts = tts & ltto = tto & lpsi = psi
   ; LEAF OPTICAL PROPERTIES
   ;call PROSPECT only when at least one parameter is changes
   IF (TOTAL([Cab,Car,Cbrown,Cw,Cm,Ns] EQ [lCab,lCar,lCbrown,lCw,lCm,lNs]) NE 6) THEN $
     prospect_5B,Cab,Car,Cbrown,Cw,Cm,Ns,l,RN,TN $
   ELSE RN = lRN & TN = lTN                               
   rho = REFORM(RN);;                           
   tau = REFORM(TN);;                           
   ; gh = plot(data[0,*], 1-TN, YRANGE = [0,1], XRANGE = [400,2500])
   ; gh2 =plot(data[0,*], RN, OVERPLOT = 1)                                                          
   ; LEAF ANGLE DISTRIBUTION                           
   na     = 13;; M+
   rd     = !DtoR;;
   thetal = [5.0,15.0,25.0,35.0,45.0,55.0,65.0,75.0,81.0,83.0,85.0,87.0,89.0]
   ;lidf   = calc_LIDF(angl);;    M+: remove the use of strange ellispoidal
   ;avoid calling again with same parameters
   ;FO USING ala nad campbell use: lidf = campbell_test(ala), ala of 60 correspons to spherical lidf a and b
   IF ((lidf_a NE llidf_a) OR (lidf_b NE llidf_b)) THEN BEGIN
    lidf   = ladgen(lidf_a,lidf_b) 
    llidf_a = lidf_a
    llidf_b = lidf_b
    llidf = lidf
   ENDIF ELSE lidf = llidf
    
                                                               
   ; Direct (Es) / Diffuse (Ed) light (M+ strange computation, suppresse)   
   ;PARdiro = (1.0-skyl/100.0)*Es;;                                             
   ;PARdifo = (skyl/100.0)*Ed;;                                                 
                                                          
               
  
   cts    = cos(rd*tts);
   cto    = cos(rd*tto);
   ctscto = cts*cto;
   tants  = tan(rd*tts);
   tanto  = tan(rd*tto);
   cospsi = cos(rd*psi);
   dso    = sqrt(tants*tants+tanto*tanto-2.0*tants*tanto*cospsi);
  
  ;MM+ implemented a vector version to avoid the loop
   ; angular distance, compensation of shadow length
   ; Calculate geometric factors associated with extinction and scattering 
   ; Initialise sums
  ; ks  = 0.0;
  ; ko  = 0.0;
  ; bf  = 0.0;
  ; sob = 0.0;
  ; sof = 0.0;
  ;
  ; ; Weighted sums over LIDF
  ; for i=0, na-1 do begin;;                
  ;  ttl = thetal(i);;   leaf inclination discrete values
  ;  ctl = cos(rd*ttl);;
  ;  
  ;  ; SAIL volume scattering phase function gives interception and portions to be 
  ;  ; multiplied by rho and tau
  ;  
  ;  volscatt,tts,tto,psi,ttl,chi_s,chi_o,frho,ftau;; (for a small subset of values looks good, need more thorough verification)
  ;  
  ;  ;********************************************************************************
  ;  ;*                   SUITS SYSTEM COEFFICIENTS 
  ;  ;*
  ;  ;*  ks  : Extinction coefficient for direct solar flux
  ;  ;*  ko  : Extinction coefficient for direct observed flux
  ;  ;*  att : Attenuation coefficient for diffuse flux
  ;  ;*  sigb : Backscattering coefficient of the diffuse downward flux
  ;  ;*  sigf : Forwardscattering coefficient of the diffuse upward flux
  ;  ;*  sf  : Scattering coefficient of the direct solar flux for downward diffuse flux
  ;  ;*  sb  : Scattering coefficient of the direct solar flux for upward diffuse flux
  ;  ;*  vf   : Scattering coefficient of upward diffuse flux in the observed direction
  ;  ;*  vb   : Scattering coefficient of downward diffuse flux in the observed direction
  ;  ;*  w   : Bidirectional scattering coefficient
  ;  ;********************************************************************************
  ;
  ;  ; Extinction coefficients
  ;  ksli = chi_s/cts;;
  ;  koli = chi_o/cto;;
  ;
  ;  ; Area scattering coefficient fractions
  ;  sobli = frho*!pi/ctscto;;
  ;  sofli = ftau*!pi/ctscto;;
  ;  bfli  = ctl*ctl;;
  ;  ks    = ks+ksli*lidf(i);;
  ;  ko    = ko+koli*lidf(i);;
  ;  bf    = bf+bfli*lidf(i);;
  ;  sob   = sob+sobli*lidf(i);;
  ;  sof   = sof+sofli*lidf(i);;
  ; endfor;;
   ;test with array
   ttl_vec = thetal;;   leaf inclination discrete values
   ctl_vec = cos(rd*ttl_vec)
   volscatt_array,tts,tto,psi,ttl_vec,chi_s,chi_o, frho_vec,ftau_vec
   ; Extinction coefficients
   ksli = chi_s/cts;;
   koli = chi_o/cto;;
   ; Area scattering coefficient fractions
   sobli_vec = frho_vec*!pi/ctscto;;
   sofli_vec = ftau_vec*!pi/ctscto;;
   bfli_vec  = ctl_vec*ctl_vec;;
   ;be care here I have the summation
   ks    = TOTAL(ksli*lidf);;
   ko    = TOTAL(koli*lidf);;
   bf    = TOTAL(bfli_vec*lidf);;
   sob   = TOTAL(sobli_vec*lidf);;
   sof   = TOTAL(sofli_vec*lidf);;
   ;end of test
   ; Geometric factors to be used later with rho and tau
   sdb = 0.5*(ks+bf);
   sdf = 0.5*(ks-bf);
   dob = 0.5*(ko+bf);
   dof = 0.5*(ko-bf);
   ddb = 0.5*(1.0+bf);
   ddf = 0.5*(1.0-bf);
  
   ; Here rho and tau come in
   sigb= ddb*rho+ddf*tau;
   sigf= ddf*rho+ddb*tau;
   att = 1.0-sigf;
   m2  =(att+sigb)*(att-sigb);
  
   m2[WHERE(m2 LE 0.0, /NULL)] = 0.0
   ;m2_index = where(m2 le 0.0, m2_count)
   ;if (m2_count gt 0.0) then m2(m2_index) = 0.0 
  
   m=sqrt(m2);
  
   sb  = sdb*rho+sdf*tau;
   sf  = sdf*rho+sdb*tau;
   vb  = dob*rho+dof*tau;
   vf  = dof*rho+dob*tau;
   w = sob*rho+sof*tau;
  
   ; Other cases (LAI > 0)
   e1    = exp(-m*lai);
   e2    = e1*e1;
   rinf  = (att-m)/sigb;
   rinf2 = rinf*rinf;
   re    = rinf*e1;
   denom = 1.0-rinf2*e2;
  
   ;MM 14 July 2016 slightly faster computation of func1
   ;PRINT, LAI
   J1ks=Jfunc1c(ks,m,lai);
   J2ks=Jfunc2(ks,m,lai);
   ;MM 14 July 2016 faster version, compute both at once, failed bcause it slower
  ; Jfunc12, ks,m,lai, J1ks, J2ks
   J1ko=Jfunc1c(ko,m,lai);
   J2ko=Jfunc2(ko,m,lai);
  ; Jfunc12, ko,m,lai, J1ko, J2ko
  
   Ps = (sf+sb*rinf)*J1ks;
   Qs = (sf*rinf+sb)*J2ks;
   Pv = (vf+vb*rinf)*J1ko;
   Qv = (vf*rinf+vb)*J2ko;
  
   rdd = rinf*(1.0-e2)/denom;
   tdd = (1.0-rinf2)*e1/denom;
   tsd = (Ps-re*Qs)/denom;
   rsd = (Qs-re*Ps)/denom;
   tdo = (Pv-re*Qv)/denom;
   rdo = (Qv-re*Pv)/denom;
  
   tss = exp(-ks*lai);
   too = exp(-ko*lai);
   z = Jfunc3(ks,ko,lai);
   g1  = (z-J1ks*too)/(ko+m);
   g2  = (z-J1ko*tss)/(ks+m);
      
   Tv1 = (vf*rinf+vb)*g1;
   Tv2 = (vf+vb*rinf)*g2;
   T1  = Tv1*(sf+sb*rinf);
   T2  = Tv2*(sf*rinf+sb);
   T3  = (rdo*Qs+tdo*Ps)*rinf;
  
   ; Multiple scattering contribution to bidirectional canopy reflectance
   rsod = (T1+T2-T3)/(1.0-rinf2);
   ; Treatment of the hotspot-effect
   alf=1e6;
   ; Apply correction 2/(K+k) suggested by F.-M. Bréon
   if hspot gt 0.0 then alf=(dso/hspot)*2.0/(ks+ko);
   if alf gt 200.0 then alf=200.0;
   if alf eq 0.0 then begin;
    ; The pure hotspot - no shadow
    tsstoo = tss;
    sumint = (1.0-tss)/(ks*lai);
   endif else begin;
    ; Outside the hotspot
    fhot=lai*sqrt(ko*ks);
    ; Integrate by exponential Simpson method in 20 steps
    ; the steps are arranged according to equal partitioning
    ; of the slope of the joint probability function
    
    ;MM+ 08/07/2016 loop replaced by faster version
    fint=(1.0-exp(DOUBLE(-alf)))*0.05;
    i = INDGEN(19) + 1
    x2 = [-alog(1.0-i*fint)/alf, 1]
    y2=-(ko+ks)*lai*x2+fhot*(1.0-exp(DOUBLE(-alf*x2)))/alf;
    f2=exp(y2)
    f1 =[1,f2[0:-2]]
    x1 = [0,x2[0:-2]]
    y1 = [0,y2[0:-2]]
    sumint = TOTAL((f2-f1)*(x2-x1)/(y2-y1))
    tsstoo = f2[-1]
    
  ;  x1=0.0;
  ;  y1=0.0;
  ;  f1=1.0;
  ;  ;Michele cast to double to avoid undeflow if alf is big
  ;  fint=(1.0-exp(DOUBLE(-alf)))*0.05;
  ;  sumint=0;
  ;;  for i=1,20 do begin;
  ;;   if i lt 20 then begin;
  ;    x2=-alog(1.0-i*fint)/alf;
  ;   endif else begin;
  ;    x2=1;
  ;   endelse;
  ;   ;Michele cast to double to avoid undeflow if alf is big
  ;   y2=-(ko+ks)*lai*x2+fhot*(1.0-exp(DOUBLE(-alf*x2)))/alf;
  ;   f2=exp(y2);
  ;   sumint=sumint+(f2-f1)*(x2-x1)/(y2-y1);
  ;   ;print, f2, x2, y2, f1, f2, (f2-f1)*(x2-x1)/(y2-y1);
  ;   x1=x2;
  ;   y1=y2;
  ;   f1=f2;
  ;  endfor;
  ;  tsstoo=f1;
  ;MM-
   endelse;
  
  ; Bidirectional reflectance
   rsos = w*lai*sumint;         Single scattering contributiontrrrrrr
   rso=rsos+rsod;               Total canopy contribution
   dn=1.0-rsoil0*rdd;           Interaction with the soil
  
   rddt=rdd+tdd*rsoil0*tdd/dn             ;diffuse reflectance for diffuse incident  rddt: bi-hemispherical reflectance factor
   rsdt=rsd+(tsd+tss)*rsoil0*tdd/dn       ;diffuse reflectance for direct incident   rsdt: directional-hemispherical reflectance factor for solar incident flux
   rdot=rdo+tdd*rsoil0*(tdo+too)/dn       ;observer reflectance for diffuse light    rdot: hemispherical-directional reflectance factor in viewing direction
  
   rsodt=rsod+((tss+tsd)*tdo+(tsd+tss*rsoil0*rdd)*too)*rsoil0/dn;
   rsost=rsos+tsstoo*rsoil0;
   rsot=rsost+rsodt;                      ;rsot: bi-directional reflectance factor
  
   ;resh  = (rddt*PARdifo+rsdt*PARdiro)/(PARdiro+PARdifo);
   ;resv  = (rdot*PARdifo+rsot*PARdiro)/(PARdiro+PARdifo);
   resh  = (1.0 - skyl) * rsdt + skyl * rddt 
   resv  = (1.0 - skyl) * rsot + skyl * rdot    ;this is an HCRF
   ;WRITE_CSV, 'd:\Users\meronmi\Documents\IDL\PROSAIL_5B_IDL\r_lai0.csv', [rsdt,rddt], HEADER=['rsdt','rddt']
   ;M+ Compute canopy absorption for diffuse radiation (by my calculations and by Verhoef Pro5FourSail2.f90)
   absh = 1.0 - rddt - tdd/dn + tdd/dn * rsoil0
   ;M+ test for actual instantaneous fapar
   ;absact = 1.0 - skyl*rddt - (1.0 - skyl)*rsdt - (skyl* tdd + (1.0 - skyl)*(tss + tsd))+ skyl*rsoil0*tdd/dn + (1.0 - skyl)*(tsd+tss)*rsoil0/dn
   ;from email of Wout Verhoef 6/4/2017, actual instantaneous abs (for apar computation
   absa = (1 - skyl)*[1 - rsdt - (1 - rsoil0)*(tss+tsd)/dn] + skyl*[1 - rddt - (1 - rsoil0)*tdd/dn]
   ;update last results in the common block
   lresh = resh & lresv = resv & labsh = absh & labsa = absa
 ENDELSE
 ;M-
end;