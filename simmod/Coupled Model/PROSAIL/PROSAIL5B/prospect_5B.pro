; PROCEDURE COMMENTS +
; NAME: prospect_5B
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
;       prospect_5B,Cab,Car,Cbrown,Cw,Cm,Ns,l,RN,TN
; INPUTS: Cab,Car,Cbrown,Cw,Cm,Ns,l
; OUTPUTS: RN,TN
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
; _______________________________________________________________________
;
; prospect_5B (including carotenoids and brown pigments)
; version 5b (october, 20th 2009)
; subroutines required: tav, dataSpec_P5B
; _______________________________________________________________________
;
; Plant leaf reflectance and transmittance are calculated from 400 nm to
; 2500 nm (1 nm step) with the following parameters:
;
;       - Ns   = leaf structure parameter
;       - Cab = chlorophyll a+b content in µg/cm²
;       - Car = carotenoids content in µg/cm²
;       - Cw  = equivalent water thickness in g/cm² or cm
;       - Cm  = dry matter content in g/cm²
;
; Here are some examples observed during the LOPEX'93 experiment on
; fresh (F) and dry (D) leaves :
;
; ---------------------------------------------
;                N     Cab     Cw        Cm    
; ---------------------------------------------
; min          1.000    0.0  0.004000  0.001900
; max          3.000  100.0  0.040000  0.016500
; corn (F)     1.518   58.0  0.013100  0.003662
; rice (F)     2.275   23.7  0.007500  0.005811
; clover (F)   1.875   46.7  0.010000  0.003014
; laurel (F)   2.660   74.1  0.019900  0.013520
; ---------------------------------------------
; min          1.500    0.0  0.000063  0.0019
; max          3.600  100.0  0.000900  0.0165
; bamboo (D)   2.698   70.8  0.000117  0.009327
; lettuce (D)  2.107   35.2  0.000244  0.002250
; walnut (D)   2.656   62.8  0.000263  0.006573
; chestnut (D) 1.826   47.7  0.000307  0.004305
; ---------------------------------------------
;
;-

pro prospect_5B,Cab,Car,Cbrown,Cw,Cm,Ns,l,RN,TN

;; ***********************************************************************
;; Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical
;; properties spectra, Remote Sens. Environ., 34:75-91.
;; Féret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical
;; Properties Model Separating Photosynthetic Pigments, Remote Sensing of
;; Environment, 112:3030-3043
;; The specific absorption coefficient corresponding to brown pigment is
;; provided by Frederic Baret (EMMAH, INRA Avignon, baret@avignon.inra.fr)
;; and used with his autorization.
;; ***********************************************************************

;common blocks
@cb_prospect_last_parameters.comm    ;used to store the parameters and out run
@cb_prosail_data.comm                ;for abs coeff
lCab = Cab
lCar = Car
lCbrown = Cbrown
lCw = Cw
lCm = Cm
lNs = Ns

 eps = 2.0^(-52.0);;

 ;data=dataSpec_P5B();; read it in the common block
 ;l=REFORM(data(0,*));;
 ;n=REFORM(data(1,*));;
 l=data(0,*);;
 n=data(1,*);;
 k=(Cab*data(2,*)+Car*data(3,*)+Cbrown*data(4,*)+Cw*data(5,*)+Cm*data(6,*))/Ns;;
  
 k_0 = where(k eq 0, k_0_count);;
 if (k_0_count gt 0) then k(k_0) = eps;;
 
 trans=(1.0-k)*exp(-k)+k^2.0*expint(1,k);;

; ***********************************************************************
; reflectance and transmittance of one layer
; ***********************************************************************
; Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969),
; Interaction of isotropic ligth with a compact plant leaf, J. Opt.
; Soc. Am., 59(10):1376-1379.
; ***********************************************************************

; reflectivity and transmissivity at the interface
;-------------------------------------------------
 t12=tav(40.0,n);;
 t21=tav(90.0,n)/(n^2.0);; 
 r12=1.0-t12;;
 r21=1.0-t21;;
 x=tav(40.0,n)/tav(90.0,n);;
 y=x*(tav(90.0,n)-1.0)+1.0-tav(40.0,n);;

; reflectance and transmittance of the elementary layer N = 1
;------------------------------------------------------------

 ra=r12+(t12*t21*r21*trans^2.0d)/(1.0-r21^2.0*trans^2.0d);;
 ta=(t12*t21*trans)/(1.0-r21^2.0*trans^2.0);;
 r90=(ra-y)/x;;
 t90=ta/x;;

; ***********************************************************************
; reflectance and transmittance of N layers
; ***********************************************************************
; Stokes G.G. (1862), On the intensity of the light reflected from
; or transmitted through a pile of plates, Proc. Roy. Soc. Lond.,
; 11:545-556.
; ***********************************************************************
 delta=sqrt((t90^2.0-r90^2.0-1.0)^2.0-4.0*r90^2.0);;
 
 delta0_index = where(delta lt 0.0, delta0_count);;
 if (delta0_count gt 0.0) then delta(delta0_index) = eps;;

 beta=double(1.0+r90^2.0-t90^2.0-delta)/(2.0*r90);;
 va=double(1.0+r90^2.0-t90^2.0+delta)/(2.0*r90);;

 vb_base = beta-r90;;
 vb = vb_base
 vb_index = where(vb_base le 0.0, vb_count);;
 if (vb_count gt 0.0) then vb(vb_index)=sqrt(beta(vb_index)*(va(vb_index)-r90(vb_index))/(va(vb_index)*eps));;
 
 vb_index2 = where(vb_base gt 0.0, vb_count2);;
 if (vb_count2 gt 0.0) then vb(vb_index2)=sqrt((beta(vb_index2)*(va(vb_index2)-r90(vb_index2)))/(va(vb_index2)*(beta(vb_index2)-r90(vb_index2))));; 
  
 vbNN = vb^(Ns-1.0);;
 vbNNinv = 1.0/vbNN;;
 vainv = 1.0/va;;
 
 s1=ta*t90*(vbNN-vbNNinv);;
 s2=ta*(va-vainv);;
 s3=va*vbNN-vainv*vbNNinv-r90*(vbNN-vbNNinv);;

 RN=ra+s1/s3;;
 TN=s2/s3;;
 lRN = RN
 lTN = TN
end;