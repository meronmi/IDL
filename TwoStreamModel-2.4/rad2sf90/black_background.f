! ---------------------------------------------------
! File            : black_background.f
! Author          : Lavergne Thomas                          
! Creation Date   : 11 August 2005                            
! Purpose         :                                  
! 	Radiative tranfer module for the Black Background 
!       2-streams model.                                      
! ---------------------------------------------------

MODULE black_background

IMPLICIT NONE

PRIVATE

PUBLIC :: dhrT1, bhrT1


CONTAINS

FUNCTION dhrT1(rl,tl,gamma1,gamma2,gamma3,gamma4,tta,tau,AlbBS,Tdif,AbsVgt)
   REAL(KIND=kind(1.0d0)), INTENT(IN) :: rl,tl,gamma1,gamma2,gamma3,gamma4,tta,tau
   REAL(KIND=kind(1.0d0)), INTENT(OUT) :: AlbBS,Tdif,AbsVgt
   LOGICAL :: dhrT1

   REAL(KIND=kind(1.0d0)) :: alpha1,alpha2,ksquare,k
   REAL(KIND=kind(1.0d0)) :: first_term,secnd_term1,secnd_term2,secnd_term3
   REAL(KIND=kind(1.0d0)) :: expktau,Tdir
   REAL(KIND=kind(1.0d0)) :: mu0,w0 

   mu0=cos(tta)
   w0=rl+tl

 
   Tdir = exp(-tau/mu0)

   ! There is a difference between conservative and non-conservative scattering conditions */
   IF (w0 .ne. 1.0 .AND. w0 .ne. 0.0) THEN
      !NON_CONSERVATIVE SCATTERING

      ! some additional parameters 
      alpha1  = gamma1*gamma4 + gamma2*gamma3
      alpha2  = gamma1*gamma3 + gamma2*gamma4
      ksquare = gamma1*gamma1 - gamma2*gamma2
      k       = sqrt(ksquare)
      
      expktau = exp(k*tau)

      !Black Soil Albedo
      first_term  = ((1.0d0-ksquare*mu0*mu0)*((k+gamma1)*expktau + (k-gamma1)/expktau))
      IF (first_term .eq. 0.0) THEN
         !we will be dividing by zero : cannot continue.
         dhrT1 = .false.
      ELSE 
         first_term = 1.0d0/first_term
         secnd_term1 = (1.0d0 - k*mu0)*(alpha2 + k*gamma3)*expktau
         secnd_term2 = (1.0d0 + k*mu0)*(alpha2 - k*gamma3)/expktau
         secnd_term3 = 2.0d0 * k * (gamma3 - alpha2*mu0)*Tdir
         AlbBS = (w0 * first_term * (secnd_term1 - secnd_term2 - secnd_term3))
     
         !Transmission
         IF (ksquare .eq. 0.0) THEN 
            first_term = 1.0d0
         ENDIF
         secnd_term1 = (1.0d0+k*mu0)*(alpha1+k*gamma4)*expktau
         secnd_term2 = (1.0d0-k*mu0)*(alpha1-k*gamma4)/expktau
         secnd_term3 = 2.0d0 * k * (gamma4 + alpha1*mu0)
         Tdif = - w0*first_term*(Tdir*(secnd_term1 - secnd_term2) - secnd_term3)
     
         !Absorption by vegetation
         AbsVgt = (1.0d0- (Tdif+Tdir) - AlbBS)
      ENDIF
   ELSE IF (w0 .eq. 0.) THEN
      !BLACK CANOPY
      AlbBS = 0.0d0
      Tdif  = 0.0d0
      AbsVgt = 1.0d0 - Tdir
   ELSE
      !CONSERATIVE SCATTERING
      AlbBS =  (1.0d0/(1.0d0 + gamma1*tau))*(gamma1*tau + (gamma3-gamma1*mu0)*(1.0d0-exp(-tau/mu0)));
      Tdif   = 1.0d0 - AlbBS - Tdir;
      AbsVgt = 0.0d0;
   ENDIF
    
   dhrT1 = .true.
END FUNCTION dhrT1

FUNCTION bhrT1(rl,tl,gamma1,gamma2,gamma3,gamma4,tau,AlbBS,Tdif,AbsVgt)
   REAL(KIND=kind(1.0d0)), INTENT(IN) :: rl,tl,gamma1,gamma2,gamma3,gamma4,tau
   REAL(KIND=kind(1.0d0)), INTENT(OUT) :: AlbBS,Tdif,AbsVgt
   LOGICAL :: bhrT1
   
   REAL(8) :: a=0.705d0
   bhrT1=dhrT1(rl,tl,gamma1,gamma2,gamma3,gamma4,acos(0.5d0/a),tau,AlbBS,Tdif,AbsVgt)
END FUNCTION bhrT1

END MODULE black_background



