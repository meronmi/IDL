! ---------------------------------------------------
! File            : gammaproc.f
! Author          : Lavergne Thomas                          
! Creation Date   : 11 August 2005                            
! Purpose         :                                  
!     Isolate the low-level procedures for the gamma coefficients
!     calculations.
! ---------------------------------------------------

MODULE gammaproc

IMPLICIT NONE

PRIVATE

PUBLIC :: gammas


CONTAINS

SUBROUTINE gammas(rl,tl,mu0,gammaCoeff)
   REAL(8), INTENT(IN) :: rl,tl,mu0
   REAL(8), DIMENSION(:), INTENT(OUT) :: gammaCoeff 

   REAL(8) :: wd,w0,w0half,wdsixth

   w0      = rl+tl
   wd      = rl-tl
   w0half  = w0*0.5_8
   wdsixth = wd/6.0_8

   gammaCoeff(1)=2.*(1. - w0half + wdsixth)
   gammaCoeff(2)=2.*(w0half + wdsixth)
   gammaCoeff(3)=2.*(w0half*0.5 + mu0*wdsixth)/w0
   gammaCoeff(4)=1.-gammaCoeff(3)
   
END SUBROUTINE gammas

END MODULE gammaproc



