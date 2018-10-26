! ---------------------------------------------------
! File            : black_canopy.f
! Author          : Lavergne Thomas                          
! Creation Date   : 12 August 2005                            
! Purpose         :                                  
! 	Radiative tranfer module for the Black Canopy component
! ---------------------------------------------------

MODULE black_canopy

IMPLICIT NONE

PRIVATE

PUBLIC :: TBarreUncoll_exact


CONTAINS

FUNCTION TBarreUncoll_exact(tau)
   REAL(KIND=kind(1.0d0)), INTENT(IN) :: tau
   REAL(KIND=kind(1.0d0)) :: TBarreUncoll_exact

   INTEGER :: j,ind
   REAL(KIND=kind(1.0d0)) :: iGammaloc
   INTEGER :: order

   iGammaloc = 0.0d0
   order=20
   
   DO j=0,order-1
      ind = order - j
      iGammaloc = ind / (1.0d0 + ind/(tau+iGammaloc))
   END DO
   iGammaloc=1.0d0/(tau + iGammaloc)
   
   TBarreUncoll_exact = exp(-tau)*(1.0d0 - tau + tau*tau*iGammaloc)
END FUNCTION TBarreUncoll_exact


END MODULE black_canopy



