!* ---------------------------------------------------
! * File            : Simple2stream.f                       
! * Author          : Lavergne Thomas                          
! * Creation Date   : 13 August 2005                            
! * Purpose         :                                  
! *
! * 	Very simple main program unit to compute the total 2stream solution
! * 	in reflectance, transmittance and absorption. The whole user interface
! * 	is skipped, only the "full" model is coded and solutions of the isotropic
! * 	and collimated illuminations are not weighted by the fdiff ratio. 
! * 
! * Rationale       :
! * 	This main unit is not used conveniently and is only intended at helping the
! * 	integration of the present 2stream model in other codes by presenting a simple
! * 	procedural interface.
! * ---------------------------------------------------
! */

!/*
! * Copyright (C) 2005 by STARS
! *
! * 	Academic users:
! * 	Are authorized to use this code for research and teaching,
! * 	but must acknowledge the use of these routines explicitly
! * 	and refer to the references in any publication or work.
! * 	Original, complete, unmodified versions of these codes may be
! * 	distributed free of charge to colleagues involved in similar
! * 	activities.  Recipients must also agree with and abide by the
! * 	same rules. The code may not be sold, nor distributed to
! * 	commercial parties, under any circumstances.
! *
! * 	Commercial and other users:
! * 	Use of this code in commercial applications is strictly
! * 	forbidden without the written approval of the authors.
! * 	Even with such authorization the code may not be distributed
! * 	or sold to any other commercial or business partners under
! * 	any circumstances.
! * 
! * This software is provided as is without any warranty whatsoever.
! */

PROGRAM Simple2stream

USE black_background
USE black_canopy
USE gammaproc

IMPLICIT NONE

REAL(KIND=kind(1.0d0)),PARAMETER :: pi = 3.14159265358979323846d0

REAL(KIND=kind(1.0d0)),DIMENSION(3) :: sun_zenith_angle_degrees = (/ 0.0d0,30.0d0,65.0d0 /)
REAL(KIND=kind(1.0d0)),DIMENSION(3) :: true_lai = (/ 1.5d0,3.0d0,5.0d0 /)
REAL(KIND=kind(1.0d0)),DIMENSION(3) :: structure_factor_zeta  = (/0.6d0,0.67d0,0.72d0/)
REAL(KIND=kind(1.0d0)),DIMENSION(3) :: structure_factor_ratio = (/1.0d0,1.15d0,1.30d0/)
REAL(KIND=kind(1.0d0)),DIMENSION(4) :: leaf_single_scattering_albedo = (/0.12,0.19,0.76,0.85 /)
REAL(KIND=kind(1.0d0)),DIMENSION(3) :: leaf_forward_efficiency = (/ 0.5d0,1.0d0,5.0d0 /)
REAL(KIND=kind(1.0d0)),DIMENSION(3) :: background_reflectance = (/ 0.21d0,0.62d0,0.87d0 /)

REAL(KIND=kind(1.0d0)) :: leaf_reflectance, leaf_transmittance,structure_factor_zetaStar

REAL(KIND=kind(1.0d0)) :: Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot
REAL(KIND=kind(1.0d0)) :: Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot

INTEGER :: i_sun,i_lai,i_zeta,i_zetaRat,i_wl,i_dl,i_bgd

write (0,*) "Angle <LAI>  zeta   zeta*  rleaf   tleaf  rbgd  Rtot(sun)  Ttot(sun)  Atot(sun)  Rtot(iso)  Ttot(iso)  Atot(iso)"

DO i_lai = 1,3
   DO i_zeta = 1,3
      DO i_zetaRat = 1,3
         DO i_wl = 1,4
            DO i_dl = 1,3
               DO i_bgd = 1,3
                  DO i_sun=1,3

                     ! calculate the rleaf and tleaf from wl=rl+tl and dl=rl/tl
                     leaf_transmittance = leaf_single_scattering_albedo(i_wl)/ & 
                        (leaf_forward_efficiency(i_dl)+1)
                     leaf_reflectance = leaf_forward_efficiency(i_dl) * &
                        leaf_transmittance

                     ! calculate a "star" structure factor
                     structure_factor_zetaStar = &
                     structure_factor_ratio(i_zetaRat)*structure_factor_zeta(i_zeta)

                     ! call the 2stream routine
                     call twostream_solver(leaf_reflectance,leaf_transmittance,background_reflectance(i_bgd),&
                 true_lai(i_lai),structure_factor_zeta(i_zeta),structure_factor_zetaStar,sun_zenith_angle_degrees(i_sun),&
                 Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot,Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot)
                  
                    ! print input values and calculated fluxes on a single line
                    write(*,FMT="(F5.2,' ',F5.3,' ',2(F6.4,' '),2(F7.5,' '),F5.3,' ',6(F10.8,' '))") &
                       sun_zenith_angle_degrees(i_sun),true_lai(i_lai),&
                       structure_factor_zeta(i_zeta),structure_factor_zetaStar,&
                       leaf_reflectance,leaf_transmittance,background_reflectance(i_bgd),&
                       Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot,&
                       Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot

                  END DO
               END DO
            END DO
         END DO
      END DO
   END DO
END DO

write (0,*) "Angle <LAI>  zeta   zeta*  rleaf   tleaf  rbgd  Rtot(sun)  Ttot(sun)  Atot(sun)  Rtot(iso)  Ttot(iso)  Atot(iso)"



CONTAINS
SUBROUTINE twostream_solver(leaf_reflectance,leaf_transmittance,background_reflectance,&
   true_lai,structure_factor_zeta,structure_factor_zetaStar,sun_zenith_angle_degrees,&
   Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot,Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot)

   REAL(KIND=kind(1.0d0)), INTENT(IN) :: leaf_reflectance,leaf_transmittance,background_reflectance
   REAL(KIND=kind(1.0d0)), INTENT(IN) :: true_lai,structure_factor_zeta,structure_factor_zetaStar,sun_zenith_angle_degrees
   REAL(KIND=kind(1.0d0)), INTENT(OUT) :: Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot
   REAL(KIND=kind(1.0d0)), INTENT(OUT) :: Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot

   !internal and intermediate variables
   LOGICAL :: ok
   REAL(KIND=kind(1.0d0)), DIMENSION(4) :: gammaCoeffs,gammaCoeffs_star
   REAL(KIND=kind(1.0d0)) :: tauprimetilde,tauprimestar,sun_zenith_angle_radians
   REAL(KIND=kind(1.0d0)) :: cosine_sun_angle
   REAL(KIND=kind(1.0d0)), PARAMETER :: isotropic_cosine_constant=0.5/0.705

   !calculated fluxes
   REAL(KIND=kind(1.0d0)) :: Collim_Alb_BB, Collim_Tran_BB, Collim_Abs_BB
   REAL(KIND=kind(1.0d0)) :: Isotrop_Alb_BB,Isotrop_Tran_BB,Isotrop_Abs_BB
   REAL(KIND=kind(1.0d0)) :: Collim_Tran_BC, Isotrop_Tran_BC
   REAL(KIND=kind(1.0d0)) :: Collim_Tran_TotalOneWay,Isotrop_Tran_TotalOneWay
   REAL(KIND=kind(1.0d0)) :: Bellow_reinject_rad

   ! convert angular values
   sun_zenith_angle_radians = pi * sun_zenith_angle_degrees / 180.00;
   cosine_sun_angle = cos(sun_zenith_angle_radians);

   ! calculate the 4 gamma coefficients both in isotropic and collimated illumination 
   call gammas(leaf_reflectance,leaf_transmittance,cosine_sun_angle,gammaCoeffs)
   call gammas(leaf_reflectance,leaf_transmittance,isotropic_cosine_constant,gammaCoeffs_star)
   
   ! estimate the effective value of the optical thickness 
   tauprimetilde = 0.5d0 * true_lai * structure_factor_zeta
   tauprimestar  = 0.5d0 * true_lai * structure_factor_zetaStar

   ! +++++++++++++ BLACK BACKGROUND ++++++++++++++++++++++ 
   ! * Apply the black-background 2stream solution 
   ! * These equations are written for the part of the incoming radiation 
   ! * that never hits the background but does interact with the vegetation 
   ! * canopy.
   ! * 
   ! * Note : the same routine dhrT1() is used for both the isotropic and
   ! * collimated illumination conditions but the calling arguments differ.
   ! * (especially the solar angle used).
   ! */

   !	/* 1) collimated source */
   ok=dhrT1(leaf_reflectance,leaf_transmittance,&
	 gammaCoeffs(1),gammaCoeffs(2),gammaCoeffs(3),gammaCoeffs(4),&
         sun_zenith_angle_radians,tauprimetilde,&
	 Collim_Alb_BB,Collim_Tran_BB,Collim_Abs_BB)
   ! 	/* 2) isotropic source */
   ok=dhrT1(leaf_reflectance,leaf_transmittance,&
	 gammaCoeffs_star(1),gammaCoeffs_star(2),gammaCoeffs_star(3),gammaCoeffs_star(4),&
         acos(isotropic_cosine_constant),tauprimestar,&
	 Isotrop_Alb_BB,Isotrop_Tran_BB,Isotrop_Abs_BB)

   ! +++++++++++++ BLACK CANOPY ++++++++++++++++++++++ 
   ! * Apply the black canopy solution.
   ! * These equations hold for the part of the incoming radiation 
   ! * that do not interact with the vegetation, travelling through 
   ! * its gaps.
   ! */

  ! 	/* 1) collimated source */
   Collim_Tran_BC  = exp( - tauprimetilde/cosine_sun_angle)
  ! 	/* 2) isotropic source */
   Isotrop_Tran_BC = TBarreUncoll_exact(tauprimestar)

   ! /* Total one-way transmissions:
   ! * The vegetation canopy is crossed (one way) by the uncollided radiation
   ! * (black canopy) and the collided one (black background). */

   !	/* 1) collimated source */
   Collim_Tran_TotalOneWay  = Collim_Tran_BC  + Collim_Tran_BB 
   !	/* 2) isotropic source */
   Isotrop_Tran_TotalOneWay = Isotrop_Tran_BC + Isotrop_Tran_BB 
   
   ! * The Bellow_reinject_rad describes the process of reflecting toward the background
   ! * the upward travelling radiation (re-emitted from bellow the canopy). It appears in 
   ! * the coupling equations as the limit of the series: 
   ! *    1 + rg*rbv + (rg*rbv)^2 + (rg*rbv)^3 + ...
   ! *      where rg is the background_reflectance and rbv is Isotrop_Alb_BB
   ! *      (with Isotrop describing the Lambertian reflectance of the background).
   ! */
   Bellow_reinject_rad = 1.0d0 / (1.0d0 - background_reflectance*Isotrop_Alb_BB)

   !/* TOTAL ALBEDO */
   ! 	/* 1) collimated source */
   Collim_Alb_Tot = Collim_Alb_BB + &
      background_reflectance * Collim_Tran_TotalOneWay  * Isotrop_Tran_TotalOneWay * Bellow_reinject_rad 
   !	/* 2) isotropic source */
   Isotrop_Alb_Tot = Isotrop_Alb_BB + & 
      background_reflectance * Isotrop_Tran_TotalOneWay * Isotrop_Tran_TotalOneWay * Bellow_reinject_rad 
   
   !/* TOTAL TRANSMITION TO THE BACKGROUND LEVEL */
   !	/* 1) collimated source */
   Collim_Tran_Tot = Collim_Tran_TotalOneWay * Bellow_reinject_rad ;
   !	/* 2) isotropic source */
   Isotrop_Tran_Tot = Isotrop_Tran_TotalOneWay * Bellow_reinject_rad ;

   !/* TOTAL ABSORPTION BY THE VEGETATION LAYER */
   !	/* 1) collimated source */
   Collim_Abs_Tot = 1.0d0 - (Collim_Tran_Tot + Collim_Alb_Tot) + background_reflectance * Collim_Tran_Tot;
   !	/* 2) isotropic source */
   Isotrop_Abs_Tot = 1.0d0 - (Isotrop_Tran_Tot + Isotrop_Alb_Tot) + background_reflectance * Isotrop_Tran_Tot;

END SUBROUTINE twostream_solver

END PROGRAM Simple2stream
