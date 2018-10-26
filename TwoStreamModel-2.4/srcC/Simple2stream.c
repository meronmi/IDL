/* ---------------------------------------------------
 * File            : Simple2stream.c                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 08 August 2005                            
 * Purpose         :                                  
 *
 * 	Very simple main program unit to compute the total 2stream solution
 * 	in reflectance, transmittance and absorption. The whole user interface
 * 	is skipped, only the "full" model is coded and solutions of the isotropic
 * 	and collimated illuminations are not weighted by the fdiff ratio. 
 * 
 * Rationale       :
 * 	This main unit is not used conveniently and is only intended at helping the
 * 	integration of the present 2stream model in other codes by presenting a simple
 * 	procedural interface.
 * ---------------------------------------------------
 */

/*
 * Copyright (C) 2005 by STARS
 *
 * 	Academic users:
 * 	Are authorized to use this code for research and teaching,
 * 	but must acknowledge the use of these routines explicitly
 * 	and refer to the references in any publication or work.
 * 	Original, complete, unmodified versions of these codes may be
 * 	distributed free of charge to colleagues involved in similar
 * 	activities.  Recipients must also agree with and abide by the
 * 	same rules. The code may not be sold, nor distributed to
 * 	commercial parties, under any circumstances.
 *
 * 	Commercial and other users:
 * 	Use of this code in commercial applications is strictly
 * 	forbidden without the written approval of the authors.
 * 	Even with such authorization the code may not be distributed
 * 	or sold to any other commercial or business partners under
 * 	any circumstances.
 * 
 * This software is provided as is without any warranty whatsoever.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "commonrad.h"
#include "common.h" 

#include "gammaproc.h"
#include "black_canopy.h"
#include "black_background.h"


static void twostream_solver(double leaf_reflectance,double leaf_transmittance,double background_reflectance, 
      double true_lai,double structure_factor_zeta,double structure_factor_zetaStar,double sun_zenith_angle_degrees,
      double *Collim_Alb_Tot, double *Collim_Tran_Tot, double *Collim_Abs_Tot,
      double *Isotrop_Alb_Tot,double *Isotrop_Tran_Tot, double *Isotrop_Abs_Tot);

/*
 * MAIN ROUTINE
 */

int main(void)
{
   /* model's input variables */
   double sun_zenith_angle_degrees[3]={0.,30.,65.};
   double true_lai[3] = {1.5,3.0,5.0};
   double structure_factor_zeta[3] = {0.6,0.67,0.72};
   double structure_factor_ratio[3] = {1.,1.15,1.3};
   double leaf_single_scattering_albedo[4] = {0.12,0.19,0.76,0.85};
   double leaf_forward_efficiency[3] = {0.5,1.0,5.};
   double background_reflectance[3] = {0.21,0.62,0.87};
  
   double leaf_reflectance, leaf_transmittance, structure_factor_zetaStar;
   
   /* models's output fluxes */
   double Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot;
   double Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot;

   /* loops counters */
   int i_sun,i_lai,i_zeta,i_zetaRat,i_wl,i_dl,i_bgd;
   
   /* write a first ASCII line to name the columns */
   fprintf(stderr,"Angle <LAI>  zeta   zeta*  rleaf   tleaf  rbgd  Rtot(sun)  Ttot(sun)  Atot(sun)  Rtot(iso)  Ttot(iso)  Atot(iso)\n");
   
   /* begin the loops over all conditions: 3*3*3*3*4*3*3=2916 conditions */
   for (i_lai=0 ; i_lai < 3 ; i_lai++) {
      for (i_zeta=0 ; i_zeta < 3 ; i_zeta++) {
	 for (i_zetaRat=0; i_zetaRat < 3; i_zetaRat++) {
            for (i_wl=0 ; i_wl < 4 ; i_wl++) {
	       for (i_dl=0 ; i_dl < 3 ; i_dl++) {
		  for (i_bgd=0 ; i_bgd < 3 ; i_bgd++) {
                     for (i_sun=0; i_sun < 3; i_sun++) {
			
			/* calculate the rleaf and tleaf from wl=rl+tl and dl=rl/tl */
			leaf_transmittance = leaf_single_scattering_albedo[i_wl]/(leaf_forward_efficiency[i_dl]+1.);
			leaf_reflectance = leaf_forward_efficiency[i_dl] * leaf_transmittance;

			/* calculate a "star" structure factor */
			structure_factor_zetaStar = structure_factor_ratio[i_zetaRat]*structure_factor_zeta[i_zeta];
                        
			/* call the 2stream routine */
                        twostream_solver(leaf_reflectance,leaf_transmittance,background_reflectance[i_bgd],
	                   true_lai[i_lai],structure_factor_zeta[i_zeta],structure_factor_zetaStar,sun_zenith_angle_degrees[i_sun],
	                   &Collim_Alb_Tot,&Collim_Tran_Tot,&Collim_Abs_Tot,
                           &Isotrop_Alb_Tot,&Isotrop_Tran_Tot,&Isotrop_Abs_Tot);

			/* print input values and calculated fluxes on a single line */
                        printf("%05.2f %5.3f %6.4f %6.4f %7.5f %7.5f %5.3f %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f\n",
			      sun_zenith_angle_degrees[i_sun],true_lai[i_lai],
			      structure_factor_zeta[i_zeta],structure_factor_zetaStar,
			      leaf_reflectance,leaf_transmittance,background_reflectance[i_bgd],
			      Collim_Alb_Tot,Collim_Tran_Tot,Collim_Abs_Tot,
			      Isotrop_Alb_Tot,Isotrop_Tran_Tot,Isotrop_Abs_Tot);
		     }
		  }
	       }
	    }
	 }
      }
   } 
   /* recall the columns' name */
   fprintf(stderr,"Angle <LAI>  zeta   zeta*  rleaf   tleaf  rbgd  Rtot(sun)  Ttot(sun)  Atot(sun)  Rtot(iso)  Ttot(iso)  Atot(iso)\n");

}


void twostream_solver(double leaf_reflectance,double leaf_transmittance,double background_reflectance, 
      double true_lai,double structure_factor_zeta,double structure_factor_zetaStar,double sun_zenith_angle_degrees,
      double *Collim_Alb_Tot, double *Collim_Tran_Tot, double *Collim_Abs_Tot,
      double *Isotrop_Alb_Tot,double *Isotrop_Tran_Tot, double *Isotrop_Abs_Tot) {

   /* intermediate variables */
   int    ok;
   double gammaCoeffs[4],gammaCoeffs_star[4];
   double tauprimetilde,tauprimestar;
   double sun_zenith_angle_radians;
   double cosine_sun_angle;   
   double isotropic_cosine_constant=0.5/0.705;

   /* calculated fluxes */
   double Collim_Alb_BB, Collim_Tran_BB, Collim_Abs_BB;
   double Isotrop_Alb_BB,Isotrop_Tran_BB,Isotrop_Abs_BB;
   double Collim_Tran_BC, Isotrop_Tran_BC;
   double Collim_Tran_TotalOneWay,Isotrop_Tran_TotalOneWay;
   double Bellow_reinject_rad;

   /* convert angular values */
   sun_zenith_angle_radians = PI * sun_zenith_angle_degrees / 180.00;
   cosine_sun_angle = cos(sun_zenith_angle_radians);
   
   /* calculate the 4 gamma coefficients both in isotropic and collimated illumination */
   gammas(leaf_reflectance,leaf_transmittance,cosine_sun_angle,gammaCoeffs);
   gammas(leaf_reflectance,leaf_transmittance,isotropic_cosine_constant,gammaCoeffs_star);
   
   /* estimates the effective value of the optical thickness */
   tauprimetilde = 0.5 * true_lai * structure_factor_zeta;
   tauprimestar  = 0.5 * true_lai * structure_factor_zetaStar;

   /* +++++++++++++ BLACK BACKGROUND ++++++++++++++++++++++ 
    * Apply the black-background 2stream solution 
    * These equations are written for the part of the incoming radiation 
    * that never hits the background but does interact with the vegetation 
    * canopy.
    * 
    * Note : the same routine dhrT1() is used for both the isotropic and
    * collimated illumination conditions but the calling arguments differ.
    * (especially the solar angle used).
    */

   	/* 1) collimated source */
   dhrT1(leaf_reflectance,leaf_transmittance,
	 gammaCoeffs[0],gammaCoeffs[1],gammaCoeffs[2],gammaCoeffs[3],
         sun_zenith_angle_radians,tauprimetilde,
	 &Collim_Alb_BB,&Collim_Tran_BB,&Collim_Abs_BB,&ok);
   	/* 2) isotropic source */
   dhrT1(leaf_reflectance,leaf_transmittance,
	 gammaCoeffs_star[0],gammaCoeffs_star[1],gammaCoeffs_star[2],gammaCoeffs_star[3],
         acos(isotropic_cosine_constant),tauprimestar,
	 &Isotrop_Alb_BB,&Isotrop_Tran_BB,&Isotrop_Abs_BB,&ok);

   
   /* +++++++++++++ BLACK CANOPY ++++++++++++++++++++++ 
    * Apply the black canopy solution.
    * These equations hold for the part of the incoming radiation 
    * that do not interact with the vegetation, travelling through 
    * its gaps.
    */

   	/* 1) collimated source */
   Collim_Tran_BC  = exp( - tauprimetilde/cosine_sun_angle);
   	/* 2) isotropic source */
   TBarreUncoll_exact(tauprimestar,&Isotrop_Tran_BC);

   /* +++++++++++++ COUPLING EQUATIONS +++++++++++++++++++ 
    */
   
   /* Total one-way transmissions:
    * The vegetation canopy is crossed (one way) by the uncollided radiation
    * (black canopy) and the collided one (black background). */

   	/* 1) collimated source */
   Collim_Tran_TotalOneWay  = Collim_Tran_BC  + Collim_Tran_BB ;
   	/* 2) isotropic source */
   Isotrop_Tran_TotalOneWay = Isotrop_Tran_BC + Isotrop_Tran_BB ;

   /* The Bellow_reinject_rad describes the process of reflecting toward the background
    * the upward travelling radiation (re-emitted from bellow the canopy). It appears in 
    * the coupling equations as the limit of the series: 
    *    1 + rg*rbv + (rg*rbv)^2 + (rg*rbv)^3 + ...
    *      where rg is the background_reflectance and rbv is Isotrop_Alb_BB
    *      (with Isotrop describing the Lambertian reflectance of the background).
    */
   Bellow_reinject_rad = 1.0 / (1.0 - background_reflectance*Isotrop_Alb_BB);
 
   /* TOTAL ALBEDO */
   	/* 1) collimated source */
   *Collim_Alb_Tot = Collim_Alb_BB + 
      background_reflectance * Collim_Tran_TotalOneWay  * Isotrop_Tran_TotalOneWay * Bellow_reinject_rad ;
   	/* 2) isotropic source */
   *Isotrop_Alb_Tot = Isotrop_Alb_BB + 
      background_reflectance * Isotrop_Tran_TotalOneWay * Isotrop_Tran_TotalOneWay * Bellow_reinject_rad ;
   
   /* TOTAL TRANSMITION TO THE BACKGROUND LEVEL */
   	/* 1) collimated source */
   *Collim_Tran_Tot = Collim_Tran_TotalOneWay * Bellow_reinject_rad ;
   	/* 2) isotropic source */
   *Isotrop_Tran_Tot = Isotrop_Tran_TotalOneWay * Bellow_reinject_rad ;

   /* TOTAL ABSORPTION BY THE VEGETATION LAYER */
   	/* 1) collimated source */
   *Collim_Abs_Tot = 1. - (*Collim_Tran_Tot + *Collim_Alb_Tot) + background_reflectance * *Collim_Tran_Tot;
   	/* 2) isotropic source */
   *Isotrop_Abs_Tot = 1. - (*Isotrop_Tran_Tot + *Isotrop_Alb_Tot) + background_reflectance * *Isotrop_Tran_Tot;
 
}
