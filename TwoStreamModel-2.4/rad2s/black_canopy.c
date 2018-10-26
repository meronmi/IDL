/* ---------------------------------------------------
 * File            : black_canopy.c
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2004                            
 * Purpose         :                                  
 * 	Radiative tranfer module for the Black Canopy component.                                      
 * ---------------------------------------------------
*/


#include <math.h>
#include "black_canopy.h"


/*++++++++++ PROCEDURE-LIKE INTERFACE ++++++++++*/
/* Implements equation 16 of the JGR paper */
void TBarreUncoll_exact(const double tau,double *tbu)
{
   unsigned int j,ind;

   /* the incomplete Gamma function is calculated using a 
    * continued fraction development as in Numerical Recipes */
   double iGammaloc=0.;
   unsigned int order=20;
   
   for (j=0 ; j < order; j++) {
      ind = order - j;
      iGammaloc = ind / (1. + ind/(tau+iGammaloc));
   }
   iGammaloc=1./(tau + iGammaloc);
   *tbu = exp(-tau)*(1. - tau + tau*tau*iGammaloc);

}
/* Implements equation 18 of the JGR paper */
void TBarreUncoll_approxA(const double tau, double *tbu)
{
   *tbu = exp(-tau)/(1.+tau);
}

/* Implements equation 18 of the JGR paper with the tuning of the 0.5 factor into a 0.45
 * For the current variable: tau = LAI*0.5 this factor is 0.45/0.5=0.90 */
void TBarreUncoll_approxATuned(const double tau,double *tbu)
{
   double a = tau*0.90;
   *tbu = exp(-a)/(1.+a);
}


/* Implements equation 19 of the JGR paper */
void TBarreUncoll_approxB(const double tau,double *tbu)
{
   *tbu = exp(-2.*tau);
}

/* Implements equation 19 of the JGR paper with a 0.705 tuning factor to get near the exact solution (eq. 16) 
 * For the current variable tau = lai/2., this factor becomes 1.41 */
void TBarreUncoll_approxBTuned(const double tau, double *tbu)
{
   *tbu = exp(-1.41*tau);
}

/* Direct transmission through the canopy gaps for a collimated radiation */
void TdirCollimated(const double tau,const double tta0,double *tdir) 
{
   *tdir = exp(-tau/cos(tta0));
}

/* Reflectance's "shape" for a collimated beam travelling downwards through
   the canopy gaps before being diffused by a Lambertian background. The diffuse
   flux created at z_bgd travels then back through the canopy gaps to reach z_toc.
   This is equation 13, where the background reflectance Rbgd has been factorized
   to only yields the "shape" of the radiative signal. */
void R_T2Collimated(const double tau, const double tta0, const double taustar, TbarreUncoll_P tbu, double *rt2)
{
   double loctbu,loctdir;
   (*tbu)(taustar,&loctbu);
   TdirCollimated(tau,tta0,&loctdir);
   *rt2 = loctdir*loctbu;
}

/* Reflectance's "shape" for an isotropic illumination travelling downwards through
   the canopy gaps before being diffused by a Lambertian background. The diffuse
   flux created at z_bgd travels then back through the canopy gaps to reach z_toc.
   This quantity enters e.g. in equation 36 as TBarreUncoll^2. The background 
   reflectance Rbgd has been factorized to only yields the "shape" of the radiative 
   signal. */
void R_T2Isotropic(const double taustar, TbarreUncoll_P tbu, double *rt2)
{
   double loctbu;
   (*tbu)(taustar,&loctbu);  
   *rt2 = pow(loctbu,2); 
}
